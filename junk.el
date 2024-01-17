;;; junk.el --- Package expansion packs -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/junk
;; Version: 0.2.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: extensions

;;; Commentary:

;; `junk' is a macro to define packages as belonging to a common group
;; with optional extras.
;;
;; The entry-point is `junk-expand' to define.

;;; Code:

(require 'cl-lib)
(require 'package)

(defvar junk-expansion-packs nil
  "Packs of expansion packages to be installed using `junk-install'.

Individual languages build this list using macro `junk'.")

;; Macros

(cl-defmacro junk--with-parts (item (&key with-docs &allow-other-keys) &rest body)
  "Execute BODY with an ITEM's junk parts bound.

Also bind docs if WITH-DOCS is t."
  (declare (indent defun))

  `(cl-destructuring-bind (packages extras recipes ,(if with-docs 'docs '_docs)) (junk--parts ,item)
     ,@body))

;; Installation

(cl-defun junk-install--packages (packages &key delete-windows installer)
  "Install PACKAGES.

Calls `delete-other-windows' if DELETE-WINDOWS is t.

Uses `package-install' unless custom INSTALLER is provided."
  (let ((installer (or installer #'package-install)))

    (mapc installer packages)

    (when delete-windows
      (delete-other-windows))))

(defun junk-install--pack (pack)
  "Install expansion PACK."
  (junk--with-parts pack nil
    (let ((name (symbol-name (car pack)))
          (normal (junk--filter packages))
          (from-recipe (junk--filter recipes :mapper #'car)))

      (if (not (append normal from-recipe))
          (if (and (junk--filter extras)
                   (yes-or-no-p (format "Want to install an extra for '%s'?" name)))
              (junk-install--extras extras)
            (message "Package '%s' is already installed." name))
        (junk-install--packages normal :delete-windows t)
        (junk-install--packages from-recipe :installer 'junk-install--recipe)
        (message "Installed '%s'." name)))))

(defun junk-install--recipe (recipe)
  "Install RECIPE."
  (unless (fboundp 'package-vc-install)
    (user-error "Recipes can only be installed with `package-vc-install'"))

  (package-vc-install recipe)
  (package--update-selected-packages (list (car recipe)) nil))

(defun junk-install--extras (extras)
  "Install one or all packages in EXTRAS."
  (let* ((selection (intern-soft
                     (completing-read
                      "Select extra to install: " (append extras '(all))))))

    (pcase selection
      ('all
       (junk-install--packages extras)
       (message "Installed all extras."))
      (_
       (junk-install--packages (list selection))
       (message (format "Installed extra '%s'." selection))))))

;; Utility

(defvar junk--keywords '(:packages :extras :recipes :docs))
(defun junk--parts (pack)
  "Get the parts from expansion pack PACK.

Returns a list of (PACKAGES EXTRAS RECIPES DOCS)."
  (let ((plist-pack-get (apply-partially 'plist-get (cdr pack))))

    (mapcar plist-pack-get junk--keywords)))

(defun junk--packages ()
  "Get a list of all expansion pack packages."
  (seq-reduce
   (lambda (acc it)
     (cl-destructuring-bind
         (packages extras recipes _)
         (junk--parts it)
       (append acc packages extras (mapcar #'car recipes))))
   junk-expansion-packs '()))

(defun junk--pack-package-p (pack)
  "Check if PACK is an expansion pack package."
  (memq pack (junk--packages)))

(defun junk--pack-from-name (name &optional no-error)
  "Get pack named NAME.

If NO-ERROR is t, don't signal an error if the pack can't be
found."
  (if-let ((pack (assoc (intern-soft name) junk-expansion-packs)))
      pack
    (and (not no-error)
         (user-error "Unknown pack '%s', check `junk-expansion-packs'" pack))))

(defun junk--read-package ()
  "Read a `junk' package."
  (let ((name (completing-read "Select pack to install: " (junk--filter-candidates))))
    (junk--pack-from-name name)))

(cl-defun junk--filter (packages &key mapper)
  "Return PACKAGES that are not yet installed.

Apply MAPPER to packages if set."
  (let* ((mapper (if mapper mapper #'identity))
         (not-installed-p
          (lambda (it)
            (not (package-installed-p (funcall mapper it))))))

    (seq-filter not-installed-p packages)))

(defun junk--filter-candidates ()
  "Filter candidates for completion."
  (seq-filter (lambda (it)
                (junk--with-parts it nil
                  (not (zerop (length (append (junk--filter packages)
                                              (junk--filter recipes :mapper #'car)
                                              (junk--filter extras)))))))
              junk-expansion-packs))

(defun junk--symbolize (symbol?)
  "Make sure SYMBOL? is a symbol."
  (if (symbolp symbol?) symbol? (intern symbol?)))

;; Integration

(defun junk-annotate--face-for-package (package)
  "Get the face for PACKAGE."
  (if (package-installed-p package)
      'marginalia-null
    'marginalia-value))

(defun junk-annotate--propertize (package)
  "Propertize PACKAGE."
  (propertize (symbol-name package) 'face (junk-annotate--face-for-package package)))

(defvar junk-annotate--separator (propertize ", " 'face 'marginalia-null))

(defun junk-annotate--stringify (package-list)
  "Stringify PACKAGE-LIST."
  (mapconcat
   #'junk-annotate--propertize
   package-list
   junk-annotate--separator))

(defun junk--annotate (pack)
  "Annotate PACK."
  (junk--with-parts pack (:with-docs t)
    (eval
     (macroexpand
      `(marginalia--fields
        (,docs :face 'marginalia-documentation :truncate 0.6)
        (,(junk-annotate--stringify (append packages (mapcar #'car recipes))) :truncate 0.8)
        (,(junk-annotate--stringify extras) :truncate 0.4))))))

(defun junk--ensure-advice (name args _state &optional _no-refresh)
  "Verify that NAME (or package in ARGS) is not a `junk' package."
  (when-let* ((ensure (car-safe args))
              (package (or (and (eq ensure t) (junk--symbolize name))
                           ensure)))
    (when (consp package)
      (setq package (car package)))

    (junk--pack-package-p package)))

;; API

;;;###autoload
(defun junk-annotate (candidate)
  "Annotate CANDIDATE expansion pack for `marginalia'."
  (when-let ((annotation (junk--pack-from-name candidate t)))

    (junk--annotate annotation)))

;;;###autoload
(defun junk-setup-use-package (&optional undo)
  "Set up `use-package'.

Undo that setup if UNDO is t.

This will advise the `use-package-ensure-fuction' using
combinator `before-until'. If the package to be ensured is a
`junk' pack package, the package will not be installed."
  (interactive "P")

  (defvar use-package-ensure-function)
  (if undo
      (advice-remove use-package-ensure-function #'junk--ensure-advice)
    (advice-add use-package-ensure-function :before-until #'junk--ensure-advice)))

;;;###autoload
(defun junk-install (pack)
  "Install expansion PACK."
  (interactive (list (junk--read-package)))

  (junk-install--pack pack))


;;;###autoload
(cl-defmacro junk-expand (name docs &key packages extras recipes)
  "Define an expansion pack of PACKAGES under NAME.

Documented using DOCS."
  (declare (indent defun) (doc-string 2))

  `(add-to-list
    'junk-expansion-packs
    '(,name . (:packages ,packages :extras ,extras :docs ,docs :recipes ,recipes))))

(provide 'junk)

;;; junk.el ends here
