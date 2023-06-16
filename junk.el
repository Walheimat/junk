;;; junk.el --- Package expansion packs -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/partial-recall
;; Version: 0.1.0
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

(cl-defmacro junk--with-parts (item &rest body &key with-docs &allow-other-keys)
  "Execute BODY with an ITEM's junk parts bound.

Also bind docs if WITH-DOCS is t."
  (declare (indent defun))

  `(cl-destructuring-bind (packages extras recipes ,(if with-docs 'docs '_docs)) (junk--parts ,item)
     ,@(delq nil
             (cl-loop for (key val)
                      on body by 'cddr
                      unless (memq key '(:with-docs))
                      collect key
                      and collect val))))

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
  (junk--with-parts pack
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

(defun junk--pack-from-name (name)
  "Get pack named NAME."
  (if-let ((pack (assoc (intern-soft name) junk-expansion-packs)))
      pack
    (user-error "Unknown pack '%s', check `junk-expansion-packs'" pack)))

(defun junk--read-package ()
  "Read a `junk' package."
  (let ((name (completing-read "Select pack to install: " junk-expansion-packs)))
    (junk--pack-from-name name)))

(cl-defun junk--filter (packages &key mapper)
  "Return PACKAGES that are not yet installed.

Apply MAPPER to packages if set."
  (let* ((mapper (if mapper mapper #'identity))
         (not-installed-p
          (lambda (it)
            (not (package-installed-p (funcall mapper it))))))

    (seq-filter not-installed-p packages)))

(defun junk--stringify (package-list)
  "Stringify PACKAGE-LIST."
  (mapconcat #'symbol-name package-list ", "))

(defun junk--annotate (pack)
  "Annotate PACK."
  (junk--with-parts pack :with-docs t
    (eval
     (macroexpand
      `(marginalia--fields
        (,docs :face 'marginalia-documentation :truncate 0.6)
        (,(junk--stringify (append packages (mapcar #'car recipes))) :face 'marginalia-value :truncate 0.8)
        (,(junk--stringify extras) :face 'marginalia-value :truncate 0.4))))))

;; API

;;;###autoload
(defun junk-annotate (candidate)
  "Annotate CANDIDATE expansion pack for `marginalia'."
  (junk--annotate (junk--pack-from-name candidate)))

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
