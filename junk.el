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

(cl-defun junk--install (packages &key delete-windows installer)
  "Install PACKAGES.

Calls `delete-other-windows' if DELETE-WINDOWS is t.

Uses `package-install' unless custom INSTALLER is provided."
  (let ((installer (or installer #'package-install)))

    (mapc installer packages)

    (when delete-windows
      (delete-other-windows))))

(defun junk-package-vc-install (recipe)
  "Install RECIPE using `package-vc-install'."
  (if (fboundp 'package-vc-install)
      (cl-destructuring-bind (package url) recipe
        (package-vc-install url)
        (package--update-selected-packages (list package) nil))
    (user-error "Recipes can only be installed with `package-vc-install'")))

(cl-defun junk--filter (packages &key mapper)
  "Return PACKAGES that are not yet installed.

Apply MAPPER to packages if set."
  (seq-filter (lambda (it)
                (let ((package (if mapper
                                   (funcall mapper it)
                                 it)))
                  (not (package-installed-p package))))
                packages))

(defun junk--packs ()
  "Get a list of all expansion packs."
  (seq-reduce
   (lambda (acc it)
     (cl-destructuring-bind
         (packages extras recipes _)
         (junk--parts it)
       (append acc packages extras (mapcar #'car recipes))))
   junk-expansion-packs '()))

(defun junk--pack-p (pack)
  "Check if PACK is an expansion pack package."
  (memq pack (junk--packs)))

(defun junk--install-extras (extras)
  "Install one or all packages in EXTRAS."
  (let* ((selection (intern-soft
                     (completing-read
                      "Select extra to install: " (append extras '(all))))))

    (pcase selection
      ('all
       (junk--install extras)
       (message "Installed all extras."))
      (_
       (junk--install (list selection))
       (message (format "Installed extra '%s'." selection))))))

(defun junk-install (pack)
  "Install the given expansion PACK."
  (interactive
   (list (completing-read "Select pack to install: "
                          (mapcar (lambda (pack) (car pack)) junk-expansion-packs))))

  (let* ((sym (intern-soft pack))
         (item (assoc sym junk-expansion-packs)))

    (cl-destructuring-bind
        (packages extras recipes _)
        (junk--parts item)

      (when (not item)
        (user-error "Unknown pack '%s', check `junk-expansion-packs'" sym))

      (let ((normal (junk--filter packages))
            (from-recipe (junk--filter recipes :mapper #'car)))

        (if (not (append normal from-recipe))
            (if (and (junk--filter extras)
                     (yes-or-no-p (format "Want to install an extra for '%s'?" pack)))
                (junk--install-extras extras)
              (message "Package '%s' is already installed." pack))
          (junk--install normal :delete-windows t)
          (junk--install from-recipe :installer 'junk-package-vc-install)
          (message "Installed '%s'." pack))))))

(defun junk--stringify (package-list)
  "Stringify PACKAGE-LIST."
  (if package-list
      (mapconcat (lambda (it) (format "%s" it)) package-list ", ")
    ""))

(defun junk-annotate (candidate)
  "Annotate CANDIDATE expansion pack."
  (let* ((item (assoc (intern candidate) junk-expansion-packs))
         (parts (junk--parts item)))

    (cl-destructuring-bind (packages extras recipes docs) parts

      (eval
       (macroexpand
        `(marginalia--fields
          (,docs :face 'marginalia-documentation :truncate 0.6)
          (,(junk--stringify (append packages recipes)) :face 'marginalia-value :truncate 0.8)
          (,(junk--stringify extras) :face 'marginalia-value :truncate 0.4)))))))

;; API

(cl-defmacro junk-expand (name docs &key packages extras recipes)
  "Define an expansion pack of PACKAGES under NAME.

Documented using DOCS."
  (declare (indent defun) (doc-string 2))

  `(add-to-list
    'junk-expansion-packs
    '(,name . (:packages ,packages :extras ,extras :docs ,docs :recipes ,recipes))))

(defun junk--parts (pack)
  "Get the parts from expansion pack PACK.

Returns a list of (PACKAGES EXTRAS RECIPES DOCS)."
  (let ((p (cdr pack)))

    (list
     (plist-get p :packages)
     (plist-get p :extras)
     (plist-get p :recipes)
     (plist-get p :docs))))

(provide 'junk)

;;; junk.el ends here
