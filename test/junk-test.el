;;; junk-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'junk nil t)

(ert-deftest junk--install ()
  (sulphur-with-mock (package-install
                      delete-other-windows
                      package-vc-install)

    (junk--install '(one two) :delete-windows t)
    (sulphur-was-called-nth-with package-install '(one) 0)
    (sulphur-was-called-nth-with package-install '(two) 1)
    (sulphur-was-called delete-other-windows)
    (sulphur-clear-mocks)

    (junk--install '(four) :installer 'package-vc-install)
    (sulphur-was-called-with package-vc-install '(four))
    (sulphur-was-not-called delete-other-windows)))

(ert-deftest junk-expand ()
  (sulphur-match-expansion
   (junk-expand test
     "Tasteful expansion pack."
     :packages '(pull out of the package)
     :extras '(prep some ketchup)
     :recipes '(heat in oven))
   `(add-to-list
     'junk-expansion-packs
     '(test . (:packages '(pull out of the package)
                         :extras '(prep some ketchup)
                         :docs "Tasteful expansion pack."
                         :recipes '(heat in oven))))))

(defvar wal-test-packs '((one :packages
                              (one)
                              :extras nil :docs "That's one." :recipes nil)
                         (two :packages
                              (two)
                              :extras (twofer) :docs "That's two." :recipes nil)
                         (three :packages nil :extras nil :docs "That's three." :recipes
                                ((three-mode "https://get-three-mode")))))

(ert-deftest junk--packs ()
  (let ((junk-expansion-packs wal-test-packs))

    (should (equal (junk--packs) '(one two twofer three-mode)))))

(ert-deftest junk--pack-p ()
  (let ((junk-expansion-packs wal-test-packs))

    (should (junk--pack-p 'three-mode))))

(ert-deftest junk--filter--items-may-be-mapped ()
  (sulphur-with-mock ((package-installed-p . (lambda (p) (memq p '(test best)))))

    (should (equal (junk--filter '((test "test") (rest "rest") (best "best")) :mapper #'car)
                   '((rest "rest"))))))

(ert-deftest junk--install-extras ()
  (let ((extras (plist-get (nth 2 junk-expansion-packs) :extras))
        (selection 'all))

    (ert-with-message-capture messages
      (sulphur-with-mock ((package-installed-p . #'ignore)
                          (package-install . #'always)
                          (completing-read . (lambda (_m _l) selection)))

        (junk--install-extras extras)

        (setq selection 'twofer)
        (junk--install-extras extras)

        (should (string-equal messages "Installed all extras.\nInstalled extra ’twofer’.\n"))))))

(ert-deftest junk-install ()
  (let ((messages '()))
    (sulphur-with-mock ((completing-read . (lambda (_m _v) "one"))
                        (package-installed-p . #'ignore)
                        (package-install . #'always)
                        (message . (lambda (m &rest args) (add-to-list 'messages (format m (car args))))))

      (let ((junk-expansion-packs wal-test-packs))

        (call-interactively 'junk-install)

        (should (string-equal (car messages) "Installed 'one'."))))))

(ert-deftest junk-install--installed-already ()
  (let ((messages '()))
    (sulphur-with-mock ((completing-read . (lambda (_m _v) "one"))
                        (package-installed-p . #'always)
                        (message . (lambda (m &rest args) (add-to-list 'messages (format m (car args))))))
      (let ((junk-expansion-packs wal-test-packs))

        (call-interactively 'junk-install)

        (should (string-equal (car messages) "Package 'one' is already installed."))))))

(ert-deftest junk-install--with-extras ()
  (let ((messages '()))
    (sulphur-with-mock ((completing-read . (lambda (_m _v) "two"))
                        (package-installed-p . #'ignore)
                        (package-install . #'always)
                        (message . (lambda (m &rest args) (add-to-list 'messages (format m (car args)))))
                        (yes-or-no-p . #'ignore))

      (let ((junk-expansion-packs wal-test-packs))
        (call-interactively 'junk-install)

        (should (string-equal (car messages) "Installed 'two'."))))

    (sulphur-with-mock ((completing-read . (lambda (_m _v) "two"))
                        (package-installed-p . (lambda (it) (equal 'two it)))
                        (package-install . #'always)
                        (yes-or-no-p . #'always)
                        (junk--install-extras . (lambda (_) 'extra)))

      (let ((junk-expansion-packs wal-test-packs))

        (should (equal (call-interactively 'junk-install) 'extra))))))

(ert-deftest -junk-package-vc-install ()
  (sulphur-with-mock (package-vc-install package--update-selected-packages)

    (junk-package-vc-install '(test "http://test.com"))

    (sulphur-was-called-with package-vc-install (list "http://test.com"))
    (sulphur-was-called-with package--update-selected-packages (list '(test) nil))))

(ert-deftest -junk-package-vc-install--shows-error-if-not-present ()
  (sulphur-with-mock ((fboundp . #'ignore))

    (should-error (junk-package-vc-install '(test "http://test.com")) :type 'user-error)))

(ert-deftest junk-install--errors-for-non-existing ()
  (let ((junk-expansion-packs wal-test-packs))

    (should-error (junk-install 'four))))

(ert-deftest junk--stringify ()
  (should (string-equal (junk--stringify '(one two three)) "one, two, three"))
  (should (string-empty-p (junk--stringify '()))))

;; Mock implementation
(defmacro marginalia--fields (&rest body)
  "Mock implementation of `marginalia--fields'."
  `(progn
     (cons 'result ',body)))

(ert-deftest junk-annotate ()
  (let ((junk-expansion-packs nil)
        (expected '(result ("test" :face 'marginalia-documentation :truncate 0.6)
                           ("" :face 'marginalia-value :truncate 0.8)
                           ("" :face 'marginalia-value :truncate 0.4))))

    (sulphur-with-mock ((junk--parts . (lambda (_) '(nil nil nil "test"))))

      (should (equal expected (junk-annotate "test"))))))

;;; junk-test.el ends here
