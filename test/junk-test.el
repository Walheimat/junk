;;; junk-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'junk nil t)

;; Macros

(ert-deftest junk--with-parts ()
  (bydi-match-expansion
   (junk--with-parts test :with-docs t
     (message docs))
   '(cl-destructuring-bind
        (packages extras recipes docs)
        (junk--parts test)
      (message docs))))


;; Installing

(defvar junk-test-packs
  '((one :packages (one)
         :extras nil
         :docs "That's one."
         :recipes nil)
    (two :packages (two)
         :extras (twofer)
         :docs "That's two."
         :recipes nil)
    (three :packages nil
           :extras nil
           :docs "That's three."
           :recipes ((three-mode (:url "https://get-three-mode"))))))

(ert-deftest junk-install--packages ()
  (bydi (package-install
         delete-other-windows
         package-vc-install)

    (junk-install--packages '(one two) :delete-windows t)
    (bydi-was-called-nth-with package-install '(one) 0)
    (bydi-was-called-nth-with package-install '(two) 1)
    (bydi-was-called delete-other-windows)
    (bydi-clear-mocks)

    (junk-install--packages '(four) :installer 'package-vc-install)
    (bydi-was-called-with package-vc-install '(four))
    (bydi-was-not-called delete-other-windows)))

(ert-deftest junk-install--pack ()
  (let ((junk-expansion-packs junk-test-packs))

    (bydi ((:ignore package-installed-p)
           (:always package-install))
      (ert-with-message-capture messages
        (junk-install--pack (car junk-test-packs))
        (should (string-equal messages "Installed ’one’.\n"))))))


(ert-deftest junk-install--pack--installed-already ()
  (let ((junk-expansion-packs junk-test-packs))

    (bydi ((package-installed-p . #'always))
      (ert-with-message-capture messages
        (junk-install--pack (car junk-test-packs))
        (should (string-equal messages "Package ’one’ is already installed.\n"))))))

(ert-deftest junk-install--recipe ()
  (bydi (package-vc-install package--update-selected-packages)

    (junk-install--recipe '(test (:url "http://test.com")))

    (bydi-was-called-with package-vc-install (list '(test (:url "http://test.com"))))
    (bydi-was-called-with package--update-selected-packages (list '(test) nil))))

(ert-deftest junk-install--recipe--shows-error-if-not-present ()
  (bydi ((:ignore fboundp))

    (should-error (junk-install--recipe '(test "http://test.com")) :type 'user-error)))

(ert-deftest junk-install--extras ()
  (let ((extras (plist-get (nth 2 junk-expansion-packs) :extras))
        (selection 'all))

    (bydi ((:ignore package-installed-p)
           (:always package-install)
           (:mock completing-read :return selection))

      (ert-with-message-capture messages
        (junk-install--extras extras)
        (should (string-equal messages "Installed all extras.\n"))

        (setq messages nil)
        (setq selection 'twofer)
        (junk-install--extras extras)

        (should (string-equal messages "Installed extra ’twofer’.\n"))))))

(ert-deftest junk-install--with-extras ()
  (let ((junk-expansion-packs junk-test-packs))

    (bydi ((completing-read . (lambda (_m _v) "two"))
           (package-installed-p . #'ignore)
           (package-install . #'always)
           (yes-or-no-p . #'ignore))

      (ert-with-message-capture messages
        (call-interactively 'junk-install)
        (should (string-equal messages "Installed ’two’.\n"))))))

(ert-deftest junk-install--errors-for-non-existing ()
  (let ((junk-expansion-packs junk-test-packs))

    (should-error (junk-install 'four))))

;; Utility

(ert-deftest junk--packages ()
  (let ((junk-expansion-packs junk-test-packs))

    (should (equal (junk--packages) '(one two twofer three-mode)))))

(ert-deftest junk--pack-package-p ()
  (let ((junk-expansion-packs junk-test-packs))

    (should (junk--pack-package-p 'three-mode))))

(ert-deftest junk--read-package--errors-for-missing ()
  (let ((junk-expansion-packs junk-test-packs))

    (bydi ((:mock completing-read :return "four"))
      (should-error (junk--read-package)))))

(ert-deftest junk--filter--items-may-be-mapped ()
  (bydi ((:mock package-installed-p :with (lambda (p) (memq p '(test best)))))

    (should (equal (junk--filter '((test "test") (rest "rest") (best "best")) :mapper #'car)
                   '((rest "rest"))))))

(ert-deftest junk--stringify ()
  (should (string-equal (junk--stringify '(one two three)) "one, two, three"))
  (should (string-empty-p (junk--stringify '()))))

;; API

(defmacro marginalia--fields (&rest body)
  "Mock implementation of `marginalia--fields' using BODY."
  `(progn
     (cons 'result ',body)))

(ert-deftest junk-annotate ()
  (let ((junk-expansion-packs '((test :packages (test))))
        (expected '(result ("test" :face 'marginalia-documentation :truncate 0.6)
                           ("" :face 'marginalia-value :truncate 0.8)
                           ("" :face 'marginalia-value :truncate 0.4))))

    (bydi ((:mock junk--parts :with (lambda (_) '(nil nil nil "test"))))

      (should (equal expected (junk-annotate "test"))))))

(ert-deftest junk-install ()
  (let ((junk-expansion-packs junk-test-packs))

    (bydi ((:mock completing-read :return "one")
           junk-install--pack)
      (call-interactively 'junk-install)
      (bydi-was-called junk-install--pack))))

(ert-deftest junk-expand ()
  (bydi-match-expansion
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


;;; junk-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
