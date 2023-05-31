;;; test-helper.el --- Test helpers. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Helper macros and functions.

;;; Code:

(require 'ert-x)
(require 'compat nil t)
(require 'sulphur nil t)

(declare-function sulphur-path-setup "ext:sulphur.el")
(declare-function sulphur-ert-runner-setup "ext:sulphur.el")
(declare-function sulphur-undercover-setup "ext:sulphur.el")

;; Setup

(sulphur-path-setup)
(sulphur-undercover-setup (list "junk.el"))
(sulphur-ert-runner-setup)

;;; test-helper.el ends here
