;;; xjira-test.el --- Description -*- lexical-binding: t; -*-
;; Copyright (C) 2021 Aleksandar Dimitrov
;;
;; Author: Aleksandar Dimitrov <code@aleks.bg>
;; Maintainer: Aleksandar Dimitrov <code@aleks.bg>
;; Created: August 12, 2021
;; Modified: August 12, 2021
;; Homepage: https://github.com/adimit/xjira
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Tests for xjira
;;; Code:

(require 'xjira)

(ert-deftest xjira--parsing-test ()
    "Parsing with `xjira--parse-issue' should find and format all important fields."

    (let ((specimen
           '((key . "PROJECT-12345")
             (fields
              (issuetype
               (name . "issue type name"))
              (created . "2020-10-26T09:18:13.684+0100") ; not touching this wasp's nest yet.
              (assignee
               (displayName . "The Assignee"))
              (description . "The description")
              (summary . "The Title")
              (reporter
               (displayName . "The Reporter"))))))

      (let-alist (xjira--parse-issue specimen)
        (should (equal .issue "PROJECT-12345"))
        (should (equal .issue-type "issue type name"))
        (should (equal .title "The Title"))
        (should (equal .reporter "The Reporter"))
        (should (equal .description "The description")))))

(ert-deftest xjira-get ()
  "Should return empty string when result is :null."
  ; dynamic scoping
  (defvar xjira--org-capture-latest-issue-result)
  (let ((xjira--org-capture-latest-issue-result '((description . :null))))
    (should (equal (xjira-get 'description) "")))
  (let ((xjira--org-capture-latest-issue-result '((title . "foo"))))
    (should (equal (xjira-get 'title) "foo"))))

(ert-deftest xjira--strip-cr ()
  "Should strip carriage returns from strings and ignore everything else."
  (should (equal (xjira--strip-cr "foo") "foo"))
  (should (equal (xjira--strip-cr :null) :null)))

(provide 'xjira-test)

;;; xjira-test.el ends here
