;;; xjira.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Aleksandar Dimitrov
;;
;; Author: Aleksandar Dimitrov <code@aleks.bg>
;; Maintainer: Aleksandar Dimitrov <code@aleks.bg>
;; Created: Juli 23, 2021
;; Modified: Juli 23, 2021
;; Version: 0.1.0
;; Keywords: tools, comm
;; Homepage: https://github.com/adimit/xjira
;; Package-Requires: ((emacs "27.1") let-alist)
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;  Interact with Jira.  This library requires Emacs be
;;  built with libjansson support.  Use
;;
;;    (functionp 'json-parse-buffer)
;;
;;  to check if yours is.
;;
;;; Description:
;;  First, set `xjira-host' and `xjira-auth'. If they're unset, you will be prompted
;;  for their values as soon as you try to use any function.
;;  Use `xjira-get-issue' to download issue information from a Jira instance.
;;
;;  To obfuscate the authentication, use something like `base64-encode-region' or
;;
;;    echo -n "abc:password" | base64
;;
;;  But be aware that the latter will leave your password in your
;;  shell history and that base64 is just obfuscated plain text, so
;;  everybdoy who has this string has your credentials.
;;
;;  You can also create a temporary file with the above string and base64 encode
;;  that. This bypasses the shell history. Just make sure the editor does not
;;  sneak in a final newline. Use hexdump -C to check.
;;
;;; Code:

; Configuration:

(defvar xjira--org-capture-latest-issue-result nil
  "Stores the latest result of an org capture process.")

(defvar xjira-host nil
  "The protocol and host of your Jira instance, without a final slash, e.g. `http://my-jira'.")

(defvar xjira-auth nil
  "Your base64 encoded username:password string to access the Jira REST API.")

; Private:

(defun xjira--obj-or-null (obj)
  "Return OBJ as is unless it is :null, in which case return the empty string."
  (if (eq :null obj) "" obj))

(defun xjira--strip-cr (obj)
  "Strip carriage-returns (^M) from OBJ."
  (if (stringp obj)
      (replace-regexp-in-string "\r" "" obj)
    obj))

(defun xjira--get-url (path host auth)
  "Retrieve PATH using AUTH from HOST."
  (defvar url-request-extra-headers) ; dynamic binding
  (let* ((jira-url (concat host "/rest/api/2/" path))
         (jira-auth (concat "Basic " auth))
         (url-request-extra-headers `(("Content-Type" . "application/json")
                                      ("Authorization" . ,jira-auth))))
    (with-temp-buffer
      (url-insert-file-contents jira-url)
      (json-parse-buffer :object-type 'alist))))

(defun xjira--parse-issue (issue-data)
  "Parse ISSUE-DATA.
Creates an alist with issue, issue-type, title, reporter, and
description fileds.  Also includes a raw field that contains ISSUE-DATA."
  (let-alist issue-data
    `((issue . ,.key)
      (issue-type . ,.fields.issuetype.name)
      (title . ,.fields.summary)
      (reporter . ,.fields.reporter.displayName)
      (description . ,(xjira--strip-cr .fields.description))
      (raw . ,issue-data))))

(defun xjira--get-issue (issue host auth)
  "Fetch ISSUE from HOST using AUTH.
The result is an alist with issue, issue-type, title, reporter and description assocs."
  (xjira--parse-issue (xjira--get-url (concat "issue/" issue) host auth)))

; Public:

(defun xjira-org-capture-issue (&optional project)
  "Prompt for an issue number and retrieve it from Jira PROJECT.
Will the issue's details for later use with `xjira-get*' functions.
Will return nil as to allow for calling it at the start of a capture template
without inserting any text."
  (interactive)
  (let* ((project (if project project (read-string "Jira project: ")))
         (issue-number (concat project "-" (read-string (concat project "-")))))
    (unless xjira-host (setq xjira-host (read-string "Jira host (e.g. http://my-jira): ")))
    (unless xjira-auth (setq xjira-auth (read-string "Jira auth (echo -n user:pass | base64 -): ")))
    (setq xjira--org-capture-latest-issue-result
          (xjira--get-issue issue-number xjira-host xjira-auth)))
  nil)

(defun xjira-get (symbol)
  "Get SYMBOL from the last captured Jira ticket."
  (xjira--obj-or-null (alist-get symbol xjira--org-capture-latest-issue-result)))

(provide 'xjira)
;;; xjira.el ends here
