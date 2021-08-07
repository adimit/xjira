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
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;  Interact with Jira. This library requires Emacs be built with
;;  libjansson support. Use
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
;;  The authentication needs to be provided in base64 encoded form. Use something like
;;
;;    echo -n "abc:password" | base64
;;
;;  to create it. But be aware that this will leave your password in your shell history
;;  and that base64 is just obfuscated plain text, so everybdoy who has this
;;  string has your credentials.
;;
;;  You can also create a temporary file with the above string and base64 encode
;;  that. This bypasses the shell history. Just make sure the editor does not
;;  sneak in a final newline. Use hexdump -C to check.
;;
;;; Code:

;;; Private Utility:

(defun xjira--get-url (path host auth)
  "Retrieve PATH using AUTH from HOST."
  (let* ((jira-url (concat host "/rest/api/2/" path))
         (jira-auth (concat "Basic " auth))
         (url-request-extra-headers `(("Content-Type" . "application/json")
                                      ("Authorization" . ,jira-auth))))
    (with-temp-buffer
      (url-insert-file-contents jira-url)
      (json-parse-buffer :object-type 'alist))))

(defvar xjira--org-capture-latest-issue-result nil
  "Stores the latest result of an org capture process.")

(defun xjira--get-issue (issue host auth)
  "Fetch ISSUE from HOST using AUTH.
The result is an alist with issue, issue-type, title, reporter and description assocs."
  (let* ((jira-result (xjira--get-url (concat "issue/" issue) host auth))
         (issue (alist-get 'key jira-result))
         (issue-type (alist-get 'name (alist-get 'issuetype (alist-get 'fields jira-result))))
         (title (alist-get 'summary (alist-get 'fields jira-result)))
         (reporter (alist-get 'displayName (alist-get 'reporter (alist-get 'fields jira-result))))
         (description (alist-get 'description (alist-get 'fields jira-result))))
    `((issue . ,issue)
      (issue-type . ,issue-type)
      (title . ,title)
      (reporter . ,reporter)
      (description . ,description)
      (raw . ,jira-result))))

(defun xjira--obj-or-null (obj)
  "Return OBJ as is unless it is :null, in which case return the empty string."
  (if (eq :null obj) "" obj))

(defun xjira--get-last (symbol)
  "Get SYMBOL from the last captured Jira ticket conveniently."
  (xjira--obj-or-null (alist-get symbol xjira--org-capture-latest-issue-result)))

(defun xjira--strip-cr (string)
  "Strip carriage-returns (^M) from STRING."
  (replace-regexp-in-string "\r" "" string))

;;; Public API:

(defvar xjira-host nil
  "The protocol and host of your Jira instance, without a final slash, e.g. `http://my-jira'.")

(defvar xjira-auth nil
  "Your base64 encoded username:password string to access the Jira REST API.")

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

(defun xjira-get-captured-issue ()
  "Get the last captured jira issue."
  (xjira--get-last 'issue))

(defun xjira-get-captured-description ()
  "Get the last captured jira issue description."
  (xjira--strip-cr (xjira--get-last 'description)))

(defun xjira-get-captured-reporter ()
  "Get the last captured jira issue reporter."
  (xjira--get-last 'reporter))

(defun xjira-get-captured-title ()
  "Get the last captured jira issue title."
  (xjira--get-last 'title))

(provide 'xjira)
;;; xjira.el ends here
