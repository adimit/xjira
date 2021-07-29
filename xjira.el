;;; xjira.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Aleksandar Dimitrov
;;
;; Author: Aleksandar Dimitrov <https://github.com/adimit>
;; Maintainer: Aleksandar Dimitrov <mail@aleks.bg>
;; Created: Juli 23, 2021
;; Modified: Juli 23, 2021
;; Version: 0.0.1
;; Keywords: Jira, REST API
;; Homepage: https://github.com/aleks/jira
;; Package-Requires: ((emacs "27.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;  Interact with itdesign xjira. This library requires Emacs be built with
;;  libjansson support. Use
;;
;;    (functionp 'json-parse-buffer)
;;
;;  to check if yours is.
;;
;;; Description:
;;  Use `xjira-get-issue' to download issue information from the itdesign xjira instance.
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
;;  sneak in a final newline. Use hexdump -C to make sure.
;;
;;; Code:

(defvar xjira--org-capture-latest-issue-result nil
  "Stores the latest result of an org capture process.")

(defvar xjira-auth nil
  "Your base64 encoded username:password string to access the Jira REST API.")

(defun xjira--get-url (path auth-string)
    "Retrieve PATH using AUTH-STRING from XJIRA."
  (let ((jira-url (concat "http://xjira/rest/api/2/" path))
        (url-request-extra-headers `(("Content-Type" . "application/json")
                                     ("Authorization" . ,(concat "Basic " auth-string)))))
    (with-temp-buffer
      (url-insert-file-contents jira-url)
      (json-parse-buffer :object-type 'alist))))

(defun xjira--get-issue (issue auth)
  "Fetch ISSUE from Jira using AUTH.
The result is an alist with issue, issue-type, title, reporter and description assocs."

  (let* ((jira-result (xjira--get-url (concat "issue/" issue) auth))
       (issue (alist-get 'key jira-result))
       (issue-type (alist-get 'name (alist-get 'issuetype (alist-get 'fields jira-result))))
       (title (alist-get 'summary (alist-get 'fields jira-result)))
       (reporter (alist-get 'displayName (alist-get 'reporter (alist-get 'fields jira-result))))
       (description (alist-get 'description (alist-get 'fields jira-result))))
    `((issue . ,issue)
      (issue-type . ,issue-type)
      (title . ,title)
      (reporter . ,reporter)
      (description . ,description))))

(defun xjira-org-capture-issue (&optional project)
  "Prompt for an issue number and retrieve it from Jira PROJECT.
Will insert the issue's number in the buffer and store the issue's details for later use."
  (interactive)
  (let* ((project (if project project (read-string "Jira project: ")))
         (issue-number (concat project "-" (read-string (concat project "-")))))
    (unless xjira-auth (setq xjira-auth (read-string "Jira auth: ")))
    (setq xjira--org-capture-latest-issue-result
          (xjira--get-issue issue-number xjira-auth))))

(defun xjira-get-captured-issue ()
  "Get the last captured jira issue."
  (alist-get 'issue xjira--org-capture-latest-issue-result))

(defun xjira-get-captured-description ()
  "Get the last captured jira issue description."
  (xjira--obj-or-null (replace-regexp-in-string "\r" "" (alist-get 'description xjira--org-capture-latest-issue-result))))

(defun xjira-get-captured-reporter ()
  "Get the last captured jira issue reporter."
  (xjira--obj-or-null (alist-get 'reporter xjira--org-capture-latest-issue-result)))

(defun xjira-get-captured-title ()
  "Get the last captured jira issue title."
  (xjira--obj-or-null (alist-get 'title xjira--org-capture-latest-issue-result)))

(defun xjira--obj-or-null (obj)
  "Return OBJ unless it is :null, in which case return the empty string."
  (if (eq :null obj) "" obj))

(provide 'xjira)
;;; xjira.el ends here
