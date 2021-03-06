;;; xjira.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Aleksandar Dimitrov
;;
;;  Redistribution and use in source and binary forms, with or without
;;  modification, are permitted provided that the following conditions
;;  are met:
;;
;;  1. Redistributions of source code must retain the above copyright
;;  notice, this list of conditions and the following disclaimer.
;;
;;  2. Redistributions in binary form must reproduce the above
;;  copyright notice, this list of conditions and the following
;;  disclaimer in the documentation and/or other materials provided
;;  with the distribution.
;;
;;  3. Neither the name of the copyright holder nor the names of its
;;  contributors may be used to endorse or promote products derived
;;  from this software without specific prior written permission.
;;
;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;  CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
;;  BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;  ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
;;  TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
;;  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;  SUCH DAMAGE.
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
;;
;;  Capture Jira-issues as org-mode items.
;;
;;; Description:
;;
;;  First, set `xjira-host' and `xjira-auth'. If they're unset, you will be prompted
;;  for their values as soon as you try to use any function.
;;
;;  Use `xjira-org-capture-issue' in your capture templates to capture an org mode
;;  headline from a Jira issue. Using `xjira-get', you can query items from the alist stored in
;;  `xjira--org-capture-latest-issue-result'
;;
;;  Here's an example capture template:
;;
;;     ("j" "Add MP Jira ticket" entry
;;      (file+headline my-org-work-file "Tasks")
;;      ,(join-lines
;;        '("* TODO %(xjira-org-capture-issue \"MYPROJ\") %(xjira-get 'issue) %(xjira-get 'title)"
;;          "SCHEDULED: %t"
;;          ":PROPERTIES:"
;;          ":REFERENCE: %(eval xjira-host)/browse/%(xjira-get 'issue)"
;;          ":Reporter: %(xjira-get 'reporter)"
;;          ":Parent: [[%(eval xjira-host)/browse/%(xjira-get 'parent)][%(xjira-get 'parent-title)]]"
;;          ":END:"
;;          ""
;;          "%(xjira-get 'description)")))
;;
;;  You can also use `xjira-get-issue' to download issue information from a Jira instance.
;;
;;; Authentication:
;;
;;  Jira supports basic auth using the username:password scheme. Jira Cloud allows you to use the
;;  same basic auth, but you need to use a token instead of your password.
;;  Token based auth will work with SAML and 2FA enabled. Create a Jira Cloud token here:
;;
;;  https://id.atlassian.com/manage-profile/security/api-tokens
;;
;;  To set `xjira-auth', use
;;
;;    (let ((username "your-jira-username")
;;          (password "your password or token"))
;;     (custom-set-variables
;;      '(xjira-auth (base64-encode-string (format "%s:%s" username password) 'no-line-break))))
;;
;;  But be aware that base64 is just obfuscated plain text, so
;;  everybody who has this string has your credentials.
;;
;;; Dependencies:
;;
;;  This library requires Emacs be built with libjansson support.  Use
;;
;;    (functionp 'json-parse-buffer)
;;
;;  to check if yours is.

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
      (parent . ,.fields.parent.key)
      (parent-title . ,.fields.parent.fields.summary)
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
