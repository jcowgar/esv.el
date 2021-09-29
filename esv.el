;;; esv.el --- Insert ESV Bible passages into the current buffer.

;; Copyright (C) 2021  Jeremy Cowgar <jeremy@cowgar.com>

;; Author: Jeremy Cowgar <jeremy@cowgar.com>
;; URL: https://github.com/jcowgar/esv.el
;; Keywords: lisp, writing, esv, Bible
;; Version: 1.0
;; Package-Requires: ((emacs "26.1"))

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; Prompt the user for a verse or passage and using https://api.esv.org,
;; retrieve the passage and insert it into the current buffer.

;; You can customize the quotes shown by `M-X customize-group RET
;; esv RET`.

;; To install add:
;;
;;   (use-package 'esv
;;     :ensure t
;;     :bind (("C-c b" . esv-insert-verse)))
;;
;; to your ~/.emacs (or ~/.emacs.d/init.d).

;;; Code:

(require 'url)

;; Customization

(defgroup esv nil
  "Options concering the configuration of esv."
  :group 'esv
  :version "1.0")

(defcustom esv-authorization-token nil
  "Authorization token to use for the API requests to https://api.esv.org.
To use esv, you must have an authorization token. To create one, please
visit https://my.crossway.org/cas/login/?service=https://api.esv.org/login/."
  :group 'esv
  :type 'string)

(setq esv-authorization-token "5376b7e690792fd619a9e4f39e2d532abc56a2b8")

(defun esv~get-passage (passage passage-callback)
  "Get PASSAGE from https://api.esv.org."
  (let* ((passage-encoded (url-encode-url passage))
	 (url (format "https://api.esv.org/v3/passage/text/?q=%s&line-length=52&include-headings=false&include-footnotes=false"
		      passage-encoded))
	 (url-request-extra-headers `(("Authorization" . ,(format "Token %s" esv-authorization-token))))
	 (result-buffer-name (url-retrieve-synchronously url)))
    (switch-to-buffer result-buffer-name)
    (save-excursion
      (beginning-of-buffer)
      (mark-beginning-of-buffer)
      (search-forward "{")
      (backward-char)
      (delete-active-region)
      (replace-string "\\n" "\\\\n"))
    (let* ((json-raw (buffer-string))
	   (json-data (json-parse-string json-raw :array-type 'list))
	   (passage-raw (car (gethash "passages" json-data)))
	   (passage (replace-regexp-in-string "\\\\n" "\n" passage-raw)))
      (kill-buffer)
      (funcall passage-callback passage))))

(defun esv-insert-verse (passage-reference)
  "Insert a verse from the ESV Bible into the current buffer."
  (interactive "sVerse/Passage: ")
  (esv~get-passage passage-reference
		   (lambda (passage)
		     (insert passage))))

(provide 'esv)

;;; esv.el ends here
