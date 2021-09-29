(setq esv-authorization-token "5376b7e690792fd619a9e4f39e2d532abc56a2b8")

(defun esv-get-passage (passage passage-callback)
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

(defun jc/insert-esv-verse (passage-reference)
  "Insert a verse from the ESV Bible into the current buffer."
  (interactive "sVerse/Passage: ")
  (esv-get-passage passage-reference
		   (lambda (passage)
		     (insert passage))))

(global-set-key (kbd "<f12> i v") 'jc/insert-esv-verse)
