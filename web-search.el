;; -*- lexical-binding: t; -*-
(defun smart-rename (basename)
  "Renames generated buffer depending on basename arg given, but doesn't move buffer"
  (let* (
	 ;; Count existing buffers to generate new buffer name
	 (existing-count (length (seq-filter (lambda (x) (cl-search basename x)) (mapcar #'buffer-name (buffer-list)))))
	 )
    (format "*%s%d*" basename existing-count)
  )
)

(defun smart-eww (url new-buffer-name)
    (eww url new-buffer-name)
)
;; TODO name buffer based on search term
(defun wikipedia (search-term)
  (interactive "sSearch on Wikipedia: ")
   (smart-eww
    (format "https://en.wikipedia.org/w/index.php?search=%s" (subst-char-in-string ? ?+ search-term))
    (smart-rename (format "wiki-%s" (car (split-string search-term))))
   )
)

(defun ddg (search-term)
  (interactive "sEnter ddg search terms: ")
   (smart-eww
    (format "https://duckduckgo.com/html?q=%s" (subst-char-in-string ? ?+ search-term))
    (smart-rename (format "ddg-%s" (car (split-string search-term))))
   )
)

;; TODO google requires javascript
(defun google (search-term)
  (interactive)
  (ddg search-term)
)

(defun hacker-news ()
  (interactive)
  (progn 
   (smart-eww "https://news.ycombinator.com/")
   (smart-rename "hn")
  )
)

(provide 'web-search)
