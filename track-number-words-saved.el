(defcustor-save-interactively-hook nil
  "Normal hook that is run after a buffer is saved interactively to its file.
  See `run-hooks'."
  :group 'files
  :type 'hook)

(defun save-buffer-and-call-interactive-hooks (&optional arg)
  (interactive "p")
  (save-buffer arg)
  (when (called-interactively-p 'all)  ;; run post-hooks only if called interactively
    (run-hooks 'after-save-interactively-hook)))

(global-set-key [(control x) (control s)] 'save-buffer-and-call-interactive-hooks)

(defun run-mode-specific-after-save-interactively-buffer-hooks ())

(add-hook 'after-save-interactively-hook 'run-mode-specific-after-save-interactively-buffer-hooks t)


(defun length-buffer-words ()
  (length (split-string (buffer-string)))
)
(defun length-buffer-chars ()
  (length (buffer-string))
)
(defun length-buffer-lines ()
  (length (split-string (buffer-string) "\n" t))
)


(length-buffer-words)
(length-buffer-chars)
(length-buffer-lines)


;; Something like this next
(defun my-after-save-actions ()
  "Used in `after-save-hook'."
  (when (memq this-command '(save-buffer save-some-buffers))
    ;; put a copy of the current file in a specific folder...
    ))

;;(add-hook 'after-save-hook
;;          (lambda ()
;;            (when (called-interactively-p 'any)
;;              (message "Saved %d characters." (buffer-size)))))

