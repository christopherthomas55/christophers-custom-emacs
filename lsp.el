;; To do dev on local EP mac, need to get shell variables right
(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; TODO look at these
(use-package dap-mode
  :ensure t
  )
;(use-package dap-python
;  :ensure t
;  )

;; TODO - actually do these 3
(use-package flycheck
  :ensure t
  )

;; TODO - I think this interacts poorly with evil
(use-package company
  :ensure t
  :init
  (setq company-backends '((company-capf company-dabbrev-code))))

  ;; TODO - capf is very annoying for code, so maybe restrict to code modes (prog-mode)
  ;;If you only want Company active while coding (and not in plain text files), do not use =global-company-mode=. Use this instead:
  ;;#+begin_src elisp
  ;;(add-hook 'prog-mode-hook 'company-mode)
  ;;#+end_src
  ;;(defun my/use-standard-completion ()
  ;;  (setq-local completion-styles '(basic partial-completion)))

  ;;(add-hook 'prog-mode-hook #'my/use-standard-completion)

(use-package which-key
    :ensure t
    :config
    (which-key-mode))



;; PYTHON DEV ENV(use-package pet
;; Pet finds virtual envs (very useful at EP)
(use-package pet
  :ensure t
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))


;; COPILOT
(use-package editorconfig
  :ensure t)
(use-package f
  :ensure t)

(use-package copilot
  :ensure t
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
	    :rev :newest
	    :branch "main")
  :init
  (add-hook 'prog-mode-hook 'copilot-mode)
  :bind (:map copilot-mode-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion))
  )
