;; -*- lexical-binding: t; -*-
;; To do dev on local EP mac, need to get shell variables right
(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x))
    ;; These are for claude code.
    (exec-path-from-shell-copy-envs
     '("CLAUDE_CODE_USE_VERTEX" "ANTHROPIC_VERTEX_PROJECT_ID" "GOOGLE_CLOUD_PROJECT" "CLOUD_ML_REGION"))
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


;; Rust dev env
(use-package rustic
  :ensure t
  :config
  (setq rustic-format-on-save nil)
  :custom
  (rustic-cargo-use-last-stored-arguments t))

;; Racket mode? Why am I learning racket??? Idk lol
(use-package racket-mode
  :ensure t
)

;; PYTHON DEV ENV
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

;; only on workmac for now
(when (eq system-type 'darwin)
  ;; Copilot
  (use-package copilot
    :ensure t
    :vc (:url "https://github.com/copilot-emacs/copilot.el"
	      :rev :newest
	      :branch "main")
    :init
    (add-hook 'prog-mode-hook 'copilot-mode)
    :bind (:map copilot-mode-map
		("<tab>" . copilot-accept-completion)
		("TAB" . copilot-accept-completion)))


  ;; Claude

  ;; install required inheritenv dependency:
  (use-package inheritenv
    :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

  ;; for eat terminal backend:
  (use-package eat :ensure t)

  ;; Unusedfor vterm terminal backend:
  ;;(use-package vterm :ensure t)
  (use-package monet
    :vc (:url "https://github.com/stevemolitor/monet" :rev :newest))

  ;; install claude-code.el
  (use-package claude-code :ensure t
    :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
    :config
    ;; optional IDE integration with Monet
    (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
    (monet-mode 1
		)
    (claude-code-mode)
    :bind-keymap ("C-c d" . claude-code-command-map
		  ) ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
    :bind
    (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))

  )
