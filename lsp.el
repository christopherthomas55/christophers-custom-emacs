;; -*- lexical-binding: t; -*-
;; To do dev on local EP mac, need to get shell variables right
;; In general shell variables are a bit weird, may need to do something
(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x))
    ;; These are for claude code.
    (exec-path-from-shell-copy-envs
     '("CAUDE_CODE_USE_VERTEX" "ANTHROPIC_VERTEX_PROJECT_ID" "GOOGLE_CLOUD_PROJECT" "CLOUD_ML_REGION"))
    )
  (exec-path-from-shell-initialize))

;; Some QOL settings for programming modes
;; Show line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'column-number-mode)

;; Treesitter isn't lsp but helps some coding tools
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)))

(use-package treesit-fold
  :ensure t
  :hook (prog-mode . treesit-fold-mode)
  :config
  (evil-define-key 'normal treesit-fold-mode-map
    (kbd "za") 'treesit-fold-toggle
    (kbd "zc") 'treesit-fold-close
    (kbd "zo") 'treesit-fold-open
    (kbd "zm") 'treesit-fold-close-all
    (kbd "zr") 'treesit-fold-open-all))
  
(use-package eglot
  :ensure t
  :hook ((rustic-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (racket-mode . eglot-ensure)
	 (go-mode . eglot-ensure)
	 (go-ts-mode . eglot-ensure)
	 ;; Tons of js and typescript bs
         (js-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '(rustic-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("basedpyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(racket-mode . ("racket-langserver")))
  (add-to-list 'eglot-server-programs
               '((js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode)
                 . ("vtsls" "--stdio")))
  ;;LLM says this is necesary for vtsls to work but I don't know
  ;; (setq eglot-workspace-configuration
  ;; 	'((:vtsls . ((typescript . ((updateImportsOnFileMove . ((enabled . "always")))))
  ;; 		     (javascript . ((updateImportsOnFileMove . ((enabled . "always"))))))
  ;; 		  )))

  ;; Keybindings for eglot
  ;; Note gd - go to definition is global already and very useful
  ;; C-o to go back from that (C-i is forward too) is also global
  (evil-define-key 'normal eglot-mode-map
	(kbd "gr") 'xref-find-references
	(kbd "gR") 'eglot-rename
	(kbd "ga") 'eglot-code-actions)

  ;; Warnings on right side of window
  (setq flymake-show-diagnostics-at-end-of-line t)
  )

  

;; TODO - actually do these 3
(use-package flycheck
  :ensure t
  )

(use-package company
  :ensure t
  :init
  ;; Minimal, could expand
  (setq company-backends '((company-capf company-dabbrev-code company-abbrev company-files)))
  (setq company-tooltip-align-annotations t)

  ;; This overlaps with copilot. I like the vim keybings anyways
  ;; TODO - this is causing a warning on load
  ;; Error (use-package): company/:init: Symbol’s value as variable is void: company-active-map
  :config
  (define-key company-active-map (kbd "TAB") nil)
  (define-key company-active-map (kbd "<tab>") nil)

  (add-hook 'prog-mode-hook 'company-mode)
  )


(use-package which-key
    :ensure t
    :config
    (which-key-mode))


;; Rust dev env
(use-package rustic
  :ensure t
  :config
  (setq rustic-format-on-save nil)
  (setq rustic-lsp-client 'eglot)
  (setq rustic-lsp-server 'rust-analyzer)
  :custom
  (rustic-cargo-use-last-stored-arguments t))

;; Racket mode? Why am I learning racket??? Idk lol
(use-package racket-mode
  :ensure t
)

;; PYTHON DEV ENV - TODO not using
;; Pet finds virtual envs (very useful at EP)
(use-package pet
  :ensure t
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

;; Go mode
(use-package go-mode
  :ensure t
)

(use-package dap-mode
  :ensure t
  :after eglot
  :config
  ;; Enable the visual debugger UI
  (dap-ui-mode 1)
  ;; Enable tooltips when hovering over variables while debugging
  (dap-tooltip-mode 1)
  ;; Use VS Code-like window layout automatically
  (dap-ui-controls-mode 1)
  ;; Load the Go specific configuration
  (require 'dap-dlv-go))




;; Typescript/js lsp is vtlsp
;;

;; COPILOT
(use-package editorconfig
  :ensure t)

;; only on workmac for now, now none
(when (eq system-type 'ignore)
  ;; Probably need to fix this
  (defun my/tab-override-function ()
    (interactive)
    (if (copilot--overlay-visible)
	(copilot-accept-completion)
      (indent-for-tab-command)))
  ;; Copilot
  (use-package copilot
    :ensure t
    :vc (:url "https://github.com/copilot-emacs/copilot.el"
	      :rev :newest
	      :branch "main")
    :init
    (add-hook 'prog-mode-hook 'copilot-mode)
    :bind (:map copilot-mode-map
		("<tab>" . my/tab-override-function)
		("TAB" . my/tab-override-function))))
  


;; Magit and code review tools
(use-package magit
  :ensure t)
(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
)

;; Let's connect magit to github
(use-package forge
  :ensure t
  :after magit
  :init
  ;; Annoyingly use something other than my custom secrets for source
  ;; Have to run git config --global github.user christopherthomas55 first
  (setq auth-sources (list (file-name-concat user-emacs-directory ".authinfo")))

  )

(use-package mermaid-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.mmd\\'" . mermaid-mode))
  )


;; (use-package emojify
;;   :ensure t
;;   :config
;;   (emojify-set-emoji-styles '(github))
;;   )
;; 
;; ;; Enable github code review
;; ;; This is a potential project to fix these
;; (use-package code-review
;;   :ensure t
;;   :after forge
;;   :bind (:map forge-topic-mode-map
;;               ("r" . code-review-forge-pr-at-point))
;;   :config
;;   (add-hook 'code-review-mode-hook #'emojify-mode)
;;   )
