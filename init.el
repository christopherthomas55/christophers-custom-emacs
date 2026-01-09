;; -*- lexical-binding: t; -*-
;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-refresh-contents)
(package-initialize)

;; Helpful package management
(require 'use-package)

(setq custom-safe-themes t) ;; TODO - Load my lovely themes. Maybe this is risky?

(setopt server-use-tcp nil)
;; Failed attempt to se tcp socket so I can use gui emacs over lan if desired
;;(setopt server-use-tcp t)
;;(setq server-port 20263)
;; Could listen on any connected machine, not just 127.0.0.1, but currently ssh forwarding
;;(setq server-host "0.0.0.0")
;;(setq server-host "127.0.0.1")
(server-start)

;; In emacs < 29 this helps with annoying key issues
;;(require 'gnu-elpa-keyring-update)

;; Allow bundled packages to upgrade (used for seq package dependencies in Magit), generally useful even after building from source
(setq package-install-upgrade-built-in t)

;; 80 MB is much better than default of 800 KB
(setq gc-cons-threshold 80000000)

;; Tabs are just 4 spaces. I hate tabs!
(setq indent-tabs-mode nil)
(setq tab-width 4)

;; Line wrapping is a must
(add-hook 'text-mode-hook 'visual-line-mode) 


;; Helper fn to load secrets from secret files
;; Loads from file with lines key=val
;; TODO - support multiple files
(defun load-secret (key)
  (with-temp-buffer
    (insert-file-contents (file-name-concat user-emacs-directory ".secrets"))
    ;;let* evaluates sequentially, not parallel so we can use secrets list
    (let* (
	  (secrets-list (split-string (buffer-string) "\n" t)) ; Get each line. 't' omits empty strings
	  (matching-secret (car (seq-filter
					  (lambda (x)  (equal key (car (split-string x "=" t))))
					  secrets-list)))
	  )
	  ;; Return here, have to use
          (substring matching-secret (+ 1 (string-match "=" matching-secret)))
    )
  )
)


;; Use ibuffer instead of standard buffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Enable Evil MODE
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-undo-system 'undo-redo)
  (setq evil-want-C-u-scroll t)
  (setq evil-symbol-word-search t) ;; Helps search for full words in python like example_compound_word
  ;; Use M-u instead for C-u stuff
  (global-set-key (kbd "M-u") 'universal-argument)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  )
;; Lazy, but has to happen after evil loaded
(define-key evil-insert-state-map (kbd "C-n") 'hippie-expand)

;; Evil keybindings in many modes
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))


;; More in depth customizations. Notice this is after evil installed
(load-file "~/.emacs.d/christophers-custom-emacs/org.el")
(load-file "~/.emacs.d/christophers-custom-emacs/lsp.el")
(load-file "~/.emacs.d/christophers-custom-emacs/themes.el")
(load-file "~/.emacs.d/christophers-custom-emacs/writing.el")


;; Magit git
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

(use-package tramp
  :ensure t
  :config
  (tramp-parse-sconfig "~/.ssh/config")
  (setq tramp-default-method "ssh")

  ;; All from this great article https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "ssh") ; Changed from "scp"
   'remote-direct-async-process)
  
  ;; SPEED FIXES - cache for 200 seconds
  (setq remote-file-name-inhibit-cache 200) ;; Can't make null....cause claude code will run
  (setq remote-file-name-inhibit-locks t)  ;; Can do this if one emacs session
  (setq remote-use-scp-direct-remote-copying t)
  (setq remote-file-name-inhibit-auto-save-visited t)

  (setq vc-handled-backends '(Git)) ; Faster: only checks for Git. TBH I don't know exactly what this does but I'm trying to speed up tramp and it's in the faq
  (setq tramp-verbose 1)            ; Reduce logging

  (defun ssh ()
    "Open find-file pre-populated with the SSH prefix."
    (interactive)
    (find-file (read-file-name "Remote file: " "/ssh:")))

  (setq magit-tramp-pipe-stty-settings 'pty)

     

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


;; LLM chat interface
(use-package gptel
  :ensure t
  :init
  ;; Load GEMINI_API_KEY=VAL\n from secrets file
  (setq gptel-backend (gptel-make-gemini "Gemini" :key (load-secret "GEMINI_API_KEY") :stream t))
  ;; I wish
  ;; TODO route to gemma if rate limited
  (setq gptel-model 'gemini-3-flash-preview)
  (setq gptel-default-mode 'org-mode)

  (defun my/gptel-new-session ()
    "Create a new gptel chat buffer without prompting."
    (interactive)
    (let* (
	   ;; Count existing buffers to generate new buffer name
	   (existing-count (length (seq-filter (lambda (x) (cl-search "gpt" x)) (mapcar #'buffer-name (buffer-list)))))
	   (buf (generate-new-buffer (format "*gpt%d*" existing-count)))
    )
      (with-current-buffer buf
	(org-mode)
	(gptel-mode 1))
      (pop-to-buffer buf))
  )

  ;; Mostly AI generated, this saves all gpts to myh emacs.d dir
  (defun my/gptel-save-to-org-file (&rest _args)
    "Automatically name and save gptel buffers to a specific directory."
    ;; Optionally could yes/no with (when (y-or-n-p "Save this chat to a file? ")
    (let* ((dir-base (expand-file-name "~/.emacs.d/gpt_convos/"))
           (year-month (format-time-string "%Y/%m"))
           (dir (expand-file-name year-month dir-base))
           (date-time (format-time-string "%Y%m%d_%H%M%S"))
	   )
      ;; Create directory if it doesn't exist
      (unless (file-directory-p dir)
	(make-directory dir t))
      ;; Only name the file if it hasn't been saved yet
      (unless (buffer-file-name)
	(let* (
	       ;; Get first 20 words, remove non-alphanumeric chars for filename safety
	       (content (save-excursion
			  (goto-char (point-min))
			  (buffer-substring-no-properties (point-min) (line-end-position))))
	       (slug (let* ((alphanum (replace-regexp-in-string "[^[:alnum:] ]" "" content))
			    (first-40 (substring alphanum 0 (min (length alphanum) 40))))
		       (string-join (split-string first-40) "_")))
	  (filename (expand-file-name (format "gpt-%s-%s.org" date-time slug) dir)))
        (set-visited-file-name filename)
	(org-mode)
	(gptel-mode 1))) ; Ensure it's in org-mode for later loading
    ;; Save the buffer
    (save-buffer)))

  (add-hook 'gptel-post-response-functions #'my/gptel-save-to-org-file)


  ;; For now C-c g is gpt start
  (global-set-key (kbd "C-c g") 'my/gptel-new-session)
)

;; Dired in sidebar and toggle open dir with tab
;; TODO Look into dired-ranger and abo abo's dired hacks in general
(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  ;(add-hook 'dired-sidebar-mode-hook
  ;; Commented is recommended from author. Idk so not turning on
  ;          (lambda ()
  ;            (unless (file-remote-p default-directory)
  ;              (auto-revert-mode))))
  :config
  ;(push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  ;(push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  (setq dired-sidebar-subtree-line-prefix "---")
  (setq dired-sidebar-theme 'icon)
  )

(use-package dired-subtree
        :ensure t
	:after (dired evil)
	;; Was doing this but evil mode is weird
        ;;:bind (:map dired-mode-map
        ;;            ("<tab>" . dired-subtree-toggle)
        ;;            ("TAB" . dired-subtree-toggle))
	:init
	(evil-define-key 'normal dired-mode-map (kbd "TAB") 'dired-subtree-toggle)
	(evil-define-key 'normal dired-mode-map (kbd "<tab>") 'dired-subtree-toggle)
	)
		    

;; Activities to save
(use-package activities
  :ensure t
  :init
  (activities-mode)
  (activities-tabs-mode)
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)

  :bind
  (("C-x C-a C-n" . activities-new)
   ("C-x C-a C-d" . activities-define)
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a C-s" . activities-suspend)
   ("C-x C-a C-k" . activities-kill)
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a b" . activities-switch-buffer)
   ("C-x C-a g" . activities-revert)
   ("C-x C-a l" . activities-list)))

;; w3m


;; Quickscope is fun for moving with fFtT fast in evil mode
(use-package evil-quickscope
  :ensure t
  :init
  ;; There's an always mode that may be worth hooking to some code
  ;; (add-hook 'prog-mode-hook 'turn-on-evil-quickscope-always-mode)
  ;; For now this works
  (global-evil-quickscope-mode 1)
)

;; Dired mode ordering pref
;; lah standard,  F shows dir type, v makes dotfiles handling same, reverse makes prettier
(setq dired-listing-switches "-rlahFv --group-directories-first")

;; Used to do IVY for completion
;; Include ivy swiper and counsel for better completion (TODO: 
;; (add-to-list 'load-path "~/downloaded_repos/elisp_repos/swiper/")
;; (require ')
;;(use-package swiper
;;  :ensure t
;;  :init 
;;  (ivy-mode 1)
;;  (setq ivy-use-virtual-buffers t)
;;  (setq ivy-count-format "(%d/%d) ")
;;)

;; Enable Vertico.
(use-package vertico
  :ensure t
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Save history - in vertico docs
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure t
  :init
  (savehist-mode 1)
  (recentf-mode 1))

;; Now do orderless completion
(use-package orderless
  :ensure t
  :init
  ;;(completion-styles '(orderless basic))
  (setq completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq completion-styles '(orderless basic)
      completion-category-defaults nil ; This fixes C-h v and C-h f to use orderless
      orderless-matching-styles '(orderless-literal orderless-flex))

  ;; 2. CASE INSENSITIVITY
  (setq completion-ignore-case t
	read-buffer-completion-ignore-case t
	read-file-name-completion-ignore-case t
        completion-category-overrides '((file (styles basic partial-completion)))
        completion-pcm-leading-wildcard t) ;; Emacs 31: partial-completion behaves like substring
)

;; Enable rich annotations using the Marginalia package - copy pasted from github
(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  ;; The :init section is always executed.
  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))
	

;; Prescient for completion ordering. Stolen from https://kristofferbalintona.me/posts/202504050923/
;; Just using prescient for ordering minibuffer results, TODO - make custom solution based on frequency + recency
;; I haven't really tested this
(use-package prescient
  :custom
  ;; My settings for relevant user options:
   (prescient-aggressive-file-save t)
   (prescient-sort-length-enable nil)
   (prescient-sort-full-matches-first t)
   (prescient-history-length 200)
   (prescient-frequency-decay 0.997)
   (prescient-frequency-threshold 0.05)
  :config
  ;; Optional: persist prescient statistics to an on-disk cache
  (prescient-persist-mode 1))

;; Also stolen from https://kristofferbalintona.me/posts/202504050923/
(use-package vertico-prescient
  :ensure t
  :demand t
  :after vertico prescient
  :custom
  ;; Sorting.
  (vertico-prescient-enable-sorting t)
  (vertico-prescient-override-sorting nil) ; Don't override `display-sort-function'

  ;; Filtering
  (vertico-prescient-enable-filtering nil) ; We want orderless to do the filtering
  ;; See also `vertico-prescient-completion-styles',
  ;; `vertico-prescient-completion-category-overrides', and
  ;; `prescient--completion-recommended-overrides'.  Those options apply only
  ;; when when `vertico-prescient-enable-filtering' is non-nil.
  :config
  (vertico-prescient-mode 1))

;; MY custom things
;; Right now only some web searches
(load-file (file-name-concat user-emacs-directory "christophers-custom-emacs" "web-search.el"))
(require 'web-search)

;; Settings for my work mac
(when (eq system-type 'darwin)
  (progn
        ;; use gls since I have "advanced ordering" lol
	(setq insert-directory-program "gls"
              dired-use-ls-dired t)
	;; Default font size bigger
	(set-face-attribute 'default nil :height 175)

	;; I am only setting up jira on my work emacs lol
	;; TODO - integrate with evil mode
	(use-package jira
	  :ensure t
	  :config
	  (setq jira-base-url "https://simplerpostage.atlassian.net") ;; Jira instance URL
	  (setq jira-username "cthomas@easypost.com") ;; Jira username (usually, an email)
	  ;; API token for Jira
	  ;; See https://support.atlassian.com/atlassian-account/docs/manage-api-tokens-for-your-atlassian-account/
	  (setq jira-token (load-secret "JIRA_TOKEN"))
	  (setq jira-token-is-personal-access-token nil)
	  (setq jira-api-version 3) ;; Version 2 is also allowed
	  :init 
	  (global-set-key (kbd "C-x j") 'jira-issues)
	  )

  )
)


