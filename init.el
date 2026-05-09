;; -*- lexical-binding: t; -*-

;; Before anything, setting the face in here prevents flashes of
;; color as the theme gets activated. Thanks internet!
(setq default-frame-alist '(
                            (background-color . "#000000")
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)))

;; IDK, magit says this is important on my fresh work mac install
(setq package-install-upgrade-built-in t)


;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-refresh-contents)
(package-initialize)

;; Helpful package management
(require 'use-package)



;; Settings for my work mac
;; This must be early in process
(when (eq system-type 'darwin)
  (progn
	;; Default font size bigger
	(set-face-attribute 'default nil :height 175)
         ;; use gls since I have "advanced ordering" lol
 	(setq insert-directory-program "gls"
               dired-use-ls-dired t)


	;; Use more modern curl than what apple ships with
	(setenv "PATH" (concat "/opt/homebrew/opt/curl/bin:" (getenv "PATH")))
	(setq exec-path (cons "/opt/homebrew/opt/curl/bin" exec-path))

	;; This is needed too for aws bedrock and may need to make sure curl doesn't hit conflict above! The upgraded curl (above)
	;; needs to run first
        (add-to-list 'exec-path "/opt/homebrew/bin")


  )
)



;; TODO This will need to get revamped eventually
(setq my/data-drive "~/data")
(setq custom-safe-themes t) ;; TODO - Load my lovely themes. Maybe this is risky?

(setq backup-directory-alist '(("." . "~/.emacs.d/file-backups")))

(setopt server-use-tcp nil)
;; Failed attempt to se tcp socket so I can use gui emacs over lan if desired
;;(setopt server-use-tcp t)
;;(setq server-port 20263)
;; Could listen on any connected machine, not just 127.0.0.1, but currently ssh forwarding
;;(setq server-host "0.0.0.0")
;;(setq server-host "127.0.0.1")
;;(server-start)

;; In emacs < 29 this helps with annoying key issues
;;(require 'gnu-elpa-keyring-update)

;; Allow bundled packages to upgrade (used for seq package dependencies in Magit), generally useful even after building from source
(setq package-install-upgrade-built-in t)

;; 80 MB is much better than default of 800 KB
(setq gc-cons-threshold 80000000)

;; Tabs are just 4 spaces. I hate tabs!
(setq indent-tabs-mode nil)
(setq tab-width 4)


;; Setting smooth isntead of page scrolling. What is this, 1975?!
(setq scroll-conservatively 101) ;; > 100 means "never jump"
(setq scroll-margin 5)

;; Line wrapping is a must
;;(add-hook 'text-mode-hook 'visual-line-mode) 
(global-visual-line-mode t)


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

  (setq evil-respect-visual-line-mode t)

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


;; LLM chat interface
(use-package gptel
  :ensure t
  :init
  (if (eq system-type 'darwin)
    (progn
      ;; OPTIONAL configuration

      (setq gptel-log-level t)

      (setq gptel-model 'claude-sonnet-4-6
          gptel-backend
          (gptel-make-bedrock "AWS"
            ;; optionally enable streaming
            :stream t
            ;; optionally specify the aws profile
	    ;; TODO - parameterize
	    :aws-profile "GenAI-Dev-457090734503"
            :region "us-east-1"
            ;; subset of gptel--bedrock-models
            :models '(claude-sonnet-4-6)
            ;; Model region for cross-region inference profiles. Required for models such
            ;; as Claude without on-demand throughput support. One of 'apac, 'eu or 'us.
            ;; https://docs.aws.amazon.com/bedrock/latest/userguide/inference-profiles-use.html
            :model-region 'us
	    )))
    (progn
      ;; Load GEMINI_API_KEY=VAL\n from secrets file
      (setq gptel-backend (gptel-make-gemini "Gemini" :key (load-secret "GEMINI_API_KEY") :stream t))
      ;; TODO route to gemma if rate limited
      (setq gptel-model 'gemini-3-flash-preview))
  )



  ;; I wish
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
  (global-set-key (kbd "C-c g g") 'my/gptel-new-session)
  (global-set-key (kbd "C-c g s") 'gptel-send)
  (global-set-key (kbd "C-c g r") 'gptel-rewrite)
  (global-set-key (kbd "C-c g m") 'gptel-menu)  ; bonus - very useful!
)


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

(use-package ranger
  :ensure t
  ;; Important that this is in bind so dired redirects immediately
  :bind ([remap dired] . deer)

  :config
  (setq ranger-show-hidden t) ; Show dotfiles
  (setq ranger-cleanup-eagerly t) ; Clean up buffers when moving to another directory
  (setq ranger-cleanup-on-disable nil) ; Don't kill buffers on disable (entry into other mode), I like having them around
  (setq ranger-preview-file t) ; Default to previewing file on the right
  (setq ranger-show-literal nil) ; Default to formatting
  (setq ranger-max-preview-size 10) ; Anything bigger than 10 mb not worth previewing
  (setq ranger-parent-depth 1) ; I like just 1 parent
  (setq ranger-override-dired t) ; Set to 'ranger instead to start in ranger, i got sick of it
  (ranger-override-dired-mode t)

  ;; There's some strange evil mode interation going on so we force these keybindings to make sure previews work
  ;; This in an opption but I am just going to use ranger mode as initial state
  ;; (with-eval-after-load 'evil
  ;;   (evil-define-key 'normal ranger-mode-map
  ;;     (kbd "j") 'ranger-next-file
  ;;     (kbd "k") 'ranger-prev-file
  ;;     (kbd "h") 'ranger-up-directory
  ;;     (kbd "l") 'ranger-find-file))
)

;; TODO this doesn't work and I need to figure it out
;; This handles the weird evil mode interaction where I can't scroll the preview with j and k
(evil-set-initial-state 'ranger-mode 'emacs)


;; Term emulator?
(use-package vterm
    :ensure t)

;; Use the prefix argument to force new buffer
(defun my/new-vterm ()
  (interactive)
  (vterm t)) ; t forces new buffer
(global-set-key (kbd "C-x m") 'vterm)

;; Terminals named by last command
(defun my/vterm-rename-buffer-by-command (command)
  (when (and command (not (string-empty-p command)))
    (rename-buffer (format "vterm: %s" command) t)))

(add-hook 'vterm-cmd-hook #'my/vterm-rename-buffer-by-command)

(setq vterm-buffer-name-string "vterm %s")
