;; -*- lexical-binding: t; -*-

;; Storing everything I use for writing here
;; I would love to write a novel in emacs one day

;; TODO - Find a nice font like garamond or something. Also this doesn't work over terminal
;; (set-face-attribute 'variable-pitch nil :font "Source Sans Pro" :height 140)

;; A great dictionary is a must. See my org roam on dictionary for why
;; WARNING this package claims to need to be laoded before all others, so put it at top of init.el if having issues
(use-package quick-sdcv
  :ensure t
  :custom
  (quick-sdcv-dictionary-prefix-symbol "►")
  (quick-sdcv-ellipsis " ▼")

  :config
  (global-set-key (kbd "C-c d") 'quick-sdcv-search-at-point)
  (global-set-key (kbd "C-c D") 'quick-sdcv-search-input)
  (setq quick-sdcv-dictionary-data-dir (file-name-concat my/data-drive "dictionaries/dic/"))

  ;; I like unique buffers for words to explore the dict
  (setq quick-sdcv-unique-buffers t)
  )

;; Subtle, but nice to not have letter monospacing when writing
(use-package mixed-pitch
  :ensure t
  :hook (text-mode . mixed-pitch-mode))

;; Allows C-) to actually move through sentences
(setq sentence-end-double-space nil)

;; Flyspell mode for writing
(add-hook 'text-mode-hook 'flyspell-mode)

;; TODO Add powerthesaurus
(use-package powerthesaurus
  :ensure t
  :config
  (global-set-key (kbd "C-c t") 'powerthesaurus-lookup-dwim))

;; For concentrating on writing go full screen and olivettis

(use-package olivetti
  :ensure t
  :init
  (setq olivetti-body-width 0.6)
  (setq olivetti-style 'fancy)
  ;; Git icons/colors fill margins in olivetti mode. EWWW
  (add-hook 'olivetti-mode-hook (lambda () (diff-hl-mode (if olivetti-mode -1 1))))
)
(defun my/focus ()
  (interactive)
  (olivetti-mode))
