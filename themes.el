;; -*- lexical-binding: t; -*-
;; THEME! I like randomness
;; Darks
;; My favorite theme - gruvbox dark
(use-package gruvbox-theme
  :ensure t)
(use-package grayscale-theme
  :ensure t)
(use-package zenburn-theme
  :ensure t)
(use-package color-theme-sanityinc-tomorrow
  :ensure t)
(use-package nord-theme
  :ensure t)

;; Circadian flux like behavior
(use-package circadian
  :ensure t
  :config
  ;; TODO - variabile this, but I'm running into noob lisp quoting issues
  ;;(setq cthomas-dark-themes '(grayscale gruvbox-dark-soft modus-vivendi))
  ;;(setq cthomas-light-themes '(modus-operandi-tinted whiteboard))
  (setq calendar-latitude 30.26)
  (setq calendar-longitude -97.7)
  (defvar my-dark-themes '(grayscale gruvbox-dark-medium modus-vivendi zenburn nord))
  (defvar my-light-themes '(modus-operandi-tinted))
  ;; Change every 2 hours
  (setq circadian-themes `(("6:00" . ,my-dark-themes)
			   ("8:00" . ,my-dark-themes)
			   ("10:00" . ,my-dark-themes)
			   ("12:00" . ,my-dark-themes)
			   ("14:00" . ,my-dark-themes)
			   ("16:00" . ,my-dark-themes)
			   ("18:00" . ,my-dark-themes)
			   ("21:00" . grayscale)))
  (add-hook 'emacs-startup-hook #'circadian-setup)
  (circadian-setup))

;; I'm a cmd line hacker - get these out of here!
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq default-frame-alist '((undecorated . t)))
