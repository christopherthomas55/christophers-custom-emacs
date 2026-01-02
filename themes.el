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
(use-package ample-theme
  :ensure t
)

;; Circadian flux like behavior
(use-package circadian
  :ensure t
  :config
  ;; TODO - variabile this, but I'm running into noob lisp quoting issues
  ;;(setq cthomas-dark-themes '(grayscale gruvbox-dark-soft modus-vivendi))
  ;;(setq cthomas-light-themes '(modus-operandi-tinted whiteboard))
  (setq calendar-latitude 30.26)
  (setq calendar-longitude -97.7)
  (defvar my-dark-themes '(grayscale gruvbox-dark-soft modus-vivendi zenburn-theme nord-theme ample-flat))
  (defvar my-light-themes '(modus-operandi-tinted sanityinc-tomorrow-day whiteboard solarized-light ample-light))
  ;; Grayscale is least red when late
  (setq circadian-themes `(("23:00" . (grayscale))
			   ("6:00" . ,my-dark-themes)
                           ("9:00" . ,my-light-themes)
                           (:sunset . ,my-dark-themes)))
  (add-hook 'emacs-startup-hook #'circadian-setup)
  (circadian-setup))
