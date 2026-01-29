;; -*- lexical-binding: t; -*-
;; Contains my org settings
(use-package org
  :ensure t
  :after evil-org
  :config
  (if (file-directory-p "~/org/") () (make-directory "~/org"))
  ;; Allows us to do advanced TODO tracking
  ;; The @ and ! are just timestamp tracking. We could theoretically log more if you rtfm
  (setq org-todo-keywords '((sequence "TODO(t!)" "LINUX (l)" "CODING(c)" "READING(r)" "WRITING(w)" "BRAINSTORM(b)" "|" "DONE(d!)")))

  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq my/todo-file "~/org/hosted_org_notes/todos.org")

  ;; TODO - short not working, rename to writing snippet
  ;; TODO - I really customized up my setup after a month using org
  ;; Very similar to suggested templates
  (setq org-capture-templates
	'(("t" "TODO" entry (file+headline my/todo-file "Tasks")
	   "* TODO %?\n  %i\n  %a")
	  ("i" "IDEA" entry (file+datetree "~/org/unprocessed_ideas.org")
	   "* %?\nEntered on %U\n  %i\n  %a")
	  ("p" "PROJECT" entry (file+headline "~/org/unprocessed_projects.org" "Project ideas")
	   "* PROJECT %?\n  %i\n  %a")
	  ("j" "Journal" entry (file+datetree "~/org/writing/journal.org")
	   "* %?\nEntered on %U\n  %i\n  %a")
	  ))

  (global-set-key (kbd "C-c l") #'org-store-link)
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture)
  ;; Lambda because I was experimenting with new buffers and what not
  (add-hook 'org-capture-after-finalize-hook (lambda () (org-capture-goto-last-stored)))

  ;; Whow my working ons
  (setq org-agenda-custom-commands
	'(("w" "Work Overview"
	   tags-tree "working_on"
	   ((org-show-context-detail '((default . tree)))))))

  ;; Only difference compared to default is we find-file, not in other window or fram
  ;; I don't remember the details
  (setq org-link-frame-setup
	'((vm . vm-visit-folder-other-frame)
	 (vm-imap . vm-visit-imap-folder-other-frame)
	 (gnus . org-gnus-no-new-news) (file . find-file)
	 (wl . wl-other-frame)))

  ;; Set up habits
  (add-to-list 'org-modules 'org-habit)
  ;; I don't really use this as a hardcore time tracker
  (setq org-agenda-span 1)
  (setq org-agenda-start-on-weekday nil)
  (setq  org-agenda-start-day "-0d")
  ;; I don't have many deadlines so I like seeing them
  (setq org-deadline-warning-days 180)

  ;; I just edited org agenda
  (setq  org-agenda-sorting-strategy
	 '((agenda habit-up tag-up deadline-up alpha-up)
	   (todo urgency-down category-keep)
	   (tags urgency-down category-keep)
	   (search category-keep)))




  ;; Record the date/time when a task is marked DONE
  (setq org-log-done 'time)
  ;; Record a note/timestamp specifically when a repeating task repeats
  (setq org-log-repeat 'time)
  ;; Put these logs into a drawer called :LOGBOOK: (keeps things tidy)
  (setq org-log-into-drawer t)
  ;; Native org agenda support for logs?
  (setq org-agenda-start-with-log-mode t)

  ;; Habits nicer
  (setq org-habit-preceding-days 20)
  (setq org-habit-following-days 2)
  (setq org-habit-show-all-today t)
  (setq org-habit-graph-column 65)
  (setq org-habit-show-done-always-green t)

  (defun my/org-agenda-auto-refresh ()
    "Refresh the agenda view if it is currently displayed."
    (let ((buffer (get-buffer "*Org Agenda*")))
      (when buffer
	(with-current-buffer buffer
	  (org-agenda-redo-all)))))
  ;; Maybe try this? Idk
  ;;(add-hook 'org-after-todo-state-change-hook 'my/org-agenda-auto-refresh)

  ;; TODO this can theoretically get slow
  ;; The string search avoids emacs swap files that start with .#
  ;; TODO rerun on org agenda load
  (defun my/reload-org-agenda-files ()
   (interactive)
   (setq org-agenda-files
    (seq-filter (lambda (x) (not (string-search "/.#" x)))
	(append
	 (list "~/org/habits.org"
	       "~/org/writing/working_on.org"
	       "~/org/computing/working_on.org"
	       my/todo-file
	  )

	 ;; Any todos in projects are saved too
	 (if (file-directory-p "~/org/")
	     (directory-files-recursively "~/org/" "\\todos.org$")
	     ())
	 (if (file-directory-p "~/org/")
	     (directory-files-recursively "~/org/" "\\todo.org$")
	     ())
	 ;; Any todos in projects are saved too
	 (if (file-directory-p "~/projects/")
	     (directory-files-recursively "~/projects/" "\\.org$")
	     ())
	 ))))

  (my/reload-org-agenda-files)
  (org-mode-restart))

;; EVILLLLLL in org
(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))


;; Org roam dependencies are bad
(use-package sqlite3                       
  :ensure t
)
(use-package dash
  :ensure t
)
(use-package f
  :ensure t
)
(use-package s
  :ensure t
)
(use-package emacsql
  :ensure t
)
(use-package magit-section
  :ensure t
)

(use-package org-roam
  :ensure t
  :after org
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (define-key org-roam-mode-map (kbd "RET") #'my/org-roam-node-visit-swapped)
  :init
  (if (file-directory-p "~/org/roam") () (make-directory "~/org/roam"))
  (setq org-roam-directory (file-truename "~/org/"))
  (setq org-roam-db-location (concat org-roam-directory "roam.sqlite"))

  (setq org-roam-node-display-template
	(concat
	 (propertize "${tags:30}" 'face 'org-tag)
	 " ${title:*}"
	 ))

  (setq org-roam-mode-sections
	(list #'org-roam-backlinks-section
	      #'org-roam-reflinks-section
	      ;; Can remove below once speed needed
	      ;; TODO - I don't think it works anyways
	      #'org-roam-unlinked-references-section
	      ))

  ;; This makes org roam a side window
  (add-to-list 'display-buffer-alist
	       '("\\*org-roam\\*"
		 (display-buffer-in-direction)
		 (direction . right)
		 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))

  (defun my/org-roam-node-visit-swapped (node &optional arg)
    "Visit NODE. With no prefix, opens in main window. With prefix, opens in current.
     This swaps the default behavior"
    (interactive (list (org-roam-node-at-point) current-prefix-arg))
    (when node
	;; The second argument to org-roam-node-visit is 'other-window'
      ;; We pass the inverse of the prefix argument logic
      (org-roam-node-visit node (not arg))))
  
  ;; General template
  (setq org-roam-capture-templates
	'(
	  ("c" "computing" plain "%?"
	    :target (file+head "roam/%<%Y%m%d%H%M%S>-${slug}.org"
			       "#+title: ${title}\n#+FILETAGS: :roam:computing:\n")
	    :unnarrowed t)
	  ("w" "writing" plain "%?"
	    :target (file+head "roam/%<%Y%m%d%H%M%S>-${slug}.org"
			       "#+title: ${title}\n#+FILETAGS: :roam:writing:\n")
	    :unnarrowed t)
	  ("d" "default" plain "%?"
	    :target (file+head "roam/%<%Y%m%d%H%M%S>-${slug}.org"
			       "#+title: ${title}\n#+FILETAGS: :roam:\n")
	    :unnarrowed t)
	  ("i" "irl" plain "%?"
	    :target (file+head  "roam/%<%Y%m%d%H%M%S>-${slug}.org"
			       "#+title: ${title}\n#+FILETAGS: :roam:irl:\n")
	    :unnarrowed t)
	  ))

  ;; GPT recommended n , but I do m for roaM too
  (global-set-key (kbd "C-c m f") #'org-roam-node-find)
  (global-set-key (kbd "C-c n f") #'org-roam-node-find)

  (global-set-key (kbd "C-c m i") #'org-roam-node-insert)
  (global-set-key (kbd "C-c n i") #'org-roam-node-insert)

  ;; Converting normal org stuff into a roam node
  ;; t for Track
  (global-set-key (kbd "C-c m t") #'org-id-get-create)
  (global-set-key (kbd "C-c n t") #'org-id-get-create)
 
  (defun force-org-roam-capture ()
    "Force capture a new node by skipping the lookup."
    (interactive)
    (org-roam-capture- :node (org-roam-node-create :title (read-string "Node Title: "))))

  ;; Converting normal org stuff into a roam node
  (global-set-key (kbd "C-c m c") #'force-org-roam-capture)
  (global-set-key (kbd "C-c n c") #'force-org-roam-capture)


  ;; This is cool. Allows us to complete any [[word-or-none]] with a link
  ;; TODO - Is this just a normal completion???? And should it be completion always?
  ;; TODO - I never use this
  (global-set-key (kbd "C-c m TAB") #'completion-at-point)
  (global-set-key (kbd "C-c n TAB") #'completion-at-point)

  ;; GPT recommended eventually
  (global-set-key (kbd "C-c m l") #'org-roam-buffer-toggle)
  (global-set-key (kbd "C-c n l") #'org-roam-buffer-toggle)

  ;;(global-set-key (kbd "C-c m g") #'org-roam-graph)
  ;;(global-set-key (kbd "C-c n g") #'org-roam-graph)

)

;; Load org-roam db after startup
;; TODO - this may have  astartup performance cost
(org-roam-db-autosync-mode)
(org-roam-db-sync)

;; Webserver ui for this too, over port XXXX
(use-package websocket
    :ensure t
    :after org-roam)

;; Should happen on ports 35901 for html/js, and 35903 for websocket
(use-package org-roam-ui
    :ensure t
    :after org-roam websocket
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;; Trying this - TODO verify
;(use-package org-habit-stats
;  :ensure t
;  :after org
;)
