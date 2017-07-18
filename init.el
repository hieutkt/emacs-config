(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Use use-package to reduce load time
(eval-when-compile
  (require 'use-package))
(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant
(require 'key-chord)

;; Requice common-lisp library
(require 'cl-lib)

;; Files searching
(require 'f)
;; Auto-revert mode
(global-auto-revert-mode 1)
(setq auto-revert-interval 0.5)

;; Backup stored in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Delete old backup
(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
	       (> (- current (float-time (fifth (file-attributes file))))
		  week))
      (message "%s" file)
      (delete-file file))))

;; Startup
(add-hook 'after-init-hook 
	  (lambda () 
	    (find-file (format "%s/%s" config-directory "init.org"))))


;; Information settings
(setq user-full-name "Nguyễn Đức Hiếu"
      user-mail-address "hieunguyen31371@gmail.com")

;; Set emacs as a client
;; (server-start)

(use-package hydra
  :ensure t)

;; Startup screen
(use-package dashboard
  :ensure t
  :init 
  (dashboard-setup-startup-hook)
  :config 
  (setq dashboard-startup-banner 'logo)
  )

;; Global truncate line, except in text-based modes
(set-default 'truncate-lines t)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Initialize Emacs full screen 
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(global-set-key (kbd "<f11>") 'toggle-frame-maximized)

;; No startup messages on *scratch* buffer
(setq initial-scratch-message "")

;; Cursor type
(setq-default cursor-type 'bar)

;; Global font-lock mode
(setq global-font-lock-mode t)


;; Enable line number and column number
(setq column-number-mode t)

;; Disable tool bar, menu bar, and scroll bar
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode 1)

;; Paren mode
(show-paren-mode t)
(setq show-paren-delay 0)

;; Diminish some modes
(diminish 'visual-line-mode)

;; Default font
(use-package unicode-fonts
  :ensure t
  :init
  (unicode-fonts-setup)
  :config
  (set-frame-font "Source Code Pro 10" nil t))


;; Set themes
(use-package gruvbox-theme
  :ensure t
  :init
  :config
  (load-theme 'gruvbox-dark-hard t)
  (set-face-attribute 'font-lock-comment-face nil :foreground "#27ae60")
  )

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'ess-mode-hook 'rainbow-delimiters-mode)
  ;; Custom pallete
  (custom-set-faces
   '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "Darkolivegreen3"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "IndianRed"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "Gold"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "DeepPink"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1")))))
  )

  (use-package popup
  :config
  (set-face-attribute 'popup-tip-face nil 
		      :foreground "#1d2021"
		      :background "#f9f5d7")
  )

(use-package airline-themes
  :ensure t
  :config
  (setq powerline-utf-8-separator-left        #xe0b0
  	powerline-utf-8-separator-right       #xe0b2
  	airline-utf-glyph-separator-left      #xe0b0
  	airline-utf-glyph-separator-right     #xe0b2
  	airline-utf-glyph-subseparator-left   #xe0b1
  	airline-utf-glyph-subseparator-right  #xe0d2
  	airline-utf-glyph-branch              #xe0a0
  	airline-utf-glyph-readonly            #xe0a2
  	airline-utf-glyph-linenumber          #xe0a1
  	airline-cursor-colors nil
  	airline-helm-colors nil
  	airline-shortened-directory-length 0
  	airline-eshell-colors nil)
  (load-theme 'airline-luna t)
  )


;; Comint
(set-face-attribute 'minibuffer-prompt nil 
		    :foreground "DarkOrange"
		    :background nil)


;; (use-package powerline
;;   :ensure t
;;   :config
;;   (powerline-default-theme)
;;   (set-face-attribute 'mode-line nil
;;   		    :foreground "Black"
;;   		    :background "DarkOrange"
;;   		    :box nil)
;;   (setq powerline-arrow-shape 'curve)
;;   )

;; (use-package telephone-line
;;   :ensure t
;;   :config
;;   (telephone-line-mode 1)
;;   )

;; Ignore disabled command
(setq disabled-command-function 'ignore)

;; Set some command disabled
(put 'overwrite-mode 'disabled t)

;; Delete marked region when input
(delete-selection-mode 1)

;; Global mark ring
(setq global-mark-ring-max 50000)

;; "Yes or no"? Too much writing
(defalias 'yes-or-no-p 'y-or-n-p)

;; Make comint promts read-only
(setq comint-prompt-read-only t)

;; Auto close bracket insertion.
(electric-pair-mode 1)
(setq electric-pair-pairs '(
			    (?\" . ?\")
			    (?\( . ?\))
			    (?\{ . ?\})
			    ) )

(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

;; Set kill ring size
(setq global-mark-ring-max 50000)

;; Bound undo to C-z
(global-set-key (kbd "C-z") 'undo)


;; Expand region with C-' and return to original position with C-g
(use-package expand-region
  :ensure t
  :init
  (defadvice keyboard-quit (before collapse-region activate)
    (when (memq last-command '(er/expand-region er/contract-region))
      (er/contract-region 0)))
  :bind 
  ("C-'" . er/expand-region)
  )


;; Multi-cursor
(use-package multiple-cursors
  :ensure t
  :init
  ;; In case commands behavior is messy with multiple-cursors,
  ;; check your ~/.emacs.d/.mc-lists.el
  (defun mc/check-command-behavior ()
    "Open ~/.emacs.d/.mc-lists.el. 
So you can fix the list for run-once and run-for-all multiple-cursors commands."
    (interactive)
    (find-file "~/.emacs.d/.mc-lists.el"))  
  :config
  (defhydra multiple-cursors-hydra (:columns 3 :idle 1.0)
    "Multiple cursors"
    ("l" mc/edit-lines "Edit lines in region" :exit t)
    ("b" mc/edit-beginnings-of-lines "Edit beginnings of lines in region" :exit t)
    ("e" mc/edit-ends-of-lines "Edit ends of lines in region" :exit t)
    ("a" mc/mark-all-like-this "Mark all like this" :exit t)
    ("S" mc/mark-all-symbols-like-this "Mark all symbols likes this" :exit t)
    ("w" mc/mark-all-words-like-this "Mark all words like this" :exit t)
    ("r" mc/mark-all-in-region "Mark all in region" :exit t)
    ("R" mc/mark-all-in-region-regexp "Mark all in region (regexp)" :exit t)
    ("i" (lambda (n) 
	   (interactive "nInsert initial number:") 
	   (mc/insert-numbers n)) 
     "Insert numbers" :exit t)
    ("s" mc/sort-regions "Sort regions")
    ("v" mc/reverse-regions "Reverse order")
    ("d" mc/mark-all-dwim "Mark all dwim")
    ("n" mc/mark-next-like-this "Mark next like this")
    ("N" mc/skip-to-next-like-this "Skip to next like this")
    ("M-n" mc/unmark-next-like-this "Unmark next like this")
    ("p" mc/mark-previous-like-this "Mark previous like this")
    ("P" mc/skip-to-previous-like-this "Skip to previous like this")
    ("M-p" mc/unmark-previous-like-this "Unmark previous like this")
    ("q" nil "Quit" :exit t))

  (global-set-key (kbd "C-c m") 'multiple-cursors-hydra/body)
  )




;; Define function: fill character to 80
(defun fill-to-end (char)
  (interactive "HcFill Character:")
  (save-excursion
    (end-of-line)
    (while (< (current-column) 80)
      (insert-char char))))

;; Eval and replace lisp expression
(defun fc-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (prin1 (eval (read (current-kill 0)))
	 (current-buffer)))
(global-set-key (kbd "C-c e") 'fc-eval-and-replace)

;; Move line/region up/down
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
	(exchange-point-and-mark))
    (let ((column (current-column))
	  (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
	(transpose-lines arg))
      (forward-line -1)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key [\M-up] 'move-text-up)
(global-set-key [\M-down] 'move-text-down)

(use-package ace-window
  :ensure t
  :config
  ;; ace-window uses home row
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind*
  (("M-p" . ace-window))
  )

(use-package company
  :ensure t
  :init
  ;; Activate globally
  (add-hook 'after-init-hook 'global-company-mode)

  ;; Press <F1> to show the documentation buffer and press C-<F1> to jump to it
  (defun my/company-show-doc-buffer ()
    "Temporarily show the documentation buffer for the selection."
    (interactive)
    (let* ((selected (nth company-selection company-candidates))
	   (doc-buffer (or (company-call-backend 'doc-buffer selected)
			   (error "No documentation available"))))
      (with-current-buffer doc-buffer
	(goto-char (point-min)))
      (display-buffer doc-buffer t)))  

  :config
  ;; Some useful configs
  (setq company-selection-wrap-around t
  	company-tooltip-align-annotations t
  	company-idle-delay 0.36
  	company-minimum-prefix-length 2
  	company-tooltip-limit 10)

  :bind 
  (:map company-active-map
	("C-<f1>" . my/company-show-doc-buffer)
	("C-n" . company-select-next)
	("C-p" . company-select-previous)
	)
  )

;; math backend, this will input math symbols everywhere except in 
;; LaTeX math evironments
(use-package company-math
  :ensure t
  :config
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  )

(use-package electric-operator
  :ensure t
  :config
  (setq electric-operator-R-named-argument-style 'spaced)
  (add-hook 'ess-mode-hook #'electric-operator-mode)
  (add-hook 'python-mode-hook #'electric-operator-mode)

  (electric-operator-add-rules-for-mode 'ess-mode
					(cons ":=" " := ")
					(cons "%in%" " %in% "))
  )

;; Enable Yasnippets
(use-package yasnippet
  :ensure t
  :init
  ;; It will test whether it can expand, if yes, cursor color -> green.
  (defun yasnippet-can-fire-p (&optional field)
    (interactive)
    (setq yas--condition-cache-timestamp (current-time))
    (let (templates-and-pos)
      (unless (and yas-expand-only-for-last-commands
		   (not (member last-command yas-expand-only-for-last-commands)))
	(setq templates-and-pos (if field
				    (save-restriction
				      (narrow-to-region (yas--field-start field)
							(yas--field-end field))
				      (yas--templates-for-key-at-point))
				  (yas--templates-for-key-at-point))))

      (set-cursor-color (if (and templates-and-pos (first templates-and-pos)) 
			    "green" "#f9f5d7"))))
  (add-hook 'post-command-hook 'yasnippet-can-fire-p)  

  (yas-global-mode 1)

  (yas-reload-all)
  :config
  (setq yas-snippet-dirs (format "%s/%s" config-directory "Snippets"))
  :bind
  ("<C-tab>" . yas-insert-snippet)
  :diminish company-mode
  )

;; With backquote warnings:
;; (add-to-list 'warning-suppress-types '(yasnippet backquote-change))

(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (helm-mode 1)
  :config
  (require 'helm-config)
  (global-unset-key (kbd "C-x c"))


  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
	helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source	.	
	helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
	helm-ff-file-name-history-use-recentf t
	helm-echo-input-in-header-line t 
	helm-M-x-fuzzy-match t
	helm-autoresize-max-height 0
	helm-autoresize-min-height 30)

  (helm-autoresize-mode 1)

  :bind-keymap
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  ("C-c h" . helm-command-prefix)  
  :bind (
 	 ("C-x b" . helm-buffers-list)
 	 ("M-x" . helm-M-x)
 	 ("C-x C-f" . helm-find-files)
	 ("M-y" . helm-show-kill-ring)
 	 :map helm-map
 	 ("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
 	 ("C-i" . helm-execute-persistent-action)   ; make TAB work in terminal
 	 ("M-x" . helm-select-action)              ; list actions using C-z    
 	 )
  :diminish helm-mode
  )


(setq helm-full-frame nil)
;; Use "C-:" to switch to Helm interface during company-ing
(use-package helm-company
  :ensure t
  :config
  (eval-after-load 'company
    '(progn
       (define-key company-mode-map (kbd "C-:") 'helm-company)
       (define-key company-active-map (kbd "C-:") 'helm-company)))    
  )

;; Helm-bibtex
(use-package helm-bibtex
  :ensure t
  :config
  ;; Set bib folder
  (setq bibtex-completion-bibliography
	(expand-file-name "~/Dropbox/references.bib"))
  (setq bibtex-completion-library-path
	(append (f-directories "~/Dropbox" nil t)
		(f-directories "~/Documents" nil t)))
  ;; Set display format    
  (setq bibtex-completion-display-formats
	'((article       . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} ${journal:40}")
	  (inbook        . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
	  (incollection  . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (t             . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*}")))
  :bind(
	:map helm-command-map
	     ("b" . helm-bibtex)
	     )
  )

(helm-autoresize-mode t)

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (setq projectile-mode-line '(:eval (format " 𝐏[%s]" (projectile-project-name))))
  )

;; Helm-projectile
(use-package helm-projectile
  :ensure t
  :config 
  (helm-projectile-on))

(use-package polymode
  :ensure t
  :diminish (poly-org-mode
	     poly-markdown-mode
	     poly-noweb+r-mode
	     poly-noweb+r-mode
	     poly-markdown+r-mode
	     poly-rapport-mode
	     poly-html+r-mode
	     poly-brew+r-mode
	     poly-r+c++-mode
	     poly-c++r-mode)
  :init 
  (require 'poly-R)
  (require 'poly-markdown)
  (require 'poly-org)

  :mode (("\\.org" . poly-org-mode)
	 ("\\.md" . poly-markdown-mode)
	 ("\\.Snw$" . poly-noweb+r-mode)
	 ("\\.Rnw$" . poly-noweb+r-mode)
	 ("\\.Rmd$" . poly-markdown+r-mode)
	 ("\\.rapport$" . poly-rapport-mode)
	 ("\\.Rhtml$" . poly-html+r-mode)
	 ("\\.Rbrew$" . poly-brew+r-mode)
	 ("\\.Rcpp$" . poly-r+c++-mode)
	 ("\\.cppR$" . poly-c++r-mode))
  :config
  (setq polymode-exporter-output-file-format "%s")
  )

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
	(backward-char 1)
	(if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (cond
   ((minibufferp)
    (minibuffer-complete))
   (t
    (indent-for-tab-command)
    (if (or (not yas/minor-mode)
	    (null (do-yas-expand)))
	(if (check-expansion)
	    (progn
	      (company-manual-begin)
	      (if (null company-candidates)
		  (progn
		    (company-abort)
		    (indent-for-tab-command)))))))))

(defun tab-complete-or-next-field ()
  (interactive)
  (if (or (not yas/minor-mode)
	  (null (do-yas-expand)))
      (if company-candidates
	  (company-complete-selection)
	(if (check-expansion)
	    (progn
	      (company-manual-begin)
	      (if (null company-candidates)
		  (progn
		    (company-abort)
		    (yas-next-field))))
	  (yas-next-field)))))

(defun expand-snippet-or-complete-selection ()
  (interactive)
  (if (or (not yas/minor-mode)
	  (null (do-yas-expand))
	  (company-abort))
      (company-complete-selection)))

(defun abort-company-or-yas ()
  (interactive)
  (if (null company-candidates)
      (yas-abort-snippet)
    (company-abort)))

(global-set-key [tab] 'tab-indent-or-complete)
(global-set-key (kbd "TAB") 'tab-indent-or-complete)
(global-set-key [(control return)] 'company-complete-common)

(define-key company-active-map [tab] 'expand-snippet-or-complete-selection)
(define-key company-active-map (kbd "TAB") 'expand-snippet-or-complete-selection)

(define-key yas-minor-mode-map [tab] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

(define-key yas-keymap [tab] 'tab-complete-or-next-field)
(define-key yas-keymap (kbd "TAB") 'tab-complete-or-next-field)
(define-key yas-keymap [(control tab)] 'yas-next-field)
(define-key yas-keymap (kbd "C-g") 'abort-company-or-yas)

(use-package focus
  :ensure t
  :bind ("<f4>" . focus-mode))

(use-package dired+
  :ensure t
  :config
  (set-face-attribute 'diredp-dir-name nil :foreground "#fe8019")
  (set-face-attribute 'diredp-number nil :foreground "#8ec07c")
  (setq dired-listing-switches "-alh")
  )

(use-package ag
  :ensure t
  :init
  ;; Truncate long results
  (add-hook 'ag-mode-hook (lambda () (setq truncate-lines t)))

  :config
  ;; Add highlighting
  (setq ag-highlight-search t)

  ;; Set ag to reuse the same buffer
  (setq ag-reuse-buffers nil)
  )


(use-package wgrep-ag
  :ensure t
  :config
  ;; wgrep-ag allows you to edit a ag buffer and apply those changes to
  ;; the file buffer. 
  (autoload 'wgrep-ag-setup "wgrep-ag")
  (setq wgrep-auto-save-buffer t)
  (add-hook 'ag-mode-hook 'wgrep-ag-setup)
  )

;; Word-wrap
;; (add-hook 'org-mode-hook (lambda () (visual-line-mode 1)))

;; Omit the headline-asterisks except the last one:
(setq org-hide-leading-stars t)

;; Auto indent normally
(setq org-src-tab-acts-natively t)

;; Hide emphasis markers
(setq org-hide-emphasis-markers t)

;; Enable shift selection
(setq org-support-shift-select t)

;; Fontification
(setq org-src-fontify-natively t)
(set-face-attribute 'org-level-1 nil :weight 'bold :height 120)
(set-face-attribute 'org-level-2 nil :weight 'bold)
(set-face-attribute 'org-block nil :foreground "#ffffff")  
(set-face-attribute 'org-block-begin-line nil :foreground "#d5c4a1")
(set-face-attribute 'org-block-end-line nil :foreground "#d5c4a1")

(font-lock-add-keywords 'org-mode
                      '(("^ +\\([-*]\\) "
                         (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Org agenda folders
(setq org-agenda-files '("~/Dropbox/org"))

;; Set monday as the start of the week
(setq org-agenda-start-on-weekday 1)

;; Org keyword
(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE" "SUSPENDED")
	(sequence "PLANNING" "|" "OVER")
	))

(setq org-todo-keyword-faces
      '(("TODO" . "yellow") ("DONE" . "green") ("SUSPENDED" . "gray50")
	("PLANNING" . "light blue") ("OVER" . "slate gray")))

;; Agenda summary 
(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
	 ((agenda "")
	  (alltodo "")))))
;; And bind it to <f8>
(global-set-key (kbd "<f8>") 'org-agenda)

;; Active Babel languages:
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (plantuml . t)
   ))


;; Show inline images
(setq org-startup-with-inline-images t)

(use-package pdf-tools
  :ensure t
  :init 
  (pdf-tools-install)
  :config
  (setq pdf-view-display-size "fit-page"
	auto-revert-interval 0
	ess-pdf-viewer-pref "emacsclient"
	TeX-view-program-selection '((output-pdf "PDF Tools"))
	pdf-view-midnight-colors '("#ffffc8" . "#1d2021"))
  )

(use-package magit
  :ensure t
  :bind
  ;; Set magit-status to F9
  ("<f9>" . magit-status)
  )

  ;; Currently magit cause some error when auto revert mode is on
  (setq magit-auto-revert-mode nil)

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-use-curl t)
  (setq elfeed-search-filter "@3-days-ago")
  (setq elfeed-db-directory "~/Dropbox/Emacs/db.elfeed")
  (add-hook 'elfeed-show-mode-hook (lambda () (visual-line-mode 1)))

  :bind 
  ("C-x w" . elfeed)
  )

(use-package elfeed-goodies
  :ensure t
  :config
  (elfeed-goodies/setup)
  )

;; elfeed-org allows you to organize elfeed with org
(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files 
	(list "~/Dropbox/org/elfeed.org"))
  )

(use-package ibuffer
  :ensure t
  :config
  (setq ibuffer-saved-filter-groups
	(quote (("Default"
		 ("Dired" (mode . dired-mode))
		 ("Org" (name . "^.*org$"))
		 ("Process" (or (mode . inferior-ess-mode)
				(mode . shell-mode)))
		 ("Programming" (or
				 (mode . ess-mode)
				 (mode . python-mode)
				 (mode . c++-mode)))
		 ("Helm" (mode . Hmm-mode))
		 ("Emacs" (or
			   (name . "^\\*scratch\\*$")
			   (name . "^\\*Messages\\*$")
			   (name . "^\\*dashboard\\*$")))
		 ))))

  (add-hook 'ibuffer-mode-hook
	    (lambda ()
	      (ibuffer-auto-mode 1)
	      (ibuffer-switch-to-saved-filter-groups "default")))

  ;; Don't show filter groups if there are no buffers in that group
  (setq ibuffer-show-empty-filter-groups nil)

  :bind
  ("C-x C-b" . ibuffer))

(use-package ess
  :ensure t
  :config
  (require 'ess-site)
  (require 'ess-rutils)
  (require 'ess-eldoc)
  (require 'ess-help)
  (require 'ess-custom)
  )

;; ;; Truncate long lines
;; (add-hook 'special-mode-hook (lambda () (setq truncate-lines t)))
;; (add-hook 'inferior-ess-mode-hook (lambda () (setq truncate-lines t)))
;; (add-hook 'ess-mode-hook (lambda () (setq truncate-lines t)))

;; ;; Indentation style
(setq ess-default-style 'DEFAULT)



;; ESS syntax highlight  
(setq ess-R-font-lock-keywords 
      '((ess-R-fl-keyword:modifiers . nil)
  	(ess-R-fl-keyword:fun-defs . t)
  	(ess-R-fl-keyword:keywords . nil)
  	(ess-R-fl-keyword:assign-ops . t)
  	(ess-R-fl-keyword:constants . t)
  	(ess-fl-keyword:fun-calls . t)
  	(ess-fl-keyword:numbers . t)
  	(ess-fl-keyword:operators . t)
  	(ess-fl-keyword:delimiters . t)
  	(ess-fl-keyword:= . nil)
  	(ess-R-fl-keyword:F&T . t)
  	(ess-R-fl-keyword:%op% . nil)
  	)
      )


(setq inferior-ess-r-font-lock-keywords 
      '((ess-R-fl-keyword:modifiers . nil)
	(ess-R-fl-keyword:fun-defs . nil)
	(ess-R-fl-keyword:keywords . nil)
	(ess-R-fl-keyword:assign-ops . nil)
	(ess-R-fl-keyword:constants . nil)
	(ess-fl-keyword:fun-calls . nil)
	(ess-fl-keyword:numbers . nil)
	(ess-fl-keyword:operators . nil)
	(ess-fl-keyword:delimiters . nil)
	(ess-fl-keyword:= . nil)
	(ess-R-fl-keyword:F&T . t)
	(ess-R-fl-keyword:%op% . nil)) 
      )


(set-face-attribute 'ess-numbers-face nil :foreground "#8ec07c")
;; Disable IDO so helm is used instead
(setq ess-use-ido nil)

(setq ess-use-company 'script-only)
(setq ess-tab-complete-in-script t)	;; Press <tab> inside functions for completions

;; Describe object
;; (setq ess-R-describe-object-at-point-commands
;; 	'(("str(%s)")
;; 	  ("print(%s)")
;; 	  ("summary(%s, maxsum = 20)")))

(define-key ess-doc-map (kbd "C-r") 'ess-rdired)
(define-key ess-doc-map (kbd "r") 'ess-rdired)

;; Returm C-c h as prefix to Helm"
(defun ess-map-control-h-to-helm ()
  "Return C-c h to helm prefix instead of ess-handy-commands"
  (interactive)
  (local-unset-key (kbd "C-c h"))
  (local-set-key (kbd "C-c h") 'helm-command-prefix))

(add-hook 'ess-mode-hook 'ess-map-control-h-to-helm)

;; Remap "<-" key to M-- instead of smart bind to "_"
(ess-toggle-underscore nil)
(define-key ess-mode-map (kbd "M--") 'ess-smart-S-assign)
(define-key inferior-ess-mode-map (kbd "M--") 'ess-smart-S-assign)

;; Hot key C-S-m for pipe operator in ESS
;; Temporary removed and use Yasnippet instead
(defun then_R_operator ()
  "R - %>% operator or 'then' pipe operator"
  (interactive)
  (just-one-space 1)
  (insert "%>%")
  (just-one-space 1))

(define-key ess-mode-map (kbd "C-S-m") 'then_R_operator)
(define-key inferior-ess-mode-map (kbd "C-S-m") 'then_R_operator)



(defun ess-rmarkdown ()
  "Compile R markdown (.Rmd). Should work for any output type."
  (interactive)
  ;; Check if attached R-session
  (condition-case nil
      (ess-get-process)
    (error
     (ess-switch-process)))
  (let* ((rmd-buf (current-buffer)))
    (save-excursion
      (let* ((sprocess (ess-get-process ess-current-process-name))
	     (sbuffer (process-buffer sprocess))
	     (buf-coding (symbol-name buffer-file-coding-system))
	     (R-cmd
	      (format "library(rmarkdown); rmarkdown::render(\"%s\")"
		      buffer-file-name)))
	(message "Running rmarkdown on %s" buffer-file-name)
	(ess-execute R-cmd 'buffer nil nil)
	(switch-to-buffer rmd-buf)
	(ess-show-buffer (buffer-name sbuffer) nil)))))

(define-key polymode-mode-map "\M-ns" 'ess-rmarkdown)

(defun ess-rshiny ()
  "Compile R markdown (.Rmd). Should work for any output type."
  (interactive)
  ;; Check if attached R-session
  (condition-case nil
      (ess-get-process)
    (error
     (ess-switch-process)))
  (let* ((rmd-buf (current-buffer)))
    (save-excursion
      (let* ((sprocess (ess-get-process ess-current-process-name))
	     (sbuffer (process-buffer sprocess))
	     (buf-coding (symbol-name buffer-file-coding-system))
	     (R-cmd
	      (format "library(rmarkdown);rmarkdown::run(\"%s\")"
		      buffer-file-name)))
	(message "Running shiny on %s" buffer-file-name)
	(ess-execute R-cmd 'buffer nil nil)
	(switch-to-buffer rmd-buf)
	(ess-show-buffer (buffer-name sbuffer) nil)))))

(define-key polymode-mode-map "\M-nr" 'ess-rshiny)

(setq python-shell-interpreter "ipython3")
(setq python-shell-interpreter-args "--pprint")

(use-package elpy
  :ensure t
  :init
  ;; Truncate long line in inferior mode
  (add-hook 'inferior-python-mode-hook (lambda () (setq truncate-lines t)))
  ;; Enable company
  (add-hook 'python-mode-hook 'company-mode)
  (add-hook 'inferior-python-mode-hook 'company-mode)
  ;; Enable highlight indentation
  (add-hook 'highlight-indentation-mode-hook 
	    'highlight-indentation-current-column-mode)
  ;; Enable elpy
  (elpy-enable)
  :config
  ;; Do not enable elpy flymake for now
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (remove-hook 'elpy-modules 'elpy-module-highlight-indentation)

  ;; Use python3
  (elpy-use-ipython "ipython3")
  ;;     (setq python-shell-interpreter-args "--simple-prompt --pprint")
  (setq elpy-rpc-python-command "python3")

  ;; Completion backend
  (setq elpy-rpc-backend "rope")

  ;; Function: send block to elpy: bound to C-c C-c
  (defun forward-block (&optional n)
    (interactive "p")
    (let ((n (if (null n) 1 n)))
      (search-forward-regexp "\n[\t\n ]*\n+" nil "NOERROR" n)))

  (defun elpy-shell-send-current-block ()
    (interactive)
    (beginning-of-line)
    "Send current block to Python shell."
    (push-mark)
    (forward-block)
    (elpy-shell-send-region-or-buffer)
    (display-buffer (process-buffer (elpy-shell-get-or-create-process))
		    nil
		    'visible))

  ;; Font-lock
  (add-hook 'python-mode-hook
    '(lambda()
       (font-lock-add-keywords
        nil
        '(("\\<\\([_A-Za-z0-9]*\\)(" 1
	   font-lock-function-name-face) ; highlight function names
	  ))))

  :bind
  (:map python-mode-map
	("C-c <RET>" . elpy-shell-send-region-or-buffer)
	("C-c C-c" . elpy-send-current-block))
  )

(use-package sql
  :ensure t
  :config
  ;; Use a more friendly keyword face
  (copy-face 'font-lock-keyword-face 'sql-keyword-face)
  (set-face-attribute 'sql-keyword-face nil 
		      :foreground "#fabd2f"
		      :weight 'bold)
  (add-hook 'sql-mode-hook (lambda ()
			     (set (make-local-variable 'font-lock-keyword-face)
				  'sql-keyword-face)))

  )

;; Upcase sql keywords
(use-package sqlup-mode
  :ensure t
  :diminish sqlup-mode
  :init
  ;; Capitalize keywords in SQL mode
  (add-hook 'sql-mode-hook 'sqlup-mode)
  ;; Capitalize keywords in an interactive session (e.g. psql)
  (add-hook 'sql-interactive-mode-hook 'sqlup-mode)
  )

(use-package tex 
  :ensure auctex)

;; Appearance
(require 'font-latex)

;; Preview-latex
;; (set-default 'preview-scale-function 1.2)

;; Math mode
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;; (set-face-attribute 'font-latex-math-face nil :foreground "#ffffff")

;; Enable query for master file
(setq-default TeX-master nil)		    
(setq TeX-auto-save t			    
      TeX-parse-self t
      TeX-save-query nil
      TeX-PDF-mode t	    
      font-latex-fontify-sectioning 'color
      font-latex-fontify-script nil)    

;; Word-wrap
 (add-hook 'TeX-mode-hook (lambda () (visual-line-mode 1)))


;; Completion
(use-package company-auctex
  :ensure t
  :init
  (company-auctex-init)
  )

(use-package markdown-mode
:ensure t
:commands (markdown-mode gfm-mode)
:mode (("README\\.md\\'" . gfm-mode)
       ("\\.md\\'" . markdown-mode)
       ("\\.markdown\\'" . markdown-mode))
:init
:config
(defun markdown-insert-code-chunk (header) 
  "Insert an code chunk in markdown mode. Necessary due to interactions between polymode and yas snippet" 
  (interactive "sChunk header: ") 
  (insert (concat "```{" header "}\n\n```")) 
  (forward-line -1))
:bind
(:map markdown-mode-map
 ("C-c i" . markdown-insert-code-chunk))
)

(use-package highlight-defined
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode)
   )

(use-package highlight-quoted
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode)
  (set-face-attribute 'highlight-quoted-symbol nil
		      :inherit 'font-lock-string-face))

;; Keybinding for terminal
(global-set-key [f2] 'eshell)

;; Company
(use-package company-shell
  :ensure t
  :config
  (add-to-list 'company-backends '(company-shell company-shell-env company-fish-shell))
  )

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-element-highlight t)
  (set-face-attribute 'web-mode-current-element-highlight-face nil
		      :weight 'bold
		      :background "#665c54")
  )

(use-package web-beautify
  :ensure t
  :config
  :bind (:map web-mode-map
	      ("C-c b" . web-beautify-html)
	 :map css-mode-map
	      ("C-c b". web-beautify-css))
  )

(use-package gnuplot-mode
  :ensure t
  :mode ("\\.\\(gp\\|gnuplot\\)$" . gnuplot-mode)
  )

(use-package plantuml-mode
  :ensure t
  :mode ("\\.plantuml\\'" . plantuml-mode)
  :config
  ;; Path to jar file, remember to put it in the right folder
  (setq plantuml-jar-path (expand-file-name "~/Java/plantuml.jar"))
  ;; Add to org-plantuml
  (setq org-plantuml-jar-path (expand-file-name "~/Java/plantuml.jar"))
  )

;; Use flyspell for English spell-checking
(use-package flyspell
  :ensure t
  :config
  ;; (add-hook 'text-mode-hook 'flyspell-mode)
  (set-face-attribute 'flyspell-duplicate nil :underline "DeepPink")
  (set-face-attribute 'flyspell-incorrect nil :underline "Red1")
  :diminish flyspell-mode
  )

(use-package ispell)

;; (use-package langtool
;;   :ensure t
;;   :config
;;   ;; Set path to the Java tool
;;   (setq langtool-language-tool-jar 
;; 	  "~/Java/LanguageTool-3.8/languagetool-commandline.jar")
;;   ;; Show messages as pop-up
;;   (defun langtool-autoshow-detail-popup (overlays)
;;     (when (require 'popup nil t)
;; 	(unless (or popup-instances
;; 		    (memq last-command '(keyboard-quit)))
;; 	  (let ((msg (langtool-details-error-message overlays)))
;; 	    (popup-tip msg)))))
;;   (setq langtool-autoshow-message-function
;; 	  'langtool-autoshow-detail-popup)
;;   )


;; (use-package writegood-mode
;;   :ensure t
;;   :config
;;   (set-face-attribute 'writegood-duplicates-face nil :underline "DeepPink")
;;   (set-face-attribute 'writegood-passive-voice-face nil :underline "Cyan")
;;   (set-face-attribute 'writegood-weasels-face nil :underline "DarkOrange")
;;   (add-hook 'text-mode-hook 'writegood-mode)
;;   :diminish writegood-mode
;;   )


(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  )

(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
	    (id (one-or-more (not (any " "))))
	    (message) line-end))
  :modes (text-mode markdown-mode gfm-mode org-mode))

(add-to-list 'flycheck-checkers 'proselint)

;; (add-hook 'markdown-mode-hook 'flycheck-mode)
;; (add-hook 'text-mode-hook 'flycheck-mode)


(use-package flycheck-pos-tip
  :ensure t
  :diminish flycheck-pos-tip-mode
  :config
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode))
  )

(use-package define-word
  :ensure t
  :bind
  (:map pdf-view-mode-map
	("l" . define-word))
  )

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode 1)
  )
