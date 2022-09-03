(setq gc-cons-threshold (* 50 1000 1000))

(setq inhibit-startup-message t)
;; Set default directory

(when (eq system-type 'windows-nt)
    (setq default-directory "c:/Users/aitor/")
  )

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode 1)
(global-display-line-numbers-mode 1)
(load-theme 'modus-vivendi t)

(recentf-mode 1)
(set-default-coding-systems 'utf-8)
(setq large-file-warning-threshold nil)

(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Tab width --------------------------------------------

(setq-default tab-width 2)

;; Use spaces instead of tabs for indentation

(setq-default indent-tabs-mode nil)

;; Enable line numbers for some modes ---------------------------
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Disable line numbers for some modes ---------------------------
(dolist (mode '(org-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Disable line hl-line for some modes

(dolist (mode '(shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (setq-local global-hl-line-mode nil))))

;; Font family
(set-face-attribute 'default nil :family "MesloLGS NF" :height 110)

(set-face-attribute 'fixed-pitch nil :family "MesloLGS NF" :height 110)

;; Initialize package sources 

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-install 'use-package))

(column-number-mode)
(global-display-line-numbers-mode t)

;; Initialize use-package on non-Linux platforms

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ivy mode for lower buffer completion

(use-package swiper :ensure t)
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-n" . ivy-next-line)
	 ("C-p" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-p" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
 	 ("C-p" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))

  ;; Individual history elements can be configured separately
  ;;(put 'minibuffer-history 'history-length 25)
  ;;(put 'evil-ex-history 'history-length 50)
  ;;(put 'kill-ring 'history-length 25))

(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-minibuffer-flag t)
  :config
  (ace-window-display-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :after counsel)

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config (setq ivy-initial-inputs-alist nil)
  )

(use-package flx  ;; Improves sorting for fuzzy-matched results
  :after ivy
  :defer t
  :init
  (setq ivy-flx-limit 10000))

(use-package ivy-posframe
  :disabled
  :custom
  (ivy-posframe-width      115)
  (ivy-posframe-min-width  115)
  (ivy-posframe-height     10)
  (ivy-posframe-min-height 10)
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters '((parent-frame . nil)
                                  (left-fringe . 8)
                                  (right-fringe . 8)))
  (ivy-posframe-mode 1))

(use-package prescient
  :after counsel
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :after prescient
  :config
  (ivy-prescient-mode 1))


(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Set evil mode if you want

(use-package hydra) ;; A package for transitient keybindings


;; Set default connection mode to SSH
(setq tramp-default-method "ssh")

;; Emacs as an external editor

(defun dw/show-server-edit-buffer (buffer)
  ;; TODO: Set a transient keymap to close with 'C-c C-c'
  (split-window-vertically -15)
  (other-window 1)
  (set-buffer buffer))

(setq server-window #'dw/show-server-edit-buffer)

;; Automatically clean whitespace

(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

;; Highlight matching parens

(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

;; Projectile

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-excepr-diff-v1))

(use-package forge
  :after magit)

;; Org Mode Configuration ------------------------------------------------------

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "MesloLGS NF" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (efs/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

;; shell settings

(when (eq system-type 'windows-nt)
    (progn
      (setq explicit-shell-file-name "powershell.exe")
      (setq explicit-powershell.exe-args '()))
  )

;; eshell settings

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (setq eshell-history-size         10000
	eshell-buffer-maximum-lines 10000
	eshell-hist-ignoredups t
	eshell-scroll-to-bottom-on-input t))
  

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config
  (eshell-git-prompt-use-theme 'powerline))

;; Dired

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first")))

(use-package dired-single
  :after dired)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :after dired
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode))

;; Update packages ----------------------------------------------------------

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;; lsp-mode and company-mode configuration ----------------------------------------

(use-package lsp-mode
  :commands lsp
  :hook ((typescript-mode js2-mode web-mode) . lsp)
  :bind (:map lsp-mode-map
         ("TAB" . completion-at-point))
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-identation nil)
  (lsp-enable-on-type-formatting nil)
  )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show))

(use-package lsp-treemacs
  :after lsp-mode
  )

(setq lsp-ui-sideline-enable nil)
(setq lsp-ui-sideline-show-hover nil)

(use-package dap-mode
  :custom
  (lsp-enable-dap-auto-configure nil)
  :config
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (require 'dap-node)
  (dap-node-setup))

(use-package company
  :after (lsp-mode)
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; Languages

;; Typescript and javascript ---------------------------------

(use-package nvm
  :defer t)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(defun dw/set-js-indentation ()
  (setq js-indent-level 2)
  (setq-default tab-width 2))

(use-package js2-mode
  :mode "\\.jsx?\\'"
  :config
  ;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil)

  ;; Set up proper indentation in JavaScript and JSON files
  (add-hook 'js2-mode-hook #'dw/set-js-indentation)
  (add-hook 'json-mode-hook #'dw/set-js-indentation))

(use-package apheleia
  :config
  (apheleia-global-mode +1))

(use-package prettier-js
  :hook ((js2-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)
         (web-mode . prettier-js-mode))
  :config
  (setq prettier-js-show-errors nil))

;; HTML

(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

;; 1. Start the server with `httpd-start'
;; 2. Use `impatient-mode' on any buffer

(use-package impatient-mode)

(use-package skewer-mode)

;; Prettier

(use-package rainbow-mode
  :defer t
  :hook (org-mode
         emacs-lisp-mode
         web-mode
         typescript-mode
         js2-mode
	       css-mode
         html-mode))

(use-package emmet-mode
  :defer t
  :hook (web-mode
         html-mode
	       css-mode))

;; Python

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))

(use-package anaconda-mode
  :after python-mode
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)))

;; Parinfy for lispy languages

(use-package parinfer
  :disabled
  :hook ((clojure-mode . parinfer-mode)
         (emacs-lisp-mode . parinfer-mode)
         (common-lisp-mode . parinfer-mode)
         (scheme-mode . parinfer-mode)
         (lisp-mode . parinfer-mode))
  :config
  (setq parinfer-extensions
      '(defaults       ; should be included.
        pretty-parens  ; different paren styles for different modes.
        ;;evil           ; If you use Evil.
        smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
        smart-yank)))  ; Yank behavior depend on mode.

(add-hook 'emacs-lisp-mode-hook 'company-mode)

;; Flycheck syntax checking

(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))

;; Snippets

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

;; Smartparens

(use-package smartparens
  :hook (prog-mode . smartparens-mode))
(require 'smartparens-config)

(add-hook 'prog-mode-hook (lambda() (electric-pair-mode t)))

;; PDF Tools

(use-package pdf-tools
  :defer t
  :config
  (progn
    (pdf-tools-install)
    (setq-default pdf-view-display-size 'fit-page)
    (define-key pdf-view-mode-map (kbd "h")
			'pdf-annot-add-highlight-markup-annotation)
		(define-key pdf-view-mode-map (kbd "t")
			'pdf-annot-add-text-annotation)
		(define-key pdf-view-mode-map (kbd "d")
			'pdf-annot-delete)
		(define-key pdf-view-mode-map (kbd "s")
				   'pdf-annot-add-strikeout-markup-annotation))
  )

;; LaTex

(use-package reftex
  :ensure t
  :defer t
  :config
  (setq reftex-cite-prompt-optional-args t)) ;; Prompt for empty optional arguments in cite

(setq ispell-program-name "c:/Users/aitor/Dropbox/utils/hunspell/bin/hunspell.exe")
(setq ispell-local-dictionary "en_US")
(setq ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))

(use-package auto-dictionary
  :ensure t
  :init(add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1))))

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config (progn
			      (setq TeX-source-correlate-mode t)
			      (setq TeX-source-correlate-method 'synctex)
			      (setq reftex-plug-into-AUCTeX t)
			      (pdf-tools-install)
			      (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
				          TeX-source-correlate-start-server t)
			      ;; Update PDF buffers after successful LaTeX runs
			      (add-hook 'TeX-after-compilation-finished-functions
					            #'TeX-revert-document-buffer)
			      (add-hook 'LaTeX-mode-hook
					            (lambda ()
						            (reftex-mode t)
						            (flyspell-mode t)
                        (company-mode t)))
			      )
  )

(use-package company-auctex
  :after (latex-mode company-mode)
  :config (company-auctex-init)
  )

;; Startup time

;; Make gc pauses faster by decreasing the threshold. ----------------------------------

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                   (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(setq gc-cons-threshold (* 2 1000 1000))
