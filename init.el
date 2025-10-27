(setq gc-cons-threshold (* 50 1000 1000))

(setq debug-on-error nil)
(setq make-backup-files nil)

(setq inhibit-startup-message t)

;; Set default directory for Windows, for Linux is ~/
(when (eq system-type 'windows-nt)
  (setq default-directory "c:/Users/aitor/"))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode 1)

(load-theme 'modus-vivendi-tinted t)

(recentf-mode 1)
(set-default-coding-systems 'utf-8)
(setq large-file-warning-threshold nil)

(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(column-number-mode)


;; Tab width --------------------------------------------

;; Use spaces instead of tabs for indentation in programming modes
(add-hook 'prog-mode-hook (lambda ()
                            (setq tab-width 2)
                            (setq indent-tabs-mode nil)))

;; Enable smart indentation on new lines
(electric-indent-mode 1)

;; Mode-specific indentation settings
(add-hook 'css-mode-hook (lambda () (setq css-indent-offset 2)))
(add-hook 'scss-mode-hook (lambda () (setq css-indent-offset 2)))
(add-hook 'python-mode-hook (lambda () (setq python-indent-offset 4)))
(add-hook 'js-ts-mode-hook (lambda ()
                             (setq js-indent-level 2)
                             (setq js-jsx-indent-level 2)))
(add-hook 'typescript-ts-mode-hook (lambda () (setq typescript-ts-mode-indent-offset 2)))
(add-hook 'tsx-ts-mode-hook (lambda () (setq typescript-ts-mode-indent-offset 2)))


;; Enable line numbers by default in programming modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Display line numbers mode trigger in programming modes
(define-key global-map (kbd "<f9>") #'display-line-numbers-mode)


;; Disable line numbers for some modes, even if pressing <f9>
(dolist (mode '(org-mode-hook
                eshell-mode-hook
                shell-mode-hook
                term-mode-hook
                vterm-mode-hook
                doc-view-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Disable line hl-line for some modes

(dolist (mode '(shell-mode-hook
                eshell-mode-hook
                term-mode-hook
                vterm-mode-hook
                doc-view-mode-hook))
  (add-hook mode (lambda () (setq-local global-hl-line-mode nil))))

;; Copy and cut line keybindins

(defun as/copy-line ()
  "Copy the current line to the kill ring without moving cursor."
  (interactive)
  (save-excursion
    (kill-ring-save (line-beginning-position) (line-beginning-position 2)))
  (message "Line copied to kill ring"))

(defun as/cut-line ()
  "Cut current line to kill ring."
  (interactive)
  (kill-whole-line)
  (message "Line cut"))

(global-set-key (kbd "C-c c") 'as/copy-line)
(global-set-key (kbd "C-c x") 'as/cut-line)


(defun as/reload-buffer ()
  "Reload the current buffer."
  (interactive)
  (revert-buffer t t))

(global-set-key (kbd "<f5>") 'as/reload-buffer)

;; Font family
(set-face-attribute 'default nil :family "Iosevka NF" :height 120)

(set-face-attribute 'fixed-pitch nil :family "Iosevka NF" :height 120)

;; Support for Unicode Emoji Characters

(when (member "Segoe UI Emoji" (font-family-list))
  (set-fontset-font t 'emoji "Segoe UI Emoji" nil 'prepend)
  (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'prepend)
  (set-fontset-font t 'unicode "Segoe UI Emoji" nil 'prepend))

;; Initialize straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package via straight
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t))


(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  ;; Set the PATH variable from the shell
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH"))
  ;; Initialize exec-path-from-shell
  (exec-path-from-shell-initialize))

;; Revert buffer settings

(use-package autorevert
  :straight nil
  :config
  (global-auto-revert-mode 1)
  (setq auto-revert-use-notify t)
  (setq auto-revert-verbose nil)
  (setq auto-revert-interval 0.5))

;; ivy mode for lower buffer completion

(use-package swiper)
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

(use-package undo-tree
  :config
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode 1))

(use-package nerd-icons)

;; Mode line configuration --------------------------------------------

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 3)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil))

(use-package spacious-padding
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8
           :fringe-width 8))

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as it
  ;; is very flexible and provides several examples.
  (setq spacious-padding-subtle-frame-lines
        `( :mode-line-active 'default
           :mode-line-inactive vertical-border)))


(spacious-padding-mode 1)

;; Set a key binding if you need to toggle spacious padding.
(define-key global-map (kbd "<f8>") #'spacious-padding-mode)


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package savehist
  :init (savehist-mode)
  :ensure nil
  :config
  (setq history-length 25))

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
  (ace-window-display-mode 0))

(use-package avy
  :config
  (global-set-key (kbd "C-:") 'avy-goto-char)
  (global-set-key (kbd "C-'") 'avy-goto-char-2)
  (global-set-key (kbd "C-;") 'avy-goto-char-timer)
  (global-set-key (kbd "M-g f") 'avy-goto-line)
  (global-set-key (kbd "M-g w") 'avy-goto-word-1)
  (global-set-key (kbd "M-g e") 'avy-goto-word-0))

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
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Transient keybindings

(use-package hydra) ;; A package for transitient keybindings
(use-package ivy-hydra
  :after hydra
  :config
  (setq ivy-hydra-virtual-buffers t)
  (setq ivy-hydra-virtual-buffers-display-style 'one-line))
(use-package transient) ;; A package for transient keybindings

;; vterm

(use-package vterm
  :config
  (setq vterm-timer-delay nil))

(use-package multi-vterm
  :after vterm
  :bind (("C-c v t" . multi-vterm)
         ("C-c v p" . multi-vterm-prev)
         ("C-c v n" . multi-vterm-next)))

;; Set default connection mode to SSH

(use-package tramp
  :config
  (setq tramp-default-method "ssh"))

;; Docker

(use-package docker
  :bind ("C-c d" . docker))

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
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))


;; Magit

(use-package magit
  :after transient
  :bind (("C-x g" . magit-status))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-excepr-diff-v1))

(use-package forge
  :after magit)

;; shell and term settings

(if (eq system-type 'windows-nt)
    (progn
      (setq explicit-shell-file-name "powershell.exe")
      (setq explicit-powershell.exe-args '()))
  (setq explicit-shell-file-name "/bin/bash"))


(use-package term
  :config
  (define-key term-raw-map (kbd "M-o") 'ace-window)
  (define-key term-raw-map (kbd "M-x") 'counsel-M-x))

;; eshell settings

(defun as/configure-eshell ()
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
  :hook (eshell-first-time-mode . as/configure-eshell)
  :config
  (eshell-git-prompt-use-theme 'powerline))

(use-package eshell-toggle
  :bind ("C-M-'" . eshell-toggle)
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil))

;; Dired

(require 'dired)  ;; Ensure dired is loaded

;; Keybinding for dired-jump
(define-key global-map (kbd "C-x C-j") 'dired-jump)

;; Custom settings for dired
(setq dired-listing-switches "-agho --group-directories-first")

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

;; Tree-sitter configuration --------------------------------------------

(setq treesit-language-source-alist
   '((clojure "https://github.com/sogaiu/tree-sitter-clojure")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (haskell "https://github.com/tree-sitter/haskell-tree-sitter")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/py-tree-sitter")
     (java "https://github.com/serenadeai/java-tree-sitter")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (node "https://github.com/tree-sitter/node-tree-sitter")))

;; Eglot (built-in client for LSP)
(use-package eglot
  :ensure nil
  :functions (eglot-ensure)
  :commands (eglot)
  :hook ((typescript-ts-mode js-ts-mode typescript-mode web-mode 
          css-ts-mode css-mode scss-mode python-mode) . eglot-ensure)
  :custom
  (eglot-sync-connect nil)
  (eglot-autoshutdown t)
  :config
  (add-to-list 'eglot-server-programs 
               '((js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode) 
                 . ("typescript-language-server" "--stdio" "--log-level" "1")))
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pylsp"))))

;; Orderless
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides nil))


;; Corfu completion framework
(use-package corfu
  :ensure t
  :init (global-corfu-mode)
  :bind
  (:map corfu-map
        ("TAB" . corfu-complete)
        ([M-backspace] . backward-kill-word)
        ([M-d] . kill-word)
        ([M-o] . ace-window))
  :config
  (setq corfu-auto t)
  (setq tab-always-indent 'complete)
  (setq corfu-preview-current nil)
  (setq corfu-auto-delay 0.2)
  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1)
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))
)

;; Icons for Corfu
(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


;; Treemacs git mode set to simple. Does not require python 3
(setq treemacs-git-mode 'simple)

;; Tremacs config

(use-package treemacs
  :bind (("C-c t" . as/treemacs-toggle-and-focus))
  :config
  (setq treemacs-width 35)
  (setq treemacs-follow-mode t)
  (setq treemacs-filewatch-mode t))

(use-package treemacs-projectile
  :after (treemacs-projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs-magit)
  :ensure t)



;; Languages

;; Typescript and javascript ---------------------------------

(use-package nvm
  :config
  (nvm-use "24.3.0"))

;; Use tree-sitter modes for JavaScript and typescript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cjs\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js-ts-mode))

;; Use tree-sitter modes for TypeScript
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

;; Prettier integration

(use-package prettier-js
  :hook ((js-ts-mode . prettier-js-mode)
         (typescript-ts-mode . prettier-js-mode)
         (tsx-ts-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)))

;; HTML

(use-package web-mode
  :mode ("\\.\\(html?\\|ejs\\)\\'")
  :config
  (setq-default web-mode-engines-alist '(("django" . "\\.html\\'")))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-attribute-indent-offset 2
        web-mode-enable-auto-closing t
        web-mode-enable-auto-quoting t
        web-mode-enable-css-colorization t))

;; 1. Start the server with `httpd-start'
;; 2. Use `impatient-mode' on any buffer

(use-package impatient-mode)

(use-package skewer-mode)

(use-package rainbow-mode
  :defer t
  :hook (org-mode
         emacs-lisp-mode
         web-mode
         typescript-ts-mode
         tsx-ts-mode
         js-ts-mode
         typescript-mode
	       css-mode
         scss-mode
         html-mode))

(use-package emmet-mode
  :defer t
  :hook (web-mode
         html-mode
	       css-mode
         scss-mode
         js-ts-mode
         tsx-ts-mode
         typescript-ts-mode
         typescript-mode)
  :config
  (add-to-list 'emmet-jsx-major-modes 'js-ts-mode)
  (add-to-list 'emmet-jsx-major-modes 'tsx-ts-mode)
  (add-to-list 'emmet-jsx-major-modes 'typescript-ts-mode))

;; Python

(use-package python-mode
  :hook (python-mode . eglot-ensure)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  (python-indent-offset 4))

(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))

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


;; Flycheck syntax checking

(use-package flycheck
  :defer t
  :hook (prog-mode . flycheck-mode))

;; Smartparens

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config)
  ;; Make smartparens less aggressive with auto-pairing to avoid conflicts with Copilot
  (setq sp-autoinsert-if-followed-by-word nil)
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-pair "[" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-pair "(" nil :post-handlers '(("||\n[i]" "RET"))))


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

;; Yaml

(use-package yaml-mode
  :mode "\\.yaml\\'")

;; Markdown mode

(use-package markdown-mode
  :mode "\\.md\\'"
  ;; Set the markdown compiler
  :init (if (eq system-type 'windows-nt)
            (setq markdown-command "c:/libMultiMarkdown-6.7.0/bin/multimarkdown.exe")
          (setq markdown-command "~/utils/MultiMarkdown-6/build/multimarkdown"))
  :config
  (defun as/set-markdown-header-font-sizes ()
    (dolist (face '((markdown-header-face-1 . 1.2)
                    (markdown-header-face-2 . 1.1)
                    (markdown-header-face-3 . 1.0)
                    (markdown-header-face-4 . 1.0)
                    (markdown-header-face-5 . 1.0)))
      (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))

  (defun as/markdown-mode-hook ()
    (as/set-markdown-header-font-sizes)
    (display-line-numbers-mode 0))

  (add-hook 'markdown-mode-hook 'as/markdown-mode-hook))

;; Org Mode Visual Enhancements

(use-package org
  :straight nil  ; Use built-in org-mode
  :config
  ;; Better looking org-mode
  (setq org-hide-emphasis-markers t)
  (setq org-startup-indented t)
  (setq org-pretty-entities t)
  (setq org-use-sub-superscripts '{})
  (setq org-image-actual-width '(400))

  ;; Custom font sizes for headers
  (dolist (face '((org-level-1 . 1.4)
                  (org-level-2 . 1.3)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :weight 'bold :height (cdr face)))

  ;; Better looking lists
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Custom colors for TODO keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "#ff6c6b" :weight bold))
          ("IN-PROGRESS" . (:foreground "#ECBE7B" :weight bold))
          ("WAITING" . (:foreground "#a9a1e1" :weight bold))
          ("DONE" . (:foreground "#98be65" :weight bold))
          ("CANCELLED" . (:foreground "#5B6268" :weight bold))))

  ;; Better block styling
  (setq org-src-block-faces
        '(("emacs-lisp" (:background "#2d3748"))
          ("javascript" (:background "#2d3748"))
          ("python" (:background "#2d3748"))
          ("css" (:background "#2d3748"))))

  ;; Enable syntax highlighting in code blocks
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-confirm-babel-evaluate nil)

  ;; Better table alignment
  (setq org-table-auto-blank-field nil)

  ;; Org mode hook for additional customizations
  (add-hook 'org-mode-hook
            (lambda ()
              (variable-pitch-mode 1)
              (visual-line-mode 1)
              (setq line-spacing 0.1))))

;; Org Bullets for prettier bullet points
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Org Superstar (alternative to org-bullets with more features)
(use-package org-superstar
  :disabled  ; Enable this instead of org-bullets if preferred
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●"))
  (org-superstar-special-todo-items t)
  (org-superstar-todo-bullet-alist '(("TODO" . "☐")
                                     ("IN-PROGRESS" . "⚡")
                                     ("WAITING" . "⏳")
                                     ("DONE" . "☑")
                                     ("CANCELLED" . "✘"))))

;; Visual Fill Column for better reading experience
(use-package visual-fill-column
  :hook (org-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 100)
  (visual-fill-column-center-text t))

;; Org Modern for a more modern look
(use-package org-modern
  :disabled  ; Enable if you want an even more modern look
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-keyword nil)
  (org-modern-checkbox nil)
  (org-modern-table nil))

;; LaTex

(use-package reftex
  :defer t
  :config
  (setq reftex-cite-prompt-optional-args t)) ;; Prompt for empty optional arguments in cite

(if (eq system-type 'windows-nt)
    (setq ispell-program-name "c:/Users/aitor/Dropbox/utils/hunspell/bin/hunspell.exe")
  (setq ispell-program-name "/usr/bin/hunspell" ))

(setq ispell-local-dictionary "en_US")
(setq ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
        ("es_ES" "[[:alpha:]]" "[^[:alpha:]]" "['’]" nil ("-d" "es_ES") nil utf-8)))

(use-package auto-dictionary
  :init(add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1))))

(use-package auctex
  :straight t
  :mode ("\\.tex\\'" . latex-mode)
  :config (progn
			      (setq TeX-source-correlate-mode t)
			      (setq TeX-source-correlate-method 'synctex)
			      (setq reftex-plug-into-AUCTeX t)
			      (pdf-tools-install)
			      (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
				          TeX-source-correlate-start-server t)
            (add-to-list 'safe-local-variable-values
                         '(TeX-command-extra-options . "-shell-escape"))
            (add-to-list 'TeX-command-list
                         '("LaTeX-shell-escape" "pdflatex -shell-escape --synctex=1 %s"
                           TeX-run-command nil t
                           :help "Run pdflatex with -shell-escape"))
			      ;; Update PDF buffers after successful LaTeX runs
			      (add-hook 'TeX-after-compilation-finished-functions
					            #'TeX-revert-document-buffer)
			      (add-hook 'LaTeX-mode-hook
					            (lambda ()
						            (reftex-mode t)
						            (flyspell-mode t)
                        (smartparens-mode t)
                        ;; Add cape-tex for TeX symbol completion
                        (add-to-list 'completion-at-point-functions #'cape-tex)))
			      )
  )


;; Add cape-elisp-symbol to Emacs Lisp mode
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)))

;; AI tools ------------------------------------------------------------

(defun as/read-openai-key ()
  (with-temp-buffer
    (if (eq system-type 'windows-nt)
        (insert-file-contents "c:/Users/aitor/emacs-openai-key.txt")
      (insert-file-contents "~/Documents/keys/emacs-openai-key.txt"))
    (string-trim (buffer-string))))

(defun as/read-deepseek-key ()
  (with-temp-buffer
    (if (eq system-type 'windows-nt)
        (insert-file-contents "c:/Users/aitor/emacs-deepseek-key.txt")
      (insert-file-contents "~/Documents/keys/emacs-deepseek-key.txt"))
    (string-trim (buffer-string))))

(defun as/read-anthropic-key ()
  (with-temp-buffer
    (if (eq system-type 'windows-nt)
        (insert-file-contents "c:/Users/aitor/emacs-anthropic-key.txt")
      (insert-file-contents "~/Documents/keys/emacs-anthropic-key.txt"))
    (string-trim (buffer-string))))

(use-package gptel
  :defer t
  :init
  (setq-default gptel-model 'o1
                gptel-api-key #'as/read-openai-key
                gptel-stream t
                gptel-track-media t)
  (gptel-make-anthropic "Claude" :stream t :key #'as/read-anthropic-key)
  (gptel-make-deepseek "Deepseek" :stream t :key #'as/read-deepseek-key)
  (gptel-make-gh-copilot "Copilot")
  :config
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (defvar gptel-lookup--history nil)
  (defun gptel-lookup (prompt)
    (interactive (list (read-string "Ask ChatGPT: " nil gptel-lookup--history)))
    (when (string= prompt "") (user-error "A prompt is required."))
    (gptel-request
        prompt
      :callback
      (lambda (response info)
        (if (not response)
            (message "gptel-lookup failed with message: %s" (plist-get info :status))
          (with-current-buffer (get-buffer-create "*gptel-lookup*")
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert response))
            (special-mode)
            (display-buffer (current-buffer)
                            `((display-buffer-in-side-window)
                              (side . bottom)
                              (window-height . ,#'fit-window-to-buffer)))))))))



(use-package copilot
  :config
  (setq copilot-log-max 0)
  (setq copilot-idle-delay 1.2)
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(closure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  :bind
  ("M-/" . copilot-complete)
  ("C-c f". copilot-accept-completion)
  ("C-c l" . copilot-accept-line)
  ("C-c m" . copilot-accept-completion-by-word)
  :hook (prog-mode . copilot-mode))


(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (setenv "ANTHROPIC_API_KEY" (as/read-anthropic-key))
  (setenv "DEEPSEEK_API_KEY" (as/read-deepseek-key))
  (setenv "OPENAI_API_KEY" (as/read-openai-key))
  ;; (setq aidermacs-extra-args '("--thinking-tokens" "16k"))
  :custom
  (aidermacs-default-chat-mode 'architect)
  (aidermacs-default-model "r1")
  (aidermacs-backend 'vterm))


;; Startup time

(defun as/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                   (time-subtract after-init-time before-init-time)))
           gcs-done))


(add-hook 'emacs-startup-hook #'as/display-startup-time)


;; Avoid emacs to add custom-set-variables and custom-set-faces

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)


;; Suppress specific Eglot notification messages
(defun as/eglot-handle-notification-advice (orig-fun server method &rest params)
  "Suppress noisy Eglot notification messages."
  (unless (and (string= method "window/showMessage")
               (plist-get (car params) :message)
               (string-match-p "Inlay Hints request failed\\|document should be opened first"
                              (plist-get (car params) :message)))
    (apply orig-fun server method params)))

(advice-add 'eglot-handle-notification :around #'as/eglot-handle-notification-advice)

;; Make gc pauses faster by decreasing the threshold. ----------------------------------

(setq gc-cons-threshold (* 2 1000 1000))
