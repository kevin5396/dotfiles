; beginning
(setq message-log-max 10000)

(setq load-prefer-newer t)

;; use-package bootstrap
(require 'package)
(setq package-enable-at-startup nil)

;; set to faster mirrors
(setq package-archives
      '(("melpa"        . "http://elpa.emacs-china.org/melpa/")
        ("gnu"          . "http://elpa.emacs-china.org/gnu/")
        ("org"          . "http://elpa.emacs-china.org/org/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

(require 'diminish)
(require 'bind-key)

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

(setq tmp-file-directory (expand-file-name "~/.emacs.d/tmp"))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)


					; User interface

;; theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(use-package dracula-theme
  :ensure t
  :config (load-theme 'dracula t))

(use-package spaceline)
(use-package spaceline-config
  :ensure spaceline
  :config (spaceline-emacs-theme)
  )
;;(use-package all-the-icons)

;; (use-package spaceline-all-the-icons
;;   :after spaceline
;;   :config (spaceline-all-the-icons-theme))



;;; cursor
(setq-default cursor-type '(bar . 2))
(setq default-frame-alist
      (append default-frame-alist
	      `((cursor-color . "#00BFFF"))))



;; Get rid of tool bar, menu bar and scroll bars.  On OS X we preserve the menu
;; bar, since the top menu bar is always visible anyway, and we'd just empty it
;; which is rather pointless.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (and (eq system-type 'darwin) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; smoothish scroll
(use-package mwheel
  :ensure nil
  :defer t
  :config
  (setq mouse-wheel-progressive-speed nil
	mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))))

;; hide the fringe
(set-fringe-mode '(0 . 0))

(set-default 'truncate-lines t)

(delete-selection-mode 1)
(transient-mark-mode 1)
;; No blinking and beeping, no startup screen, no scratch message and short
;; Yes/No questions.
(blink-cursor-mode 1)
(setq ring-bell-function #'ignore
      inhibit-startup-screen t
      echo-keystrokes 0.1
      linum-format " %d "
      initial-scratch-message "Hello, Kevin!\n")
(fset 'yes-or-no-p #'y-or-n-p)
;; Opt out from the startup message in the echo area by simply disabling this
;; ridiculously bizarre thing entirely.
(fset 'display-startup-echo-area-message #'ignore)

(global-linum-mode)


;;; font
(set-face-attribute 'default nil
                    :family "Source Code Pro" :height 130)


;; utf-8 all the things
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


;; System setup

;; `gc-cons-threshold'

;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Garbage-Collection.html
;;
;; I have a modern machine ;)
;;
(setq gc-cons-threshold 20000000)

(setq delete-old-versions t
      make-backup-files nil
      create-lockfiles nil
      ring-bell-function 'ignore
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(server-start) ;; Allow this Emacs process to be a server for client processes.


(use-package page-break-lines           ; Turn page breaks into lines
  :ensure t
  :init (global-page-break-lines-mode)
  :diminish page-break-lines-mode)


(use-package cus-edit
  :ensure nil
  :defer t
  :config
  (setq custom-file "custom.el"
        custom-buffer-done-kill nil            ; Kill when existing
        custom-buffer-verbose-help nil         ; Remove redundant help text
        ;; Show me the real variable name
        custom-unlispify-tag-names nil
        custom-unlispify-menu-entries nil)
  :init (load "custom.el" 'no-error 'no-message))


(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode)
  (show-smartparens-global-mode)
  (dolist (hook '(inferior-emacs-lisp-mode-hook
                  emacs-lisp-mode-hook))
    (add-hook hook #'smartparens-strict-mode))
  :config
  (require 'smartparens-config)
  (setq sp-autoskip-closing-pair 'always)
  :bind
  (:map smartparens-mode-map
	("C-c s u" . sp-unwrap-sexp)
	("C-c s w" . sp-rewrap-sexp))
  :diminish (smartparens-mode))


(use-package ido
  :config
  (setq ido-enable-flex-matching t)
  (ido-everywhere t)
  (ido-mode 1))



;;;; dashboard
(use-package dashboard
  :config
  (progn
    (dashboard-setup-startup-hook)
    (setq dashboard-startup-banner nil))
  )


;; Navigation
(use-package ace-window
  :commands (aw-window-list)
  :defer t
  :config
  (setq aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0)
	aw-scope 'global
	)
  :bind (("C-x o" . ace-window)))

;; Visual
(use-package undo-tree
  :ensure t
  :diminish (undo-tree-mode)
  :init (global-undo-tree-mode))

(use-package hl-line
  :init (global-hl-line-mode 1))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook emacs-lisp-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode)))

(use-package hi-lock
  :init (global-hi-lock-mode))

(use-package highlight-numbers
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'highlight-numbers-mode))


(use-package rainbow-mode
  :ensure t
  :bind (("C-c t r" . rainbow-mode))
  :config (add-hook 'css-mode-hook #'rainbow-mode)
  :diminish (rainbow-mode))


;;Project management
(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'helm)
  (projectile-global-mode)
  (helm-projectile-on)
  (setq projectile-enable-caching nil)
  :diminish (projectile-mode))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))




;;Interface Enchancement

;;                          helm

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
	 ("C-x b" . helm-buffers-list))
  :bind (:map helm-map
              ("M-i" . helm-previous-line)
              ("M-k" . helm-next-line)
              ("M-I" . helm-previous-page)
              ("M-K" . helm-next-page)
              ("M-h" . helm-beginning-of-buffer)
              ("M-H" . helm-end-of-buffer)
              ("<tab>" . helm-execute-persistent-action)
              ("C-i" . helm-execute-persistent-action))
  :config (progn
            (setq helm-buffers-fuzzy-matching t)
            (helm-mode 1)
	    (setq helm-split-window-in-side-p           t
		  helm-buffers-fuzzy-matching           t
		  helm-move-to-line-cycle-in-source     t
		  helm-ff-search-library-in-sexp        t
		  helm-ff-file-name-history-use-recentf t
		  helm-M-x-fuzzy-match                  t
		  helm-recentf-fuzzy-match              t
		  helm-ag-fuzzy-match                   t)

	    (substitute-key-definition 'find-tag 'helm-etags-select global-map)
	    (setq projectile-completion-system 'helm))
  ;; Display helm buffers always at the bottom
  ;; Source: http://www.lunaryorn.com/2015/04/29/the-power-of-display-buffer-alist.html
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*helm" (* not-newline) "*" eos)
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (window-height   . 0.4)))
  :diminish (helm-mode))

(use-package helm-descbinds
  :ensure t
  :bind ("C-h b" . helm-descbinds))

(use-package helm-swoop
  :ensure t
  :bind (("M-m" . helm-swoop)
         ("M-M" . helm-swoop-back-to-last-point))
  :init
  (bind-key "M-m" 'helm-swoop-from-isearch isearch-mode-map))

(use-package helm-ag
  :ensure helm-ag
;  :bind ("M-p" . helm-projectile-ag)
  :commands (helm-ag helm-projectile-ag)
  :init (setq helm-ag-insert-at-point 'symbol))

(use-package helm-info
  :ensure helm
  :bind (([remap info] . helm-info-at-point)
         ("C-c h e"    . helm-info-emacs))
  :config
  ;; Also lookup symbols in the Emacs manual
  (add-to-list 'helm-info-default-sources
               'helm-source-info-emacs))

(use-package helm-flycheck              ; Helm frontend for Flycheck errors
  :ensure t
  :defer t
  :after flycheck)

(use-package helm-pages
  :bind ("M-p" . helm-pages))

(use-package winner                    ; Undo and redo window configurations
  :init (winner-mode))


;; Programming



;;                     completion
(use-package company                    ; Graphical (auto-)completion
  :ensure t
  :init (global-company-mode)
  :config
  (progn
    (delete 'company-dabbrev company-backends)
    (setq company-tooltip-align-annotations t
	  company-tooltip-minimum-width 27
	  company-idle-delay 0.3
	  company-tooltip-limit 10
	  company-minimum-prefix-length 2
	  company-tooltip-flip-when-above t))
  :bind (:map company-active-map
              ("M-k" . company-select-next)
              ("M-i" . company-select-previous)
              ("TAB" . company-complete-selection))
  :diminish company-mode)

(use-package ag
  :ensure t
  :commands (ag ag-regexp ag-project))

(use-package yasnippet
  :ensure t
  :defer t
  :config
  (setq yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-global-mode 1)
  :diminish (yas-minor-mode . " yas"))


;; version control

(use-package magit
  :ensure t
  :defer 2
  :bind (("C-x g" . magit-status))
  :config
  (progn
    (delete 'Git vc-handled-backends)))


;; Latex

(use-package auctex
  :defer t
  :ensure t)


;; python
(use-package anaconda-mode
  :ensure t
  :commands anaconda-mode
  :diminish anaconda-mode
  :init
  (progn
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)))

(use-package company-anaconda
  :ensure t
  :init (add-to-list 'company-backends 'company-anaconda))
