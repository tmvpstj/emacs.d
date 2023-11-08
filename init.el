;; -*- coding: utf-8; lexical-binding: t; -*-

(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tooltip-mode -1)
  ;;(pixel-scroll-mode)
  )

(blink-cursor-mode 0)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("ublt" . "https://elpa.ubolonton.org/packages/") t)
(setq package-native-compile t)
(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (message "refreshing contents")
  (unless package-archive-contents (package-refresh-contents))
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq gc-cons-threshold 100000000)
(setq max-spedcl-size 5000)

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(use-package use-package-ensure-system-package)
(use-package try)

(setq
 inhibit-startup-screen nil
 initial-scratch-message nil
 sentence-end-double-space nil
 ring-bell-function 'ignore
 save-interprogram-paste-before-kill t
 use-dialog-box nil
 mark-even-if-inactive nil
;; will delete entire line on c-k
;; kill-whole-line t
 case-fold-search nil
 use-short-answers t
 ;; choose a good dir
 ;; default-directory "~/src"
 ;; fast-but-imprecise-scrolling t
 load-prefer-newer t
 confirm-kill-processes nil
 native-comp-async-report-warnings-errors 'silent
 help-window-select t
 delete-by-moving-to-trash t
 scroll-preserve-screen-position t
 completions-detailed t
 next-error-message-highlight t
 read-minibuffer-restore-windows t
 save-some-buffers-default-predicate 'save-some-buffers-root
 kill-do-not-save-duplicates t
 )

(setq-default indent-tabs-mode nil)

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

(delete-selection-mode t)
(global-display-line-numbers-mode t)
(column-number-mode)
(savehist-mode)

(require 'hl-line)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

(setq custom-file (make-temp-name "/tmp/"))
(setq custom-safe-themes t)

(use-package s)
(use-package shut-up)

(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq require-final-newline t)
(bind-key "C-c q" #'fill-paragraph)
(bind-key "C-c Q" #'set-fill-column)

(setq mouse-wheel-tilt-scroll t)
(setq-default truncate-lines t)

(use-package fancy-compilation :config (fancy-compilation-mode))

(set-face-attribute 'default nil :font "Iosevka Term-13")
(set-face-attribute 'variable-pitch nil :font "Iosevka Term-13")

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package doom-themes
  :config
  (let ((chosen-theme 'doom-opera-light))
    (doom-themes-visual-bell-config)
    (load-theme chosen-theme)))

(use-package mood-line)

(mood-line-mode)

(use-package which-key
  :diminish
  :custom
  (which-key-enable-extended-define-key t)
  :config
  (which-key-mode)
  (which-key-setup-side-window-right))

(use-package magit)

;; (use-package spacious-padding)
;; (spacious-padding-mode)

(modify-all-frames-parameters
  '((internal-border-width . 50)
  (right-divider-width . 12)))

;;(dolist (face '(window-divider
;;                window-divider-first-pixel
;;              window-divider-last-pixel))
;;  (face-spec-reset-face face)
;;(set-face-foreground face (face-attribute 'default :background)))
;;(set-face-background 'fringe (face-attribute 'default :background))
