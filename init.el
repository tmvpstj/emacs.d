;; -*- coding: utf-8; lexical-binding: t; -*-

;; removes window decorations in non-daemon windows
(when (window-system)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (menu-bar-mode -1)
    (tooltip-mode -1)
  ;;(pixel-scroll-mode)
)

(defun my-frame-config (frame)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tooltip-mode -1)
  (set-face-attribute 'default nil :font "Iosevka Term-12.5")
  (set-face-attribute 'variable-pitch nil :font "Iosevka Term-12.5")
  ;;(remove-hook 'after-make-frame-functions #'my-frame-config)
  )

;; removes window decorations and sets font for client windows
(add-hook 'after-make-frame-functions #'my-frame-config)

;; (add-hook 'after-make-frame-functions
;;          (lambda (f) (set-face-attribute 'default :font "Iosevka Term-12.5")))

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
(setq-default tab-width 2)

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

(delete-selection-mode t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
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

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(set-face-attribute 'default nil :font "Iosevka Term-13")
(set-face-attribute 'variable-pitch nil :font "Iosevka Term-13")

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

;; org-mode stuff
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(setq calendar-week-start-day 0)
(setq org-agenda-files  (list "~/org/")
      org-refile-targets '((org-agenda-files :maxlevel . 5))
      org-refile-use-outline-path 'file
      org-log-into-drawer t
      org-log-done 'time
      org-log-redeadline 'time
      org-log-reschedule 'time
      )

;; auctex
(use-package tex
  :ensure auctex)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(use-package auctex-latexmk)

(require 'auctex-latexmk)
(auctex-latexmk-setup)

(use-package all-the-icons)

;; dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
;; (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
(setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
;; (setq dashboard-startup-banner 'logo)
(setq dashboard-center-content t)
(setq dashboard-set-navigator t)
(setq dashboard-set-footer nil)
(setq dashboard-week-agenda t)
(setq dashboard-icon-type 'all-the-icons)
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-heading-icons '((recents   . "clock")
                                (bookmarks . "bookmark")
                                (agenda    . "calendar")
                                (projects  . "rocket")
                                (registers . "database")))

(add-hook 'server-after-make-frame-hook (lambda()
    (switch-to-buffer dashboard-buffer-name)
    (dashboard-mode)
    (dashboard-insert-startupify-lists)

    (dashboard-refresh-buffer)))

;; treesitter
(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)))
