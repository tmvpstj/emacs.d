; -*- coding: utf-8; lexical-binding: t; -*-

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.guix_extra_profiles/emacs/emacs/share/emacs/site-lisp")

(guix-emacs-autoload-packages)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("ublt" . "https://elpa.ubolonton.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(setq package-native-compile t)
(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (message "refreshing contents")
  (unless package-archive-contents
    (package-refresh-contents))
   (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq gc-cons-threshold (* 50 1000 1000))
(setq max-spedcl-size 5000)

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(use-package use-package-ensure-system-package)
(use-package try)

;; removes window decorations and sets font faces
(defun my/frame-setup ()
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tooltip-mode -1)
  (set-fringe-mode 0)
  (spacious-padding-mode 1)
  (tab-bar-mode 1)
  (set-face-attribute 'default nil :font "Iosevka Comfy" :height 130)
  (set-face-attribute 'fixed-pitch nil :font "Iosevka Comfy" :height 130)
  ;; (set-face-attribute 'variable-pitch nil :font "Sarasa Mono CL" :height 130)
  (set-face-attribute 'variable-pitch nil :font "Latin Modern Roman" :height 130)
  ;;(remove-hook 'after-make-frame-functions #'my-frame-config)
)

;; defers frame setup until after a frame is made for client windows
;; sets up frame immediately for standalone emacs
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame (my/frame-setup))))
  (my/frame-setup))

;; show time in modeline
(use-package time
  :ensure nil
  :defer 2
  :custom
  (display-time-default-load-average nil)
  (display-time-24hr-format nil)
  ;; (display-time-day-and-date t)
  :config
  (display-time-mode 1))

(display-battery-mode 1)

;; Shorten the modeline if it is wider than the window
(setopt mode-line-compact 'long)

;; (modify-all-frames-parameters
;;   '((internal-border-width . 30)
;;     (right-divider-width . 4)))

(blink-cursor-mode 0)
(setopt cursor-type '(hbar . 3)
        cursor-in-non-selected-windows nil)

;; (use-package ef-themes
;;   :config
;;   (load-theme 'ef-kassio t))

(use-package modus-themes
  :ensure nil
  :custom
  (modus-themes-subtle-line-numbers nil)
  (modus-themes-org-blocks 'gray-background)
  (modus-themes-mode-line '(borderless))
  (modus-themes-scale-headings t)
  (modus-themes-fringes nil)
  :init
  (load-theme 'modus-operandi t))

(use-package spacious-padding
  :defer t
  :config
  (setq spacious-padding-widths
        '( :right-divider-width 30
           :internal-border-width 15
           :tab-width 4
           :fringe-width 8))
  (setq spacious-padding-subtle-mode-line nil)
  ;; Enable spacious padding mode right away for emacs launched in graphical
  ;; mode, but only activate the mode for client frames when a frame is made
  ;; (if (window-system)
  ;;     (spacious-padding-mode 1)
  ;;   (add-hook 'after-make-frame-functions #'spacious-padding-mode)))
  )

(setopt frame-resize-pixelwise t)

;; (use-package kaolin-themes
;;   :config
;;   (load-theme 'kaolin-light t))

(use-package rainbow-delimiters
  :defer t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package rainbow-blocks)

;; Make the mode line nicer (worse?)
;; (use-package mood-line)

;; (mood-line-mode)

(setq tab-bar-format '(tab-bar-format-history
               tab-bar-format-tabs
               tab-bar-separator
               tab-bar-format-align-right
               tab-bar-format-global))
(setq tab-bar-show 1)
(setq tab-bar-new-tab-choice "*scratch*")

;; (use-package centaur-tabs
;;   :demand t
;;   :custom
;;   (centaur-tabs-set-modified-marker t)
;;   (centaur-tabs-modified-marker " * ")
;;   (centaur-tabs-set-icons t)
;;   (centaur-tabs-icon-type 'all-the-icons)
;;   :config
;;   (centaur-tabs-mode t))

;; (use-package smart-mode-line)

(setopt mode-line-compact 'long)

;; Override default behaviors
(setq
 inhibit-startup-screen nil
 initial-scratch-message nil
 sentence-end-double-space t
 ring-bell-function 'ignore
 save-interprogram-paste-before-kill t
 use-dialog-box nil
 mark-even-if-inactive t
;; will delete entire line on c-k, including newline
 kill-whole-line t
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

(use-package olivetti
  :defer t
  :custom
  (olivetti-style nil)   ;; change to t for fringes or fancy for margins
  :bind ([f5] . olivetti-mode))

(use-package simple
  :ensure nil
  :config
  (global-set-key (kbd "M-u") 'upcase-dwim)
  (global-set-key (kbd "M-l") 'downcase-dwim)
  (global-set-key (kbd "M-c") 'capitalize-dwim))

(defun back-to-indentation-or-beginning () (interactive)
		       (if (= (point) (progn (back-to-indentation) (point)))
			   (beginning-of-line)))

;; Move point to indentation first, then to beginning of line
(global-set-key "\C-a" 'back-to-indentation-or-beginning)

(global-set-key "\C-x\M-t" 'transpose-sentences)

(global-set-key (kbd "C-S-O") 'open-previous-line)
(global-set-key (kbd "C-o") 'open-next-line)

(global-set-key (kbd "C-S-d") 'duplicate-line)
(global-set-key (kbd "C-S-k") 'kill-whole-line)

(global-set-key (kbd "M-j") 'join-line)

(global-set-key (kbd "<backtab>") 'indent-according-to-mode)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

(setq visual-line-mode nil)

(defvar newline-and-indent t)

(defun open-next-line (arg)
  "Move to the next line and then opens a line."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (forward-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

;;; behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

;; (use-package cua-rect
;;   :ensure nil
;;   :bind ("C-x SPC" . cua-rectangle-mark-mode))(global-set-key (kbd "C-S-O") 'open-previous-line)
(global-set-key (kbd "C-o") 'open-next-line)

;; Switch focus to help window when it appears
(setopt help-window-select t)

;; Include path in buffer name when multiple files of the same name are open
(setopt uniquify-buffer-name-style 'forward)

;; Show recently opened files in menus
(recentf-mode)

;; Show count of matches in isearch
(setopt isearch-lazy-count t)

(setopt indent-tabs-mode nil
        tab-width 2
        fill-column 80)

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

(delete-selection-mode t)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(column-number-mode)
(savehist-mode)

(setopt large-file-warning-threshold nil)

;; Don't wrap lines
(setq-default truncate-lines t)

;; Insert closing brackets automatically
(electric-pair-mode)

(use-package hl-line
  :ensure nil
  :hook ((prog-mode . hl-line-mode)
         (text-mode . hl-line-mode)))

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

(pixel-scroll-precision-mode)
(setq pixel-scroll-precision-large-scroll-height 20.0)
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)

;; Get rid of annoying minimize chord
(keymap-global-unset "C-z")
(keymap-global-unset "C-x C-z")

(use-package fancy-compilation
  :config
  (fancy-compilation-mode))

(use-package diminish)

;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package which-key
  :diminish
  :custom
  (which-key-enable-extended-define-key t)
  :config
  (which-key-mode)
  ;;  (which-key-setup-side-window-right))
  )

(use-package magit)

(setopt read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        completion-ignore-case t
        completion-styles '(basic substring initials flex)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (basic partial-completion)))))

(use-package icomplete
  :ensure nil
  :defer t
  :bind (:map icomplete-fido-mode-map
         ("<tab>" . icomplete-force-complete)
         ("RET" . icomplete-force-complete-and-exit))
  :hook (icomplete-minibuffer-setup
         . (lambda ()
             (setq-local truncate-lines t
                         completion-styles '(basic substring initials flex))))
  :custom-face (icomplete-selected-match ((t (:extend t))))
  ;; :config
  ;; (fido-vertical-mode 1))
  )

;; reduce scale factor for C-x C-+/-
(setq text-scale-mode-step 1.1)

;;(dolist (face '(window-divider
;;                window-divider-first-pixel
;;              window-divider-last-pixel))
;;  (face-spec-reset-face face)
;;(set-face-foreground face (face-attribute 'default :background)))
;;(set-face-background 'fringe (face-attribute 'default :background))

;; previews in dired
(use-package dired-preview
  :config
  (setq dired-preview-delay 0.7)
  (setq dired-preview-max-size (expt 2 20))
  (setq dired-preview-ignored-extensions-regexp
        (concat "\\."
                "\\(mkv\\|webm\\|mp4\\|mp3\\|ogg\\|m4a"
                "\\|gz\\|zst\\|tar\\|xz\\|rar\\|zip"
                "\\|iso\\|epub\\|pdf\\)")))

;; Ibuffer
(use-package ibuffer
  :defer t
  :ensure nil
  :bind (([remap list-buffers] . ibuffer)
         ("C-c b" . ibuffer))
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :custom
  (ibuffer-formats
   '((mark modified read-only locked " "
           (name 20 20 :left :elide)
           " "
           (size-h 11 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " "
           (name 16 -1)
           " " filename)))

  :config
  (defun my/human-readable-file-sizes-to-bytes (string)
    "Convert a human-readable file size into bytes."
    (cond
     ((string-suffix-p "G" string t)
      (* 1000000000 (string-to-number (substring string 0 (- (length string) 1)))))
     ((string-suffix-p "M" string t)
      (* 1000000 (string-to-number (substring string 0 (- (length string) 1)))))
     ((string-suffix-p "K" string t)
      (* 1000 (string-to-number (substring string 0 (- (length string) 1)))))
     (t
      (string-to-number (substring string 0 (- (length string) 1))))))

  (defun my/bytes-to-human-readable-file-sizes (bytes)
    "Convert number of bytes to human-readable file size."
    (cond
     ((> bytes 1000000000) (format "%10.1fG" (/ bytes 1000000000.0)))
     ((> bytes 100000000) (format "%10.0fM" (/ bytes 1000000.0)))
     ((> bytes 1000000) (format "%10.1fM" (/ bytes 1000000.0)))
     ((> bytes 100000) (format "%10.0fK" (/ bytes 1000.0)))
     ((> bytes 1000) (format "%10.1fK" (/ bytes 1000.0)))
     (t (format "%10d" bytes))))

  (define-ibuffer-column size-h
    (:name "Size"
           :inline t
           :summarizer
           (lambda (column-strings)
             (let ((total 0))
               (dolist (string column-strings)
                 (setq total
                       (+ (float (my/human-readable-file-sizes-to-bytes string))
                          total)))
               (my/bytes-to-human-readable-file-sizes total))))
    (my/bytes-to-human-readable-file-sizes (buffer-size))))

;; Visual undo tree
(use-package vundo
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

;; Improve window switching
(use-package ace-window
  :defer t
  :bind ("M-o" . ace-window)
  :custom
  (aw-scope 'frame)
  (aw-ignore-current t))

;; Better handling of popup windows
(use-package popper
  ;; :ensure t ; or :straight t
  :defer t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"))
  :config
  (popper-mode 1)
  (popper-echo-mode 1))

(use-package transpose-frame
  :defer t
  :bind (
         :map ctl-x-4-map
         ("|" . flop-frame)
         ("_" . flip-frame)
         ("\\" . rotate-frame-anticlockwise)))

;; Show most recent command in modeline
;; (use-package keycast)
;; (keycast-mode-line-mode)

(use-package vterm
  :defer t
  :ensure nil
  :custom
  (vterm-kill-buffer-on-exit t))

;; (use-package counsel
;;   :custom
;;   (setq ivy-use-virtual-buffers t)
;;   (setq ivy-count-format "(%d/%d) ")

;;   :config
;;   (ivy-mode 1))

;; Ivy/counsel/swiper default keybinds
;; (global-set-key (kbd "C-s") 'swiper-isearch)
;; (global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; (global-set-key (kbd "M-y") 'counsel-yank-pop)
;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;; (global-set-key (kbd "<f1> l") 'counsel-find-library)
;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;; (global-set-key (kbd "<f2> j") 'counsel-set-variable)
;; (global-set-key (kbd "C-x b") 'ivy-switch-buffer)

;; Ripgrep
;; using guix package instead
;; (use-package rg)

;; Vertico
(use-package vertico
  :defer 1
  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-char)
              ("C-;" . vertico-grid-mode)
              ("C-'" . vertico-quick-exit))
  :config
  (setq vertico-multiform-categories
        '((file grid)
          (consult-grep buffer)
          (imenu buffer)))
  (vertico-multiform-mode)
  (vertico-mode 1))

(use-package consult
  :after vertico
  :demand nil
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-s i" . consult-imenu)
         ("M-s g" . consult-ripgrep)
         ("M-s e" . consult-isearch-history) ;; somewhat redundant
         ("M-s k" . consult-keep-lines)
         ("M-s f" . consult-focus-lines)
         ("M-g g" . consult-goto-line)
         ("M-g m" . consult-mark)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         :map minibuffer-mode-map
         ("C-r" . consult-history))
  :custom
  (completion-in-region-function
   (lambda (&rest args)
     (apply (if (or vertico-mode fido-vertical-mode)
                'consult-completion-in-region
              'completion--in-region)
            args)))
  :config
  (consult-customize consult-org-heading :preview-key nil))

(use-package marginalia
  :after vertico
  :demand nil
  :config
  (marginalia-mode 1))

(use-package orderless
  :after vertico
  :custom
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-ignore-case t)
  (completion-styles '(orderless substring initials flex basic))
  (completion-category-overrides '((file (styles . (basic partial-completion orderless)))))
  (completion-category-defaults nil)
  (orderless-matching-styles '(orderless-prefixes orderless-regexp)))

(setopt enable-recursive-minibuffers t)

(minibuffer-depth-indicate-mode 1)

;; Avy keybinds
(avy-setup-default)
(global-set-key (kbd "C-c C-j") 'avy-resume)
(global-set-key (kbd "M-g M-g") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g e") 'avy-goto-word-0)
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-j") 'avy-goto-char-timer)

;; Free up "k" for avy-dispatch, add "n" and "v" in its place
(setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?l ?n ?v))
(define-key isearch-mode-map (kbd "C-'") 'avy-isearch)

;; Functions stolen from karthinks' blog
(defun avy-action-kill-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (kill-whole-line))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
t)

(defun avy-action-copy-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (cl-destructuring-bind (start . end)
        (bounds-of-thing-at-point 'line)
      (copy-region-as-kill start end)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
nt)

(defun avy-action-yank-whole-line (pt)
  (avy-action-copy-whole-line pt)
  (save-excursion (yank))
t)

(defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
(save-excursion (yank)) t)

(defun avy-action-mark-to-char (pt)
  (activate-mark)
  (goto-char pt))

(setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
      (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line)

(setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
      (alist-get ?w avy-dispatch-alist) 'avy-action-copy
      (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
      (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line)

(setf (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
      (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line)

(setf (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char)

(defun isearch-forward-other-window (prefix)
    "Function to isearch-forward in other-window."
    (interactive "P")
    (unless (one-window-p)
      (save-excursion
        (let ((next (if prefix -1 1)))
          (other-window next)
          (isearch-forward)
          (other-window (- next))))))

(defun isearch-backward-other-window (prefix)
  "Function to isearch-backward in other-window."
  (interactive "P")
  (unless (one-window-p)
    (save-excursion
      (let ((next (if prefix 1 -1)))
        (other-window next)
        (isearch-backward)
        (other-window (- next))))))

(define-key global-map (kbd "C-M-s") 'isearch-forward-other-window)
(define-key global-map (kbd "C-M-r") 'isearch-backward-other-window)

;; Corfu
(use-package corfu
  :custom
  (corfu-popupinfo-delay 2.0)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package cape
  :bind(("C-c p p" . completion-at-point) ;; capf
        ("C-c p t" . complete-tag)        ;; etags
        ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
        ("C-c p h" . cape-history)
        ("C-c p f" . cape-file)
        ("C-c p k" . cape-keyword)
        ("C-c p s" . cape-elisp-symbol)
        ("C-c p e" . cape-elisp-block)
        ("C-c p a" . cape-abbrev)
        ("C-c p l" . cape-line)
        ("C-c p w" . cape-dict)
        ("C-c p :" . cape-emoji)
        ("C-c p \\" . cape-tex)
        ("C-c p _" . cape-tex)
        ("C-c p ^" . cape-tex)
        ("C-c p &" . cape-sgml)
        ("C-c p r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; Eshell
(use-package esh-mode
  :ensure nil
  :bind (("C-c e" . eshell))
  :custom
  (eshell-buffer-maximum-lines 1000)
  (eshell-scroll-to-bottom-on-input t)
  (eshell-destroy-buffer-when-process-dies t)
  :config
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer))

(use-package mingus
  :custom
  (mingus-mode-line-show-elapsed-time nil)
  (mingus-mode-line-show-volume nil))

(use-package erc
  :ensure nil
  :custom
  (erc-server "irc.libera.chat")
  (erc-nick "uwihz")
  (erc-track-shorten-start 8)
  (erc-autojoin-channels-alist '(("irc.libera.chat" "#guix" "#emacs")))
  (erc-kill-buffer-on-part t)
  (erc-auto-query 'bury)
  (erc-hide-list '("JOIN" "PART" "QUIT")))

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

(use-package org
  :ensure nil
  :custom
  (org-src-window-setup 'current-window)
  (org-src-preserve-indentation t)
  (org-edit-src-content-indentation 0)
  :config
  (require 'org-tempo)
  (add-to-list 'org-modules 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("li" . "src lisp"))
  (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-structure-template-alist '("eltn" . "src emacs-lisp :tangle no :noweb-ref"))

  :hook ((org-mode . org-toggle-pretty-entities)
         (org-mode . visual-line-mode)
         (org-mode . visual-fill-column-mode)
         (org-mode . variable-pitch-mode)
         (org-mode . mixed-pitch-mode)
         (org-mode . org-modern-mode)))

(setq org-preview-latex-default-process 'dvisvgm)

;; Org-modern
(use-package org-modern
  :defer t
  :custom
  (setq
 ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

(use-package visual-fill-column)

(use-package mixed-pitch)

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config

  ;; stolen almost verbatim from doom emacs
  ;; not sure if it even works or if pdf-tools just works with epdf already
  ;; installed
  ;; (defun pdf--install-epdfinfo-a (fn &rest args)
  ;;   "Install epdfinfo after the first PDF file, if needed."
  ;;   (if (and (require 'pdf-info nil t)
  ;;            (or (pdf-info-running-p)
  ;;                (ignore-errors (pdf-info-check-epdfinfo) t)))
  ;;       (apply fn args)
  ;;     ;; If we remain in pdf-view-mode, it'll spit out cryptic errors. This
  ;;     ;; graceful failure is better UX.
  ;;     (fundamental-mode)
  ;;     (message "Viewing PDFs in Emacs requires epdfinfo. Use `M-x pdf-tools-install' to build it")))

  ;; (advice-add 'pdf-view-mode :around #'pdf--install-epdfinfo-a)
  ;; (pdf-tools-install-noverify)
  (pdf-tools-install :no-query)

  ;; Bootstrap pdf-tools
  ;; (pdf-loader-install)
  (setq-default pdf-view-display-size 'fit-width)

  ;; Use isearch in pdfs
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (setq pdf-view-use-scaling t))

;; auctex
(use-package latex
  :ensure auctex
  :hook ((LaTeX-mode . prettify-symbols-mode))
  :bind (:map LaTeX-mode-map
              ("C-S-e" . latex-math-from-calc))
  :config
  (defun latex-math-from-calc ()
    "Evaluate `calc' on the contents of line at point."
    (interactive)
    (cond ((region-active-p)
           (let* ((beg (region-beginning))
                  (end (region-end))
                  (string (buffer-substring-no-properties beg end)))
             (kill-region beg end)
             (insert (calc-eval `(,string calc-language latex
                                          calc-prefer-frac t
                                          calc-angle-mode rad)))))
          (t (let ((l (thing-at-point 'line)))
               (end-of-line 1) (kill-line 0)
               (insert (calc-eval `(,l
                                    calc-language latex
                                    calc-prefer-frac t
                                    calc-angle-mode rad))))))))

;; TODO: include this in use-package form
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(setq LaTeX-indent-item 0)
(setq reftex-plug-into-AUCTeX t)
(use-package auctex-latexmk)
(require 'auctex-latexmk)
(auctex-latexmk-setup)
(setq auctex-latexmk-inherit-TeX-PDF-mode t)
(setq TeX-view-program-selection '((output-pdf "PDF Tools")))
;; (setq TeX-view-program-selection '((output-pdf "Zathura")))
(setq TeX-source-correlate-mode 'synctex)
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

(use-package latex
  :ensure nil
  :bind (
         :map LaTeX-mode-map ("C-j" . avy-goto-char-timer)))

;; (add-hook 'LaTeX-mode-hook
;;           (defun preview-smaller-previews ()
;;             (setq preview-scale-function
;;                   (lambda () (* 0.65
;;                            (funcall (preview-scale-from-face)))))))


(use-package latex-change-env
  :after latex
  :bind (:map LaTeX-mode-map ("C-c r" . latex-change-env))
  :custom
  (latex-change-env-math-display '("\\[" . "\\]"))
  (latex-change-env-math-inline  '("$"   . "$")))

;; math-delimiters from oantolin/math-delimiters on github
(autoload 'math-delimiters-insert "math-delimiters")

;; enable $ mapping for math-delimiters in org and tex mode buffers
(with-eval-after-load 'org
  (define-key org-mode-map "$" #'math-delimiters-insert))

;; applies to all tex buffers, not just LaTeX!
(with-eval-after-load 'tex
  (define-key TeX-mode-map "$" #'math-delimiters-insert))

;; unbind cdlatex's $ mapping
(with-eval-after-load 'cdlatex
  (define-key cdlatex-mode-map "$" nil))

(setq math-delimiters-inline '("$" . "$"))

;; insert newlines before and after display math, rather than placing everything on one line
(setq math-delimiters-compressed-display-math nil)

;; CDLatex settings
(use-package cdlatex
  :defer t
;;   :ensure t
  :hook (LaTeX-mode . turn-on-cdlatex)
  :bind (:map cdlatex-mode-map
              ("<tab>" . cdlatex-tab)))

(require 'lazytab)

(add-hook 'LaTeX-mode-hook 'lazytab-mode)

(setq cdlatex-env-alist
      '(("theoremd" "\\begin{theoremd}\n\t?\n\\end{theoremd}\n" nil)
        ("theorem" "\\begin{theorem}\n\t?\n\\end{theorem}\n" nil)
        ("definitiond" "\\begin{definitiond}\n\t?\n\\end{definitiond}\n" nil)
        ("definition" "\\begin{definition}\n\t?\n\\end{definition}\n" nil)
        ("proof" "\\begin{proof}\n\t?\n\\end{proof}\n" nil)
        ("eg" "\\begin{eg}\n\t?\n\\end{eg}\n" nil)
        ("lemma" "\\begin{lemma}\n\t?\n\\end{lemma}\n" nil)
        ("claim" "\\begin{claim}\n\t?\n\\end{claim}\n" nil)
        ("remark" "\\begin{remark}\n\t?\n\\end{remark}\n" nil)
        ("corollary" "\\begin{corollary}\n\t?\n\\end{corollary}\n" nil)))

(setq cdlatex-command-alist
      '(("thm" "Insert theoremd environment"  "" cdlatex-environment ("theoremd") t nil)
        ("defn" "Insert definitiond environment"  "" cdlatex-environment ("definitiond") t nil)
        ("prf" "Insert proof environment"  "" cdlatex-environment ("proof") t nil)
        ("eg" "Insert eg environment"  "" cdlatex-environment ("eg") t nil)
        ("lem" "Insert lemma environment"  "" cdlatex-environment ("lemma") t nil)
        ("clm" "Insert claim environment"  "" cdlatex-environment ("claim") t nil)
        ("rmk" "Insert remark environment"  "" cdlatex-environment ("remark") t nil)
        ("sum" "Insert summation"  "\\sum_{?}^{}" cdlatex-position-cursor nil nil t)
        ("cases" "Insert cases" "\\begin{cases} ? \\end{cases}" cdlatex-position-cursor nil nil t)
        ("itl" "Insert item with custom bullet" "\\item[?]" cdlatex-position-cursor nil t nil)))

(add-to-list 'cdlatex-command-alist '("bmat" "Insert bmatrix env"
                                       "\\begin{bmatrix} ? \\end{bmatrix}"
                                       lazytab-position-cursor-and-edit
                                       nil nil t))

(add-to-list 'cdlatex-command-alist '("pmat" "Insert pmatrix env"
                                       "\\begin{pmatrix} ? \\end{pmatrix}"
                                       lazytab-position-cursor-and-edit
                                       nil nil t))

(add-to-list 'cdlatex-command-alist '("tbl" "Insert table"
                                       "\\begin{table}\n ? \\end{table}\n"
                                       lazytab-position-cursor-and-edit
                                       nil nil t))

(setq cdlatex-math-symbol-alist
      '((?0 ("\\nil"))
        (?B ("\\sup{?}"))
        (?C ("\\inf{?}"))
        (?. ("\\cdot" "\\circ"))))

;; Yasnippet settings
(use-package yasnippet
  :defer t
  :hook ((LaTeX-mode . yas-minor-mode)
         (LaTeX-mode . yas-reload-all)
         (post-self-insert . my/yas-try-expanding-auto-snippets))
  :config
  (use-package warnings
    :config
    (cl-pushnew '(yasnippet backquote-change)
                warning-suppress-types
                :test 'equal))

  (setq yas-trigger-in-field t)
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))
  (setq yas-indent-line 'auto)

  ;; yasnippet tries to prompt for a snippet name even when the expansion is
  ;; unambiguous, so don't use any prompt at all
  ;; kind of hacky but it works for me
  (setq yas-prompt-functions nil)

  (defun my/yas-try-expanding-auto-snippets ()
    (when (and (boundp 'yas-minor-mode) yas-minor-mode)
      (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
        (yas-expand)))))

;; Try after every insertion
;; (add-hook 'post-self-insert-hook #'my/yas-try-expanding-auto-snippets)

;; Allows cdlatex to use tab inside yasnippet fields
(use-package cdlatex
  :hook ((cdlatex-tab . yas-expand)
         (cdlatex-tab . cdlatex-in-yas-field)
         (org-mode . org-cdlatex-mode))
  :config
  (use-package yasnippet
    :bind (:map yas-keymap
           ("<tab>" . yas-next-field-or-cdlatex)
           ("TAB" . yas-next-field-or-cdlatex))
    :config
    (defun cdlatex-in-yas-field ()
      ;; Check if we're at the end of the Yas field
      (when-let* ((_ (overlayp yas--active-field-overlay))
                  (end (overlay-end yas--active-field-overlay)))
        (if (>= (point) end)
            ;; Call yas-next-field if cdlatex can't expand here
            (let ((s (thing-at-point 'sexp)))
              (unless (and s (assoc (substring-no-properties s)
                                    cdlatex-command-alist-comb))
                (yas-next-field-or-maybe-expand)
                t))
          ;; otherwise expand and jump to the correct location
          (let (cdlatex-tab-hook minp)
            (setq minp
                  (min (save-excursion (cdlatex-tab)
                                       (point))
                       (overlay-end yas--active-field-overlay)))
            (goto-char minp) t))))

    (defun yas-next-field-or-cdlatex nil
      (interactive)
      "Jump to the next Yas field correctly with cdlatex active."
      (if
          (or (bound-and-true-p cdlatex-mode)
              (bound-and-true-p org-cdlatex-mode))
          (cdlatex-tab)
        (yas-next-field-or-maybe-expand)))))

;; stolen from https://superuser.com/questions/125027/word-count-for-latex-within-emacs
(defun latex-word-count ()
   (interactive)
    (let* ((this-file (buffer-file-name))
           (word-count
            (with-output-to-string
              (with-current-buffer standard-output
                (call-process "texcount" nil t nil "-brief" this-file)))))
      (string-match "\n$" word-count)
      (message (replace-match "" nil nil word-count))))

(use-package all-the-icons)

;; dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
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
                                  (registers . "database"))))

(add-hook 'server-after-make-frame-hook (lambda()
    ;; (switch-to-buffer dashboard-buffer-name)
    (dashboard-mode)
    (dashboard-insert-startupify-lists)
    (dashboard-refresh-buffer)))

;; treesitter
;;(setq major-mode-remap-alist
;;    '((bash-mode . bash-ts-mode)))

(defun my-toggle-margins ()
"Set margins in current buffer."
(interactive)
  (if (or (> left-margin-width 0) (> right-margin-width 0))
    (progn
      (setq left-margin-width 0)
      (setq right-margin-width 0)
      (set-window-buffer (selected-window) (current-buffer)))
    (setq left-margin-width 26)
    (setq right-margin-width 26)
    (set-window-buffer (selected-window) (current-buffer))))

;; epub support
(use-package nov
  :mode ("\\.epub\\'" . nov-mode))
