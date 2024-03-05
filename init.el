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
  (set-face-attribute 'default nil :font "Iosevka Term-13")
  (set-face-attribute 'variable-pitch nil :font "Iosevka Term-13")
  ;;(remove-hook 'after-make-frame-functions #'my-frame-config)
)

;; removes window decorations and sets font for client windows
(add-hook 'after-make-frame-functions #'my-frame-config)

(set-face-attribute 'default nil :font "Iosevka Term-13")
(set-face-attribute 'variable-pitch nil :font "Iosevka Term-13")

;; (add-hook 'after-make-frame-functions
;;          (lambda (f) (set-face-attribute 'default :font "Iosevka Term-12.5")))

(blink-cursor-mode 0)

(add-to-list 'load-path "~/.emacs.d/lisp")

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
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(column-number-mode)
(savehist-mode)

(require 'hl-line)
;; (add-hook 'prog-mode-hook #'hl-line-mode)
;; (add-hook 'text-mode-hook #'hl-line-mode)

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
(setq-default truncate-lines t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)

(use-package fancy-compilation :config (fancy-compilation-mode))

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
  ;;  (which-key-setup-side-window-right))
  )

(use-package magit)

(fido-mode)

;; (use-package spacious-padding)
;; (spacious-padding-mode)

(modify-all-frames-parameters
  '((internal-border-width . 50)
    (right-divider-width . 12)))

;; reduce scale factor for C-x C-+/-
(setq text-scale-mode-step 1.1)

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
(setq org-preview-latex-default-process 'dvisvgm)

(use-package pdf-tools)

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

(setq TeX-parse-self t)
(setq TeX-save-query nil)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
;; (add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq LaTeX-indent-item 0)
(setq reftex-plug-into-AUCTeX t)
(use-package auctex-latexmk)
(require 'auctex-latexmk)
(auctex-latexmk-setup)
(setq auctex-latexmk-inherit-TeX-PDF-mode t)
;; (setq TeX-view-program-list '((output-pdf "pdf-tools")))
(setq TeX-view-program-selection '((output-pdf "Zathura")))
(setq TeX-source-correlate-mode 'synctex)

(add-hook 'LaTeX-mode-hook
          (defun preview-smaller-previews ()
            (setq preview-scale-function
                  (lambda () (* 0.70
                           (funcall (preview-scale-from-face)))))))


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

(setq

;; CDLatex settings
(use-package cdlatex
  :ensure t
  :hook (LaTeX-mode . turn-on-cdlatex)
  :bind (:map cdlatex-mode-map
              ("<tab>" . cdlatex-tab)))

(require 'lazytab "~/.emacs.d/lisp/lazytab.el")

(setq cdlatex-env-alist
      '(("theoremd" "\\begin{theoremd}\nAUTOLABEL\n?\n\\end{theoremd}\n" nil)
        ("theorem" "\\begin{theorem}\nAUTOLABEL\n?\n\\end{theorem}\n" nil)
        ("definitiond" "\\begin{definitiond}\nAUTOLABEL\n?\n\\end{definitiond}\n" nil)
        ("definition" "\\begin{definition}\nAUTOLABEL\n?\n\\end{definition}\n" nil)
        ("proof" "\\begin{proof}\nAUTOLABEL\n?\n\\end{proof}\n" nil)
        ("eg" "\\begin{eg}\nAUTOLABEL\n?\n\\end{eg}\n" nil)
        ("lemma" "\\begin{lemma}\nAUTOLABEL\n?\n\\end{lemma}\n" nil)
        ("claim" "\\begin{clm}\nAUTOLABEL\n?\n\\end{clm}\n" nil)
        ("remark" "\\begin{remark}\nAUTOLABEL\n?\n\\end{remark}\n" nil)))

(setq cdlatex-command-alist
      '(("thm" "Insert theoremd environment"  "" cdlatex-environment ("theoremd") t nil)
        ("defn" "Insert definitiond environment"  "" cdlatex-environment ("definitiond") t nil)
        ("prf" "Insert proof environment"  "" cdlatex-environment ("proof") t nil)
        ("eg" "Insert eg environment"  "" cdlatex-environment ("eg") t nil)
        ("lem" "Insert lemma environment"  "" cdlatex-environment ("lemma") t nil)
        ("clm" "Insert claim environment"  "" cdlatex-environment ("claim") t nil)
        ("rmk" "Insert remark environment"  "" cdlatex-environment ("remark") t nil)
        ("sum" "Insert summation"  "\\sum_{?}^{}" cdlatex-position-cursor nil nil t)
        ("cases" "Insert cases" "\\begin{cases} ? \\end{cases}" cdlatex-position-cursor nil nil t)))

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
        (?C ("\\inf{?}"))))

;; Yasnippet settings
(use-package yasnippet
  :ensure t
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

  (defun my/yas-try-expanding-auto-snippets ()
    (when (and (boundp 'yas-minor-mode) yas-minor-mode)
      (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
        (yas-expand)))))

;; Try after every insertion
(add-hook 'post-self-insert-hook #'my/yas-try-expanding-auto-snippets)

;; Allows cdlatex to use tab inside yasnippet fields
(use-package cdlatex
  :hook ((cdlatex-tab . yas-expand)
         (cdlatex-tab . cdlatex-in-yas-field))
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
;;(setq major-mode-remap-alist
;;    '((bash-mode . bash-ts-mode)))

(put 'downcase-region 'disabled nil)

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

(global-set-key [f5] 'my-toggle-margins)
