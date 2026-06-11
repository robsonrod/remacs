;;; init.el --- emacs configuration -*- lexical-binding: t; -*-
;;;
;;; The MIT License (MIT)
;;;
;;; Copyright (c) 2025 Robson Rodrigues
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.
;;;
;;; Commentary:
;;; Code:

(when (version< emacs-version "28.2")
  (error "Emacs 28.2 is required"))

(require 'package)

;; ---------------------------------------------------------------------------
;; Package archives
;; ---------------------------------------------------------------------------
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("melpa"  . "https://melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("org"    . "https://orgmode.org/elpa/")))

(setq package-native-compile t)

;; Initialize only if not already done
(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; Refresh package list if empty
(unless package-archive-contents
  (package-refresh-contents))

;; ---------------------------------------------------------------------------
;; Persistent state
;; ---------------------------------------------------------------------------
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

;; ---------------------------------------------------------------------------
;; Auto-upgrade settings
;; ---------------------------------------------------------------------------
(defvar package-last-upgrade-time 0
  "Timestamp of the last time packages were upgraded.")

(defvar package-upgrade-interval (* 7 24 60 60)
  "Interval in seconds between automatic upgrades (default: 7 days).")

(defun package-upgrade-builtins ()
  "Upgrade installed packages if enough time has passed since the last upgrade."
  (when (> (- (float-time) package-last-upgrade-time)
           package-upgrade-interval)
    (message "📦 Checking for package upgrades...")
    (package-refresh-contents)
    (if (fboundp 'package-upgrade-all)
        ;; Emacs 29+ built-in upgrade command
        (package-upgrade-all t)
      ;; Fallback for older Emacs versions
      (dolist (pkg (mapcar #'car package-archive-contents))
        (when (package-installed-p pkg)
          (package-install pkg))))
    (setq package-last-upgrade-time (float-time))
    (customize-save-variable 'package-last-upgrade-time package-last-upgrade-time)
    (message "✅ Packages upgraded successfully.")))

;; Schedule auto-upgrade after startup (non-blocking)
(run-with-idle-timer
  (* 5 60)  ;; 5 minutes of idle time
  nil
  #'package-upgrade-builtins)

;; ---------------------------------------------------------------------------
;; Bootstrap use-package
;; ---------------------------------------------------------------------------
(unless (require 'use-package nil 'noerror)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-expand-minimally t
      use-package-compute-statistics nil)

;; Optional: GC tuning for faster startup
(setq read-process-output-max (* 10 1024 1024)) ;; 10mb
(setq gc-cons-threshold #x40000000)  ;; 1GB
(setq gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 80 1024 1024))))

(message "✅ Package system initialized successfully.")

(use-package emacs
  :ensure nil
  :init
  ;; Load custom file
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (load custom-file :no-error-if-file-is-missing)

  ;; System-specific settings
  (when (string-prefix-p "linux10" system-name)
    (message "Changing to latin1")
    (prefer-coding-system 'iso-8859-1)
    (set-default-coding-systems 'iso-8859-1)
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8))

  ;; yes-or-no -> y/n
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Disable suspend keys in GUI
  (when (display-graphic-p)
    (setq confirm-kill-emacs 'y-or-n-p)
    (global-unset-key (kbd "C-x C-z"))
    (global-unset-key (kbd "C-z")))
  :hook
  (prog-mode . display-line-numbers-mode)
  (text-mode . display-line-numbers-mode)
  (markdown-mode . display-line-numbers-mode)
  :custom
  ;; General UI and behavior
  (inhibit-startup-message t)
  (initial-scratch-message "")
  (indent-tabs-mode nil)                ; use spaces instead of tabs
  (ring-bell-function 'ignore)
  (history-length 20)
  (use-dialog-box nil)
  (case-fold-search nil)
  (confirm-kill-processes nil)
  (global-auto-revert-non-file-buffers t)
  (sentence-end-double-space nil)
  (load-prefer-newer t)
  (tab-always-indent 'complete)
  (native-comp-async-report-warnings-errors 'silent)
  (mouse-wheel-scroll-amount '(2 ((shift) . 1)))
  (mouse-wheel-progressive-speed nil)
  (mouse-wheel-follow-mouse t)
  (vc-follow-symlinks t)
  (show-paren-style 'mixed)
  (treesit-font-lock-level 4)
  (browse-url-browser-function 'eww-browse-url)
  
  ;; Backup and autosave
  (backup-directory-alist '(("." . "~/.emacs.d/backup")))
  (backup-by-copying t)
  (version-control t)
  (delete-old-versions t)
  (kept-new-versions 20)
  (kept-old-versions 5)
  (create-lockfiles nil)
  (auto-save-timeout 2)
  (auto-save-file-name-transforms
   `((".*" ,(expand-file-name "auto-save-list" user-emacs-directory) t)))
  (auto-revert-verbose nil)
  (auto-revert-remote-files t)
  (dired-kill-when-opening-new-dired-buffer t)

  :config
  (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font"  :height 100)

  ;; Enable/disable built-in modes
  (menu-bar-mode +1)
  (tool-bar-mode -1)
  (blink-cursor-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1)
  (set-fringe-mode 10)

  (auto-revert-mode 1)
  (delete-selection-mode 1)
  (column-number-mode 1)
  (save-place-mode 1)
  (global-auto-revert-mode 1)
  (global-hl-line-mode 1)
  (show-paren-mode 1)
  (global-display-line-numbers-mode 'absolute)
  (xterm-mouse-mode 1)
  (winner-mode 1)
  (global-prettify-symbols-mode 1)
  (auto-save-visited-mode 1)
  (recentf-mode 1)
  (add-hook
   'emacs-startup-hook
   (lambda ()
     (message
      "*** Emacs loaded in %s with %d garbage collections."
      (format "%.2f seconds"
              (float-time
               (time-subtract after-init-time before-init-time)))
      gcs-done)))
  (dolist (mode
           '(org-mode-hook
             term-mode-hook
             vterm-mode-hook
             eat-mode-hook
             shell-mode-hook
             eshell-mode-hook
             dired-mode-hook
             pdf-view-mode-hook
             elfeed-show-mode-hook
             eww-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode -1)))))

(use-package window
  :ensure nil
  :custom
  (display-buffer-alist
   '(
     ("\\*\\(Backtrace\\|Warnings\\|[Hh]elp\\|Messages\\|Bookmark List\\|Ibuffer\\|Occur\\|eldoc.*\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))

     ("\\*\\(Compile-Log\\)\\*"
      (display-buffer-no-window)
      (allow-no-window . t))

     ;; Example configuration for the LSP help buffer,
     ;; keeps it always on bottom using 25% of the available space:
     ("\\*\\(lsp-help\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))

     ;; Configuration for displaying various diagnostic buffers on
     ;; bottom 25%:
     ("\\*\\(Flymake diagnostics\\|xref\\|ivy\\|Swiper\\|Completions\\)"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 1)))))

(use-package isearch
  :ensure nil
  :config
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil)
  (setq search-whitespace-regexp ".*?")
  :bind (("C-s" . isearch-forward)
         ("C-r" . isearch-backward)))

(use-package treesit-auto
  :ensure t
  :after emacs
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))

;;; Basic behaviour
(use-package server
  :ensure nil
  :defer 1
  :config
  (setq server-client-instructions nil)
  (unless (server-running-p)
    (server-start)))

(use-package
  delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package keycast
  :hook (after-init . keycast-mode)
  :config
  (define-minor-mode keycast-mode
	"Show current command and its key binding in the mode line (fix for use with doom-modeline)."
	:global t
	(if keycast-mode
		(add-hook 'pre-command-hook 'keycast--update t)
      (remove-hook 'pre-command-hook 'keycast--update)))

  (add-to-list 'global-mode-string '("" keycast-mode-line)))

;; Remember to do M-x and run `nerd-icons-install-fonts' to get the
;; font files.  Then restart Emacs to see the effect.
(use-package nerd-icons :ensure t)

(use-package
  nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (add-hook
   'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package
  nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package
  nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

;;; Configure the minibuffer and completions
(use-package
  vertico
  :bind
  (:map
   vertico-map
   ("C-j" . vertico-next)
   ("C-k" . vertico-previous)
   ("C-f" . vertico-exit)
   :map
   minibuffer-local-map
   ("M-h" . backward-kill-word))
  :custom (vertico-cycle t)
  :init (vertico-mode))

(use-package
  marginalia
  :after vertico
  :custom
  (marginalia-annotators
   '(marginalia-annotators-heavy marginalia-annotators-ligh nil))
  :init (marginalia-mode))

(use-package
  embark
  :after vertico
  :ensure t
  :bind
  (("C-c a" . embark-act) ; pick some comfortable binding
   ("M-." . embark-dwim) ; good alternative: M-.
   ("C-h B" . embark-bindings)) ; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list
   'display-buffer-alist
   '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
     nil
     (window-parameters (mode-line-format . none)))))

(use-package
  embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package
  consult
  :bind
  (("C-c h" . 'consult-isearch-backward)
   ("C-x C-b" . 'consult-buffer)
   ("M-g o" . 'consult-outline)
   ("M-g M-g" . 'consult-goto-line)
   ("M-s M-g" . 'consult-grep)
   ("M-s M-r" . 'consult-ripgrep)
   ("M-s M-i" . 'consult-imenu)
   ("M-s M-f" . 'consult-find)
   ("M-s M-l" . 'consult-line)
   ("M-s M-h" . 'consult-recent-file))
  :custom
  (completion-in-region-function #'consult-completion-in-region))

(use-package
  orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrrides nil))

(use-package
  savehist
  :ensure nil
  :config
  (setq history-length 25)
  :hook (after-init . savehist-mode))

(use-package recentf
  :ensure nil
  :init
  (setq recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  (setq recentf-max-saved-items 100)
  (setq recentf-max-menu-items 25) ; I don't use the `menu-bar-mode', but this is good to know
  (setq recentf-save-file-modes nil)
  (setq recentf-keep nil)
  (setq recentf-auto-cleanup nil)
  (setq recentf-initialize-file-name-history nil)
  (setq recentf-filename-handlers nil)
  (setq recentf-show-file-shortcuts-flag nil)
  (setq recentf-exclude '(".*\\.tmp$" "/tmp/" ".*recentf$"))
  :config
  (recentf-mode 1))

(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)           ; Minimum length of prefix for completion
  (corfu-auto-delay 0)            ; No delay for completion
  (corfu-popupinfo-delay '(0.5 . 0.2))  ; Automatically update info popup after that numver of seconds
  (corfu-preview-current 'insert) ; insert previewed candidate
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET"        . corfu-insert))

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode) ; Popup completion info
  :config
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                                   corfu-quit-no-match t
                                   corfu-auto nil)
              (corfu-mode))
            nil
            t))

(use-package
  corfu-terminal
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

(use-package wgrep
  :ensure t
  :after consult
  :hook (grep-mode . wgrep-setup)
  :config
  (setq wgrep-enable-key "r")
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

(use-package
  consult-dir
  :bind
  (("C-x C-d" . consult-dir)
   :map
   vertico-map
   ("C-x C-d" . consult-dir)
   ("C-x C-j" . consult-dir-jump-file))
  :custom (consult-dir-project-list-function nil))

(use-package
  avy
  :bind
  (("M-g c" . avy-goto-char)
   ("M-g w" . avy-goto-word-1))
  :config
  (setq avy-background t
        avy-timeout-seconds 0.2))

;;; Helpers
(use-package
  which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.3))

(use-package
  helpful
  :init
  :defer t
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

;;; The file manager (Dired)
(use-package
  dired
  :ensure nil
  :init (with-eval-after-load 'dired (require 'dired-x))
  :commands (dired dired-jump)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode)
   (dired-mode . dired-omit-mode))
  :bind
  (:map dired-mode-map ("b" . dired-up-directory))
  (:map dired-mode-map ("." . dired-omit-mode))
  :config
  (setq dired-listing-switches "-goah --group-directories-first --time-style=long-iso")
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  (setq dired-omit-files (rx (seq bol "."))) ;; Omit dot files
  (put 'dired-find-alternate-file 'disabled nil))

(use-package
  dired-subtree
  :ensure t
  :after dired
  :bind
  (:map
   dired-mode-map
   ("<tab>" . dired-subtree-toggle)
   ("TAB" . dired-subtree-toggle)
   ("<backtab>" . dired-subtree-remove)
   ("S-TAB" . dired-subtree-remove))
  :config (setq dired-subtree-use-backgrounds nil))

(use-package
  trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

;;; Named workspaces
(use-package
  perspective
  :demand t
  :bind
  (("s-c p" . persp-switch)
   ("s-c n" . persp-next)
   ("C-x k" . persp-kill-buffer*))
  :custom
  (persp-initial-frame-name "Main")
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :config
  ;; Running `persp-mode' multiple times resets the perspective list...
  (unless (equal persp-mode t)
    (persp-mode)))

;;; Projects
(defun remacs/switch-project-action ()
  "Switch to a workspace with the project name and start `magit-status'."
  (persp-switch (projectile-project-name))
  (magit-status))

(use-package
  projectile
  :diminish projectile-mode
  :config (projectile-mode)
  (setq projectile-globally-ignored-directories
        (append
         '(".git" ".ccls_cache")
         projectile-globally-ignored-directories))
  :demand t
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/dotfiles")
    (setq projectile-project-search-path '("~/dotfiles" "~/dev/personal" "~/dev/work")))
  (setq projectile-switch-project-action
        #'remacs/switch-project-action))

;; Git support
(use-package
  magit
  :bind ("C-x g" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1))

(use-package
  git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02)
  (set-face-background 'git-gutter:modified "blue")
  ;; (set-face-background 'git-gutter:modified "black")
  (set-face-foreground 'git-gutter:added "green")
  (set-face-foreground 'git-gutter:deleted "red")
  (custom-set-variables
   '(git-gutter:modified-sign "**")
   '(git-gutter:added-sign "++")
   '(git-gutter:deleted-sign "--"))
  )

;; Modeline and themes
(use-package minions :config (minions-mode 1))

(use-package
  doom-themes
  :ensure t
  :config
  (setq
   doom-themes-enable-bold t
   doom-themes-enable-italic t)
  (load-theme 'doom-gruvbox t)
  (doom-themes-org-config) (doom-themes-neotree-config))

;; Undo/redo framework
(use-package undo-tree
  :ensure t
  :hook
  (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        undo-limit 800000            
        undo-strong-limit 12000000           
        undo-outer-limit 120000000)
  :bind (("C-c u" . undo-tree-visualize))
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/.cache/undo"))))

;;;; Programming
;; LSP
(use-package
  lsp-mode
  :ensure t
  :defer t
  :commands (lsp lsp-deferred)
  :bind (:map lsp-mode-map ("M-<RET>" . lsp-execute-action))
  :hook(
        (lsp-mode . lsp-enable-which-key-integration)
        (lsp-mode . lsp-diagnostics-mode)
        ((c-ts-mode
          c++-ts-mode
          rust-ts-mode
          rustic-mode
          go-ts-mode
          clojure-mode
          python-base-mode) . lsp-deferred))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-xref t)
  (lsp-idle-xref 0.5)
  (lsp-session-file (expand-file-name ".lsp-session" user-emacs-directory))
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  (lsp-diagnostics-provider :flycheck)
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (lsp-keep-workspace-alive nil)
  (lsp-eldoc-hook nil)
  (lsp-file-watch-threshold 15000)
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-signature-render-documentation nil)
  (lsp-lens-enable nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-eldoc-hook nil)
  (lsp-enable-links nil)
  (lsp-log-io nil)
  (lsp-enable-file-watchers nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-completion-show-detail nil)
  (lsp-completion-show-kind nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-enable nil)

  :preface
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)

       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  :init
  (setq lsp-use-plists t)
  ;; Initiate https://github.com/blahgeek/emacs-lsp-booster for performance
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

;; a hight level UI modules of lsp
(use-package
  lsp-ui
  :ensure t
  :diminish
  :defer t
  :after lsp
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-show nil)
  :bind (:map lsp-ui-mode-map ("C-c i" . lsp-ui-menu)))

(use-package lsp-ui
  :ensure t
  :commands
  (lsp-ui-doc-show
   lsp-ui-doc-glance)
  :bind (:map lsp-mode-map
              ("C-c C-d" . 'lsp-ui-doc-glance))
  :after lsp-mode
  :config (setq lsp-ui-doc-enable t
                lsp-ui-doc-show-with-cursor nil      
                lsp-ui-doc-include-signature t       
                lsp-ui-doc-position 'at-point))


(use-package consult-lsp :init :defer t :after lsp)

(use-package diminish
  :init
  (diminish 'abbrev-mode)
  (diminish 'buffer-face-mode)
  (diminish 'flyspell-mode)
  (diminish 'org-indent-mode)
  (diminish 'org-cdlatex-mode)
  (diminish 'visual-line-mode)
  (diminish 'buffer-face-mode)
  (diminish 'highlight-indent-guides-mode)
  (diminish 'eldoc-mode)
  (diminish 'subword-mode)
  (diminish 'flycheck-mode))

(use-package crux
  :ensure t)

(use-package treesit-auto
  :ensure t
  :after emacs
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))

;; C++ Formatter
(use-package clang-format
  :ensure t
  :hook ((c-mode        . remacs/clang-format-before-save)
         (c++-mode      . remacs/clang-format-before-save)
         (objc-mode     . remacs/clang-format-before-save)
         (c-ts-mode     . remacs/clang-format-before-save)
         (c++-ts-mode   . remacs/clang-format-before-save)
         (objc-ts-mode  . remacs/clang-format-before-save))
  :config
  (defun remacs/clang-format-before-save ()
    "Buffer-local before-save hook to format C/C++/ObjC code."
    (when (executable-find "clang-format")
      (add-hook 'before-save-hook #'clang-format-buffer nil t))))

;; Rust
(use-package
 rust-ts-mode
 :defer t
 :init
 :mode "\\.rs\\'")

(use-package rustic
  :ensure t
  :init
  (setq rustic-major-mode 'rust-ts-mode)
  :config
  (setq rustic-format-on-save t)
  :custom
  (rustic-cargo-use-last-stored-arguments t))

;;; markdown
(use-package
  markdown-mode
  :mode "\\.md\\'"
  :config
  (setq markdown-command "pandoc")
  (setq markdown-asymmetric-header t)
  (setq markdown-header-scaling t)
  (setq markdown-enable-math t)
  :bind
  (:map markdown-mode-map ("M-<left>" . markdown-promote))
  (:map markdown-mode-map ("M-<right>" . markdown-demote))
  (:map markdown-mode-map ("M-S-<left>" . markdown-promote-subtree))
  (:map markdown-mode-map ("M-S-<right>" . markdown-demote-subtree)))

(use-package markdown-preview-mode :commands markdown-preview)

(use-package
  emacs-lisp-mode
  :ensure nil
  :bind (:map emacs-lisp-mode-map
              ("C-c C-r" . eval-region)
              ("C-c C-d" . eval-defun)
              ("C-c C-b" . eval-buffer)
              ("C-c C-t" . ielm)))

(use-package eldoc-box
  :ensure t
  :defer t)

;; shell
(use-package
  shell
  :defer t
  :init
  :config
  (add-hook
   'after-save-hook
   'executable-make-buffer-file-executable-if-script-p))

;; ssh
(use-package ssh-config-mode :defer t)

;; toml
(use-package
  toml-mode
  :init
  :defer t
  :mode "/\\(Cargo.lock\\|\\.cargo/config\\)\\'")

;; yaml
(use-package
  yaml-mode
  :defer t
  :init
  :mode "\\.yml\\'"
  :mode "\\.yaml\\'")

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package
  dockerfile-mode
  :defer t
  :mode (("\\Dockerfile\\'" . dockerfile-mode)
         ("\\.dockerignore\\'" . dockerfile-mode)))

(use-package
  cmake-mode
  :hook (cmake-mode . lsp-deferred))

(unless (display-graphic-p)
  (use-package clipetty
    :ensure t
    :hook (after-init . global-clipetty-mode)
    :bind ("M-w" . clipetty-kill-ring-save)))

;; clojure check
(use-package flycheck-clj-kondo)

;; clojure mode
(use-package
  clojure-mode
  :after flycheck-clj-kondo
  :config (require 'flycheck-clj-kondo))

;; cider clojure
(setq org-babel-clojure-backend 'cider)
(use-package
  cider
  :defer t
  :init
  (progn
    (add-hook 'clojure-mode-hook 'cider-mode)
    (add-hook 'clojurec-mode-hook 'cider-mode)
    (add-hook 'cider-repl-mode-hook 'cider-mode))
  :config
  (setq cider-repl-display-help-banner nil)
  (setq cider-auto-mode nil))

;; scheme
(use-package
  geiser-guile
  :ensure t
  :config
  (setq scheme-program-name "guile")
  (setq geiser-default-implementation 'guile)
  (setq geiser-active-implementations '(guile))
  (setq geiser-implementations-alist '(((regexp "\\.scm$") guile)))
  (setq geiser-guile-binary "guile")
  (add-hook 'geiser-repl-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'inferior-scheme-mode-hook 'rainbow-delimiters-mode))

(use-package nix-ts-mode :mode ("\\.nix\\'" "\\.nix.in\\'"))

(use-package nix-drv-mode :ensure nix-mode :mode "\\.drv\\'")

(use-package nixpkgs-fmt
  :ensure t
  :config
  (add-hook 'nix-mode-hook 'nixpkgs-fmt-on-save-mode)
  (add-hook 'nix-ts-mode-hook 'nixpkgs-fmt-on-save-mode))

(use-package
  nix-shell
  :ensure nix-mode
  :commands (nix-shell-unpack nix-shell-configure nix-shell-build))

(use-package nix-repl :ensure nix-mode :commands (nix-repl))

;; syntax check
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error) ; optional but recommended error navigation
              ("M-p" . flycheck-previous-error)))

(use-package flycheck-clang-tidy
  :after flycheck
  :hook
  (flycheck-mode . flycheck-clang-tidy-setup))

(use-package bash-ts-mode
  :ensure nil
  :mode ("\\.sh\\'" "\\.bash\\'" "\\.zsh\\'" "\\.command\\'")
  :hook (bash-ts-mode . eglot-ensure)
  :config
  (setq bash-ts-mode-indent-offset 2
        sh-basic-offset 2
        indent-tabs-mode nil))

(use-package
  shfmt
  :defer t
  :hook
  (bash-ts--mode . (lambda () (shfmt-on-save-mode))))

(use-package ielm
  :config
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode))

(use-package highlight-symbol
  :ensure t
  :config
  (setq highlight-symbol-on-navigation-p t)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode))

(use-package highlight-numbers
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;; vterm
(defun remacs/vterm-project-association ()
  "Associate VTerm buffer with the current project."
  (when-let ((project (project-current)))
    (setq-local project-current project)))

(use-package
  vterm
  :init
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-always-compile-module t)
  (add-hook 'vterm-mode-hook #'remacs/vterm-project-association)
  :hook
  (vterm-mode . (lambda ()
                  (hl-line-mode -1)
                  (display-line-numbers-mode -1))))

(use-package vterm-toggle
  :after vterm
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . right)
                 (dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-width . 0.5))))

(defun remacs/eshell-config ()
  "Eshell config"
  (add-hook
   'eshell-pre-command-hook
   (lambda () (setenv "TERM" "xterm-256color")))
  (add-hook
   'eshell-post-command-hook (lambda () (setenv "TERM" "dumb")))
  (setq
   eshell-prompt-function 'eshell-prompt
   eshell-highlight-prompt nil ;;
   eshell-buffer-shorthand t ;;
   eshell-history-size 5000 ;;
   eshell-buffer-maximum-lines 12000 ;; truncate after 12k lines
   eshell-hist-ignoredups t ;; ignore duplicates
   eshell-error-if-no-glob t ;;
   eshell-glob-case-insensitive t ;;
   eshell-scroll-to-bottom-on-input 'all ;;
   eshell-list-files-after-cd t ;;
   eshell-aliases-file (concat user-emacs-directory "eshell/alias") ;;
   eshell-banner-message "" ;; welcome message
   )

  (setq eshell-visual-commands
        '("vim"
          "nvim"
          "screen"
          "top"
          "less"
          "more"
          "lynx"
          "ncftp"
          "pine"
          "tin"
          "trn"
          "elm"
          "vim"
          "nmtui"
          "alsamixer"
          "htop"
          "el"
          "elinks"
          "btm"))
  (setq eshell-visual-subcommands '(("git" "log" "diff" "show")))
  (with-eval-after-load "esh-opt"
    (autoload 'epe-theme-lambda "eshell-prompt-extras")
    (setq
     eshell-highlight-prompt nil
     eshell-prompt-function 'epe-theme-multiline-with-status)))

(use-package eshell-prompt-extras :ensure t :after eshell)

(use-package eshell :config (remacs/eshell-config))

;;(setenv "EXA_COLORS" "uu=36:gu=37:sn=32:sb=32:da=34:ur=34:uw=35:ux=36:ue=36:gr=34:gw=35:gx=36:tr=34:tw=35:tx=36:")

(use-package
  eshell-syntax-highlighting
  :after esh-mode
  :config (eshell-syntax-highlighting-global-mode +1))

;; Programming enhacements
(use-package iedit :bind ("C-c ." . iedit-mode) :diminish)

(use-package
  rainbow-delimiters
  :defer t
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package
  smartparens
  :ensure t
  :init
  (require 'smartparens-config)
  (smartparens-global-mode t)
  :diminish smartparens-mode
  :config (show-smartparens-mode t))

(use-package highlight-parentheses :ensure t)

;; Buffer search
(use-package anzu
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

;; Authentication
(use-package
  pinentry
  :config (setq epg-pinentry-mode 'loopback) (pinentry-start))

;; Templates
(use-package yasnippet
  :ensure t
                                        ;:hook ((text-mode
                                        ;        prog-mode
                                        ;        conf-mode
                                        ;        c++-ts-mode
                                        ;        snippet-mode) . yas-minor-mode-on)
  :init
  (setq yas-snippet-dir "~/.emacs.d/snippets")
  :config
  (yas-global-mode +1)
  (define-key yas-minor-mode-map (kbd "C-c y") 'yas-expand))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package 
  ripgrep 
  :ensure t)

(use-package
  sxhkdrc-mode
  :ensure t)

(use-package
  whole-line-or-region
  :hook(after-init . whole-line-or-region-global-mode))

;; Orgmode
(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun efs/org-font-setup ()
  "Replace list hyphen with dot."
  (font-lock-add-keywords
   'org-mode
   '(("^ *\\([-]\\) " (0 (prog1 ()
                           (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face
           '((org-level-1 . 1.2)
             (org-level-2 . 1.1)
             (org-level-3 . 1.05)
             (org-level-4 . 1.0)
             (org-level-5 . 1.1)
             (org-level-6 . 1.1)
             (org-level-7 . 1.1)
             (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil
                        :font "RobotoMono Nerd Font"
                        :weight 'regular
                        :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil
                      :foreground "unspeficied"
                      :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil
                      :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil
                      :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil
                      :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

;; org mode
(use-package
  org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq
   org-ellipsis " ▾"
   org-hide-emphasis-markers t
   org-confirm-babel-evaluate nil
   org-fontify-quote-and-verse-blocks t
   org-startup-folded 'content
   org-agenda-start-with-log-mode t
   org-log-done 'time
   org-log-into-drawer t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (clojure . t)
     (scheme . t)))
  (efs/org-font-setup))

;; custom bullets
(use-package
  org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom (org-bullets-bullet-list '("◉" "○" "✸" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq
   visual-fill-column-width 200
   visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package
  visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(setq org-babel-clojure-backend 'cider)

;; org templates
(require 'org-tempo)

(add-to-list 'org-modules 'org-tempo t)
(add-to-list 'org-structure-template-alist '("clj" . "src clojure"))
(add-to-list 'org-structure-template-alist '("pyt" . "src python"))
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("scm" . "src scheme"))

(use-package
  org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(use-package
  org-roam
  :ensure t
  :init (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Notes/Roam/")
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M %p>: %?"
      :if-new
      (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   :map
   org-mode-map
   ("C-M-i" . completion-at-point)
   :map
   org-roam-dailies-map
   ("Y" . org-roam-dailies-capture-yesterday)
   ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode))

(use-package
  deft
  :bind ("<f8>" . deft)
  :commands (deft)
  :config (setq deft-directory "~/Dropbox/Notes"
                deft-extensions '("md" "org" "txt" "tex")))

(use-package doc-view
  :custom
  (doc-view-resolution 300)
  (doc-view-mupdf-use-svg t)
  (large-file-warning-threshold (* 50 (expt 2 20))))

(use-package
  transient
  :defer t)

(setq auto-insert-directory (expand-file-name "auto-insert/" user-emacs-directory))
(define-auto-insert "\.cpp" "template.cpp")
(define-auto-insert "\.hpp" "template.hpp")
(define-auto-insert "CMakeLists.txt" "cmake_template.txt")

(use-package time
  :config
  ;; TZs to display with `world-clock'
  (setq world-clock-list
        '(("America/Los_Angeles" "Seattle")
          ("America/New_York" "New York")
          ("America/Sao_Paulo" "Brasilia")
          ("America/Argentina/Buenos_Aires" "Buenos Aires")
          ("Europe/London" "London")
          ("Europe/Paris" "Paris")
          ("Europe/Sofia" "Sofia")
          ("Asia/Istanbul" "Istanbul")
          ("Israel" "Tel Aviv")
          ("Asia/Calcutta" "Bangalore")
          ("Asia/Tokyo" "Tokyo"))))

(use-package smartparens
  :ensure t  ;; install the package
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

;; Writing packages
(use-package
  nov
  :ensure t
  :hook
  (nov-mode . (lambda ()
                (hl-line-mode -1)
                (display-line-numbers-mode -1)))
  :config
  (setq nov-unzip-program (executable-find "bsdtar")
        nov-unzip-args '("-xC" directory "-f" filename)
        nov-text-width t
        visual-fill-column-center-text t)
  (add-hook 'nov-mode-hook 'visual-line-mode)
  (add-hook 'nov-mode-hook 'visual-fill-column-mode)
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package 
  pdf-tools 
  :magic ("%PDF" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward)
              ("C-g" . pdf-view-goto-page)
              ("C-c C-a h" . pdf-annot-add-highlight-markup-annotation)
              ("C-c C-a t" . pdf-annot-add-text-annotation)
              ("C-c C-a d" . pdf-annot-delete)
              ("C-c C-a x"   . remacs/pdf-toggle-dark-mode))
  :ensure t
  :hook
  (pdf-view-mode . (lambda ()
                     (hl-line-mode -1)
                     (display-line-numbers-mode -1)))

  :config
  (pdf-tools-install :no-query) 
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (add-hook 'pdf-view-mode-hook (lambda () (remacs/pdf-toggle-dark-mode))))

(use-package elfeed
  :custom
  (elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))
  (elfeed-show-entry-switch 'display-buffer)
  (elfeed-search-filter "@2-months-ago ")
  :bind
  ("C-c w e" . elfeed))

(use-package elfeed-org
    :config
    (elfeed-org)
    :custom
    (rmh-elfeed-org-files (list "~/Documents/elfeed.org")))

(use-package org-web-tools
  :bind
  (("C-c w w" . org-web-tools-insert-link-for-url)))

;; end writing
(use-package plantuml-mode
  :init
    (setq plantuml-default-exec-mode 'jar)
    (setq plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
    (setq org-plantuml-jar-path (expand-file-name "/usr/share/plantuml/plantuml.jar"))
    (setq org-startup-with-inline-images t)
    (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
    (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

;;; My functions
(defun remacs/pdf-toggle-dark-mode ()
  "Toggle PDF midnight mode (dark mode)."
  (interactive)
  (if (bound-and-true-p pdf-view-midnight-minor-mode)
      (pdf-view-midnight-minor-mode -1)
    (setq pdf-view-midnight-colors '("#c8d3f5" . "#191a2a"))
    (pdf-view-midnight-minor-mode)))

(defun remacs/reload-config ()
  "Reload Emacs config file."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(defun remacs/split-window-two ()
  "Split current window into two."
  (interactive)
  (split-window-right)
  (balance-windows))

(defun remacs/kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer nil))

(defun remacs/kill-all-buffers ()
  "Kill all buffers."
  (interactive)
  (let ((lsp-restart 'ignore))
    (delete-other-windows)
    (save-some-buffers)
    (let ((kill-buffer-query-functions '()))
      (mapc 'kill-buffer (buffer-list)))))

(defun remacs/text-scale-restore ()
  "Restore text scale."
  (interactive)
  (text-scale-set 0)
  (message "restored"))

(defun remacs/sha512 (&optional filename)
  "Compute sha512 message digest from FILENAME."
  (interactive)
  (let ((filename (or filename (read-file-name "Filename:"))))
    (secure-hash
     'sha512 (-> filename (message filename) (find-file-noselect)))))

(defun remacs/2base64 (&optional filename)
  "Encode FILENAME to base64."
  (-> filename (remacs/sha512) (base64-encode-string)))

(defun remacs/sha512-dir (dir)
  "Compute sha512 message digest to all files into DIR."
  (interactive)
  (mapcar
   (lambda (x)
     (cons (concat dir "/" x) (remacs/sha512 (concat dir "/" x))))
   (directory-files dir nil directory-files-no-dot-files-regexp)))

(defun remacs/load-darkmode ()
  "Load spacemacs darkmode."
  (interactive)
  (load-theme 'modus-vivendi-tinted t))

(defun remacs/load-lightmode ()
  "Load spacemacs lightmode."
  (interactive)
  (load-theme 'modus-operandi-deuteranopia t))

(defun remacs/load-dracula ()
  "Load doom dracula."
  (interactive)
  (load-theme 'doom-dracula t))

(defun remacs/choose-theme ()
  "Select color theme."
  (interactive)
  (let ((chose-theme
         (completing-read "Choose:" '(light dark dark-alternative))))
    (message chose-theme)
    (cond
     ((equal "dark" chose-theme)
      (remacs/load-darkmode))
     ((equal "light" chose-theme)
      (remacs/load-lightmode))
     ((equal "dark-alternative" chose-theme)
      (remacs/load-dracula))))
  (funcall major-mode))

(defun remacs/duplicate-line ()
  "Duplicate line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank))

;; from: https://emacs.stackexchange.com/a/34307
(defun remacs/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

;; from: https://emacs.stackexchange.com/a/34307
(defun remacs/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun remacs/switch-to-scratch-buffer ()
  "Switch to scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun remacs/switch-to-message-buffer ()
  "Switch to messages buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun remacs/custom-tab-indent ()
  "Use tabs instead of spaces."
  (setq-local indent-tabs-mode 1))

(defun remacs/su-find-file (filename)
  "Find file with FILENAME and open a super user."
  (interactive "FFind file(sudo): ")
  (let ((file-to-open (concat "/sudo::" (expand-file-name filename))))
    (find-file file-to-open)))

(defun remacs/supress-warnings ()
  "Supress emergency messages."
  (interactive)
  (setq warning-minimum-level :emergency))

(defun remacs/default-warnings ()
  "Supress warning messages."
  (interactive)
  (setq warning-minimum-level :warning))

(defun remacs/my-change-number-at-point (change increment)
  "Change the number at point."
  (let ((number (number-at-point))
        (point (point)))
    (when number
      (progn
        (forward-word)
        (search-backward (number-to-string number))
        (replace-match
         (number-to-string (funcall change number increment)))
        (goto-char point)))))

(defun remacs/my-increment-number-at-point (&optional increment)
  "Increment number at point like vim's C-a."
  (interactive "p")
  (remacs/my-change-number-at-point '+ (or increment 1)))

(defun remacs/my-decrement-number-at-point (&optional increment)
  "Decrement number at point like vim's C-x."
  (interactive "p")
  (remacs/my-change-number-at-point '- (or increment 1)))

(defun remacs/delete-file-and-buffer ()
  "Kills the current buffer and deletes the file it is visiting."
  (interactive)
  (if-let ((filename (buffer-file-name)))
      (when (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
        (delete-file filename)
        (message "Deleted file %s." filename)
        (kill-buffer))
    (message "Not a file visiting buffer!")))

;; rename-visited-file is introduced in Emacs 29.
(unless (fboundp 'rename-visited-file)
  (defun rename-visited-file ()
    "Renames the current buffer and the file it is visiting."
    (interactive)
    (let ((name (buffer-name))
          (filename (buffer-file-name)))
      (if (not (and filename (file-exists-p filename)))
          (error "Buffer '%s' is not visiting a file!" name)
        (let ((new-name (read-file-name "New name: " filename)))
          (if (get-buffer new-name)
              (error "A buffer named '%s' already exists!" new-name)
            (rename-file filename new-name 1)
            (rename-buffer new-name)
            (set-visited-file-name new-name)
            (set-buffer-modified-p nil)
            (message "File '%s' successfully renamed to '%s'"
                     name (file-name-nondirectory new-name))))))))

(defun remacs/upload-file-to-artifactory (filename)
  (interactive "FEnter file: ")
  (let ((upload-command (format "echo curl %s " filename))
        (output-buffer-name "Upload file"))
    (generate-new-buffer output-buffer-name)
    (switch-to-buffer output-buffer-name)
    (async-shell-command upload-command (current-buffer))
    (insert "\nUploading...")
    (insert "\ncurl command: " upload-command)
    (read-only-mode)))

(defun remacs/kill-to-next-whitespace ()
  "Kill characters from point to the next whitespace."
  (interactive)
  (let ((end (save-excursion
               (re-search-forward "[ \t\n]" nil t))))
    (if end
        (kill-region (point) end)
      (message "No whitespace found."))))

(defun remacs/kill-inner-word()
  "Kill the entire word that your cursor is in"
  (interactive)
  (forward-char 1)
  (backward-word)
  (kill-word 1))

(defun remacs/copy-whole-line()
  "Copies a line without regard for cursor position"
  (interactive)
  (save-excursion
    (kill-new (buffer-substring
               (point-at-bol)
               (point-at-eol)))))

(defun remacs/fix-iso ()
  "Update encoding rules around me"
  (interactive)
  (message "Changing to latin1")
  (setq locale-coding-system 'iso-8859-1)
  (set-terminal-coding-system 'iso-8859-1)
  (set-keyboard-coding-system 'iso-8859-1)
  (set-selection-coding-system 'iso-8859-1)
  (prefer-coding-system 'iso-8859-1)
  (define-coding-system-alias 'ISO-885901 'iso-8859-1))

(defun remacs/fix-utf ()
  "Update encoding rules around me"
  (interactive)
  (setq locale-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (define-coding-system-alias 'UTF-8 'utf-8))

(defun sim-vi-w (&optional arg)
  "Simulate Vi's \"w\" behavior."
  (interactive "p")
  (let ((count (or arg 1)))
    (dotimes (_ count)
      ;; Skip any whitespace
      (while (and (not (eobp))
                  (looking-at "[[:space:]]"))
        (forward-char))
      ;; If at end of buffer, stop
      (unless (eobp)
        ;; Move over word character or single punctuation
        (cond
         ((looking-at "\\w")
          (while (and (not (eobp))
                      (looking-at "\\w"))
            (forward-char)))
         ((looking-at "[^[:space:][:alnum:]_]")
          ;; Move over a *single* punctuation char like Vi
          (forward-char)))
        ;; Otherwise, move at least one char to avoid infinite loop
        (when (and (not (eobp))
                   (not (looking-at "\\w\\|[^[:space:][:alnum:]_]")))
          (forward-char))))))

(defun sim-vi-b (&optional arg)
  "Simulate Vi's \"b\" behavior: move to the beginning of the previous word."
  (interactive "p")
  (let ((count (or arg 1)))
    (dotimes (_ count)
      ;; Move back over any whitespace first
      (while (and (not (bobp))
                  (looking-back "[[:space:]]" 1))
        (backward-char))
      ;; If at beginning of buffer, stop
      (unless (bobp)
        ;; If we're on a word character, go back over word
        (cond
         ((looking-back "\\w" 1)
          (while (and (not (bobp))
                      (looking-back "\\w" 1))
            (backward-char)))
         ;; If we're on punctuation/symbol, go back one
         ((looking-back "[^[:space:][:alnum:]_]" 1)
          (backward-char)))
        ;; If we landed *inside* a word, go to beginning of it
        (while (and (not (bobp))
                    (looking-back "\\w" 1))
          (backward-char))))))

;; Remap
(global-set-key [remap keyboard-quit] #'crux-keyboard-quit-dwim)
(global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
(global-set-key (kbd "C-k") #'crux-smart-kill-line)
(global-set-key (kbd "C-S-k") #'crux-kill-line-backwards)
(global-set-key (kbd "C-o") #'crux-smart-open-line)
(global-set-key (kbd "C-S-o") #'crux-smart-open-line-above)
(global-set-key (kbd "M-f") #'sim-vi-w)
(global-set-key (kbd "M-b") #'sim-vi-b)
(global-set-key (kbd "C-<tab>") #'other-window)
(global-set-key (kbd "M-<down>") #'enlarge-window)
(global-set-key (kbd "M-<up>") #'shrink-window)
(global-set-key (kbd "M-<right>") #'enlarge-window-horizontally)
(global-set-key (kbd "M-<left>") #'shrink-window-horizontally)
(global-set-key (kbd "C-x c i") #'crux-find-user-init-file)
(global-set-key (kbd "C-x c r") #'remacs/reload-config)

(global-set-key (kbd "C-c f d") #'remacs/delete-file-and-buffer)
(global-set-key (kbd "C-c f r") #'rename-visited-file)
(global-set-key (kbd "C-x j") #'dired-jump)
(global-set-key (kbd "C-x C-l") #'downcase-dwim)
(global-set-key (kbd "C-x C-u") #'upcase-dwim)
(global-set-key (kbd "C-c K") #'remacs/kill-all-buffers)

;; https://whhone.com/emacs-config/#modern-editor-behavior
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)
(keymap-global-unset "C-x <escape> <escape>") ; repeat-complex-command

(global-set-key (kbd "M-s-<down>") #'remacs/move-line-down)
(global-set-key (kbd "M-s-<up>") #'remacs/move-line-up)
(global-set-key (kbd "M-s M-o") #'ff-get-other-file)
(global-set-key (kbd "C-c s l") #'sort-lines)
(global-set-key (kbd "C-c k w") #'remacs/kill-inner-word)

(global-set-key (kbd "C-c C-q") #'view-mode)
(define-key global-map (kbd "C-x C-r") #'recentf-open) ; override `find-file-read-only'

(global-set-key (kbd "C-c g n") 'git-gutter:next-hunk)
(global-set-key (kbd "C-c g p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-c g r") 'git-gutter:revert-hunk)
(global-set-key (kbd "C-c g v") 'git-gutter:popup-hunk)


;;; init.el ends here
