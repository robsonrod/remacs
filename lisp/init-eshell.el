(defun robsonrod/eshell-config () 
  "Eshell config"
  (add-hook 'eshell-pre-command-hook (lambda () 
(setenv "TERM" "xterm-256color"))) 
(add-hook 'eshell-post-command-hook (lambda () 
(setenv "TERM" "dumb"))) 
(setq eshell-prompt-function 'eshell-prompt eshell-highlight-prompt nil ;;
        eshell-buffer-shorthand t         ;;
        eshell-history-size 5000          ;;
        eshell-buffer-maximum-lines 12000 ;; truncate after 12k lines
        eshell-hist-ignoredups t          ;; ignore duplicates
        eshell-error-if-no-glob t         ;;
        eshell-glob-case-insensitive t    ;;
        eshell-scroll-to-bottom-on-input 'all ;;
        eshell-list-files-after-cd t          ;;
        eshell-aliases-file (concat user-emacs-directory "eshell/alias") ;;
        eshell-banner-message "" ;; welcome message
)

(setq eshell-visual-commands '("vim" "nvim" "screen" "top" "less" "more" "lynx" "ncftp" "pine" "tin" "trn" "elm" "vim" "nmtui" "alsamixer" "htop" "el" "elinks" "btm")) 
(setq eshell-visual-subcommands '(("git" "log" "diff" "show")))
(with-eval-after-load "esh-opt" (autoload 'epe-theme-lambda "eshell-prompt-extras") 
(setq eshell-highlight-prompt nil eshell-prompt-function 'epe-theme-dakrone)))

(use-package eshell-prompt-extras 
  :ensure t 
  :after eshell)

(use-package eshell 
:config (robsonrod/eshell-config))

(setenv "EXA_COLORS" "uu=36:gu=37:sn=32:sb=32:da=34:ur=34:uw=35:ux=36:ue=36:gr=34:gw=35:gx=36:tr=34:tw=35:tx=36:")

(use-package eshell-syntax-highlighting 
:after esh-mode 
:config (eshell-syntax-highlighting-global-mode +1))

(use-package 
  fish-completion 
  :hook (eshell-mode . fish-completion-mode))

(use-package 
  eshell-syntax-highlighting 
  :after esh-mode 
  :config (eshell-syntax-highlighting-global-mode +1))

(message "loading eshell")
(provide 'init-eshell)
