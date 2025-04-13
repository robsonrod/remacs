;; lsp - language server provider
(use-package 
  lsp-mode 
  :ensure t 
  :defer t 
  :commands (lsp lsp-deferred) 
  :bind (:map lsp-mode-map
              ("M-<RET>" . lsp-execute-action)) 
  :custom (lsp-auto-guess-root nil) 
  (lsp-prefer-flymake nil)           ; Use flycheck instead of flymake
  (lsp-enable-file-watchers nil) 
  (lsp-enable-folding nil) 
  (read-process-output-max (* 1024 1024)) 
  (lsp-keep-workspace-alive nil) 
  (lsp-eldoc-hook nil) 
  :hook ((c-mode . lsp-deferred) 
         (c++-mode . lsp-deferred) 
         (clojure-mode . lsp-deferred) 
         (rust-mode . lsp-deferred) 
         (lsp-mode . lsp-enable-which-key-integration)) 
  :config (setq lsp-keymap-prefix "C-c l") 
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map) 
  (setq lsp-file-watch-threshold 15000) 
  (setq lsp-ui-doc-enable nil) 
  (setq lsp-ui-doc-show-with-cursor nil) 
  (setq lsp-modeline-code-actions-enable nil) 
  (setq lsp-signature-render-documentation nil) 
  (setq lsp-lens-enable nil) 
  (setq lsp-enable-symbol-highlighting nil) 
  (setq lsp-eldoc-enable-hover nil) 
  (setq lsp-eldoc-hook nil) 
  (setq lsp-enable-links nil) 
  (setq lsp-log-io nil) 
  (setq lsp-enable-file-watchers nil) 
  (setq lsp-enable-on-type-formatting nil) 
  (setq lsp-completion-show-detail nil) 
  (setq lsp-completion-show-kind nil) 
  (setq lsp-headerline-breadcrumb-enable nil))

;; a hight level UI modules of lsp
(use-package 
  lsp-ui 
  :ensure t 
  :diminish 
  :defer t 
  :after lsp 
  :hook (lsp-mode . lsp-ui-mode) 
  :config (setq lsp-ui-sideline-enable t) 
  (setq lsp-ui-sideline-show-hover nil) 
  (setq lsp-ui-doc-position 'bottom) 
  (lsp-ui-doc-show) 
  :bind (:map lsp-ui-mode-map
              ("C-c i" . lsp-ui-menu)))

;; ivy integration
(use-package 
  lsp-ivy 
  :ensure t 
  :commands lsp-ivy-workspace-symbol)

;;  treemacs integration
(use-package 
  lsp-treemacs 
  :ensure t 
  :defer t 
  :after lsp)

(provide 'init-lsp)
