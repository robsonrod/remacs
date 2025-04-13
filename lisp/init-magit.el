;; git
(use-package 
  magit 
  :bind ("C-M-;" . magit-status) 
  :commands (magit-status magit-get-current-branch) 
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; git diff
(use-package 
  git-gutter 
  :hook (prog-mode . git-gutter-mode) 
  :config (setq git-gutter:update-interval 0.02))

;; git diff enhanced
(use-package 
  git-gutter-fringe 
  :config (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated)) 
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated)) 
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package
  magit-todos
  :defer t)

(use-package git-link
  :commands git-link
  :config
  (setq git-link-open-in-browser t)
  (robsonrod/major-mode-leader-map
    "l"  'git-link))


(provide 'init-magit)
