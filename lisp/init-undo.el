(use-package undo-fu
  :ensure t
  :config
  (global-set-key (kbd "C-/") 'undo-fu-only-undo)
  (global-set-key (kbd "C-M-/") 'undo-fu-only-redo))

(use-package undo-fu-session
  :ensure t
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (global-undo-fu-session-mode))

(provide 'init-undo)
