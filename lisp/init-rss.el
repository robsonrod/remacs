(use-package 
  elfeed 
  :custom (elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)) 
  (elfeed-show-entry-switch 'display-buffer) 
  :bind ("C-c w" . elfeed ))

(provide 'init-rss)
