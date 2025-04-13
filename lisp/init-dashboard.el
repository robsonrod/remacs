(use-package 
  dashboard 
  :ensure t
  :after all-the-icons
  :config
  (setq dashboard-banner-logo-title "Welcome" 
        dashboard-set-init-info nil 
        show-week-agenda-p t) 
    (setq dashboard-set-heading-icons t 
        dashboard-set-file-icons t 
        dashboard-center-content    t
        dashboard-show-shortcuts    t
        dashboard-set-navigator     t
        dashboard-startup-banner    'logo
        dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name
        initial-buffer-choice       (lambda () (get-buffer "*dashboard*"))
        dashboard-footer-messages '("Happy codding")) 
    (setq dashboard-items '((recents  . 15)
                            (agenda   . 10)
                            (projects . 10)))
  (dashboard-setup-startup-hook)
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer))

(provide 'init-dashboard)
