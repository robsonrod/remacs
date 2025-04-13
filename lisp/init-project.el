(defun robsonrod/switch-project-action ()
  "Switch to a workspace with the project name and start `magit-status'."
  (persp-switch (projectile-project-name))
  (magit-status))

;; project manager
(use-package 
  projectile 
  :diminish projectile-mode 
  :config (projectile-mode) 
  (setq projectile-globally-ignored-directories (append '(".git"
                                                          ".ccls_cache") projectile-globally-ignored-directories))
  :demand t 
  :custom ((projectile-completion-system 'ivy)) 
  :bind-keymap ("C-c p" . projectile-command-map) 
  :init (when (file-directory-p "~/dev/personal") 
          (setq projectile-project-search-path '("~/dev/personal"))) 
  (setq projectile-switch-project-action #'robsonrod/switch-project-action))

;; ivy integration project manager
(use-package 
  counsel-projectile 
  :after projectile 
  :bind (("C-M-p" . counsel-projectile-find-file)) 
  :config (counsel-projectile-mode))

;; find file in project
(use-package 
  find-file-in-project 
  :if (executable-find "fdfind") 
  :init (when (executable-find "fd") 
          (setq ffip-use-rust-fd t)) 
  :bind (("C-c f a" . ffap) 
         ("C-c f i" . ffip)))

(provide 'init-project)
