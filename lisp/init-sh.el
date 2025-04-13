;; sh script support
(use-package 
  sh-script 
  :ensure nil 
  :config (with-eval-after-load 'company (add-hook 'sh-mode-hook #'(lambda () 
                                                                     (company-mode -1)))))

(use-package 
  shfmt 
  :defer t 
  :config (add-hook 'sh-mode-hook 'shfmt-on-save-mode))

(use-package 
  fish-mode 
  :defer t 
  :config (setq fish-enable-auto-indent t) 
  :mode "\\.fish\\'")

(provide 'init-sh)
