(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :bind (("C-\\". 'neotree-toggle)))

(provide 'init-treemacs)
