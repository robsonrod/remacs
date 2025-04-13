;; icons
(use-package 
  all-the-icons 
  :if (display-graphic-p))

;; modeline icons
(use-package 
  minions 
  :hook (doom-modeline-mode . minions-mode))

;; doom themes
(use-package 
  doom-themes 
  :ensure t 
  :config (setq doom-themes-enable-bold t doom-themes-enable-italic t) 
  ;; (load-theme 'doom-dracula t) 
  (doom-themes-org-config) 
  (doom-themes-neotree-config))

;; spacemacs themes
(use-package spacemacs-theme
  :ensure t)

(use-package catppuccin-theme
 :ensure t
 :config
 (setq catppuccin-flavor 'macchiato))

(use-package mood-line
  :config
  (mood-line-mode)
  :custom
  (mood-line-glyph-alist mood-line-glyphs-fira-code)
  (mood-line-format mood-line-format-default))

(load-theme 'catppuccin t) 

(provide 'init-gui)
