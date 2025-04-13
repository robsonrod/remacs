(use-package 
  geiser-guile
  :ensure t 
  :config
  (setq scheme-program-name "guile")
  (setq geiser-default-implementation 'guile)
  (setq geiser-active-implementations '(guile))
  (setq geiser-implementations-alist '(((regexp "\\.scm$") guile)))
  (setq geiser-guile-binary "guile")
  (add-hook 'geiser-repl-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'inferior-scheme-mode-hook 'rainbow-delimiters-mode))

(provide 'init-scheme)
