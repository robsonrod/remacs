(use-package 
  clang-format 
  :ensure t 
  :config (add-hook 'c-mode-common-hook (lambda () (add-hook (make-local-variable 'before-save-hook) 'clang-format-buffer))))

(use-package 
  modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))
  
(provide 'init-cpp)
