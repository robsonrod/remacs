(use-package 
  dictionary 
  :bind (("C-c l" . dictionary-lookup-definition)) 
  :config (setq dictionary-server "dict.org"))

(provide 'init-dictionary)
