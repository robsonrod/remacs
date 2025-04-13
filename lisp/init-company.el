(use-package 
  company 
  :ensure t 
  :bind ("C-M-/" . company-complete-common-or-cycle) 
  :init (add-hook 'after-init-hook 'global-company-mode) 
  :config (setq company-show-quick-access t company-minimum-prefix-length 1 company-idle-delay 0.5
                company-backends '((company-files ; files & directory
                                    company-keywords ; keywords
                                    company-capf     ; what is this?
                                    company-yasnippet company-restclient) 
                                   (company-abbrev company-dabbrev))))

(use-package 
  company-box 
  :ensure t 
  :after company 
  :hook (company-mode . company-box-mode))

(use-package
  company-restclient 
  :ensure t)

(provide 'init-company)
