(use-package 
  elisp-format 
  :ensure t 
  :init)

(use-package eldoc
  :defer t
  :after company
  :init
  (eldoc-add-command 'company-complete-selection
                     'company-complete-common
                     'company-capf
                     'company-abort))


(robsonrod/major-mode-leader-map
  :keymaps 'emacs-lisp-mode-map
  "f" '(elisp-format-buffer :which-key "format the whole buffer"))

(provide 'init-elisp)
