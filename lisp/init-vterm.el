(use-package
  vterm
  :hook (vterm-mode . (lambda ()
                        (hl-line-mode -1)
                        (display-line-numbers-mode -1))))

(use-package
  multi-vterm
  :after vterm
  :ensure t
  :defer t)

(robsonrod/ctrl-c-definer
  "t" '(multi-vterm :which-key "open an arbitrary terminal")
  "j" '(multi-vterm-project :which-key "open terminal into project"))

(robsonrod/ctrl-c-definer
  :keymaps 'vterm-mode-map
  "n" '(multi-vterm-next :which-key "next terminal")
  "p" '(multi-vterm-prev :which-key "previous terminal"))

(provide 'init-vterm)
