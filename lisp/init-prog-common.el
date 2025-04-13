;; flycheck
(use-package 
  flycheck 
  :ensure t 
  :init)

;; edit mutiple regions
(use-package 
  iedit 
  :bind ("C-;" . iedit-mode) 
  :diminish)

;; rainbow delimiters
(use-package 
  rainbow-delimiters 
  :defer t 
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; paredit
(use-package 
  paredit 
  :defer t 
  :init (progn (add-hook 'emacs-lisp-mode-hook 'paredit-mode) 
               (add-hook 'clojure-mode-hook 'paredit-mode) 
               (add-hook 'clojurec-mode-hook 'paredit-mode) 
               (add-hook 'cider-repl-mode-hook 'paredit-mode)
               (add-hook 'geiser-repl-mode-hook 'paredit-mode)))

(use-package
  smartparens
  :defer t
  :hook (prog-mode . smartparens-mode))

;; rest client
(use-package 
  restclient 
  :ensure t 
  :mode (("\\.http\\'" . restclient-mode)))

;; yaml support
(use-package 
  yaml-mode 
  :defer t)

;; dockerfile support
(use-package 
  dockerfile-mode 
  :defer t)



(provide 'init-prog-common)
