(use-package 
  clojure-mode 
  :after flycheck-clj-kondo 
  :config (require 'flycheck-clj-kondo))

;; cider clojure
(setq org-babel-clojure-backend 'cider)
(use-package 
  cider 
  :defer t 
  :init (progn (add-hook 'clojure-mode-hook 'cider-mode) 
               (add-hook 'clojurec-mode-hook 'cider-mode) 
               (add-hook 'cider-repl-mode-hook 'cider-mode)) 
  :config (setq cider-repl-display-help-banner nil) 
  (setq cider-auto-mode nil))

;; clojure support
(use-package 
  flycheck-clj-kondo)

(provide 'init-clojure)
