;; comment code efficiently
(use-package 
  evil-nerd-commenter 
  :bind ("M-/" . 'evilnc-comment-or-uncomment-lines))

(provide 'init-commenter)
