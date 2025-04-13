(use-package 
  pdf-tools 
  :ensure t 
  :config
  (pdf-tools-install) 
  (setq-default pdf-view-display-size 'fit-page)
  (add-hook 'pdf-view-mode-hook (lambda () (robsonrod/pdf-midnight))))

(defun robsonrod/pdf-midnight ()
  "Set pdf-view-midnight colors"
  (interactive)
  (setq pdf-view-midnight-colors '("#d8dee9" . "#2e3440"))
  (pdf-view-midnight-minor-mode))

(defun robsonrod/pdf-clear ()
  "Set pdf-view without colors"
  (interactive)
  (pdf-view-midnight-minor-mode -1))

(provide 'init-pdf)
