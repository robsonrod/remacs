;; Hook to modes
(defun evil-hook ()
  (dolist (mode '(custom-mode eshell-mode git-rebase-mode term-mode dired-mode help-mode
                              helm-grep-mode grep-mode wdired-mode ))
    (add-to-list 'evil-emacs-state-modes mode)))

(global-set-key (kbd "C-M-u") 'universal-argument)
(global-set-key (kbd "C-SPC") nil)

;; Watch out with arrow keys
(defun dont-arrow-me-bro ()
  (interactive)
  (message "Arrow keys are bad, you know?"))

;; evil keybings
(use-package 
  evil
  :init (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-redo)
  :config (add-hook 'evil-mode-hook 'evil-hook)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (define-key evil-insert-state-map (kbd "C-s") 'evil-write)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (define-key evil-window-map "d" 'evil-delete-buffer)    ;; C-w d
  (define-key evil-window-map "\C-d" 'evil-delete-buffer) ;; C-w d
  (define-key evil-normal-state-map "\C-s" 'save-buffer)
  (define-key evil-insert-state-map "\C-s" 'save-buffer)
  (define-key evil-normal-state-map (kbd "C-.") 'evil-scroll-page-down)
  (define-key evil-normal-state-map (kbd "C-,") 'evil-scroll-page-up)
  (define-key evil-motion-state-map (kbd "C-i") 'evil-jump-forward)
  (define-key evil-normal-state-map (kbd "gr") 'lsp-find-references)
  (define-key evil-normal-state-map (kbd "gm") 'lsp-rename)
  (define-key evil-normal-state-map (kbd "gl") 'lsp-find-declaration)
  (define-key evil-normal-state-map (kbd "gi") 'lsp-find-implementation)

  ;; Disable arrow keys in normal and visual modes
  (define-key evil-normal-state-map (kbd "<left>") 'dont-arrow-me-bro)
  (define-key evil-normal-state-map (kbd "<right>") 'dont-arrow-me-bro)
  (define-key evil-normal-state-map (kbd "<down>") 'dont-arrow-me-bro)
  (define-key evil-normal-state-map (kbd "<up>") 'dont-arrow-me-bro)
  (evil-global-set-key 'motion (kbd "<left>") 'dont-arrow-me-bro)
  (evil-global-set-key 'motion (kbd "<right>") 'dont-arrow-me-bro)
  (evil-global-set-key 'motion (kbd "<down>") 'dont-arrow-me-bro)
  (evil-global-set-key 'motion (kbd "<up>") 'dont-arrow-me-bro)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dired-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; evil bindings for specific parts
(use-package
  evil-collection
  :after evil
  :init (setq evil-collection-company-use-tng nil) ;; Is this a bug in evil-collection?
  :custom (evil-collection-outline-bind-tab-p nil)
  :config (setq evil-collection-mode-list (remove 'lispy evil-collection-mode-list))
  (evil-collection-init))

(provide 'init-evil)
