;; debugger
(use-package 
  dap-mode 
  :after lsp-mode 
  :config (dap-auto-configure-mode) 
  :diminish 
  :bind (:map dap-mode-map
              (("<f12>" . dap-debug) 
               ("<f8>" . dap-continue) 
               ("<f9>" . dap-next) 
               ("<M-f11>" . dap-step-in) 
               ("C-M-<f11>" . dap-step-out) 
               ("<f7>" . dap-breakpoint-toggle))))

(provide 'init-dap)
