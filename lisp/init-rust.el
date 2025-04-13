(use-package 
  rust-mode 
  :defer t 
  :mode "\\.rs\\'" 
  :custom (rust-format-on-save t) 
  (lsp-rust-server 'rust-analyzer))

(robsonrod/ctrl-c-definer
  :keymaps 'rust-mode-map
  "r" '(rust-run :which-key "cargo run")
  "R" '(rust-run-release :which-key "cargo run --release"))

(provide 'init-rust)
