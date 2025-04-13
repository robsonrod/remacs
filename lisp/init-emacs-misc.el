(use-package
  auto-compile
  :ensure t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; hide minor modes
(use-package 
  diminish)

(use-package 
  perspective 
  :demand t 
  :bind (("C-M-k" . persp-switch) 
         ("C-M-n" . persp-next) 
         ("C-x k" . persp-kill-buffer*)) 
  :custom (persp-initial-frame-name "Main") 
  (persp-mode-prefix-key (kbd "C-c M-p")) 
  :config
  ;; Running `persp-mode' multiple times resets the perspective list...
  (unless (equal persp-mode t) 
    (persp-mode)))

;; helper
(use-package 
  which-key 
  :init (which-key-mode) 
  :diminish which-key-mode 
  :config (setq which-key-idle-delay 0.3))

;; emacs help improvements
(use-package 
  helpful 
  :custom (counsel-describe-function-function #'helpful-callable) 
  (counsel-describe-variable-function #'helpful-variable) 
  :bind ([remap describe-function] . counsel-describe-function) 
  ([remap describe-command] . counsel-help-command) 
  ([remap describe-variable] . counsel-describe-variable) 
  ([remap describe-key] . helpful-key))

(use-package 
  ripgrep 
  :ensure t)

(use-package 
  exec-path-from-shell 
  :if (memq window-system '(mac ns x)) 
  :config (exec-path-from-shell-initialize))

(use-package
  auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-prompt-before-update t)
  (setq auto-package-update-interval 2)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(robsonrod/ctrl-c-definer
  "2" '(robsonrod/split-window-two :which-key "split into two windows")
  "k" '(robsonrod/kill-current-buffer :which-key "kill current buffer")
  "K" '(robsonrod/kill-all-buffers :which-key "Kill all buffers")
  "i" '(robsonrod/open-my-config :which-key "open emacs init file")
  "e" '(eval-buffer :which-key "eval current buffer")
  "=" '(robsonrod/text-scale-restore :which-key "restore font size")
  "+" '(text-scale-increase :which-key "increase font size")
  "-" '(text-scale-decrease :which-key "decrease font size")
  )

(robsonrod/major-mode-leader-map
  "d" '(robsonrod/duplicate-line :which-key "duplicate lines")
  "u" '(robsonrod/move-line-up :which-key "move line up")
  "w" '(robsonrod/move-line-down :which-key "move line down"))

(robsonrod/ctrl-c-definer
  "d" '(robsonrod/load-dracula :which-key "dracula theme")
  "a" '(robsonrod/load-darkmode :which-key "spacemacs dark theme")
  "l" '(robsonrod/load-lightmode :which-key "spacemacs light theme"))

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (c "https://github.com/tree-sitter/tree-sitter-c")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (js . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (lua "https://github.com/Azganoth/tree-sitter-lua")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (r "https://github.com/r-lib/tree-sitter-r")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
     (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; Don't let ediff break EXWM, keep it in one frame
(setq ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package openwith
  :config
  (setq openwith-associations
        (list
          (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv"))
                "mpv"
                '(file))
          (list (openwith-make-extension-regexp
                '("xbm" "pbm" "pgm" "ppm" "pnm"
                  "png" "gif" "bmp" "tif" "jpeg"))
                  "feh"
                  '(file))
          (list (openwith-make-extension-regexp
                '("pdf"))
                "zathura"
                '(file)))))

(use-package avy
  :ensure t)

(use-package ivy-avy
  :ensure t)

(robsonrod/ctrl-c-definer
  "f" '(avy-goto-line :which-key "jump to line")
  "c" '(avy-goto-char :which-key "jump to char"))

(use-package anzu
  :ensure t
  :config
  (global-anzu-mode +1))

(provide 'init-emacs-misc)
