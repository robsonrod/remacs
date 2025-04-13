(defun robsonrod/smart-open-line-above () 
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode." 
  (interactive) 
  (move-beginning-of-line nil) 
  (newline-and-indent) 
  (forward-line -1) 
  (indent-according-to-mode))

(defun robsonrod/smart-open-line () 
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode." 
  (interactive) 
  (move-end-of-line nil) 
  (newline-and-indent))

(defun robsonrod/open-my-config () 
  "Open emacs config file." 
  (interactive) 
  (find-file user-init-file))

(defun robsonrod/split-window-two () 
  "Split current window into two." 
  (interactive) 
  (split-window-right) 
  (balance-windows))

(defun robsonrod/kill-current-buffer () 
  "Kill the current buffer." 
  (interactive) 
  (kill-buffer nil))

(defun robsonrod/kill-all-buffers () 
  "Close all buffers." 
  (interactive) 
  (let ((lsp-restart 'ignore)) 
    (delete-other-windows) 
    (save-some-buffers) 
    (let ((kill-buffer-query-functions '())) 
      (mapc 'kill-buffer (buffer-list)))))

(defun robsonrod/text-scale-restore ()
  "Restore text scale"
  (interactive)
  (text-scale-set 0)
  (message "restored"))

(defun robsonrod/sha512 
    (&optional 
     filename)
  "Compute sha512 message digest" 
  (interactive) 
  (let ((filename (or filename 
                      (read-file-name "Filename:")))) 
    (secure-hash 'sha512  (-> filename (message filename) 
                              (find-file-noselect)))))

(defun robsonrod/2base64 
    (&optional 
     filename)
  "Encode data to base64"
  (-> filename (robson/sha512) 
      (base64-encode-string)))

(defun robsonrod/sha512-dir (dir)
  "Compute sha512 message digest to all files "
  (interactive) 
  (mapcar (lambda (x)
            (cons (concat dir "/" x) (robson/sha512 (concat dir "/" x)))) 
          (directory-files dir nil directory-files-no-dot-files-regexp)))

(defun robsonrod/load-darkmode ()
  "Load spacemacs darkmode"
  (interactive)
  (load-theme 'spacemacs-dark t))

(defun robsonrod/load-lightmode ()
  "Load spacemacs lightmode"
  (interactive)
  (load-theme 'spacemacs-light t))

(defun robsonrod/load-dracula ()
  "Load doom dracula"
  (interactive)
  (load-theme 'doom-dracula t))

(defun robsonrod/choose-theme()
  (interactive)
  (let ((chose-theme (ivy-read "Choose:" '(light dark dark-alternative))))
     (message chose-theme)
     (cond
      ((equal "dark" chose-theme) (robsonrod/load-darkmode))
      ((equal "light" chose-theme) (robsonrod/load-lightmode))
      ((equal "dark-alternative" chose-theme) (robsonrod/load-dracula))
      )
    )
  (funcall major-mode))

(defun robsonrod/eshell-clear-buffer ()
  "Clear the current Eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun robsonrod/duplicate-line ()
  "Duplicate line"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

;; from: https://emacs.stackexchange.com/a/34307
(defun robsonrod/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

;; from: https://emacs.stackexchange.com/a/34307
(defun robsonrod/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun robsonrod/switch-to-scratch-buffer ()
  "Switch to scratch buffer"
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun robsonrod/switch-to-message-buffer ()
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun robsonrod/custom-tab-indent ()
  "Use tabs instead of spaces"
  (setq-local indent-tabs-mode 1))

(defun robsonrod/su-find-file (filename)
  (interactive "FFind file(sudo): ")
  (let ((file-to-open (concat "/sudo::" (expand-file-name filename))))
    (find-file file-to-open)))

(defun robsonrod/supress-warnings ()
  (interactive)
  (setq warning-minimum-level :emergency))

(defun robsonrod/default-warnings ()
  (interactive)
  (setq warning-minimum-level :warning))

(provide 'init-functions)
