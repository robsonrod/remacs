(defun remacs/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(, (car command-parts) nil 0 nil ,@ (cdr command-parts)))))

(defun remacs/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defvar remacs/polybar-process nil)
(defun remacs/kill-panel ()
  (interactive)
  (when remacs/polybar-process
    (ignore-errors
      (kill-process remacs/polybar-process)))
  (setq remacs/polybar-process nil))


(defun remacs/start-polybar ()
  (interactive)
  (remacs/kill-panel)
  (setq remacs/polybar-process (start-process-shell-command "polybar" nil "polybar emacs")))

(defun remacs/set-wallpaper ()
  (interactive)
  (start-process-shell-command "feh" nil "feh --bg-scale $HOME/.config/wallpaper/wallpaper.jpg"))

(defun remacs/exwm-init-hook ()
  (exwm-workspace-switch-create 1)
  (remacs/set-wallpaper)
  (remacs/start-polybar)
  (remacs/run-in-background "dunst")
  (remacs/run-in-background "picom")
  (remacs/run-in-background "screensaver")
  (remacs/run-in-background "low_bat_notifier")
  (exwm-modeline-mode)
  (dashboard-open))


(defun remacs/setup-window-by-class ()
  (interactive)
  (pcase exwm-class-name
    ("Emacs" (call-interactively #'exwm-input-toggle-keyboard))
    ("Firefox" (exwm-workspace-move-window 2))))

(use-package exwm
  :config
  (setq exwm-workspace-number 6)
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (pcase exwm-class-name
                ("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title))))))
  (add-hook 'exwm-update-class-hook #'remacs/exwm-update-class)
  (add-hook 'exwm-init-hook #'remacs/exwm-init-hook)
  (add-hook 'exwm-manage-finish-hook #'remacs/setup-window-by-class)
  

  ;; Hide the modeline on all X windows
  (add-hook 'exwm-floating-setup-hook
            (lambda ()
              (exwm-layout-hide-mode-line)))
  (start-process-shell-command "xrdb" nil "rxdb -merge ~/.Xresources")

  (require 'exwm-randr)
  (start-process-shell-command "xrandr" nil "xrandr --output $MONITOR --brightness 1.0")

  (setq exwm-workspace-warp-cursor t
        mouse-autoselect-window t
        focus-follows-mouse t)

  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\C-u
          ?\C-h
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:
          ?\C-\M-j
          ?\C-\ ))

  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)

          ;; Move between windows
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)

          ;; Launch applications via shell command
          ([?\s-d] . (lambda (command)
                       (interactive (list (read-shell-command "RUN: ")))
                       (start-process-shell-command command nil command)))

          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-t] . (lambda () (interactive)(start-process-shell-command "kitty" nil "kitty")))
          ([?\s-b] . (lambda () (interactive)(start-process "" nil "firefox")))
          ([?\s-B] . (lambda () (interactive)(start-process "" nil "google-chrome-stable")))
          ;; Open file manager
          ([?\s-f] . dired-jump)
          ([?\s-q] . (lambda () (interactive) (start-process-shell-command "powermenu" nil "powermenu")))
          ([?\s-k] . (lambda () (interactive) (start-process-shell-command "rofikeyboard" nil "rofikeyboard")))

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))
  
  (exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)
  (exwm-enable))

(use-package desktop-environment
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-")
  (desktop-environment-screenlock-command "screensaver")
  (desktop-environment-screenshot-command "screenshot"))

(use-package exwm-modeline
  :ensure t
  :after (exwm))

(provide 'init-exwm)
