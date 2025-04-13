(defun efs/org-mode-setup () 
  (org-indent-mode) 
  (variable-pitch-mode 1) 
  (visual-line-mode 1))

(defun efs/org-font-setup () 
  "Replace list hyphen with dot."
  (font-lock-add-keywords 
   'org-mode
   '(("^ *\\([-]\\) " (0 (prog1 () 
                           (compose-region (match-beginning 1) 
                                           (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2) 
                  (org-level-2 . 1.1) 
                  (org-level-3 . 1.05) 
                  (org-level-4 . 1.0) 
                  (org-level-5 . 1.1) 
                  (org-level-6 . 1.1) 
                  (org-level-7 . 1.1) 
                  (org-level-8 . 1.1))) 
    (set-face-attribute (car face) nil 
                        :font "RobotoMono Nerd Font"
                        :weight 'regular 
                        :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil
                      :foreground "unspeficied"
                      :inherit 'fixed-pitch) 
  (set-face-attribute 'org-code nil 
                      :inherit '(shadow fixed-pitch)) 
  (set-face-attribute 'org-table nil 
                      :inherit '(shadow fixed-pitch)) 
  (set-face-attribute 'org-verbatim nil 
                      :inherit '(shadow fixed-pitch)) 
  (set-face-attribute 'org-special-keyword nil 
                      :inherit '(font-lock-comment-face fixed-pitch)) 
  (set-face-attribute 'org-meta-line nil 
                      :inherit '(font-lock-comment-face fixed-pitch)) 
  (set-face-attribute 'org-checkbox nil 
                      :inherit 'fixed-pitch))

;; org mode
(use-package 
  org 
  :hook (org-mode . efs/org-mode-setup) 
  :config (setq org-ellipsis " ▾" org-hide-emphasis-markers t org-confirm-babel-evaluate nil
                org-fontify-quote-and-verse-blocks t org-startup-folded 'content
                org-agenda-start-with-log-mode t org-log-done 'time org-log-into-drawer t) 
  (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t) 
                                                           (python . t) 
                                                           (shell . t) 
                                                           (clojure . t) 
                                                           (scheme . t))) 
  (efs/org-font-setup))

;; custom bullets
(use-package 
  org-bullets 
  :after org 
  :hook (org-mode . org-bullets-mode) 
  :custom (org-bullets-bullet-list '("◉" "○" "✸" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill () 
  (setq visual-fill-column-width 200 visual-fill-column-center-text t) 
  (visual-fill-column-mode 1))

(use-package 
  visual-fill-column 
  :hook (org-mode . efs/org-mode-visual-fill))

(setq org-babel-clojure-backend 'cider)

;; org templates
(require 'org-tempo)

(add-to-list 'org-modules 'org-tempo t)
(add-to-list 'org-structure-template-alist '("clj" . "src clojure"))
(add-to-list 'org-structure-template-alist '("pyt" . "src python"))
(add-to-list 'org-structure-template-alist '("sh"  . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("scm" . "src scheme"))

(use-package 
  org-auto-tangle 
  :defer t 
  :hook (org-mode . org-auto-tangle-mode))

;; org-roam
(use-package 
  org-roam 
  :ensure t 
  :init (setq org-roam-v2-ack t) 
  :custom (org-roam-directory "~/Notes/Roam/") 
  (org-roam-completion-everywhere t) 
  (org-roam-capture-templates '(("d" "default" plain "%?" 
                                 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                    "#+title: ${title}\n") 
                                 :unnarrowed t) 
                                ("l" "programming language" plain (file
                                                                   "~/Notes/Roam/ProgrammingLanguagesTemplate.org") 
                                 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                    "#+title: ${title}\n") 
                                 :unnarrowed t) 
                                ("b" "book notes" plain (file "~/Notes/Roam/BookNotesTemplate.org") 
                                 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                    "#+title: ${title}\n") 
                                 :unnarrowed t) 
                                ("p" "project" plain (file "~/Notes/Roam/ProjectsTemplate.org") 
                                 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                    "#+title: ${title}\n#+filetags: Project") 
                                 :unnarrowed t))) 
  :bind (("C-c n l" . org-roam-buffer-toggle) 
         ("C-c n f" . org-roam-node-find) 
         ("C-c n i" . org-roam-node-insert) 
         :map org-mode-map ("C-M-i" . completion-at-point)) 
  :config (org-roam-setup))

(use-package 
  elfeed-org 
  :config (elfeed-org) 
  :ensure t 
  :custom (rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")))

(provide 'init-org)
