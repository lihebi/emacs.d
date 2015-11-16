;;; package.el --- External packages

;;; Commentary:
;; No Comments!

(require 'package)

;;; Code:
(setq package-archives
      (append package-archives
              '(("melpa" . "http://melpa.milkbox.net/packages/"))
              '(("org" . "http://orgmode.org/elpa/"))))
(package-initialize)
(setq package-enable-at-startup nil)

;; If never connected to repositories before, download package descriptions so
;; `use-package' can trigger installation of missing packages.
(unless package-archive-contents
  (message "Refreshing ELPA package archives...")
  (package-refresh-contents))

;; ...but before everything, make sure `use-package' is installed.
(unless (package-installed-p 'use-package)
  (message "`use-package' not found.  Installing...")
  (package-install 'use-package))

(require 'use-package)
(setq use-package-minimum-reported-time 0
      use-package-verbose t)

;; not sure if these wierd binding is what I want
(use-package windmove
  :defer t
  :bind
  (("<f2> <right>" . windmove-right)
   ("<f2> <left>" . windmove-left)
   ("<f2> <up>" . windmove-up)
   ("<f2> <down>" . windmove-down)
   ))

(use-package org-mode
  :defer t
  :bind
  (("C-c n" . org-capture)
   ("C-c o" . org-open-at-point)
   )
  :init
  (progn
    (defvar org-startup-folded)
    (defvar org-directory)
    (defvar org-capture-templates)
    (defvar org-agenda-files)
    (setq org-startup-folded nil)
    (setq org-directory "~/github/org")
    ;; capture templates
    (setq org-capture-templates
          '(("t" "TODO" entry (file+headline (concat org-directory "/scratch.org") "Tasks")
             "* TODO %?\n  %U")
            ("n" "Note" entry (file (concat org-directory "/scratch.org"))
             "* Notes on %U\n%?" :prepend t)
            ("s" "Stack" entry (file (concat org-directory "/stack.org"))
             "* New Stack on %U\n%?" :prepend t)
            ))
    (setq org-agenda-files (list org-directory))
    )
  :config
  ;; what's this
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk . t)
     (emacs-lisp . t)
     (python . t)
     (ruby . t)
     (sh . t)))
  )

(use-package smex
  ;; use ido in M-x
  :defer t
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)
   ("C-c C-c M-x" . execute-extended-command) ; my old M-x
   )
  :init
  (progn
    (smex-initialize))
  )

(use-package powerline
  :disabled t
  :init
  (progn
    (defvar sml/no-confirm-load-theme)
    (defvar sml/theme)
    (require 'smart-mode-line)
    (setq sml/no-confirm-load-theme t)
    ;; (setq sml/theme nil)
    ;; (setq sml/theme 'dark)
    ;; (setq sml/theme 'light)
    ;; (setq sml/theme 'respectful)
    (setq sml/theme 'powerline)
    (sml/setup)
    ;; not sure why use this hook instead of
    ;; using the official recommanded way above
    (add-hook 'after-init-hook #'sml/setup)
    )
  )

(use-package smart-mode-line
  :init
  (progn
    (setq sml/no-confirm-load-theme t) ; do not warn me for loading a theme
    (setq sml/theme 'dark)
    (sml/setup)
    (setq sml/name-width 15)
    (setq rm-blacklist
          (format "^ \\(%s\\)$"
                  (mapconcat #'identity
                             '("FlyC.*"
                               "Projectile.*"
                               "hebi-keys"
                               "PgLn"
                               "company"
                               "Undo-Tree"
                               "yas"
                               "GitGutter")
                             "\\|")))
    )
  )
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)))

(use-package markdown-mode
  :init
  (add-hook 'markdown-mode-hook 'turn-on-orgtbl)
  )

(use-package projectile
  :init
  (progn
    (projectile-global-mode)
    (setq projectile-enable-caching t) ; enable catch
    )
  
  )

(use-package neotree
  ;; neotree is not using because it conflicts with perspective
  ;; also, we have speedbar ^_^
  :disabled t
  ;; (define-key neotree-mode-map (kbd "i") #'neotree-enter-horizontal-split) ; TODO what's the #?
  ;; (define-key neotree-mode-map (kbd "I") #'neotree-enter-vertical-split)
  ;; when switch project, neotree change root automatically
  ;; (setq projectile-switch-project-action 'neotree-projectile-action)
  :bind
  (
   ("<f8>" . neotree-toggle))
  )

(use-package perspective
  :init
  (progn
    (persp-mode)
    )
  :config
  (progn
    ;; (use-package persp-projectile)
    ;; (define-key projectile-mode-map (kbd "s-s") 'projectile-persp-switch-project)
    )
  )

(use-package fill-column-indicator
  ;; 80 characters
  :ensure t
  :defer t
  :init
  (hook-into-modes 'fci-mode '(prog-mode-hook)))

(use-package company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :bind
  (
   ("s-/" . company-complete))
  )

(use-package yasnippet
  :init
  (progn
    (yas-global-mode 1))
  )

(use-package multiple-cursors
  :bind
  (
   ("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)
   ("C-S-<mouse-1>" . mc/add-cursor-on-click)
   )
  )

(use-package git-gutter
  :init
  (progn
    (global-git-gutter-mode t)
    (git-gutter:linum-setup)
    )
  :bind
  (
   ("C-x C-g" . git-gutter:toggle))
  :config
  (progn
    (custom-set-variables
     '(git-gutter:modified-sign "  ") ;; two space
     '(git-gutter:added-sign "++")    ;; multiple character is OK
     '(git-gutter:deleted-sign "--"))

    ;; (custom-set-variables
    ;;  '(git-gutter:window-width 2)
    ;;  '(git-gutter:modified-sign "☁")
    ;;  '(git-gutter:added-sign "☀")
    ;;  '(git-gutter:deleted-sign "☂"))

    (set-face-background 'git-gutter:modified "purple") ; background color
    (set-face-foreground 'git-gutter:added "green") ; foreground not working ...
    (set-face-foreground 'git-gutter:deleted "red")
    )
  )

(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)  
  )

(use-package expand-region
  :bind
  (
   ("s-e" . er/expand-region))
  )

(use-package helm
  :bind
  (
   ("M-x" . helm-M-x))
  )

(use-package exec-path-from-shell
  ;; when start emacs from desktop env instead of shell, the PATH is aweful.
  :ensure t
  :if window-system
  :config
  (progn
    (exec-path-from-shell-initialize)
    (message "%s: %s" "exec-path-from-shell post config" (getenv "PATH"))))

(use-package browse-kill-ring
  :ensure t
  :defer t
  :config
  (browse-kill-ring-default-keybindings))

(use-package google-c-style
  ;; c style used by google
  :ensure t
  :defer t)

(use-package goto-chg
  ;; goto last change in this buffer
  :ensure t
  :bind (("C-." . goto-last-change)
         ("C-," . goto-last-change-reverse)))

(use-package guide-key
  ;; one key to rule them all
  :ensure t
  :diminish guide-key-mode
  :config
  (progn
    (setq guide-key/popup-window-position 'bottom
          guide-key/guide-key-sequence t  ; enable for all prefixes
          guide-key/recursive-key-sequence-flag t)

    (defun guide-key/org-mode-hook ()
      (guide-key/add-local-highlight-command-regexp "org-"))
    (add-hook 'org-mode-hook 'guide-key/org-mode-hook)

    (guide-key-mode 1)))

(use-package rainbow-delimiters
  ;; different colors for different level of parens
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)))

(use-package regex-tool
  :ensure t
  :defer t)

(provide 'packages)
;;; packages.el ends here

