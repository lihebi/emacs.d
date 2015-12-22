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

;; we can use :ensure t to auto install package
;; set the following to apply this globally
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common doc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; :bind,mode,interpreter will imply :defer t

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window and frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; not sure if these wierd binding is what I want
(use-package windmove
  :defer t
  :bind
  (("<f2> <right>" . windmove-right)
   ("<f2> <left>" . windmove-left)
   ("<f2> <up>" . windmove-up)
   ("<f2> <down>" . windmove-down)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; productivity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  ;; :disabled t
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
  :bind
  (("C-c s" . persp-switch))
  :config
  (progn
    (persp-mode)
    (setq projectile-switch-project-action 'projectile-dired)
    )
  )

(use-package persp-projectile
  :bind
  (
   ("s-s" . projectile-persp-switch-project))
  )


(use-package fill-column-indicator
  ;; 80 characters
  :defer t
  :init
  (add-hook 'prog-mode-hook 'fci-mode)
  (add-hook 'LaTeX-mode-hook 'fci-mode)
  )

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :bind
  (
   ("s-/" . company-complete))
  )

(use-package yasnippet
  :init
  (progn
    (defvar yas-snippet-dirs)
    ;; (add-to-list 'yas-snippet-dirs
    ;;       '("~/.emacs.d/snippets"                 ;; personal snippets
    ;;         ))
    (yas-global-mode 1)))

;; (setq yas-snippet-dirs (append yas-snippet-dirs
;; 			       '("~/Downloads/interesting-snippets")))

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
(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  )

(use-package flyspell
  :init
  :config
  (progn
    ;; (add-hook 'LaTeX-mode-hook '(flyspell-mode t))
    ;; TODO not working for flyspell-mode
    (add-hook 'LaTeX-mode-hook #'turn-on-flyspell)
    )
  )

(use-package expand-region
  :bind
  (
   ("s-e" . er/expand-region))
  )

(use-package helm
  ;; :disabled t
  :bind
  (
   ("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files))
  )


(use-package browse-kill-ring
  :defer t
  :config
  (browse-kill-ring-default-keybindings))

(use-package google-c-style
  ;; c style used by google
  :defer t)

(use-package goto-chg
  ;; goto last change in this buffer
  :bind (("C-." . goto-last-change)
         ("C-," . goto-last-change-reverse)))

(use-package guide-key
  ;; one key to rule them all
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


(use-package regex-tool
  :defer t)

(use-package magit
  :defer t
  :bind
  (
   ("C-x g" . magit-status))
  )

(use-package ag
  :defer t
  )


(use-package popwin
  ;; use a separate window for buffers like *completion*,
  ;; close them use C-g
  :defer t
  :config
  (popwin-mode 1)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :defer t
  :bind
  (("C-c n" . org-capture)
   ;; ("C-c o" . org-open-at-point)
   ("C-c o" . org-open-at-point-global)
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

(use-package markdown-mode
  :init
  (add-hook 'markdown-mode-hook 'turn-on-orgtbl)
  )

(use-package fic-mode
  :init
  (progn
    (setq fic-highlighted-words
          '("FIXME" "TODO" "BUG"
            "KLUDGE" "HEBI" "AGREE" "DENY"
            "REFER" "DEBUG" "NOW")
          )
    )
  :config
  (progn
    (add-hook 'prog-mode-hook 'fic-mode)
    (add-hook 'latex-mode-hook 'fic-mode)
    (add-hook 'markdown-mode-hook 'fic-mode)
    )
  )

(use-package polymode
  ;; it works, but not so stable ..
  :disabled t
  :config
  (require 'poly-markdown)
  (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)))



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


(use-package exec-path-from-shell
  ;; when start emacs from desktop env instead of shell, the PATH is aweful.
  :if window-system
  :config
  (progn
    (exec-path-from-shell-initialize)
    (message "%s: %s" "exec-path-from-shell post config" (getenv "PATH"))))

(use-package rainbow-delimiters
  ;; different colors for different level of parens
  :defer t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package zenburn-theme
  :init
  (load-theme 'zenburn t)
  )
(use-package ample-theme
  :init
  (load-theme 'ample t)
  (load-theme 'ample-flat t)
  (load-theme 'ample-light t)
  )
(use-package dracula-theme
  :init
  (load-theme 'dracula t)
  )

(use-package monokai-theme
  :init
  (load-theme 'monokai t)
  )

(use-package cyberpunk-theme
  :init
  (load-theme 'cyberpunk t)
  )

(enable-theme 'monokai)

;;; packages.el ends here
