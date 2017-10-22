;;; package.el --- External packages

;;; Commentary:
;; No Comments!

(require 'package)

;;; Code:
(setq package-archives
      (append package-archives
              '(("org" . "http://orgmode.org/elpa/"))
              '(("melpa" . "http://melpa.milkbox.net/packages/"))))
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

(use-package benchmark-init
  ;; enable only when i want to debug the init time
  ;; :disabled t
  :config
  (benchmark-init/activate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modes
(use-package csv-mode
  :mode)
(use-package json-mode
  :mode)


(use-package z3-mode
  :defer t
  :config
  ;; add-hook will keep adding ... consider reset it when debugging
  ;; (setq z3-mode-hook nil)
  (setq z3-solver-cmd "/usr/bin/z3")
  ;; the actual command running:
  ;; z3 -v:1 -smt2 xxx.smt
  (add-hook 'z3-mode-hook #'(lambda()
                              (prin1 "hello")
                              ;; turn off slime mode
                              (slime-mode 0)
                              (prin1 "done"))))


(use-package bison-mode
  :defer t
  )
(use-package cmake-mode
  :defer t
  )
(use-package dockerfile-mode
  :defer t)

(use-package fic-mode
  :init
  (progn
    (setq fic-highlighted-words
          '("FIXME" "TODO" "BUG"
            "KLUDGE" "AGREE" "DENY"
            "REFER" "DEBUG" "NOW" "CAUTION"
            "DEPRECATED"
            "BUGSIG" "INVARIANT" "PRECONDITION" "TRANSFER"
            "STEP"
            "IMPORTANT" "HERE"
            "UPDATE"
            "CONFIRM"
            "TRICK" "HACK"
            )
          )
    )
  :config
  (progn
    (add-hook 'prog-mode-hook 'fic-mode)
    (add-hook 'latex-mode-hook 'fic-mode)
    (add-hook 'markdown-mode-hook 'fic-mode)
    )
  )

(use-package go-mode
  :defer t
  )
(use-package gradle-mode
  :mode)
(use-package groovy-mode
  :mode)
(use-package yaml-mode
  :mode)

(use-package rust-mode
  :mode)
;; (use-package racer)

(use-package ess
  ;; R
  ;; but cannot be defered, or the command is not found.
  ;; to use: M-x R
  ;; R-mode
  ;; this is disabled because very slow on startup, but it is very useful
  :disabled t
  :config
  (setq comint-prompt-read-only t)
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-to-bottom-on-output t)
  (setq comint-move-point-for-output t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lisp
(use-package slime
  :defer t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; libs
(use-package dash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operations
(use-package shell-switcher
  :config
  (setq shell-switcher-mode t))

(use-package string-inflection
  ;; cycle through CamelCase and under_line
  :bind
  ("C-c m" . string-inflection-cycle)
  )

(use-package visual-regexp
  ;; use
  ;; vr/query-replace
  ;; vr/replace
  :config
  ;; commenting out these keybindings
  ;; need to remember to use vr/xxx when doing replacing
  ;; (define-key global-map (kbd "C-c r") 'vr/replace)
  ;; (define-key global-map (kbd "C-c q") 'vr/query-replace)
  ;; ;; if you use multiple-cursors, this is for you:
  ;; (define-key global-map (kbd "C-c m") 'vr/mc-mark)
  )
;; insert-pair-alist
(use-package wrap-region
  :config
  (wrap-region-add-wrapper "=" "=")
  (wrap-region-add-wrapper "/" "/")
  (wrap-region-add-wrapper "*" "*")
  (wrap-region-add-wrapper "$" "$")
  (wrap-region-add-wrapper "~" "~")
  (add-hook 'org-mode-hook 'wrap-region-mode))

(use-package paredit
  ;; I'm using paredit here
  ;; smartparens is a package aims to replace paredit
  ;; it did add some functionality, but I don't like
  ;; 1. the presentation (document)
  ;; 2. lack of wrap
  ;; 3. paredit should work well,
  ;;    the only downside might be it cannot be used outside lisp mode
  :config
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)  
  (add-hook 'racket-mode-hook           #'enable-paredit-mode)  
  )



(use-package volatile-highlights
  ;; show the change after undo, yank, etc.
  :init
  (volatile-highlights-mode t))

;; a must-have package, show the position you are in this buffer
(use-package nyan-mode
  :config
  (nyan-mode))
(use-package linum-off
  ;; this is used to speed up pdf reading
  :config
  (setq linum-disabled-modes-list (append linum-disabled-modes-list '(doc-view-mode))))
;; not sure if these wierd binding is what I want
(use-package expand-region
  :bind
  (
   ("s-e" . er/expand-region))
  )
(use-package windmove
  :defer t
  :bind
  (("<f2> <right>" . windmove-right)
   ("<f2> <left>" . windmove-left)
   ("<f2> <up>" . windmove-up)
   ("<f2> <down>" . windmove-down)
   ))
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
;; These two packages are used in fuzzy complete
(use-package fuzzy)
(use-package flx)
(use-package browse-kill-ring
  :defer t
  :config
  (browse-kill-ring-default-keybindings))

(use-package dired-k
  ;; k (https://github.com/rimraf/k) is a ls alternative to show git status
  ;; dired-k is run in teh hook of dired, or as revert-buffer, so that when dired, it will load dired-k to show some fancy staff
  :config
  ;; You can use dired-k alternative to revert-buffer
  (define-key dired-mode-map (kbd "g") 'dired-k)
  ;; always execute dired-k when dired buffer is opened
  (add-hook 'dired-initial-position-hook 'dired-k)
  ;; (add-hook 'dired-after-readin-hook #'dired-k-no-revert)
  )

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
  :bind (
         ("C-x g" . magit-status)))

(use-package ag
  :defer t
  )


(use-package popwin
  ;; use a separate window for buffers like *completion*,
  ;; close them use C-g
  :defer t
  :config
  (popwin-mode 1)
  (push '("*Hebi Output*" :noselect t :tail t) popwin:special-display-config))

(use-package ace-jump-mode
  ;; jump to a char, can select by 'abcd..'
  :bind
  (
   ("C-c h SPC" . ace-jump-mode)
   ("C-x SPC" . ace-jump-mode-pop-mark)
   )
  )
(use-package ace-window
  :bind
  (
   ("C-c h w" . ace-window))
  )

(use-package helpful
  ;; try this with helpful-function then -map
  )
(use-package exec-path-from-shell
  ;; when start emacs from desktop env instead of shell, the PATH is aweful.
  ;; :if window-system
  :config
  (progn
    (exec-path-from-shell-copy-env "INFOPATH") ;; load $INFOPATH
    (exec-path-from-shell-initialize) ;; by default only load $PATH $MANPATH
    (message "%s: %s" "exec-path-from-shell post config" (getenv "PATH"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; projectile
(use-package projectile
  :init
  (progn
    (projectile-global-mode)
    (setq projectile-enable-caching t) ; enable catch
    )
  :config
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-track-known-projects-automatically nil)
  ;; when setting this to another file, emacs didn't load it
  ;; (setq projectile-known-projects-file "/home/hebi/.emacs.d/projectile-bookmarks.eld")
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
   ("C-c h s" . projectile-persp-switch-project))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python
(use-package elpy
  ;; C-c C-z (elpy-shell-switch-to-shell)
  ;; C-c C-c (elpy-shell-send-region-or-buffer)
  ;; C-c RET (elpy-shell-send-current-statement)
  ;; C-M-x (python-shell-send-defun)
  ;; C-c C-k (elpy-shell-kill)
  ;; C-c C-K (elpy-shell-kill-all)
  ;; very slow on startup
  :disabled
  :init
  (defvar python-shell-interpreter)
  (defvar python-shell-interpreter-args)
  (defvar python-shell-prompt-detect-failure-warning)
  (defvar python-shell-completion-native-enable)
  (setq python-shell-interpreter "ipython3"
        ;; must use simple prompt to avoid encoding problem for ipython
        python-shell-interpreter-args
        ;; "-i"
        "--simple-prompt -i"
        )
  (setq python-shell-prompt-detect-failure-warning nil)
  ;; this fix the python3 warnning problem
  (setq python-shell-completion-native-enable nil)
  (defun hebi-elpy-use-python2 ()
    "Change python shell to python2.
You need to kill the current *Python* buffer to take effect."
    (interactive)
    (setq python-shell-interpreter "ipython2"
          python-shell-interpreter-args "--simple-prompt -i"))

  (defun hebi-elpy-use-python3 ()
    (interactive)
    (setq python-shell-interpreter "ipython3"
          python-shell-interpreter-args "--simple-prompt -i"))
  :config
  ;; (setq python-shell-interpreter "python3")
  (elpy-enable))

(use-package pylint
  ;; have to use this for org mode to find "pylint" when exporting html
  :disabled t
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm
(use-package helm
  ;; Now I would love to summary the C++ IDE commonly used commands and features
  ;; From my helm M-x history
  ;; helm-projectile
  ;; helm-register
  ;; helm-all-mark-rings
  ;; helm-man-woman
  ;; helm-show-kill-ring

  ;; helm-semantic-or-imenu
  ;; srefactor-refactor-at-point
  :bind
  (
   ("M-x" . helm-M-x)
   ;; C-j enter directory
   ;; C-l up directory
   ;; C-u C-x C-f open history
   ("C-x C-f" . helm-find-files)
   ("M-y" . helm-show-kill-ring)
   ("C-x b" . helm-mini)
   ("C-h SPC" . helm-all-mark-rings)
   )
  :config
  ;; helm-semantic-or-imenu (C-x c i)
  ;; it shows the outline!
  ;; the actual worker is semantic, so be sure to enable it
  (setq helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet
(use-package yasnippet
  :init
  (progn
    (defvar yas-snippet-dirs)
    ;; (add-to-list 'yas-snippet-dirs
    ;;       '("~/.emacs.d/snippets"                 ;; personal snippets
    ;;         ))
    (yas-global-mode 1))
  :config
  ;;; use popup menu for yas-choose-value
  ;; it seems to be installed by default. But it is not marked built-in
  (use-package popup
    :config
    ;; (require 'popup)
    ;; add some shotcuts in popup menu mode
    (define-key popup-menu-keymap (kbd "M-n") 'popup-next)
    (global-set-key (kbd "M-n") 'popup-next)
    (define-key popup-menu-keymap (kbd "TAB") 'popup-next)
    (define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
    (define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
    (define-key popup-menu-keymap (kbd "M-p") 'popup-previous)

    (defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
      (when (featurep 'popup)
        (popup-menu*
         (mapcar
          (lambda (choice)
            (popup-make-item
             (or (and display-fn (funcall display-fn choice))
                 choice)
             :value choice))
          choices)
         :prompt prompt
         ;; start isearch mode immediately
         :isearch t
         )))
    (setq yas-prompt-functions '(yas-popup-isearch-prompt yas-ido-prompt yas-no-prompt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++
(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :bind
  (
   ("C-c c" . flycheck-buffer)
   )
  :config
  ;; (setq flycheck-clang-args '"--std=c++11")
  ;; (setq flycheck-clang-args nil)
  ;; --std=c++11 is not working with C code.
  ;; Instead, include this in .dir-locals.el
  ;; ((c++-mode . ((flycheck-clang-args . ("--std=c++11")))))
  (add-hook 'c++-mode-hook '(lambda()
                              (setq flycheck-clang-args "--std=c++11")
                              ))
  (add-hook 'c++-mode-hook #'(lambda () (setq flycheck-gcc-language-standard "c++11")))
  (add-hook 'c-mode-hook '(lambda()
                            (setq flycheck-clang-args "")
                            ))
  ;; Add include path
  ;; I found this information by:
  ;; M-x describe-checker => found c/c++-clang
  ;; Click on it, goes to the description, along with the configurable part.
  (setq flycheck-clang-include-path (list ".."))
  )

;; rtags frontend
(use-package rtags
  ;; if I don't disable it, the org mode export of java will stop the process ..
  ;; :disabled t
  ;; start rdm by rtags-start-process-unless-running
  ;; needs to build and install rtags first
  ;; TODO how to get this in my setup debian script?
  ;; how to index the current project and how to switch project?
  ;; Useful Commands
  ;; (rtags-find-symbol-at-point)
  ;; (rtags-find-references-at-point)
  ;; (rtags-find-symbol)
  ;; (rtags-find-references)
  ;; (rtags-location-stack-back)
  :config
  (add-hook 'c-mode-common-hook 'rtags-start-process-unless-running)
  (add-hook 'c++-mode-common-hook 'rtags-start-process-unless-running)
  ;; this open the keybinding with prefix C-c r
  (rtags-enable-standard-keybindings)
  (defvar antlr-mode-map)
  ;; (rtags-enable-standard-keybindings antlr-mode-map)
  (define-key c-mode-base-map (kbd "C-c r n") 'rtags-next-match)

  ;; company
  ;; not tested
  (setq rtags-autostart-diagnostics t)
  ;; (rtags-diagnostics)

  ;; FIXME this code is outside company block, but it starts company mode
  ;; (setq rtags-completions-enabled t)
  ;; (push 'company-rtags company-backends)
  ;; (global-company-mode)

  ;; flycheck
  (use-package flycheck-rtags)

  (defun my-flycheck-rtags-setup ()
    (flycheck-select-checker 'rtags)
    (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
    (setq-local flycheck-check-syntax-automatically nil))
  ;; c-mode-common-hook is also called by c++-mode
  (add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)

  
  ;; :bind
  ;; (("C-M-." . rtags-find-symbol-at-point)
  ;;  ("C-M-," . rtags-location-stack-back))
  )

(use-package google-c-style
  ;; c style used by google
  :defer t)
(use-package clang-format)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
(use-package smart-mode-line
  :init
  ;; this is actually required by smart-mode-line
  ;; don't need to explicitly load use-package it, just as a reference
  ;; actually the rm-blacklist is offered by it
  ;; it also offers the rm-text-properties variable
  ;; (use-package rich-minority)
  :config
  (setq sml/no-confirm-load-theme t) ; do not warn me for loading a theme
  (setq sml/theme 'light)
  (sml/setup)
  (setq sml/name-width 15)
  ;; highlight God-mode in minor mode
  (add-to-list 'rm-text-properties
               '("\\` God\\'" 'face 'font-lock-warning-face))
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
                           "\\|"))))
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

(use-package alert)

(use-package emms
  :config
  (emms-all)
  (emms-default-players)
  (setq emms-source-file-default-directory "~/music")
  (setq emms-tag-editor-rename-format "%a - %t")
  ;; this is very buggy, emms-mark-mode will throw error "no first track",
  ;; and never start emms at all
  ;; (setq emms-playlist-default-major-mode 'emms-mark-mode)
  ;; lyrics must be in lrc format
  ;; lyrics can be placed in the same directory as music file
  ;; or this folder: emms-lyrics-dir
  ;; but since it can only be displayed on minibuffer or mode line, I don't want it right now
  ;; (setq emms-lyrics-dir "~/music/lyrics")
  ;; (emms-lyrics 1)
  
  (emms-mode-line 1)
  (emms-playing-time 1)

  (require 'emms-player-mpd)
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6600")
  (add-to-list 'emms-info-functions 'emms-info-mpd)
  (add-to-list 'emms-player-list 'emms-player-mpd)
  )

(use-package racket-mode
  :config
  (add-hook 'racket-mode-hook
            (lambda ()
              (define-key racket-mode-map (kbd "C-c r") 'racket-run)))
  ;; (setq tab-always-indent 'complete)
  )

(use-package geiser
  :disabled t
  :config
  (setq geiser-active-implementations '(racket))
  (setq geiser-mode-smart-tab-p t))

(use-package company
  ;; my auto completion is freezing emacs!
  ;; :disabled
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :bind
  (
   ("C-;" . company-complete)
   ;; ("TAB" . company-indent-or-complete-common)
   )
  :config
  (setq company-idle-delay nil) ; do not automatically give me completion

  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  ;; (setq tab-always-indent 'complete)
  ;; (define-key company-active-map (kbd "TAB") #'company-indent-or-complete-common)
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony))

  ;; tab trigger
  (define-key company-mode-map [remap indent-for-tab-command]
    'company-indent-for-tab-command)

  (setq tab-always-indent 'complete)

  (defvar completion-at-point-functions-saved nil)

  (defun company-indent-for-tab-command (&optional arg)
    (interactive "P")
    (let ((completion-at-point-functions-saved completion-at-point-functions)
          (completion-at-point-functions '(company-complete-common-wrapper)))
      (indent-for-tab-command arg)))

  (defun company-complete-common-wrapper ()
    (let ((completion-at-point-functions completion-at-point-functions-saved))
      (company-complete-common)))  

  )

;;; packages.el ends here
