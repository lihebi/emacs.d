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
(use-package dash)

;; use
;; vr/query-replace
;; vr/replace
(use-package visual-regexp)

;; May cause problem in a clean install, manual installation of pdf-tools may be necessary
(use-package pdf-tools
  :config
  (pdf-tools-install))

(use-package csv-mode)

;; insert-pair-alist
(use-package wrap-region
  :config
  (wrap-region-add-wrapper "=" "=")
  (add-hook 'org-mode-hook 'wrap-region-mode))

(use-package paredit)

(use-package edbi)
(use-package edbi-sqlite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load (emacs-d "org-conf"))

(use-package auctex
  ;; no document installed? ;; seems to have error when compiling
  :disabled t
  )

(use-package slime
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy))
  )

(use-package z3-mode
  :config
  ;; add-hook will keep adding ... consider reset it when debugging
  ;; (setq z3-mode-hook nil)
  (add-hook 'z3-mode-hook #'(lambda()
                              (prin1 "hello")
                              ;; turn off slime mode
                              (slime-mode 0)
                              (prin1 "done"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package linum-off
  :config
  (setq linum-disabled-modes-list (append linum-disabled-modes-list '(doc-view-mode))))
 ;; '(linum-disabled-modes-list
 ;;   (quote
 ;;    (eshell-mode wl-summary-mode compilation-mode org-mode text-mode dired-mode pdf-view-mode doc-view-mode)))


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


;; Now I would love to summary the C++ IDE commonly used commands and features
;; From my helm M-x history
;; helm-projectile
;; helm-register
;; helm-all-mark-rings
;; helm-man-woman
;; helm-show-kill-ring

;; helm-semantic-or-imenu
;; srefactor-refactor-at-point


(use-package neotree
  ;; neotree is not using because it conflicts with perspective
  ;; also, we have speedbar ^_^
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
   ("C-c h s" . projectile-persp-switch-project))
  )


(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :bind
  (
   ("C-;" . company-complete)
   )
  :config
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  )

;; These two packages are used in fuzzy complete
(use-package fuzzy
  )
(use-package flx
  )
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

    (setq yas-prompt-functions '(yas-popup-isearch-prompt yas-ido-prompt yas-no-prompt))
    )

    )

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

(use-package flyspell
  :init
  :config
  (progn
    ;; (add-hook 'LaTeX-mode-hook '(flyspell-mode t))
    ;; TODO not working for flyspell-mode
    (add-hook 'LaTeX-mode-hook #'turn-on-flyspell)
    (add-hook 'latex-mode-hook #'turn-on-flyspell)
    (add-hook 'org-mode-hook #'turn-on-flyspell)
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


(use-package projectile
  :init
  (progn
    (projectile-global-mode)
    (setq projectile-enable-caching t) ; enable catch
    )
  :config
  (setq projectile-switch-project-action 'projectile-dired)
  )

(use-package helm-projectile
  ;; :disabled t
  )

(use-package ess
  ;; R
  ;; but cannot be defered, or the command is not found.
  ;; to use: M-x R
  ;; R-mode
  ;; :disabled t
  )


(use-package helm-gtags
  :bind
  ("M-." . helm-gtags-dwim)
  ("M-," . helm-gtags-pop-stack)
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
  )

(use-package ace-jump-mode
  ;; jump to a char, can select by 'abcd..'
  :bind
  (
   ("C-c h SPC" . ace-jump-mode)
   ("C-x SPC" . ace-jump-mode-pop-mark)
   )
  )



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

(use-package helm-dash
  ;; look up dash too
  :config
  ;; buffer local docsets
  (defun c++-doc ()
    (interactive)
    (setq-local helm-dash-docsets '("C" "C++"))
    )
  (add-hook 'c++-mode-hook 'c++-doc)
  (defun R-doc ()
    (interactive)
    (setq-local helm-dash-docsets '("R"))
    )
  (add-hook 'R-mode-hook 'R-doc)
  ;; (setq helm-dash-browser-func 'eww)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package bison-mode)

(use-package cmake-mode
  :defer t
  )

(use-package tuareg
  ;; The ocaml mode
  )


(use-package markdown-mode
  :init
  (add-hook 'markdown-mode-hook 'turn-on-orgtbl)
  )


;; (defun hebi-highlight ()
;;   "."
;;   (font-lock-add-keywords nil
;;                           '(("\\<\\(HEBI\\)\\>" . fic-face))
;;                           'append)
;;   ;; (font-lock-remove-keywords nil kwlist))
;;   (font-lock-fontify-buffer)
;;   )



(use-package fic-mode
  ;; this is buggy even if I disabled it...
  ;; see here file:///Users/hebi/github/wiki-new/js.html
  ;; basically when it is enabled, the exporting from org to html with htmlize to fontify the code will add three strange characters in the end of each line.
  ;; it turns out that the fic-mode is the culprit
  ;; to fix this, in ox-html.el file, add
  ;; (when (require 'fill-column-indicator nil 'noerror)
  ;;   (fci-mode -1))
  ;; after line (funcall lang-mode) in defun org-html-fontify-code
  ;; that should be around line 2048 ..
  ;; remove the ox-html.elc to make it in effect!
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  ;; :if window-system
  :config
  (progn
    (exec-path-from-shell-copy-env "INFOPATH") ;; load $INFOPATH
    (exec-path-from-shell-initialize) ;; by default only load $PATH $MANPATH
    (message "%s: %s" "exec-path-from-shell post config" (getenv "PATH"))))

(use-package rainbow-delimiters
  ;; different colors for different level of parens
  :defer t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'org-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package dracula-theme
;;   :init
;;   (load-theme 'dracula t t)
;;   )

;; (use-package monokai-theme
;;   ;; monokai
;;   :init
;;   (load-theme 'monokai t t))



;; This is theme is not clear! So bad.
;; (use-package zenburn-theme
;;   :init
;;   (load-theme 'zenburn t t))

;; Not so clear either
;; (use-package ample-theme
;;   :init
;;   (load-theme 'ample t t)
;;   (load-theme 'ample-flat t t)
;;   (load-theme 'ample-light t t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package string-inflection
  ;; cycle through CamelCase and under_line
  :bind
  ("C-c m" . string-inflection-cycle)
  )

(use-package ecb
  :defer t
  )

(use-package etags-select
  :defer t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package simple-httpd)
(use-package js2-mode)

(use-package skewer-mode
  ;; interactive web development
  ;; depend on simple-httpd and js2-mode
  ;; use run-skewer to attach browser to emacs
  :config
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode)
  )

(use-package scss-mode
  :defer t
  )

(use-package edit-server
  ;; Use this because I want to use "edit with emacs" chrome extension.
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For piano overtone
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package clojure-mode
  :defer t
  )
(use-package cider
  :defer t
  )
;; for highlight flashing when evaluating
(use-package eval-sexp-fu
  :defer t
  )
(use-package cider-eval-sexp-fu
  :defer t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; disabled
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package srefactor
  ;; it is disabeld because semantic parses all library header files, which is super slow.
  ;; I will try this unless I find a way to not parse so many files
  :disabled t
  ;; semantic refactor
  ;; https://github.com/tuhdo/semantic-refactor
  :config
  (semantic-mode 1) ;; -> this is optional for Lisp
  (define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
  (define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
  )


;; check out swank-js when I want to develop web

(use-package elfeed
  :disabled t
  :config
  (setq elfeed-feeds
        '("http://nullprogram.com/feed/"
          "http://www.terminally-incoherent.com/blog/feed/"
          ("https://news.ycombinator.com/rss" hackernews)))
  (progn
    (defface important-elfeed-entry
      '((t :foreground "#f77"))
      "Marks an important Elfeed entry.")
    (push '(important important-elfeed-entry)
          elfeed-search-face-alist)
    (push '(hackernews hebi-red-face)
          elfeed-search-face-alist)
    (push '(unread elfeed-search-unread-title-face)
          elfeed-search-face-alist)
    )
  ;; (use-package elfeed-goodies
  ;;   :config
  ;;   (elfeed-goodies/setup)
  ;;   )
  )


;; this seems to be a collection
;; M-x color-theme-sanityinc-tomorrow-day
;; M-x color-theme-sanityinc-tomorrow-night
;; M-x color-theme-sanityinc-tomorrow-blue
;; M-x color-theme-sanityinc-tomorrow-bright
;; M-x color-theme-sanityinc-tomorrow-eighties
;; in newer emacs
;; M-x customize-themes
(use-package color-theme-sanityinc-tomorrow
  :disabled t
  )

(use-package solarized-theme
  ;; disabled because it will give a very bright foreground selection in magit buffer.
  :disabled t
  :init
  (load-theme 'solarized-dark t)
  (load-theme 'solarized-light t)
  )

(use-package ample-theme
  :disabled t
  :init
  (load-theme 'ample t)
  (load-theme 'ample-flat t)
  (load-theme 'ample-light t)
  )
(use-package dired+
  :disabled t
  )
(use-package polymode
  ;; it works, but not so stable ..
  :disabled t
  :config
  (require 'poly-markdown)
  (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
  )

(use-package w3m
  ;; emacs-w3m interface
  ;; text based browser is not seemed to work very good.
  ;; in particular, this one is not able to even "click link", without kill the "current process"
  :disabled t
  )
(use-package function-args
  :disabled t
  ;; C++ completion
  ;; fa-show
  ;; fa-jump
  ;; moo-complete
  ;; moo-propose-virtual
  ;; moo-propose-override
  ;; moo-jump-local
  ;; force refresh: semantic-force-refresh
  :defer t
  )

;; I'm disabling this,
;; because it has a bug causing OrgMode html exporter
;; to have garbage string for code
;; Also, I'm a little bit senior now and don't really need the indicator
(use-package fill-column-indicator
  ;; 80 characters
  :disabled t
  :defer t
  :init
  (add-hook 'prog-mode-hook 'fci-mode)
  (add-hook 'LaTeX-mode-hook 'fci-mode)
  )

;; Disabled because I have no idea how to make its fuzzy matching working contineously.
;; i.e. when I type something, I need to use the comand 'ac-fuzzy-complete again to show the reult
(use-package auto-complete
  :disabled t
  :config
  (ac-config-default)
  (setq ac-use-fuzzy t)
  ;; (define-key ac-complete-mode-map (kbd "C-;") 'auto-complete)
  (define-key ac-complete-mode-map (kbd "C-;") 'ac-fuzzy-complete)
  (define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
  (ac-set-trigger-key "TAB")
  (define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)
  )

(use-package key-chord
  ;; a "key chord" is two keys pressed simultaneously,
  ;; or a single key quickly pressed twice.
  :disabled t
  )

(use-package iy-go-to-char
  ;; similar to vim's f and t
  ;; don't really need it for now, because I can use just isearch
  :disabled t
  )

;; Also, sgml-mode provide many utilities to edit html, such as closing tags
(use-package emmet-mode
  ;; generate html structures by '#myid>ul#ulid>li.clsli*4'
  :disabled t
  )

(use-package restclient
  ;; make http requests!
  :disabled t
  )
(use-package dash-at-point
  ;; dash documentation browser
  ;; this just take the string at point, and open it in Dash.app.
  ;; No use at all
  :disabled t
  )



;;; packages.el ends here
