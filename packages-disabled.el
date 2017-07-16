;;; packages-disabled.el --- External packages

;;; Commentary:
;; No Comments!

;;; Code:




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; disabled
(use-package irony
  :disabled
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-eldoc)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package irony-eldoc
  :disabled
  )

(use-package neotree
  ;; neotree is not using because it conflicts with perspective
  ;; also, we have speedbar ^_^
  ;; (define-key neotree-mode-map (kbd "i") #'neotree-enter-horizontal-split) ; TODO what's the #?
  ;; (define-key neotree-mode-map (kbd "I") #'neotree-enter-vertical-split)
  ;; when switch project, neotree change root automatically
  ;; (setq projectile-switch-project-action 'neotree-projectile-action)
  :disabled
  :bind
  (
   ("<f8>" . neotree-toggle))
  )

(use-package company
  ;; my auto completion is freezing emacs!
  :disabled
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :bind
  (
   ("C-;" . company-complete)
   )
  :config
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony)))
(use-package multiple-cursors
  :disabled
  :bind
  (
   ("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)
   ("C-S-<mouse-1>" . mc/add-cursor-on-click)
   )
  )
(use-package flyspell
  ;; disabling because Starting new Ispell process ... all the time
  ;; when exporting html
  :disabled
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
(use-package helm-dash
  :disabled
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

;; anoying auto runing of rc -J, too slow
;; and I don't need those features at all
(use-package cmake-ide
  :disabled
  :config
  (cmake-ide-setup))

(use-package sr-speedbar
  :disabled
  ;; in-frame speedbar
  ;; will not be closed by C-x 1
  ;; use sr-speedbar-toggle to toggle it
  :config
  ;; left side
  (setq sr-speedbar-right-side nil)
  ;; skip other window
  (setq sr-speedbar-skip-other-window-p t))

(use-package rainbow-delimiters
  ;; different colors for different level of parens
  :disabled
  :defer t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
    ;; (add-hook 'org-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
    ))


;; web packages
;; check out swank-js when I want to develop web
(use-package simple-httpd
  :disabled
  )

(use-package js2-mode
  ;; disable because slow on startup
  :disabled
  )

(use-package skewer-mode
  ;; interactive web development
  ;; depend on simple-httpd and js2-mode
  ;; use run-skewer to attach browser to emacs
  ;; disable because slow on startup for js2
  :disabled
  :config
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode)
  )

(use-package scss-mode
  :disabled
  )

(use-package edit-server
  ;; Use this because I want to use "edit with emacs" chrome extension.
  :disabled
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For piano overtone
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; disabling clojure related because slow on start up

(use-package clojure-mode
  :disabled
  :defer t
  )
(use-package cider
  :disabled
  ;; :defer t
  ;; :bind
  ;; (("C-c C-t" . cider-test-run-test))
  :config
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  )
;; for highlight flashing when evaluating
(use-package eval-sexp-fu
  :disabled
  :defer t
  )
(use-package cider-eval-sexp-fu
  :disabled
  :defer t
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; old disable
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
  ;; :defer t
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
;; attempting to get a animated scrolling (less than 1 line per step)
;; but failed to find one
(use-package smooth-scrolling
  :disabled t
  :config
  (smooth-scrolling-mode 1))
(use-package sublimity
  :disabled t
  :config
  (require 'sublimity-scroll)
  (setq sublimity-scroll-weight 10
        sublimity-scroll-drift-length 5)
  (setq sublimity-scroll-weight 4
        sublimity-scroll-drift-length 1)
  (sublimity-mode 1))
(use-package android-mode
  ;; some useful commends
  ;; 1. download android SDK and add the tools into PATH
  ;; 2. android sdk, and select version to download
  ;; 3. android avd, to create virtual device
  ;; 4. android-create-project
  ;; Inside emacs
  ;; - android-start-emulator
  ;; - android-ant: compile and install
  :disabled t
  )

(use-package bing-dict
  :disabled
  :bind
  (("C-c d" . bing-dict-brief)))

(use-package firefox-controller
  :disabled
  )
(use-package ecb
  :disabled
  )

(use-package etags-select
  :disabled
  )
(use-package tuareg
  ;; The ocaml mode
  ;; disabled because slow on startup
  :disabled
  )
(use-package markdown-mode
  ;; disabled because slow on start up
  ;; And I don't use markdown except reading readmes
  :disabled
  :init
  ;; (add-hook 'markdown-mode-hook 'turn-on-orgtbl)
  )

;; edbi pretty slow on start up
(use-package edbi
  :disabled
  )
(use-package edbi-sqlite
  :disabled
  )

(use-package helm-projectile
  ;; I didn't use this at all, and it is very slow on startup
  :disabled
  )
(use-package helm-gtags
  :disabled
  :bind
  ("M-." . helm-gtags-dwim)
  ("M-," . helm-gtags-pop-stack)
  )

(use-package god-mode
  :disabled
  :config
  (global-set-key (kbd "<escape>") 'god-local-mode)
  (global-set-key (kbd "M-'") 'god-local-mode)
  ;; (global-set-key (kbd "<escape>") 'god-mode-all)
  ;; cursor style indicator
  (defun my-update-cursor ()
    (setq cursor-type (if (or god-local-mode buffer-read-only)
                          'box
                        'bar)))

  (add-hook 'god-mode-enabled-hook 'my-update-cursor)
  (add-hook 'god-mode-disabled-hook 'my-update-cursor)


  ;; disabling this. When changing buffer, god mode is not active,
  ;; and the backup solution does not work
  ;; mode line indicator
  ;; (defun c/god-mode-update-cursor ()
  ;;   (defvar back-mode-line-background)
  ;;   (defvar back-mode-line-inactive-background)
  ;;   (let ((limited-colors-p (> 257 (length (defined-colors)))))
  ;;     (cond (god-local-mode
  ;;            (progn
  ;;              (setq back-mode-line-background (face-attribute 'mode-line :background))
  ;;              (setq back-mode-line-inactive-background (face-attribute 'mode-line-inactive :background))
  ;;              (set-face-background 'mode-line "dark green")
  ;;              (set-face-background 'mode-line-inactive "forest green")
  ;;              ))
  ;;           (t (progn
  ;;                (set-face-background 'mode-line
  ;;                                     back-mode-line-background)
  ;;                (set-face-background 'mode-line-inactive
  ;;                                     back-mode-line-inactive-background)
  ;;                )))))

  ;; (add-hook 'god-mode-enabled-hook 'c/god-mode-update-cursor)
  ;; (add-hook 'god-mode-disabled-hook 'c/god-mode-update-cursor)

  ;; keybindings
  ;; (define-key god-local-mode-map (kbd "z") 'repeat)
  (define-key god-local-mode-map (kbd "i") 'god-local-mode)
  (define-key god-local-mode-map (kbd ".") 'repeat)

  (global-set-key (kbd "C-x C-1") 'delete-other-windows)
  (global-set-key (kbd "C-x C-2") 'split-window-below)
  (global-set-key (kbd "C-x C-3") 'split-window-right)
  (global-set-key (kbd "C-x C-0") 'delete-window)
  )

;;; packages-disabled.el ends here
