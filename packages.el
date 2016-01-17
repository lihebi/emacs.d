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
  :disabled t
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

(use-package ace-jump-mode
  ;; jump to a char, can select by 'abcd..'
  ;; :disabled t
  :bind
  (
   ("C-c SPC" . ace-jump-mode)
   ("C-x SPC" . ace-jump-mode-pop-mark)
   )
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

(use-package dash-at-point
  ;; dash documentation browser
  ;; this just take the string at point, and open it in Dash.app.
  ;; No use at all
  :disabled t
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

(use-package w3m
  ;; emacs-w3m interface
  ;; text based browser is not seemed to work very good.
  ;; in particular, this one is not able to even "click link", without kill the "current process"
  :disabled t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ess
  ;; R
  ;; but cannot be defered, or the command is not found.
  ;; to use: M-x R
  ;; R-mode
  )

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
  ;; highlight
  (setq org-src-fontify-natively t)
  ;; my srcml converter
  (require 'ob-srcml)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk . t)
     (emacs-lisp . t)
     (python . t)
     (ruby . t)
     ;; (shell . t)
     ;; other babel languages
     (plantuml . t)
     ;; this should be capital C, the same as in #+begin_src C
     (C . t)
     (srcml . t)
     )
   )
  ;; to use plantuml in org-mode:
  
  ;; #+begin_src plantuml :file tryout.png
  ;;   Alice -> Bob: synchronous call
  ;;   Alice ->> Bob: asynchronous call
  ;; #+end_src

  ;; #+results:
  ;; [[file:tryout.png]]

  ;; org mode have a babel support for plantuml..., built-in!
  ;; just go to the code, than press C-c C-c to evaluate it. The #+results section is gnerated by org-mode.
  ;; not sure if I can use plantuml command itself instead of setting the following jar path.

  ;; To load the image in to emacs:
  ;; org-toggle-inline-images
  ;; C-c C-x C-v
  (setq org-plantuml-jar-path
        (expand-file-name "~/bin/plantuml.jar"))
  ;; latex templates
  (require 'ox-latex)
  ;; (setq org-export-latex-listings t)
  (add-to-list 'org-latex-classes
               '("fse"
                 "\\documentclass{sig-alternate-05-2015}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; org to latex
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; from 8.3, it TOC becomes a top-level option
  ;; #+TOC: headlines 2
  ;; #+TOC: tables
  ;; #+TOC: listings
  ;; to have correct bibtex citation, we need to remove the toc completely,
  ;; because it will insert a strange \tableofcontent, which is not recognized by latex
  ;; #+OPTIONS: toc:nil

  ;; the org mode comes with emacs 24.5 is 8.2, but 8.3 introduce some new features.
  ;; also, some community contrib package is not shipped with emacs, which need to be installed by org-plus-contrib
  ;; to make it compitible, I also want to replace the default org with the one in melpa.
  ;; there're two source of org I can install from: gnu and org.
  ;; the org version, for some reason, will not compile for #:TITLE or #:AUTHOR.
  ;; but the gnu version works just fine
  ;; there's another method to install the most recent version of org: homebrew.
  ;; I cannot manage the version of it from emacs, so this is just a backup plan, but it works too.

  ;; this is needed to have #+BIBLIOGRAPHY: buffer-overflow plain works.
  ;; insert it in the end of the page can put the reference at the end.
  ;; it will do something else like change [[cite:xxx]] into \cite{xxx}

  (require 'ox-bibtex)
  ;; org-latex-pdf-process should also be customized, or it can not parse bibtex correctly
  ;; the following works, but it's slow
  ;; (setq org-latex-pdf-process
  ;;       (quote ("texi2dvi --pdf --clean --verbose --batch %f"
  ;;               "bibtex %b"
  ;;               "texi2dvi --pdf --clean --verbose --batch %f"
  ;;               "texi2dvi --pdf --clean --verbose --batch %f")))
  ;; (setq org-latex-pdf-process
  ;;       (list "latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f"))
  ;; (setq org-latex-pdf-process (list "latexmk -pdf %f"))
  ;; (setq org-latex-to-pdf-process 
  ;;       '("pdflatex %f" "bibtex %b" "pdflatex %f" "pdflatex %f"))
  ;; (setq org-latex-pdf-process (quote ("texi2dvi -p -b -V %f")))
  ;; I add this one on my own based on my experience, and it seems to work well
  (setq org-latex-pdf-process (list "latexmk -cd -quiet -pdf -shell-escape %f"))
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
            "REFER" "DEBUG" "NOW" "CAUTION")
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

(use-package go-mode
  :defer t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dired+
  :disabled t
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; packages.el ends here
