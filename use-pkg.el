;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modes
(use-package csv-mode)
(use-package json-mode)


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
  :defer t)
(use-package cmake-mode
  :defer t)
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
            "DOING")))
  :config
  (progn
    (add-hook 'prog-mode-hook 'fic-mode)
    (add-hook 'latex-mode-hook 'fic-mode)
    (add-hook 'markdown-mode-hook 'fic-mode)))

(use-package go-mode
  :defer t)
(use-package gradle-mode)
(use-package groovy-mode)
(use-package yaml-mode)

(use-package rust-mode)
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
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy))
  (use-package slime-company)
  (slime-setup '(slime-fancy slime-company)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; libs
(use-package dash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operations
(use-package shell-switcher
  :config
  (setq shell-switcher-mode t))

(use-package string-inflection
  :disabled t
  ;; cycle through CamelCase and under_line
  :bind
  ("C-c m" . string-inflection-cycle))

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
  (add-hook 'clojure-mode-hook          #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
  (add-hook 'racket-mode-hook           #'enable-paredit-mode)

  (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
  (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly))



(use-package volatile-highlights
  ;; show the change after undo, yank, etc.
  :init
  (volatile-highlights-mode t))

;; a must-have package, show the position you are in this buffer
(use-package nyan-mode
  :config
  (nyan-mode))

;; this is used to speed up pdf reading
(use-package linum-off
  ;; My fork adds support for display-line-numbers-mode
  :straight (linum-off :type git :host github
                       :repo "lihebi/linum-off")
  :config
  (add-to-list 'linum-disabled-modes-list 'doc-view-mode)
  ;; it only checks major mode
  ;; (add-to-list 'linum-disabled-modes-list 'comint-mode)
  (add-to-list 'linum-disabled-modes-list 'julia-true-repl-mode)
  (delete 'org-mode linum-disabled-modes-list))

;; not sure if these wierd binding is what I want
(use-package expand-region
  :bind
  (("s-e" . er/expand-region)))
(use-package windmove
  :defer t
  :bind
  (("<f2> <right>" . windmove-right)
   ("<f2> <left>" . windmove-left)
   ("<f2> <up>" . windmove-up)
   ("<f2> <down>" . windmove-down)))
(use-package smex
  ;; use ido in M-x
  :defer t
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)
   ; my old M-x
   ("C-c C-c M-x" . execute-extended-command))
  :init
  (progn
    (smex-initialize)))
;; These two packages are used in fuzzy complete
(use-package fuzzy)
(use-package flx)
(use-package browse-kill-ring
  :defer t
  :config
  (browse-kill-ring-default-keybindings))

(use-package dired-k
  ;; k (https://github.com/rimraf/k) is a ls alternative to show git status
  ;;
  ;; dired-k is run in teh hook of dired, or as revert-buffer, so that
  ;; when dired, it will load dired-k to show some fancy staff
  ;; :disabled
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
   ("C-x SPC" . ace-jump-mode-pop-mark)))

(use-package ace-window
  ;; C-u M-o switch window
  :bind
  (
   ;; ("C-c h w" . ace-window)
   ("M-o" . ace-window)))

(use-package helpful
  ;; try this with helpful-function then -map
  )
(use-package exec-path-from-shell
  ;; when start emacs from desktop env instead of shell, the PATH is aweful.
  ;; :if window-system
  ;;
  ;; I'm disabling it, as it is mostly useful on Mac. On Linux, I can
  ;; now use gdm to load .bash_profile, all applications started from
  ;; there should automatically get those variables
  :disabled
  :config
  (progn
    (exec-path-from-shell-initialize) ;; by default only load $PATH $MANPATH
    ;; (exec-path-from-shell-copy-env "INFOPATH") ;; load $INFOPATH
    (exec-path-from-shell-copy-env "LD_LIBRARY_PATH")
    (exec-path-from-shell-copy-env "LIBRARY_PATH")
    (exec-path-from-shell-copy-env "CPATH")
    (exec-path-from-shell-copy-env "CLASSPATH")
    (exec-path-from-shell-copy-env "ACLOCAL_PATH")
    (exec-path-from-shell-copy-env "PKG_CONFIG_PATH")
    (exec-path-from-shell-copy-env "CMAKE_PREFIX_PATH")
    (exec-path-from-shell-copy-env "PYTHONPATH")
    (exec-path-from-shell-copy-env "C_INCLUDE_PATH")
    (exec-path-from-shell-copy-env "CPLUS_INCLUDE_PATH")
    (exec-path-from-shell-copy-env "GUIX_LOCPATH")
    (exec-path-from-shell-copy-env "GUIX_PACKAGE_PATH")
    (message "%s: %s" "exec-path-from-shell post config" (getenv "PATH"))))

(when (string= system-type "darwin")
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (setq exec-path (append exec-path '("/usr/local/bin"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; projectile
(use-package projectile
  :init
  (progn
    (projectile-global-mode)
    ; enable catch
    (setq projectile-enable-caching t))
  :bind
  (("C-c p c" . projectile-compile-project))
  :config
  (setq projectile-switch-project-action 'projectile-dired)
  ;; (setq projectile-track-known-projects-automatically nil)
  ;; when setting this to another file, emacs didn't load it
  ;; (setq projectile-known-projects-file "/home/hebi/.emacs.d/projectile-bookmarks.eld")
  ;; use this to load known projects
  (defun hebi-reload-projectile-known-projects ()
    (interactive)
    (projectile-load-known-projects)))

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
;; currently 7.2.0 ipython is not working, see:
;; https://github.com/jorgenschaefer/elpy/issues/1517 and
;; https://github.com/ipython/ipython/issues/11541
;; So downgrade to 7.1.1: pip3 install ipython==7.1.1

(use-package elpy
  ;; C-c C-z (elpy-shell-switch-to-shell)
  ;; C-c C-c (elpy-shell-send-region-or-buffer)
  ;; C-c RET (elpy-shell-send-current-statement)
  ;; C-M-x (python-shell-send-defun)
  ;; C-c C-k (elpy-shell-kill)
  ;; C-c C-K (elpy-shell-kill-all)
  :init
  (defvar python-shell-interpreter)
  (defvar python-shell-interpreter-args)
  (defvar python-shell-prompt-detect-failure-warning)
  (defvar python-shell-completion-native-enable)
  (setq python-shell-interpreter "ipython3"
        ;; must use simple prompt to avoid encoding problem for ipython
        python-shell-interpreter-args
        ;; "-i"
        "--simple-prompt -i")
  (setq python-shell-prompt-detect-failure-warning nil)
  (setq elpy-rpc-python-command "python3")
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
  (setq comint-scroll-to-bottom-on-output t)
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
  (("M-x" . helm-M-x)
   ;; C-j enter directory
   ;; C-l up directory
   ;; C-u C-x C-f open history
   ("C-x C-f" . helm-find-files)
   ("M-y" . helm-show-kill-ring)
   ("C-x b" . helm-mini)
   ("C-h SPC" . helm-all-mark-rings))
  :config
  ;; helm-semantic-or-imenu (C-x c i)
  ;; it shows the outline!
  ;; the actual worker is semantic, so be sure to enable it
  (setq helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet
(use-package yasnippet
  :init
  (progn
    (defvar yas-snippet-dirs)
    (yas-global-mode 1))
  (use-package yasnippet-snippets)
  :config
  ;; https://guix.gnu.org/manual/en/html_node/The-Perfect-Setup.html
  ;; FIXME tangled code
  (add-to-list 'yas-snippet-dirs "~/git/reading/guix/etc/snippets")
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
;; In order to use flycheck, the checkers need to be installed. To
;; verify a checker is properly installed, use
;; flycheck-verify-checker.
;;
;; python: pylint
;;
;; Seems that the xref-find-definitions also rely on pylint (or maybe
;; just virtualenv .. when ipython is not installed system-wise)
(use-package flycheck
  :disabled
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :bind
  (("C-c c" . flycheck-buffer))
  :config
  ;; (setq flycheck-clang-args '"--std=c++11")
  ;; (setq flycheck-clang-args nil)
  ;; --std=c++11 is not working with C code.
  ;; Instead, include this in .dir-locals.el
  ;; ((c++-mode . ((flycheck-clang-args . ("--std=c++11")))))
  (add-hook 'c++-mode-hook '(lambda()
                              (setq flycheck-clang-args "--std=c++11")))
  (add-hook 'c++-mode-hook #'(lambda () (setq flycheck-gcc-language-standard "c++11")))
  (add-hook 'c-mode-hook '(lambda()
                            (setq flycheck-clang-args "")))
  ;; Add include path
  ;; I found this information by:
  ;; M-x describe-checker => found c/c++-clang
  ;; Click on it, goes to the description, along with the configurable part.
  (setq flycheck-clang-include-path (list "..")))

(use-package google-c-style
  ;; c style used by google
  :disabled t
  :defer t)
(use-package clang-format)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
(use-package smart-mode-line
  :disabled
  :init
  ;; this is actually required by smart-mode-line
  ;; don't need to explicitly load use-package it, just as a reference
  ;; actually the rm-blacklist is offered by it
  ;; it also offers the rm-text-properties variable
  ;; (use-package rich-minority)
  :config
  (setq sml/no-confirm-load-theme t) ; do not warn me for loading a theme
  (setq sml/theme 'respectful)
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
    ;; (git-gutter:linum-setup)
    )
  ;; :bind
  ;; (("C-x C-g" . git-gutter:toggle))
  :config
  (progn
    (custom-set-variables
     '(git-gutter:modified-sign "  ")
     '(git-gutter:added-sign "++")
     '(git-gutter:deleted-sign "--"))
    (set-face-background 'git-gutter:modified "purple")
    (set-face-foreground 'git-gutter:added "green")
    (set-face-foreground 'git-gutter:deleted "red")))

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


(use-package guix)

(use-package geiser
  ;; geiser is not the REPL I want for racket
  ;; :disabled
  :config
  (setq geiser-active-implementations '(guile))
  ;; https://guix.gnu.org/manual/en/html_node/The-Perfect-Setup.html
  ;; FIXME why the variable is undefined?
  ;; (add-to-list 'geiser-guile-load-path "~/git/reading/guix")
  (setq geiser-mode-smart-tab-p t))

(use-package racket-mode
  ;; :disabled
  :config
  (add-hook 'racket-mode-hook
            (lambda ()
              (define-key racket-mode-map (kbd "C-c r") 'racket-run)))
  ;; (setq tab-always-indent 'complete)
  ;;
  ;; geiser does not need that, it has a dedicated function "C-c C-\"
  ;; to insert lambda
  ;;
  ;; (add-hook 'scheme-mode-hook #'racket-unicode-input-method-enable)
  ;;
  ;; use C-\ to toggle the input method
  (add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
  (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)
  ;; reinforce the racket-mode selection. otherwise it is occupied by geiser
  (add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))

  (defun racket-visit-definition (&optional prefix)
    "Testing my simple visit definition without asking y-or-n eveytime buffer changes."
    (interactive "P")
    (pcase (racket--symbol-at-point-or-prompt prefix "Visit definition of: ")
      (`nil nil)
      (str (racket--do-visit-def-or-mod 'def str)))))


(use-package company
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
;; require installing aspell-en package
(use-package flyspell
  ;; disabling because Starting new Ispell process ... all the time
  ;; when exporting html
  :disabled
  :init
  :config
  (progn
    ;; (add-hook 'LaTeX-mode-hook '(flyspell-mode t))
    ;; TODO not working for flyspell-mode
    (setq ispell-program-name "aspell")
    (add-hook 'LaTeX-mode-hook #'turn-on-flyspell)
    (add-hook 'latex-mode-hook #'turn-on-flyspell)
    (add-hook 'org-mode-hook #'turn-on-flyspell)))

;; this is not the scribble mode
;; (use-package scribble-mode)

(use-package scribble
  :straight (scribble-mode :type git :host github
                           :repo "lihebi/scribble-mode.el"))

(use-package hackernews)
(use-package haskell-mode)

(use-package sml-mode)
(use-package markdown-mode)


;; To make the citation works:
;;
;; 1. in orgmode, run hebi-gen-bib. This will insert bib file links at
;; the end of the org file.
;;
;; 2. In latex file, C-c TAB (translated from C-c <tab>) runs the
;; command tex-bibtex-file (found in latex-mode-map).  This is defined
;; in tex-mode instead of auctex.
;;
;; Of course you need to run pdflatex or org-latex-export-to-pdf again

;; (use-package ebib)
(use-package tex
  :straight auctex
  :defer t
  :config
  (setq TeX-open-quote "\"")
  (setq TeX-close-quote "\"")
  (add-hook 'LaTeX-mode-hook
            '(lambda()
               (define-key LaTeX-mode-map (kbd "C-c ]") 'helm-bibtex)))
  ;; (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
  (add-hook 'LaTeX-mode-hook
            '(lambda()
               (add-to-list 'LaTeX-verbatim-environments "lstlisting")))
  ;; (define-key LaTeX-mode-map (kbd "C-c t") 'reftex-toc)
  (setq TeX-open-quote "\"")
  (setq TeX-close-quote "\"")
  (if (string= system-type "darwin")
      (progn
        (setq TeX-view-program-selection '((output-pdf "Skim"))))
    (setq TeX-view-program-selection '((output-pdf "PDF Tools"))))
  ;; supporting indentation of [] in LaTeX mode
  (defun TeX-brace-count-line ()
    "Count number of open/closed braces."
    (save-excursion
      (let ((count 0) (limit (line-end-position)) char)
        (while (progn
                 (skip-chars-forward "^{}[]\\\\" limit)
                 (when (and (< (point) limit) (not (TeX-in-comment)))
                   (setq char (char-after))
                   (forward-char)
                   (cond ((eq char ?\{)
                          (setq count (+ count TeX-brace-indent-level)))
                         ((eq char ?\})
                          (setq count (- count TeX-brace-indent-level)))
                         ((eq char ?\[)
                          (setq count (+ count TeX-brace-indent-level)))
                         ((eq char ?\])
                          (setq count (- count TeX-brace-indent-level)))
                         ((eq char ?\\)
                          (when (< (point) limit)
                            (forward-char)
                            t))))))
        count))))

(when (not (string= system-type "darwin"))
  (use-package pdf-tools
    :init
    ;; FIXME why emacs keeps remove the build directory?
    ;; (setq pdf-info-epdfinfo-program "/home/hebi/.emacs.d/straight/build/pdf-tools/epdfinfo")
    (setq pdf-info-epdfinfo-program "~/.emacs.d/epdfinfo")
    :config
    ;; This seems also sets the default viewing mode of pdf, and it
    ;; seems to honor the pdf-info-epdfinfo-program variable, i.e. put
    ;; the executable there, and don't build if exist
    (pdf-tools-install)
    (setq pdf-view-resize-factor 1.03)
    (defun pdf-view-fit-paper(number)
      ;; using P for horizontal reading
      ;; using C-u P for vertical reading
      (interactive "p")
      (if (= number 1)
          (progn
            ;; landscape
            (setq pdf-view-display-size 1.53)
            (image-set-window-vscroll 6))
        (progn
          ;; portrait
          (setq pdf-view-display-size 2.05)
          (image-set-window-hscroll 11)))
      (pdf-view-redisplay t))
    (defun hebi-pdf-vert-22 ()
      (interactive)
      (setq pdf-view-display-size 2.05)
      (image-set-window-hscroll 11)
      (pdf-view-redisplay t))
    ;; C-c C-r m
    ;; pdf-view-midnight-minor-mode
    (setq pdf-view-midnight-colors
          ;; '("white" . "black")
          ;; '("#839496" . "#002b36")
          '("white" . "#002b36"))
    (define-key pdf-view-mode-map (kbd "P") 'pdf-view-fit-paper)))


;; usage: create ~/.virtualenvs, and run mkvirtualenv (in eshell),
;; with a name. M-x venv-workon will activate it, while
;; venv-deactivate stop it. It works with both eshell and python.el
;;
;; The pip might not work properly. Try python -m pip instead.
(use-package virtualenvwrapper
  :config
  ;; if you want interactive shell support
  (venv-initialize-interactive-shells)
  ;; if you want eshell support
  (venv-initialize-eshell))

;; M-x set-input-method eim-wb
;; toggle-input-method
;;
;; starting from z, use pinyin as input and show wb code
;; eim-describe-char show code at point
(use-package emacs-eim
  :straight
  (emacs-eim :type git :host github :repo "wenbinye/emacs-eim"
             :files ("*.txt" :defaults))
  ;; no require, otherwise use-package error: cannot load
  :no-require t
  :init
  (autoload 'eim-use-package "eim" "Another emacs input method")
  :config
  ;; 用 ; 暂时输入英文
  ;; (require 'eim-extra)
  ;; (global-set-key ";" 'eim-insert-ascii)
  (setq eim-use-tooltip nil)
  (register-input-method
   "eim-wb" "euc-cn" 'eim-use-package
   "五笔" "汉字五笔输入法" "wb.txt")
  (register-input-method
   "eim-py" "euc-cn" 'eim-use-package
   "拼音" "汉字拼音输入法" "py.txt"))

;; M-x simple-mpc
;; Need mpc client installed.
(use-package simple-mpc)

(use-package simple-drill
  :straight (simple-drill :type git :host github
                          :repo "lihebi/simple-drill.el")
  :config
  (setq simple-drill-history-file "~/git/history-files/simple-drill-history.el"))

(use-package repo-tracker
  :straight (repo-tracker :type git :host github
                          :repo "lihebi/repo-tracker.el")
  ;; Although it requires magit, I load magit with :defer, so adding
  ;; this seems to make this package :defer too, thus config not
  ;; loaded.
  ;;
  ;; :requires magit
  :config
  (setq repo-tracker-repos '("~/git/homepage"
                             "~/git/note"
                             "~/git/research"
                             "~/git/scratch/"
                             "~/git/AdvAE-paper"
                             "~/git/AdvAE"
                             "~/git/biber"
                             "~/git/biber-dist"
                             "~/git/guix-channel"
                             "~/git/hbkb"
                             "~/git/history-files"
                             "~/git/rackematic"
                             ;; configs
                             "~/.hebi"
                             "~/.stumpwm.d"
                             "~/.emacs.d"
                             ;; emacs packages
                             "~/.emacs.d/straight/repos/smart-scholar.el"
                             "~/.emacs.d/straight/repos/simple-drill.el"
                             "~/.emacs.d/straight/repos/scribble-mode.el/"
                             "~/.emacs.d/straight/repos/hn.el/"
                             "~/.emacs.d/straight/repos/repo-tracker.el/"
                             )))

(use-package hn
  :straight (hn :type git :host github
                :repo "lihebi/hn.el")
  :config
  (setq hn-hl-users '("okanesen" "wilsonfiifi"
                      "neilv" "FigBug" "gumby" "skybrian"
                      "coldtea"
                      "rekado" ;; Guix hacker
                      "logicprog" "dreamcompiler"))
  ;; I need to have a list of "bad" users as well
  (setq hn-bad-users '("codegladiator"))
  (setq hn-tags '("pl" "os" "hw" "ai" "space"))
  (setq hn-hl-keywords '("lisp" "racket" "scheme" "clojure"
                         "haskell" "ocaml"
                         "\\brust\\b" "julia"
                         "WebAssembly" "wasm"
                         "functional"
                         "BSD"
                         "emacs" "linux"
                         "vim"
                         "Idris"
                         "RISC"
                         "Guix" "Nix"
                         ;; "Show HN"
                         ;; "Ask HN"
                         "Raspberry Pi"
                         "\\bML\\b" "\\bAI\\b"
                         "nlp"
                         "Machine Learning" "Artificial Intelligence" "Deep Learning"
                         "Tensorflow"
                         "postmarketOS"))
  (setq hn-history-dir (expand-file-name "~/git/history-files/hn"))
  ;; FIXME can I just use the ~ in the path name without manually expanding?
  (setq hn-export-json-file "/srv/www/assets/hn.json")
  (setq hn-top-dir "~/git/hn-top/")
  (setq hn-fields
        ;; '(star time score comment tag user title)
        '(star time score comment title)
        ;; '(star score comment tag title)
        ))


;; C-c C-c runs the command arduino-upload
;; It runs arduino --upload xxx.ino
(use-package arduino-mode)

;; (use-package pamparam)

(use-package monokai-theme
  ;; this package is very weird, the theme is loaded automatically.
  :disabled
  :config
  ;; :init
  ;; (load-theme 'monokai t)
  ;; (enable-theme 'monokai)
  )

(use-package zenburn-theme
  :disabled)

(use-package color-theme-sanityinc-tomorrow
  ;; :disabled
  :config
  ;; (color-theme-sanityinc-tomorrow-night)
  ;; (color-theme-sanityinc-tomorrow-day)
  ;; (load-theme 'sanityinc-tomorrow-day t)
  ;; (color-theme-sanityinc-tomorrow-blue)
  ;; (color-theme-sanityinc-tomorrow-eighties)
  ;; (load-theme 'sanityinc-tomorrow-bright t)
  )

(use-package request)

;; FIXME one of these packages will make org-ref links unusable! How??
;; This is another example that the dynamic scoping of elisp is
;; programatic, different packages can interfere with each other!

;; (use-package alda-mode)
;; (use-package cider
;;   :config
;;   (setq cider-prompt-for-symbol nil))

(use-package scala-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Julia configuration
;; M-. not working, see https://github.com/tpapp/julia-repl/issues/50
(use-package julia-mode)
;; this package can implement "flash the sexp"
(use-package eval-sexp-fu)
(use-package julia-repl
  :straight (julia-repl :type git :host github
                        :repo "lihebi/julia-repl")
  :config
  ;; (setq julia-repl-term-type 'term)
  (setq julia-repl-term-type 'comint)
  (add-hook 'julia-mode-hook 'julia-repl-mode)
  (setq julia-repl-switches "-i --color=yes"))

(use-package dumb-jump
  ;; trying to use it for julia, but does not work
  :disabled)
;; for lsp-julia
(use-package lsp-mode)
;; for find-symbol
(use-package lsp-julia
  ;; currently lsp-mode won't start automatically (and it is slow, so
  ;; probably just start on-demand). To start the connection, run M-x
  ;; lsp, after a while, M-. should work.
  :straight (lsp-julia :type git :host github
                       :repo "non-Jedi/lsp-julia")
  :config
  ;; I need to install several julia packages
  ;; LanguageServer.jl, StaticLint.jl
  ;; To use it, simply run M-x lsp
  ;; Then M-. should work
  (setq lsp-julia-default-environment "~/.julia/environments/v1.2")
  (add-hook 'julia-mode-hook #'lsp-mode)
  ;; The julia-ls is crashing on TAB, I'm disabling these to avoid
  ;; this temporarily. Probably related:
  ;; https://github.com/julia-vscode/LanguageServer.jl/issues/389
  (setq lsp-enable-completion-at-point nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  ;; showing debug information
  (setq lsp-log-io t))

;; There does not seem to be a working find-def function in eglot
(use-package eglot
  ;; It is still not working, this time the eglot connection to
  ;; backend LSP seems to be very unstable. The progress is tracked at:
  ;;
  ;; - https://discourse.julialang.org/t/using-languageserver-jl-with-eglot-in-emacs/
  ;; - https://github.com/julia-vscode/LanguageServer.jl/issues/389
  :disabled
  :config
  (add-hook 'julia-mode-hook 'eglot-ensure)
  (setq eglot-connect-timeout 50))
(use-package eglot-julia
  ;; https://github.com/lihebi/eglot-julia
  :disabled
  :straight (eglot-julia :type git :host github
                         :repo "lihebi/eglot-julia"))

(use-package coffee-mode)


;;; use-pkg.el ends here
