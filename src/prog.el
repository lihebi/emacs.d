(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "C-M-;") 'toggle-comment-on-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++

(defun set-ff-directories()
  (defvar cc-search-directories)
  (setq cc-search-directories
        '("." "/usr/include" "/usr/local/include/*" "$PROJECT/*/include"
          "../include/*/*/*" "../../include/*/*/*"
          "../lib" "../../lib/*/*/*" "../../../lib/*/*"
          ;; FIXME the include for the higher order imbeded function is not found
          "../*/src" "../../src/*/*/*" "../../../src/*/*/*/*" "../../../src/*/*/*"
          ".." "...")))

(add-hook 'c++-mode-hook 'set-ff-directories)
(add-hook 'c-mode-hook 'set-ff-directories)

;; (defvar cc-search-directories)
;; (setq cc-search-directories
;;       (append
;;        '("../include/*/*/*" "../../include/*/*/*" "../lib" "../../lib/*/*/*" "../../../lib/*/*" "../*/src" "../../src/*/*/*")
;;        cc-search-directories))


(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))


;; switch between source and header file
(global-set-key (kbd "C-x C-o") 'ff-find-other-file)

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

(use-package json-mode)
(use-package dockerfile-mode
  :defer t)
(use-package rust-mode)

(use-package slime
  :defer t
  :config
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy))
  (use-package slime-company)
  (slime-setup '(slime-fancy slime-company)))

(use-package elpy-utils
  :straight (elpy-utils :type git :host github
                        :repo "lihebi/elpy-utils.el")
  :config
  (add-hook 'elpy-mode-hook #'elpy-utils-enable)
  (setq elpy-utils-scale 1.0))

(use-package elpy
  ;; C-c C-z (elpy-shell-switch-to-shell)
  ;; C-c C-c (elpy-shell-send-region-or-buffer)
  ;; C-c RET (elpy-shell-send-current-statement)
  ;; C-M-x (python-shell-send-defun)
  ;; C-c C-k (elpy-shell-kill)
  ;; C-c C-K (elpy-shell-kill-all)
  ;;
  ;; elpy current xref-goto-definition is not working. It turns out to be rpc
  ;; problem, mostly no virtualenv. So run elpy-rpc-reinstall-virtualenv to
  ;; solve the problem.
  ;;
  ;; Just found that python3 supports unicode as variable name. Just
  ;; set-input-method TeX and \theta in Emacs. Also, ε and ϵ seems to be the
  ;; same thing.
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

(use-package geiser
  ;; geiser is not the REPL I want for racket
  :disabled
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

;; this is not the scribble mode
;; (use-package scribble-mode)

(use-package scribble
  :straight (scribble-mode :type git :host github
                           :repo "lihebi/scribble-mode.el"))

(use-package haskell-mode)
(use-package markdown-mode)

;; this package can implement "flash the sexp"
(use-package eval-sexp-fu
  ;; FIXME weird error when using with elpy
  :disabled)

(use-package julia-mode)
(use-package julia-repl
  :straight (julia-repl :type git :host github
                        :repo "lihebi/julia-repl")
  :config
  ;; (setq julia-repl-term-type 'term)
  (setq julia-repl-term-type 'comint)
  (add-hook 'julia-mode-hook 'julia-repl-mode)
  (setq julia-utils-scale 1.0)
  (setq julia-repl-switches "-i --color=yes"))

(use-package ess
  ;; R
  ;; but cannot be defered, or the command is not found.
  ;; to use: M-x R
  ;; R-mode
  ;; this is disabled because very slow on startup, but it is very useful
  :config
  (setq inferior-R-args "--no-restore-history --no-save ")
  (setq comint-prompt-read-only t)
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-to-bottom-on-output t)
  (setq comint-move-point-for-output t))
(use-package lua-mode)
(use-package scala-mode)
(use-package graphviz-dot-mode)
(use-package cmake-mode
  :disabled
  :defer t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disabled
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package csv-mode
  :disabled)
(use-package z3-mode
  :disabled
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
(use-package coffee-mode
  :disabled)
(use-package bison-mode
  :defer t
  :disabled)
(use-package go-mode
  :defer t
  :disabled)
(use-package gradle-mode
  :disabled)
(use-package groovy-mode
  :disabled)
(use-package yaml-mode
  :disabled)

;; usage: create ~/.virtualenvs, and run mkvirtualenv (in eshell),
;; with a name. M-x venv-workon will activate it, while
;; venv-deactivate stop it. It works with both eshell and python.el
;;
;; The pip might not work properly. Try python -m pip instead.
(use-package virtualenvwrapper
  :disabled
  :config
  ;; if you want interactive shell support
  (venv-initialize-interactive-shells)
  ;; if you want eshell support
  (venv-initialize-eshell))


(use-package pylint
  ;; have to use this for org mode to find "pylint" when exporting html
  :disabled)

(use-package google-c-style
  ;; c style used by google
  :disabled t
  :defer t)
(use-package clang-format
  :disabled)
(use-package sml-mode
  :disabled)


;; C-c C-c runs the command arduino-upload
;; It runs arduino --upload xxx.ino
(use-package arduino-mode
  :disabled)


;; FIXME one of these packages will make org-ref links unusable! How??
;; This is another example that the dynamic scoping of elisp is
;; programatic, different packages can interfere with each other!

;; (use-package alda-mode)
;; (use-package cider
;;   :config
;;   (setq cider-prompt-for-symbol nil))

(use-package dumb-jump
  ;; trying to use it for julia, but does not work
  :disabled)
;; for lsp-julia
(use-package lsp-mode
  :disabled)
;; for find-symbol
(use-package lsp-julia
  :disabled
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


(use-package nix-mode)
