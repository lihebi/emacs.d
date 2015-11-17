;;; hebi-emacs-init --- What the hack of this line?
;;; Commentary:


;;; Code:

(defmacro hook-into-modes (function mode-hooks)
  "Add FUNCTION to hooks in MODE-HOOKS."
  `(dolist (hook ,mode-hooks)
     (add-hook hook ,function)))

(defconst emacs-start-time (current-time))
(setq message-log-max 16384)

(defun emacs-d (filename)
  "Expand FILENAME relative to `user-emacs-directory'."
  (expand-file-name filename user-emacs-directory))

(load (emacs-d "packages"))
(load (emacs-d "bindings"))
(load (emacs-d "smart-scholar"))

(load-theme 'zenburn t)
(load-theme 'ample t t)
(load-theme 'ample-flat t t)
(load-theme 'ample-light t t)
(load-theme 'dracula t)
(enable-theme 'ample)

;;;; Environment
(setq shell-file-name "zsh")
(add-to-list 'exec-path "/usr/local/bin")

;;; Registers
(set-register ?i
              (cons 'file (emacs-d "init.el")))
(set-register ?p
              (cons 'file (emacs-d "packages.el")))

;; set nu
(global-linum-mode 1)
;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; ignore the bell
(setq ring-bell-function 'ignore)

;; Always load newest byte code
(setq load-prefer-newer t)

(setq user-full-name "Hebi Li"
      user-mail-address "lihebi.com@gmail.com")

;; this should already there in "better default" package
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(fset 'yes-or-no-p 'y-or-n-p)

(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))

(when window-system
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))

;; smooth scroll
(when (boundp 'mouse-wheel-scroll-amount)
  (setq mouse-wheel-scroll-amount '(0.01)))

;;;; *scratch*
(setq initial-scratch-message nil)
;; Never kill, just bury
(defun dont-kill-but-bury-scratch ()
  "Don't kill but burry *scratch* buffer."
  (if (equal (buffer-name (current-buffer)) "*scratch*")
      (progn (bury-buffer) nil)
    t))
(add-hook 'kill-buffer-query-functions 'dont-kill-but-bury-scratch)

(winner-mode)

(use-package yasnippet
  :ensure t
  :init
  (progn
    (defvar yas-snippet-dirs)
    (setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        ))
    (yas-global-mode 1)))

;;; init.el ends here
