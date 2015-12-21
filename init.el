;;; hebi-emacs-init --- What the hack of this line?
;;; Commentary:


;;; Code:

(defun emacs-d (filename)
  "Expand FILENAME relative to `user-emacs-directory'."
  (expand-file-name filename user-emacs-directory))

(load (emacs-d "packages"))
(load (emacs-d "bindings"))

(load (emacs-d "hebi-defun"))
(load (emacs-d "hebi-faces")) ; must be load before smart-scholar
(load (emacs-d "smart-scholar"))

(load (emacs-d "env"))

(load (emacs-d "livedown"))

(load-theme 'zenburn t)
(load-theme 'ample t t)
(load-theme 'ample-flat t t)
(load-theme 'ample-light t t)
(load-theme 'dracula t)
(enable-theme 'ample)

;; something from better-defaults
;; ido: interactively do
(ido-mode t)
;; Flexible matching means that if the entered string does not
;; match any item, any item containing the entered characters
;; in the given sequence will match.
(setq ido-enable-flex-matching t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

;; use unique/prefix/name when buffer name conflict
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; cursor goes to the same place when you last visit
(require 'saveplace)
(setq-default save-place t)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; TODO what's the default?
;; (global-set-key (kbd "C-s") 'isearch-forward-regexp)
;; (global-set-key (kbd "C-r") 'isearch-backward-regexp)
;; (global-set-key (kbd "C-M-s") 'isearch-forward)
;; (global-set-key (kbd "C-M-r") 'isearch-backward)

(show-paren-mode 1)
(setq-default indent-tabs-mode nil)	; indention should not insert tab
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))



;; end better-defaults

(setq inhibit-startup-message t)

;;;; Environment
;; (setq shell-file-name "zsh")
;; (add-to-list 'exec-path "/usr/local/bin")

(setq show-trailing-whitespace t)
(global-linum-mode 1)                   ; set nu
(line-number-mode t)                    ; mode line settings
(column-number-mode t)
(size-indication-mode t)

;; ignore the bell
(setq ring-bell-function 'ignore)

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

;; Never kill, just bury
(defun dont-kill-but-bury-scratch ()
  "Don't kill but burry *scratch* buffer."
  (if (equal (buffer-name (current-buffer)) "*scratch*")
      (progn (bury-buffer) nil)
    t))
(add-hook 'kill-buffer-query-functions 'dont-kill-but-bury-scratch)

(winner-mode)

(setq ido-decorations '("{" "}" " | " " | ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))

(defvar man-notify-method)
;; in man mode, push a link will open in current buffer
(setq man-notify-method 'pushy)

(eshell)
(set-face-foreground 'eshell-prompt "turquoise") ; only the prompt

;;; init.el ends here
