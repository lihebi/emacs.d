;;; hebi-emacs-init --- What the hack of this line?
;;; Commentary:


;;; Code:

(defun emacs-d (filename)
  "Expand FILENAME relative to `user-emacs-directory'."
  (expand-file-name filename user-emacs-directory))

(load (emacs-d "packages"))
(load (emacs-d "hebi-faces")) ; must be load before smart-scholar
(load (emacs-d "smart-scholar"))
(load (emacs-d "bindings"))
(load (emacs-d "hebi-defun"))
(load (emacs-d "env"))
(load (emacs-d "livedown"))

(load-theme 'zenburn t)
(load-theme 'ample t t)
(load-theme 'ample-flat t t)
(load-theme 'ample-light t t)
(load-theme 'dracula t)
(enable-theme 'ample)

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

;;; init.el ends here
