;;; hebi-emacs-init --- What the hack of this line?
;;; Commentary:


;;; Code:

(defun emacs-d (filename)
  "Expand FILENAME relative to `user-emacs-directory'."
  (expand-file-name filename user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; external files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load (emacs-d "packages"))
(load (emacs-d "bindings"))

(load (emacs-d "hebi-defun"))
(load (emacs-d "hebi-faces")) ; must be load before smart-scholar
(load (emacs-d "smart-scholar"))

(load (emacs-d "env"))

(load (emacs-d "livedown"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-theme 'zenburn t)
(load-theme 'ample t t)
(load-theme 'ample-flat t t)
(load-theme 'ample-light t t)
(load-theme 'dracula t)
(enable-theme 'ample)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use unique/prefix/name when buffer name conflict
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; cursor goes to the same place when you last visit
(require 'saveplace)
(setq-default save-place t)

(show-paren-mode 1)
(setq-default indent-tabs-mode nil)	; indention should not insert tab

(setq require-final-newline t)
(setq visible-bell t)
(setq inhibit-startup-message t)

(setq show-trailing-whitespace t)
(global-linum-mode 1)                   ; set nu
(line-number-mode t)                    ; mode line settings
(column-number-mode t)
(size-indication-mode t)

;; ignore the bell
(setq ring-bell-function 'ignore)

(setq save-place-file (concat user-emacs-directory "places"))
(setq backup-directory-alist '(("." . ,(concat user-emacs-directory "backups"))))

(fset 'yes-or-no-p 'y-or-n-p)

;; smooth scroll
(when (boundp 'mouse-wheel-scroll-amount)
  (setq mouse-wheel-scroll-amount '(0.01)))

;; in man mode, push a link will open in current buffer
(setq man-notify-method 'pushy)

;; will still keep highlight, until you do another search (C-s)
(setq lazy-highlight-cleanup nil)

;; when doing search, C-s then C-w mutiple times can search word at point


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Productive
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ido-mode t)                            ; ido: interactively do
;; Flexible matching means that if the entered string does not
;; match any item, any item containing the entered characters
;; in the given sequence will match.
(setq ido-enable-flex-matching t)
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(winner-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq user-full-name "Hebi Li"
      user-mail-address "lihebi.com@gmail.com")

(eshell)
(set-face-foreground 'eshell-prompt "turquoise") ; only the prompt

;;; init.el ends here
