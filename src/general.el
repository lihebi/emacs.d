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


(if (version< emacs-version "26")
    (global-linum-mode)
  (global-display-line-numbers-mode t))


(column-number-mode t)

(setq browse-url-browser-function 'browse-url-chromium)
(setq mouse-yank-at-point t)

;; stop adding newlines automatically.
;; This cause my scripts to add newlines everytime I insert them.
(setq require-final-newline nil)

;; compilation buffer always follow
(defvar compilation-scroll-output)
;; (setq compilation-scroll-output t)
(setq compilation-scroll-output 'first-error)

(global-hl-line-mode)

(when (not window-system)
  (menu-bar-mode -1))
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; tooltip
(setq tooltip-delay 0.01)
(setq tooltip-recent-seconds 1)
(setq tooltip-short-delay 0.01)


;; key bindings
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x O")
                (lambda ()
                  (interactive)
                  (other-window -1)))

;; stop using suspend-frame
(global-unset-key (kbd "C-z"))


(dolist (item '(".pdf" ".out" ".log" ".dvi" ".DS_Store"))
  (add-to-list 'completion-ignored-extensions item))


;; use unique/prefix/name when buffer name conflict
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; cursor goes to the same place when you last visit
;; This is for 24.5 and older setup
;; (require 'saveplace)
;; (setq-default save-place t)
;; 25 setup
(save-place-mode 1)
(set-mouse-color "red")

;; auto refresh buffers when file changes
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)



(setq-default auto-revert-interval 1
              auto-revert-use-notify nil
              auto-revert-verbose nil
              global-auto-revert-mode t
              magit-auto-revert-mode t
              global-auto-revert-non-file-buffers t)


(show-paren-mode 1)
(setq-default indent-tabs-mode nil)	; indention should not insert tab

(setq require-final-newline t)
(setq visible-bell t)
(setq inhibit-startup-message t)

(setq show-trailing-whitespace t)
(size-indication-mode t)


;; ignore the bell
(setq ring-bell-function 'ignore)

(setq save-place-file (concat user-emacs-directory "places"))
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(fset 'yes-or-no-p 'y-or-n-p)

;; smooth scroll
(when (boundp 'mouse-wheel-scroll-amount)
  (setq mouse-wheel-scroll-amount '(0.01)))

;; in man mode, push a link will open in current buffer
(setq man-notify-method 'pushy)

;; will still keep highlight, until you do another search (C-s)
;; (setq lazy-highlight-cleanup nil)

;; when doing search, C-s then C-w mutiple times can search word at point

;; Mac Settings
;; use command as meta
(setq mac-command-modifier 'meta)
;; (setq mac-option-modifier 'super)
;; use option as hyper
;; option still use as meta
;; (setq mac-option-modifier 'hyper)
;; (setq ns-function-modifier 'hyper)


;; FIXME Do I still use ido-mode? Is this in the same place as helm?

(ido-mode t)                            ; ido: interactively do
;; Flexible matching means that if the entered string does not
;; match any item, any item containing the entered characters
;; in the given sequence will match.
(setq ido-enable-flex-matching t)
;; C-. and C-, is not correctly sent to emacs on terminal on Mac
(defun ido-define-keys ()
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)
