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
  ;; (add-hook 'dired-after-readin-hook #'dired-k-no-revert)
  (add-hook 'dired-initial-position-hook 'dired-k))


(use-package magit
  :defer t
  :bind (("C-x g" . magit-status)))

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
;; Disabled
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smex
  ;; use ido in M-x
  :defer t
  :disabled
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)
   ; my old M-x
   ("C-c C-c M-x" . execute-extended-command))
  :init
  (progn
    (smex-initialize)))
