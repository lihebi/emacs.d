;;; hebi-emacs-init --- What the hack of this line?
;;; Commentary:


;;; Code:

(defun emacs-d (filename)
  "Expand FILENAME relative to `user-emacs-directory'."
  (expand-file-name filename user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; external files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/packages/") ; packages downloaded from internet. Maybe outdated. No such package in elpa.

(load (emacs-d "hebi-faces")) ; must be load before smart-scholar
(load (emacs-d "packages"))
(load (emacs-d "org-conf"))
(if (and (>= emacs-major-version 24)
	     ;; FIXME 25.x
	     (>= emacs-minor-version 4))
    (load (meacs-d "org-ref-conf")))
(load (emacs-d "bindings"))

(load (emacs-d "hebi-defun"))
;; (load (emacs-d "smart-scholar"))

(load (emacs-d "env"))

(load (emacs-d "livedown"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (load-theme 'leuven t t)
;; (load-theme 'monokai)
;; (enable-theme 'leuven)

(if (not window-system)
    (load-theme 'monokai t)
  (load-theme 'leuven t)
  )

(when (not window-system)
  (menu-bar-mode -1)
  )
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; (set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
;; (set-frame-parameter (selected-frame) 'alpha '(90 90))
;; (add-to-list 'default-frame-alist '(alpha 90 90))

(defun toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (frame-parameter nil 'alpha))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(90 90))))
(global-set-key (kbd "C-c t") 'toggle-transparency)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-to-list 'completion-ignored-extensions ".pdf")
;; (add-to-list 'completion-ignored-extensions ".out")
;; (add-to-list 'completion-ignored-extensions ".log")
;; (add-to-list 'completion-ignored-extensions ".dvi")

(dolist (item '(".pdf" ".out" ".log" ".dvi" ".DS_Store"))
  (add-to-list 'completion-ignored-extensions item)
  )

(defun set-ff-directories()
  (defvar cc-search-directories)
  (setq cc-search-directories
        '("." "/usr/include" "/usr/local/include/*" "$PROJECT/*/include" "../include/*/*/*" "../../include/*/*/*" "../lib" "../../lib/*/*/*" "../../../lib/*/*" "../*/src" "../../src/*/*/*"))
  )

(add-hook 'c++-mode-hook 'set-ff-directories)
(add-hook 'c-mode-hook 'set-ff-directories)

;; (defvar cc-search-directories)
;; (setq cc-search-directories
;;       (append
;;        '("../include/*/*/*" "../../include/*/*/*" "../lib" "../../lib/*/*/*" "../../../lib/*/*" "../*/src" "../../src/*/*/*")
;;        cc-search-directories))

;; use unique/prefix/name when buffer name conflict
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; cursor goes to the same place when you last visit
(require 'saveplace)
(setq-default save-place t)
;; auto refresh buffers when file changes
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Productive
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(winner-mode)

;; C/C++
;; hs-toggle-hiding
;; hs-hide-all
;; hs-show-all
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; narrow/widen
;; narrow-to-defun
;; narrow-to-region
;; widen

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq user-full-name "Hebi Li"
      user-mail-address "lihebi.com@gmail.com")

(set-register ?s '(file . "~/github/note/stack.org"))

(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; font
;; see:
;; http://zhuoqiang.me/torture-emacs.html#id1
;; http://baohaojun.github.io/perfect-emacs-chinese-font.html
;; https://github.com/tumashu/chinese-fonts-setup

(when window-system
  (set-face-attribute 'default nil :font "Source Code Pro")
  (set-fontset-font (frame-parameter nil 'font)
                    'han (font-spec :family "司马彦简行修正版"
                                    ))
  )

;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("6c62b1cd715d26eb5aa53843ed9a54fc2b0d7c5e0f5118d4efafa13d7715c56e" default)))
 '(git-gutter:added-sign "++")
 '(git-gutter:deleted-sign "--")
 '(git-gutter:modified-sign "  ")
 '(send-mail-function (quote sendmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(nxml-element-local-name ((t (:background "textBackgroundColor" :foreground "#000088" :box nil)))))
