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
(load (emacs-d "bindings"))

(load (emacs-d "hebi-defun"))
(load (emacs-d "smart-scholar"))

(load (emacs-d "env"))

(load (emacs-d "livedown"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (load-theme 'leuven t t)
;; (enable-theme 'monokai)
;; (enable-theme 'leuven)

(if (not window-system)
    (enable-theme 'monokai)
  (load-theme 'leuven t t)
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

(set-face-attribute 'default nil :font "Source Code Pro")
(set-fontset-font (frame-parameter nil 'font)
                  'han (font-spec :family "司马彦简行修正版"
                                  ))




;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(cc-search-directories
   (quote
    ("." "/usr/include" "/usr/local/include/*" "$PROJECT/*/include" "../include/*/*/*" "../../include/*/*/*" "../lib" "../../lib/*/*/*" "../../../lib/*/*" "../*/src" "../../src/*/*/*")))
 '(custom-safe-themes
   (quote
    ("f556acacd481f54e0cb5f7c0bd7b654dc5e9dd379db4a0d90da850ce5cabc7ec" "8787155a0c7beea10d98a50afd58eec0c13256d4fa87c6b5515138554a95c571" "c99155e3bfeb343041be12efab7b6401226261d01c0159ffde4ac325c71372aa" "be79f427295cde7630cd3705a94caf828bf2eaa956e55aa4708669ee57609062" "8577da1641ed4bdf255341ca92e3d0e49c9f4d574458f09ce78159690442cade" "12b4427ae6e0eef8b870b450e59e75122d5080016a9061c9696959e50d578057" default)))
 '(ecb-options-version "2.40")
 '(ecb-windows-width 0.2)
 '(flycheck-clang-args (quote ("--std=c++11")))
 '(git-gutter:added-sign "++")
 '(git-gutter:deleted-sign "--")
 '(git-gutter:modified-sign "  ")
 '(org-babel-load-languages
   (quote
    ((awk . t)
     (emacs-lisp . t)
     (python . t)
     (ruby . t)
     (sh . t)
     (plantuml . t)
     (C . t)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
