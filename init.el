;;; hebi-emacs-init --- What the hack of this line?
;;; Commentary:


;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defun emacs-d (filename)
  "Expand FILENAME relative to `user-emacs-directory'."
  (expand-file-name filename user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; external files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/packages/") ; packages downloaded from internet. Maybe outdated. No such package in elpa.

(load (emacs-d "packages"))
(load (emacs-d "bindings"))
(load (emacs-d "hebi-defun"))
(load "helium-slice-highlighter")
(load (emacs-d "env"))


(load (emacs-d "exp"))
(load (emacs-d "hebi"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (load-theme 'leuven t t)
;; (load-theme 'monokai)
;; (enable-theme 'leuven)

;; (if (not window-system)
;;     (progn
;;       ;; load theme for terminal emacs
;;       ;; should use a dark theme
;;       ;; (load-theme 'monokai t)
;;       (enable-theme 'cyberpunk))
;;   ;; theme for window system
;;   (load-theme 'leuven t)
;;   )


;; (use-package cyberpunk-theme
;;   :init
;;   (load-theme 'cyberpunk t t)
;;   (enable-theme 'cyberpunk)
;;   )

(use-package monokai-theme
  :init
  (load-theme 'monokai t t)
  (enable-theme 'monokai)
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

;; dired-x omiting

;; (add-hook 'dired-load-hook
;;           (lambda ()
;;             (load "dired-x")
;;             ;; Set dired-x global variables here.  For example:
;;             ;; (setq dired-guess-shell-gnutar "gtar")
;;             ;; (setq dired-x-hands-off-my-keys nil)
;;             ))
;; (add-hook 'dired-mode-hook
;;           (lambda ()
;;             ;; Set dired-x buffer-local variables here.  For example:
;;             (dired-omit-mode 1)
;;             ))



(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "conkeror")

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

(require 'dash)

(defun font-candidate (&rest fonts)
  "Return the first available font.
Optional argument FONTS font names to check."
  (--first (find-font (font-spec :name it)) fonts))


(when window-system
  ;; (set-face-attribute 'default nil :font "Source Code Pro")
  (set-face-attribute 'default nil :font
                      (font-candidate "Source Code Pro" "DejaVu Sans Mono"))
  (set-fontset-font (frame-parameter nil 'font)
                    'han (font-spec :family "司马彦简行修正版"
                                    )))

;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("a800120841da457aa2f86b98fb9fd8df8ba682cebde033d7dbf8077c1b7d677a" "71ecffba18621354a1be303687f33b84788e13f40141580fa81e7840752d31bf" default)))
 '(ecb-options-version "2.40")
 '(fci-rule-color "#3E3D31")
 '(git-gutter:added-sign "++")
 '(git-gutter:deleted-sign "--")
 '(git-gutter:modified-sign "  ")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3E3D31" . 0)
     ("#67930F" . 20)
     ("#349B8D" . 30)
     ("#21889B" . 50)
     ("#968B26" . 60)
     ("#A45E0A" . 70)
     ("#A41F99" . 85)
     ("#3E3D31" . 100))))
 '(magit-diff-use-overlays nil)
 '(org-agenda-files
   (quote
    ("~/github/wiki/todo.org" "~/github/wiki/hebi.org" "~/github/wiki/org.org")))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . emacs))))
 '(package-selected-packages
   (quote
    (org-bullets yasnippet use-package tuareg string-inflection smex smartparens smart-mode-line slime skewer-mode scss-mode regex-tool rainbow-delimiters persp-projectile pdf-tools org-plus-contrib neotree multiple-cursors monokai-theme markdown-mode linum-off htmlize helm-projectile helm-gtags helm-dash guide-key goto-chg google-c-style go-mode git-gutter fuzzy flycheck flx fic-mode expand-region exec-path-from-shell etags-select ess edit-server ecb dired-k csv-mode company cmake-mode cider-eval-sexp-fu cider browse-kill-ring bison-mode ag ace-jump-mode)))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(send-mail-function (quote sendmail-send-it))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#3E3D31" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fic-face ((t (:background "black" :foreground "red" :weight bold))))
 '(nxml-element-local-name ((t (:background "textBackgroundColor" :foreground "#000088" :box nil)))))
