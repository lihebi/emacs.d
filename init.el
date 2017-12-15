;;; hebi-emacs-init --- What the hack of this line?
;;; Commentary:


;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; (defun emacs-d (filename)
;;   "Expand FILENAME relative to `user-emacs-directory'."
;;   (expand-file-name filename user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; external files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'dash)

(add-to-list 'load-path "~/.emacs.d/packages/") ; packages downloaded from internet. Maybe outdated. No such package in elpa.

;; (load "~/.emacs.d/try-package.el")

;; not sure if i want to load them. They should already have disabled
;; entry, so loading them should have no effect
;; (load "~/.emacs.d/packages-disabled.el")

;; (load "~/.emacs.d/org-conf.el")

(load "~/.emacs.d/try-straight.el")
(load "~/.emacs.d/use-pkg.el")
(load "~/.emacs.d/org-conf.el")
(load "~/.emacs.d/scholar.el")

(defun hebi-reload-org ()
  "My org config seems not being loaded correctly, I have to
manually load it."
  (interactive)
  (load "~/.emacs.d/org-conf.el"))


;; (use-package org)

(load "~/.emacs.d/packages/helium-slice-highlighter.el")

(load "~/.emacs.d/hebi.el")

;; disabling erc because it is slow on startup
;; (load "~/.emacs.d/erc.el")

;; (setq default-frame-alist '((font . "Source Code Pro-10")))
;; (set-face-attribute 'default nil :font "Source Code Pro-5")
;; (set-frame-font "Source Code Pro-10" nil t)

;; (load "~/.emacs.d/local.el")

;; disabling theme
;; (load "~/.emacs.d/theme.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration

;; if I just use the font name, the size should be read from Xresources
(setq default-frame-alist '((font . "Source Code Pro")))
;; (set-face-attribute 'default nil :font "Source Code Pro-13")
;; (set-frame-font "Source Code Pro-10" nil t)

;; remove because it cause wired color scheme in dired buffer
;; (setq dired-listing-switches "-alh")

(defvar gnus-init-file)
(defvar gnus-startup-file)
(setq gnus-init-file "~/.emacs.d/gnus.el")
(setq gnus-startup-file "~/.emacs.d/newsrc")
(setq user-full-name "Hebi Li"
      user-mail-address "lihebi.emacs@gmail.com")


;; (load "~/.emacs.d/gnus.el")

;; stop adding newlines automatically.
;; This cause my scripts to add newlines everytime I insert them.
(setq require-final-newline nil)

;; compilation buffer always follow
(defvar compilation-scroll-output)
;; (setq compilation-scroll-output t)
(setq compilation-scroll-output 'first-error)

;; tramp
(require 'tramp)
(setq tramp-default-method "ssh")
;; use C-x C-f /-:: to connect
;; (custom-set-variables
;;  '(tramp-default-method "ssh" nil (tramp))
;;  '(tramp-default-user "hebi" nil (tramp))
;;  '(tramp-default-host "office.lihebi.com" nil (tramp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-hl-line-mode)

(when (not window-system)
  (menu-bar-mode -1))

;; But I want to turn off the menu bar sometimes ... like now
(menu-bar-mode -1)


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



;; (setq browse-url-browser-function 'browse-url-generic
;;       browse-url-generic-program
;;       ;; "conkeror"
;;       "chromium"
;;       )




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

;; kill lines backward
(global-set-key (kbd "C-<backspace>")
                (lambda ()
                  (interactive)
                  (kill-line 0)
                  (indent-according-to-mode)))


;; M-^: back
;; C-^ forward
;; join line
(global-set-key (kbd "C-^")
                (lambda()
                  (interactive)
                  (join-line -1)))

;; (define-key global-map (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "RET") 'newline-and-indent)

;; when split window right, swith to that window
(global-set-key (kbd "C-x 3") (lambda ()
                                (interactive)
                                (split-window-right)
                                (other-window 1)))


;; switch between source and header file
(global-set-key (kbd "C-x C-o") 'ff-find-other-file)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mac Setting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use command as meta
(setq mac-command-modifier 'meta)
;; (setq mac-option-modifier 'super)
;; use option as hyper
;; option still use as meta
;; (setq mac-option-modifier 'hyper)
;; (setq ns-function-modifier 'hyper)



;; hyper-r
(global-set-key (kbd "s-r") 'replace-string)




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
;; This is for 24.5 and older setup
;; (require 'saveplace)
;; (setq-default save-place t)
;; 25 setup
(save-place-mode 1)

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

;; restore previous layout
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



(defun toggle-window-split ()
  "."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x 9") 'toggle-window-split)

(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "C-M-;") 'toggle-comment-on-line)

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))


(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))

(global-set-key (kbd "C-c C-SPC") 'unpop-to-mark-command)


(set-register ?s '(file . "~/github/note/stack.org"))

(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; font
;; see:
;; http://zhuoqiang.me/torture-emacs.html#id1
;; http://baohaojun.github.io/perfect-emacs-chinese-font.html
;; https://github.com/tumashu/chinese-fonts-setup


;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(compilation-message-face (quote default))
 '(cursor-type (quote bar))
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(ecb-options-version "2.40")
 '(fci-rule-color "#3E3D31")
 '(gdb-many-windows t)
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
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . emacs))))
 '(org-latex-default-packages-alist
   (quote
    (("AUTO" "inputenc" t)
     ("T1" "fontenc" t)
     ("" "fixltx2e" nil)
     ("" "graphicx" t)
     ("" "grffile" t)
     ("" "longtable" nil)
     ("" "wrapfig" nil)
     ("" "rotating" nil)
     ("normalem" "ulem" t)
     ("" "amsmath" t)
     ("" "textcomp" t)
     ("" "amssymb" t)
     ("" "capt-of" nil)
     ("" "float" nil)
     ("" "hyperref" nil))))
 '(package-selected-packages
   (quote
    (scribble-mode geiser racket-mode org emms alert paredit wrap-region z3-mode json-mode org-ref helm-bibtex auctex ace-window benchmark-init rust-mode yaml-mode groovy-mode gradle-mode android-mode helpful firefox-controller bing-dict fill-column-indicator flycheck-rtags shell-switcher dockerfile-mode srefactor sublimity smooth-scrolling cmake-ide color-theme-solarized god-mode bbdb pylint clang-format zenburn-theme solarized-theme sr-speedbar nyan-mode volatile-highlights rtags elpy visual-regexp virtual-regexp magit org-bullets yasnippet use-package tuareg string-inflection smex smartparens smart-mode-line slime skewer-mode scss-mode regex-tool rainbow-delimiters persp-projectile pdf-tools org-plus-contrib neotree multiple-cursors monokai-theme markdown-mode linum-off htmlize helm-projectile helm-gtags helm-dash guide-key goto-chg google-c-style go-mode git-gutter fuzzy flycheck flx fic-mode expand-region exec-path-from-shell etags-select ess edit-server ecb dired-k csv-mode company cmake-mode cider-eval-sexp-fu cider browse-kill-ring bison-mode ag ace-jump-mode)))
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
   (unspecified "#272822" "#3E3D31" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
 '(x-gtk-use-system-tooltips nil)
 '(z3-solver-cmd "/usr/bin/z3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
