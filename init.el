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

;; packages downloaded from internet. Maybe outdated. No such package in elpa.
(add-to-list 'load-path "~/.emacs.d/packages/")

;; (load "~/.emacs.d/try-package.el")

;; (load "~/.emacs.d/org-conf.el")

(when (not (fboundp 'make-variable-frame-local))
  (defun make-variable-frame-local (variable) variable))


;; melpa should be set up no matter I'm using package.el or
;; straight.el
(setq package-archives
      (append package-archives
              '(("org" . "http://orgmode.org/elpa/"))
              '(("melpa" . "http://melpa.milkbox.net/packages/"))))
(package-initialize)
(setq package-enable-at-startup nil)

;; loading files
(load "~/.emacs.d/try-straight.el")

;; FIXME
(if (version< emacs-version "26")
    (global-linum-mode)
  (global-display-line-numbers-mode t))

;; Info-default-directory-list
(setq Info-additional-directory-list '("/home/hebi/.guix-profile/share/info/"))

(column-number-mode t)
(load "~/.emacs.d/use-pkg.el")
(load "~/.emacs.d/org-conf.el")
(load "~/.emacs.d/mail.el")

(defun hebi-reload-org ()
  "My org config seems not being loaded correctly, I have to
manually load it."
  (interactive)
  (load "~/.emacs.d/org-conf.el"))

(load "~/.emacs.d/hebi.el")
(load "~/.emacs.d/lookup.el")

(setq default-frame-alist '((font . "Source Code Pro-12")))
;; (set-face-attribute 'default nil :font "Source Code Pro-12")
;; (set-frame-font "Source Code Pro-10" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration

(setq mouse-yank-at-point t)

;; if I just use the font name, the size should be read from Xresources
;; (setq default-frame-alist '((font . "Source Code Pro")))
;; (set-face-attribute 'default nil :font "Source Code Pro-13")
;; (set-frame-font "Source Code Pro-10" nil t)

;; remove because it cause wired color scheme in dired buffer
;; (setq dired-listing-switches "-alh")


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

;; (setq browse-url-browser-function
;;       'browse-url-default-browser
;;       ;; 'browse-url-conkeror
;;       )

(when (not window-system)
  (menu-bar-mode -1))
(menu-bar-mode -1)

;; But I want to turn off the menu bar sometimes ... like now
;; (menu-bar-mode -1)


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
(setq browse-url-browser-function 'browse-url-chromium)
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
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))

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
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(beacon-color "#f2777a")
 '(compilation-message-face (quote default))
 '(cursor-type (quote bar))
 '(custom-safe-themes
   (quote
    ("82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "2925ed246fb757da0e8784ecf03b9523bccd8b7996464e587b081037e0e98001" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(ecb-options-version "2.40")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(gdb-many-windows t)
 '(git-gutter:added-sign "++")
 '(git-gutter:deleted-sign "--")
 '(git-gutter:modified-sign "  ")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(magit-diff-use-overlays nil)
 '(org-agenda-files
   (quote
    ("~/github/research/segment.org" "~/github/bibliography/SoftwareEnginner/hebi.org" "/home/hebi/github/note/org/default.org" "/home/hebi/github/note/org/gtd.org")))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . emacs))))
 '(org-html-table-default-attributes
   (quote
    (:border "2" :cellspacing "2" :cellpadding "6" :rules "groups" :frame "border")))
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
 '(org-src-preserve-indentation t)
 '(org-structure-template-alist
   (quote
    (("a" . "export ascii")
     ("c" . "center")
     ("C" . "comment")
     ("e" . "example")
     ("E" . "export")
     ("h" . "export html")
     ("l" . "export latex")
     ("q" . "quote")
     ("x" . "quote")
     ("s" . "src")
     ("v" . "verse"))))
 '(package-selected-packages
   (quote
    (scribble-mode geiser racket-mode org emms alert paredit wrap-region z3-mode json-mode org-ref helm-bibtex auctex ace-window benchmark-init rust-mode yaml-mode groovy-mode gradle-mode android-mode helpful firefox-controller bing-dict fill-column-indicator flycheck-rtags shell-switcher dockerfile-mode srefactor sublimity smooth-scrolling cmake-ide color-theme-solarized god-mode bbdb pylint clang-format zenburn-theme solarized-theme sr-speedbar nyan-mode volatile-highlights rtags elpy visual-regexp virtual-regexp magit org-bullets yasnippet use-package tuareg string-inflection smex smartparens smart-mode-line slime skewer-mode scss-mode regex-tool rainbow-delimiters persp-projectile pdf-tools org-plus-contrib neotree multiple-cursors monokai-theme markdown-mode linum-off htmlize helm-projectile helm-gtags helm-dash guide-key goto-chg google-c-style go-mode git-gutter fuzzy flycheck flx fic-mode expand-region exec-path-from-shell etags-select ess edit-server ecb dired-k csv-mode company cmake-mode cider-eval-sexp-fu cider browse-kill-ring bison-mode ag ace-jump-mode)))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(send-mail-function (quote sendmail-send-it))
 '(weechat-color-list
   (quote
    (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
 '(x-gtk-use-system-tooltips nil)
 '(z3-solver-cmd "/usr/bin/z3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fic-author-face ((t (:foreground "orangered" :underline t))))
 '(fic-face ((t (:foreground "red" :weight bold))))
 '(hackernews-comment-count ((t (:inherit hackernews-link :foreground "dark red"))))
 '(hackernews-link ((t (:inherit link :foreground "blue" :underline nil)))))
