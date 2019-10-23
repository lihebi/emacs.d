(setq default-frame-alist '((font . "Source Code Pro-13")))
;; (set-face-attribute 'default nil :font "Source Code Pro-13")
;; (set-frame-font "Source Code Pro-10" nil t)

;; if I just use the font name, the size should be read from Xresources
;; (setq default-frame-alist '((font . "Source Code Pro")))
;; (set-face-attribute 'default nil :font "Source Code Pro-13")
;; (set-frame-font "Source Code Pro-10" nil t)

;; font
;; see:
;; http://zhuoqiang.me/torture-emacs.html#id1
;; http://baohaojun.github.io/perfect-emacs-chinese-font.html
;; https://github.com/tumashu/chinese-fonts-setup




;; active buffer gets red mode line
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fic-author-face ((t (:foreground "orangered" :underline t))))
 '(fic-face ((t (:foreground "red" :weight bold))))
 '(hackernews-comment-count ((t (:inherit hackernews-link :foreground "dark red"))))
 '(hackernews-link ((t (:inherit link :foreground "blue" :underline nil))))
 '(mode-line ((t (:background "dark red" :foreground "white"))))
 '(mode-line-inactive ((t (:background "black")))))



;; a must-have package, show the position you are in this buffer
(use-package nyan-mode
  :config
  (nyan-mode))

(use-package git-gutter
  :init
  (progn
    (global-git-gutter-mode t)
    ;; (git-gutter:linum-setup)
    )
  ;; :bind
  ;; (("C-x C-g" . git-gutter:toggle))
  :config
  (progn
    (custom-set-variables
     '(git-gutter:modified-sign "  ")
     '(git-gutter:added-sign "++")
     '(git-gutter:deleted-sign "--"))
    (set-face-background 'git-gutter:modified "purple")
    (set-face-foreground 'git-gutter:added "green")
    (set-face-foreground 'git-gutter:deleted "red")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disabled
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package monokai-theme
  ;; this package is very weird, the theme is loaded automatically.
  :disabled
  :config
  ;; :init
  ;; (load-theme 'monokai t)
  ;; (enable-theme 'monokai)
  )

(use-package zenburn-theme
  :disabled)

(use-package color-theme-sanityinc-tomorrow
  :disabled
  :config
  ;; (color-theme-sanityinc-tomorrow-night)
  ;; (color-theme-sanityinc-tomorrow-day)
  ;; (load-theme 'sanityinc-tomorrow-day t)
  ;; (color-theme-sanityinc-tomorrow-blue)
  ;; (color-theme-sanityinc-tomorrow-eighties)
  ;; (load-theme 'sanityinc-tomorrow-bright t)
  )

(use-package smart-mode-line
  :disabled
  :init
  ;; this is actually required by smart-mode-line
  ;; don't need to explicitly load use-package it, just as a reference
  ;; actually the rm-blacklist is offered by it
  ;; it also offers the rm-text-properties variable
  ;; (use-package rich-minority)
  :config
  (setq sml/no-confirm-load-theme t) ; do not warn me for loading a theme
  (setq sml/theme 'respectful)
  (sml/setup)
  (setq sml/name-width 15)
  ;; highlight God-mode in minor mode
  (add-to-list 'rm-text-properties
               '("\\` God\\'" 'face 'font-lock-warning-face))
  (setq rm-blacklist
        (format "^ \\(%s\\)$"
                (mapconcat #'identity
                           '("FlyC.*"
                             "Projectile.*"
                             "hebi-keys"
                             "PgLn"
                             "company"
                             "Undo-Tree"
                             "yas"
                             "GitGutter")
                           "\\|"))))

