
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

