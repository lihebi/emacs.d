;;; init.el --- my emacs config
;;; Commentary:

;;; Code:

;; loading files

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load "~/.emacs.d/src/init.el")

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
    (scribble-mode racket-mode org emms alert paredit wrap-region z3-mode json-mode org-ref helm-bibtex auctex ace-window benchmark-init rust-mode yaml-mode groovy-mode gradle-mode android-mode helpful firefox-controller bing-dict fill-column-indicator flycheck-rtags shell-switcher dockerfile-mode srefactor sublimity smooth-scrolling cmake-ide color-theme-solarized god-mode bbdb pylint clang-format zenburn-theme solarized-theme sr-speedbar nyan-mode volatile-highlights rtags elpy visual-regexp virtual-regexp magit org-bullets yasnippet use-package tuareg string-inflection smex smartparens smart-mode-line slime skewer-mode scss-mode regex-tool rainbow-delimiters persp-projectile pdf-tools org-plus-contrib neotree multiple-cursors monokai-theme markdown-mode linum-off htmlize helm-projectile helm-gtags helm-dash guide-key goto-chg google-c-style go-mode git-gutter fuzzy flycheck flx fic-mode expand-region exec-path-from-shell etags-select ess edit-server ecb dired-k csv-mode company cmake-mode cider-eval-sexp-fu cider browse-kill-ring bison-mode ag ace-jump-mode)))
 '(pdf-view-midnight-colors (quote ("gray" . "#002b36")))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(send-mail-function (quote sendmail-send-it))
 '(weechat-color-list
   (quote
    (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
 '(x-gtk-use-system-tooltips nil)
 '(z3-solver-cmd "/usr/bin/z3"))

