;;; theme.el --- theme configurations

;;; Commentary:

;;; Code:

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

;; (use-package monokai-theme
;;   :init
;;   (load-theme 'monokai t t)
;;   (enable-theme 'monokai))


;; This is not the official one!
;; Use the color-theme-solarized instead!
;; (use-package solarized-theme
;;   :disabled t
;;   :config
;;   ;; make the fringe stand out from the background
;;   ;; (setq solarized-distinct-fringe-background t)
;;   ;; Don't change the font for some headings and titles
;;   ;; (setq solarized-use-variable-pitch nil)
;;   ;; Use less bolding
;;   ;; (setq solarized-use-less-bold t)
;;   ;; Use more italics
;;   ;; (setq solarized-use-more-italic t)
;;   ;; Use less colors for indicators such as git:gutter, flycheck and similar
;;   ;; (setq solarized-emphasize-indicators nil)
;;   (load-theme 'solarized-light t)
;;   ;; (load-theme 'solarized-dark t)
;;   ;; (enable-theme 'solarized-light)
;;   ;; (load-theme 'solarized-light t)
;;   )

(use-package color-theme-solarized
  :init
  ;; must load this first
  (use-package color-theme)
  ;; (add-hook 'after-make-frame-functions
  ;;           (lambda (frame)
  ;;             (message "changing ...")
  ;;             (let ((mode (if (display-graphic-p frame) 'light 'dark)))
  ;;               (setq mode 'light)
  ;;               (set-frame-parameter frame 'background-mode mode)
  ;;               (set-terminal-parameter frame 'background-mode mode))
  ;;             (enable-theme 'solarized)))
  :config
  ;; (set-frame-parameter nil 'background-mode 'light)
  ;; (set-terminal-parameter nil 'background-mode 'light)
  ;; light theme in GUI and dark in terminal
  (load-theme 'solarized t t)
  (let ((mode (if (display-graphic-p) 'light 'dark)))
    (set-frame-parameter nil 'background-mode mode)
    (set-terminal-parameter nil 'background-mode mode))
  (enable-theme 'solarized))

;; (use-package zenburn-theme)


;; (load-theme 'adwaita)
;; (load-theme 'deeper-blue)
;; (load-theme 'dichromacy)
;; (load-theme 'leuven)
;; (load-theme 'light-blue)
;; (load-theme 'manoj-dark)
;; (load-theme 'misterioso)
;; (load-theme 'monokai)
;; (load-theme 'smart-mode-line-dark)
;; (load-theme 'smart-mode-line-light)
;; (load-theme 'smart-mode-line-respectful)
;; (load-theme 'tango)
;; (load-theme 'tango-dark)
;; (load-theme 'tsdh-dark)
;; (load-theme 'tsdh-light)
;; (load-theme 'wheatgrass)
;; (load-theme 'whiteboard)
;; (load-theme 'wombat)


(provide 'theme)
;;; theme.el ends here
