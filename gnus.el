;;; gnus.el --- Gnus configure file

;;; Commentary:

;;; Code:


(defvar gnus-default-nntp-server)
(defvar gnus-select-method)
(defvar gnus-use-adaptive-scoring)
(defvar gnus-secondary-select-methods)
(setq gnus-select-method
      '(nntp "news.gmane.org"))
(setq gnus-secondary-select-methods
      '((nntp "news.gmane.org")))
(setq gnus-default-nntp-server "news.gmane.org")
(setq gnus-use-adaptive-scoring t)
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(add-hook 'gnus-group-mode-hook 'hl-line-mode)
(defvar gnus-read-newsrc-file)
(defvar gnus-save-newsrc-file)
(setq gnus-read-newsrc-file nil)
(setq gnus-save-newsrc-file nil)

(defvar gnus-summary-mode-hook)
(setq gnus-summary-mode-hook 'hl-line-mode)

;; (setq gnus-summary-line-format "%U%R %5N %6k %24&user-date; â”‚ %~(max-right 75)~(pad-right 75)S â”‚ %B %f
;; ")
;; (setq gnus-user-date-format-alist
;;       (quote (((gnus-seconds-today) . "Today at %H:%M")
;;               ((+ 86400 (gnus-seconds-today)) . "Yesterday at %H:%M")
;;               ((gnus-seconds-year) . "%a %b %d at %H:%M")
;;               (t . "%a %b %d %Y at %H:%M"))))

;; from https://eschulte.github.io/emacs-starter-kit/starter-kit-gnus.html
;;; bbdb

(use-package bbdb
  :init
  ;; (require 'bbdb-autoloads)
  (setq
   bbdb-file "~/.bbdb"
   bbdb-offer-save 'auto
   bbdb-notice-auto-save-file t
   bbdb-expand-mail-aliases t
   bbdb-canonicalize-redundant-nets-p t
   bbdb-always-add-addresses t
   bbdb-complete-name-allow-cycling t
   ))

;; http://groups.google.com/group/gnu.emacs.gnus/browse_thread/thread/a673a74356e7141f
(when window-system
  (setq gnus-sum-thread-tree-indent "  ")
  (setq gnus-sum-thread-tree-root "") ;; "● ")
  (setq gnus-sum-thread-tree-false-root "") ;; "◯ ")
  (setq gnus-sum-thread-tree-single-indent "") ;; "◎ ")
  (setq gnus-sum-thread-tree-vertical        "│")
  (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
  (setq gnus-sum-thread-tree-single-leaf     "╰─► "))
(setq gnus-summary-line-format
      (concat
       "%0{%U%R%z%}"
       "%3{│%}" "%1{%&user-date;%}" "%3{│%}" ;; date
       "  "
       "%4{%-20,20f%}"               ;; name
       "  "
       "%3{│%}"
       " "
       "%1{%B%}"
       "%s\n"))
(setq gnus-summary-display-arrow t)

;;; gnus.el ends here
