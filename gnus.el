;;; gnus.el --- Gnus configure file

;;; Commentary:

;;; Code:

(setq gnus-select-method
      '(nntp "news.gmane.org"))

(setq gnus-secondary-select-methods
      '((nnimap "gmail"
                (nnimap-address "imap.gmail.com")
                (nnimap-server-port "993")
                (nnimap-stream ssl)
                (nnimap-authinfo-file "~/.authinfo"))
        (nnimap "cymail"
                (nnimap-address "imap.gmail.com")
                (nnimap-server-port "993")
                (nnimap-stream ssl)
                (nnimap-authinfo-file "~/.authinfo"))))

(setq gnus-read-newsrc-file nil)
(setq gnus-save-newsrc-file nil)
;; this is no longer needed because I'm configuring gnus not read or
;; write newsrc file
;; (setq gnus-startup-file "~/.emacs.d/newsrc")

;; make them always visible
(setq gnus-permanently-visible-groups "INBOX\\|nnfolder\\+archive.*\\|nndraft:drafts\\|nnvirtual:cymail-virt")
(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-number)))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
;; (add-hook 'gnus-group-mode-hook 'hl-line-mode)
;; (add-hook 'gnus-summary-mode-hook 'hl-line-mode)

(gnus-add-configuration
 '(article
   (horizontal 1.0
               (vertical 50 (group 1.0))
               (vertical 1.0
                         (summary 0.35 point)
                         (article 1.0)))))

(gnus-add-configuration
 '(summary
   (horizontal 1.0
               (vertical 50 (group 1.0))
               (vertical 1.0 (summary 1.0 point)))))


;; from https://eschulte.github.io/emacs-starter-kit/starter-kit-gnus.html
;;; bbdb

;; (use-package bbdb
;;   :init
;;   ;; (require 'bbdb-autoloads)
;;   (setq
;;    bbdb-file "~/.bbdb"
;;    bbdb-offer-save 'auto
;;    bbdb-notice-auto-save-file t
;;    bbdb-expand-mail-aliases t
;;    bbdb-canonicalize-redundant-nets-p t
;;    bbdb-always-add-addresses t
;;    bbdb-complete-name-allow-cycling t
;;    ))

(when window-system
  (setq gnus-sum-thread-tree-indent "  ")
  (setq gnus-sum-thread-tree-root "")          ;; "● ")
  (setq gnus-sum-thread-tree-false-root "")    ;; "◯ ")
  (setq gnus-sum-thread-tree-single-indent "") ;; "◎ ")
  (setq gnus-sum-thread-tree-vertical        "│")
  (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
  (setq gnus-sum-thread-tree-single-leaf     "╰─► "))

(setq gnus-user-date-format-alist
      (quote (((gnus-seconds-today) .
               "Today      @ %H:%M")
              ((+ 86400 (gnus-seconds-today)) .
               "Yesterday  @ %H:%M")
              ((gnus-seconds-year) . "%a %b %d @ %H:%M")
              (t . "%a %b %d %Y at %H:%M"))))

;; http://groups.google.com/group/gnu.emacs.gnus/browse_thread/thread/a673a74356e7141f
(setq gnus-summary-line-format
      (concat
       "%U%R%z"
       "%3{│%}" "%1{%&user-date;%}" "%3{│%}" ;; date
       "  "
       "%4{%-20,20f%}"               ;; name
       "  "
       "%3{│%}"
       ;; " "
       "%B"
       "%s\n"))

;;; gnus.el ends here
