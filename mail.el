;;; mail.el --- Mail configure file

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Send mail setting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq user-full-name "Hebi Li")
(setq user-mail-address "lihebi.com@gmail.com")

;; These are the configure to use Emacs to send the mail directly
;;
;; (setq smtpmail-smtp-server "smtp.gmail.com")
;; (setq smtpmail-smtp-service 587)
;; (setq send-mail-function 'smtpmail-send-it)
;; (setq message-send-mail-function 'smtpmail-send-it)

;; To use the msmtp version, you must configure the .msmtprc file for
;; the correct authinfo to use. It will read the evolope FROM field to
;; decide which account to use.
(setq send-mail-function 'message-send-mail-with-sendmail)
(setq message-send-mail-function 'message-send-mail-with-sendmail)
;; we substitute sendmail with msmtp
(setq sendmail-program "msmtp")
;;need to tell msmtp which account we're using
;; (setq message-sendmail-extra-arguments '("-a" "cymail"))
;; it automatically add -f, which seems to be interpreted as --from on
;; msmtp side, and it cannot be used together with
;; --read-envelope-from
(setq message-sendmail-f-is-evil 't)
;; msmtp will decide which account to use based on the from field
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq gnus-posting-styles
      '((".*"
         (signature "Hebi")
         (address "lihebi.com@gmail.com")
         (name "Hebi Li"))
        (".*cymail.*"
         (address "hebi@iastate.edu")
         (signature "Hebi"))
        (".*lihebicom.*"
         (address "lihebi.com@gmail.com")
         (signature "Hebi"))))
(setq message-citation-line-function 'message-insert-formatted-citation-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read mail setting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gnus-visible-headers "^From:\\|^Subject:\\|^To:\\|^Cc:\\|^Date:\\|^User-Agent:")

(setq gnus-select-method
      '(nntp "news.gmane.org"))

(setq gnus-secondary-select-methods
      '((nnimap "lihebicom"
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

;; make them always visible
(setq gnus-permanently-visible-groups
      "INBOX\\|nnfolder\\+archive.*\\|nndraft:drafts\\|nnvirtual:cymail-virt\\|nnvirtual:lihebicom-virt")


(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-number)))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(when window-system
  (setq gnus-sum-thread-tree-indent "  ")
  (setq gnus-sum-thread-tree-root "")          ;; "● ")
  (setq gnus-sum-thread-tree-false-root "")    ;; "◯ ")
  (setq gnus-sum-thread-tree-single-indent "") ;; "◎ ")
  (setq gnus-sum-thread-tree-vertical        "│")
  (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
  (setq gnus-sum-thread-tree-single-leaf     "╰─► "))

(gnus-add-configuration
 '(article
   (horizontal 1.0
             (summary 0.5 point)
             (article 1.0))))

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

;;; mail.el ends here
