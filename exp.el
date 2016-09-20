;;; exp.el -- Experiment settings

;;; Commentary:

;;; Code:

(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
      '(("freenode.net"
         "#emacs" "#latex" "#scheme" "#haskell"
         "#clojure" "#overtone" "#debian"
         ;; "#gnustep" "#screen" "#freestream"
         ;; "#fnr" "#fnr-staff" "#ducttape" "#carvux" "#unit-e" "#isys"
         ;; "#fsptb"
         )
        ;; ("crystalia.net" "#crystalia")
        ))

;; track activities
(erc-track-mode t)

(erc-fill-mode t)
(erc-ring-mode t)
(erc-netsplit-mode t)
(erc-timestamp-mode t)
(setq erc-timestamp-format "[%R-%m/%d]")

(setq erc-user-full-name "Hebi Li")


(setq gnus-select-method '(nntp "news.gmane.org"))
(setq gnus-use-adaptive-scoring t)
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;;; exp.el ends here
