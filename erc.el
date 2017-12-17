;;; erc.el -- Experiment settings

;;; Commentary:

;;; Code:

(erc-autojoin-mode 1)


;; (setq erc-autojoin-channels-alist
;;       '(("freenode.net"
;;          ;; "#linux"
;;          ;; "#emacs"
;;          ;; "#latex"
;;          ;; "#scheme"
;;          ;; "#haskell"
         
;;          ;; "#clojure"
;;          ;; "#overtone"
;;          ;; "#debian"
;;          ;; "#gnustep" "#screen" "#freestream"
;;          ;; "#fnr" "#fnr-staff" "#ducttape" "#carvux" "#unit-e" "#isys"
;;          ;; "#fsptb"
;;          )
;;         ;; ("crystalia.net" "#crystalia")
;;         ))

;; track activities
(erc-track-mode t)
(erc-fill-mode t)
(erc-ring-mode t)
(erc-netsplit-mode t)
(erc-timestamp-mode t)

;; (setq erc-timestamp-format "[%R-%m/%d]")

;; (setq erc-user-full-name "Hebi Li")

;;; erc.el ends here
