;;; bindings.el --- key-bindings
;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x O")
                (lambda ()
                  (interactive)
                  (other-window -1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; efficiency
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; kill lines backward
(global-set-key (kbd "C-<backspace>")
                (lambda ()
                  (interactive)
                  (kill-line 0)
                  (indent-according-to-mode)))

;; join line
(global-set-key (kbd "C-j")
                (lambda()
                  (interactive)
                  (join-line -1)))

;; (define-key global-map (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "RET") 'newline-and-indent)

;; when split window right, swith to that window
(global-set-key (kbd "C-x 3") (lambda ()
                                (interactive)
                                (split-window-right)
                                (other-window 1)
                                ))

(global-set-key (kbd "C-c ]") 'org-ref-helm-insert-cite-link)

;; switch between source and header file
(global-set-key (kbd "C-x C-o") 'ff-find-other-file)

(global-set-key (kbd "C-c r") 'replace-string)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode specific key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (eval-after-load 'dired
;;   '(define-key dired-mode-map (kbd "C-u") 'dired-up-directory))

;;; bindings.el ends here
