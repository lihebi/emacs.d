;;; bindings.el --- key-bindings
;;; Commentary:

;;; Code:

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x O")
                (lambda ()
                  (interactive)
                  (other-window -1)))

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M")
                (lambda () (interactive) (eshell t)))

;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x M-m") 'shell)

;; kill lines backward
(global-set-key (kbd "C-<backspace>")
                (lambda ()
                  (interactive)
                  (kill-line 0)
                  (indent-according-to-mode)))

(global-set-key (kbd "s-<return>")
                (lambda()
                  (interactive)
                  (move-end-of-line 1)
                  (newline)))

(global-set-key (kbd "S-s-<return>")
                (lambda()
                  (interactive)
                  (move-end-of-line 0)
                  (newline)))

;; backspace
(global-set-key (kbd "C-h")
                (lambda ()
                  (interactive)
                  (backward-delete-char 1)))

(global-set-key (kbd "<f1>") 'help-command)

;; join line
(global-set-key (kbd "C-j")
                (lambda()
                  (interactive)
                  (join-line -1)))

;; other window
(global-set-key (kbd "C-<return>") 'other-window)
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (shell-command "make")
                               ))

;;; bindings.el ends here
