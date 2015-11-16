;;; bindings.el --- key-bindings
;;; Commentary:

;;; Code:

(defvar hebi/keys-minor-mode-map (make-keymap)
  "My keys-minor-mode to prioritize keymap.")

(define-key hebi/keys-minor-mode-map (kbd "C-=") 'text-scale-increase)
(define-key hebi/keys-minor-mode-map (kbd "C--") 'text-scale-decrease)
(define-key hebi/keys-minor-mode-map (kbd "C-x O")
  (lambda ()
    (interactive)
    (other-window -1)))

;; Start eshell or switch to it if it's active.
(define-key hebi/keys-minor-mode-map (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(define-key hebi/keys-minor-mode-map (kbd "C-x M")
  (lambda () (interactive) (eshell t)))

;; Start a regular shell if you prefer that.
(define-key hebi/keys-minor-mode-map (kbd "C-x M-m") 'shell)

;; kill lines backward
(define-key hebi/keys-minor-mode-map (kbd "C-<backspace>")
  (lambda ()
    (interactive)
    (kill-line 0)
    (indent-according-to-mode)))

(define-key hebi/keys-minor-mode-map (kbd "s-<return>")
  (lambda()
    (interactive)
    (move-end-of-line 1)
    (newline)))

(define-key hebi/keys-minor-mode-map (kbd "S-s-<return>")
  (lambda()
    (interactive)
    (move-end-of-line 0)
    (newline)))

;; backspace
(define-key hebi/keys-minor-mode-map (kbd "C-h")
  (lambda ()
    (interactive)
    (backward-delete-char 1)))

(define-key hebi/keys-minor-mode-map (kbd "<f1>") 'help-command)

;; join line
(define-key hebi/keys-minor-mode-map (kbd "C-j")
  (lambda()
    (interactive)
    (join-line -1)))

;; other window
(define-key hebi/keys-minor-mode-map (kbd "C-<return>") 'other-window)

(define-minor-mode hebi/keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " hebi-keys" 'hebi/keys-minor-mode-map)

(hebi/keys-minor-mode 1)

;;; bindings.el ends here
