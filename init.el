;;; hebi-emacs-init --- What the hack of this line?
;;; Commentary:


;;; Code:

(defmacro hook-into-modes (function mode-hooks)
  "Add FUNCTION to hooks in MODE-HOOKS."
  `(dolist (hook ,mode-hooks)
     (add-hook hook ,function)))

(defconst emacs-start-time (current-time))
(setq message-log-max 16384)

(defun emacs-d (filename)
  "Expand FILENAME relative to `user-emacs-directory'."
  (expand-file-name filename user-emacs-directory))

(load (emacs-d "packages"))

(load-theme 'zenburn t)
(load-theme 'ample t t)
(load-theme 'ample-flat t t)
(load-theme 'ample-light t t)
(load-theme 'dracula t)
(enable-theme 'ample)

;;;; Environment
(setq shell-file-name "zsh")
(add-to-list 'exec-path "/usr/local/bin")

;; set nu
(global-linum-mode 1)
;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; ignore the bell
(setq ring-bell-function 'ignore)

;; Always load newest byte code
(setq load-prefer-newer t)

(setq user-full-name "Hebi Li"
      user-mail-address "lihebi.com@gmail.com")

;; this should already there in "better default" package
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(fset 'yes-or-no-p 'y-or-n-p)

(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))

(when window-system
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; key bindings ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar hebi/keys-minor-mode-map (make-keymap)
  "My keys-minor-mode to prioritize keymap.")

;; magit
(define-key hebi/keys-minor-mode-map (kbd "C-x g") 'magit-status)
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

(define-minor-mode hebi/keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " hebi-keys" 'hebi/keys-minor-mode-map)

(hebi/keys-minor-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Deprecated
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pop to mark
;; (bind-key "C-x p" 'pop-to-mark-command)
;; (setq set-mark-command-repeat-pop t)

;; make window splitting more useful
;; (defun my/vsplit-last-buffer (prefix)
;;   "Split the window vertically and display the previous buffer."
;;   (interactive "p")
;;   (split-window-vertically)
;;   (other-window 1 nil)
;;   (if (= prefix 1)
;;       (switch-to-next-buffer)))
;; (defun my/hsplit-last-buffer (prefix)
;;   "Split the window horizontally and display the previous buffer."
;;   (interactive "p")
;;   (split-window-horizontally)
;;   (other-window 1 nil)
;;   (if (= prefix 1) (switch-to-next-buffer)))
;; (bind-key "C-x 2" 'my/vsplit-last-buffer)
;; (bind-key "C-x 3" 'my/hsplit-last-buffer)

;; replace buffer-menu with ibuffer
;; already done with "better default" package
;; (global-set-key (kbd "C-x C-b") 'ibuffer)


;; add hook for ido match in minibuffer
;; (add-hook 'ido-setup-hook 'ido-my-keys)

;; comment out because we can already do this by `C-,`
;; (defun ido-my-keys ()
;;   "Add my keybindings for ido."
;;   (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
;;   (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
;;   )

;; comment out because this is the global mark ring!
;; cycling space
;; (global-set-key (kbd "C-x C-SPC") (lambda ()
;;                                     (interactive)
;;                                     (cycle-spacing)
;;                                     (backward-delete-char-untabify 1)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; THIS IS THE END OF MY COFNIG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" default)))
 '(git-gutter:added-sign "++")
 '(git-gutter:deleted-sign "--")
 '(git-gutter:modified-sign "  "))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
