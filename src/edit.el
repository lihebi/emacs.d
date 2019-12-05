(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))


(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)


(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))

(global-set-key (kbd "C-c C-SPC") 'unpop-to-mark-command)

;;keep cursor at same position when scrolling
;; (setq scroll-preserve-screen-position nil)

;;scroll window up/down by one line
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))

;; kill lines backward
(global-set-key (kbd "C-<backspace>")
                (lambda ()
                  (interactive)
                  (kill-line 0)
                  (indent-according-to-mode)))


;; M-^: back
;; C-^ forward
;; join line
(global-set-key (kbd "C-^")
                (lambda()
                  (interactive)
                  (join-line -1)))
;; (define-key global-map (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "RET") 'newline-and-indent)


;; it looks like this is not widely used. I'll try vterm.
(use-package shell-switcher
  :config
  (setq shell-switcher-mode t))


;; insert-pair-alist
(use-package wrap-region
  :config
  (wrap-region-add-wrapper "=" "=")
  (wrap-region-add-wrapper "/" "/")
  (wrap-region-add-wrapper "*" "*")
  (wrap-region-add-wrapper "$" "$")
  (wrap-region-add-wrapper "~" "~")
  (add-hook 'org-mode-hook 'wrap-region-mode))


(use-package paredit
  ;; I'm using paredit here
  ;; smartparens is a package aims to replace paredit
  ;; it did add some functionality, but I don't like
  ;; 1. the presentation (document)
  ;; 2. lack of wrap
  ;; 3. paredit should work well,
  ;;    the only downside might be it cannot be used outside lisp mode
  :config
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook          #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
  (add-hook 'racket-mode-hook           #'enable-paredit-mode)

  (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
  (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly))

(use-package volatile-highlights
  ;; show the change after undo, yank, etc.
  :init
  (volatile-highlights-mode t))


(use-package goto-chg
  ;; goto last change in this buffer
  :bind (("C-." . goto-last-change)
         ("C-," . goto-last-change-reverse)))


(use-package guide-key
  ;; one key to rule them all
  :diminish guide-key-mode
  :config
  (progn
    (setq guide-key/popup-window-position 'bottom
          guide-key/guide-key-sequence t  ; enable for all prefixes
          guide-key/recursive-key-sequence-flag t)

    (defun guide-key/org-mode-hook ()
      (guide-key/add-local-highlight-command-regexp "org-"))
    (add-hook 'org-mode-hook 'guide-key/org-mode-hook)

    (guide-key-mode 1)))

(use-package ag
  :defer t)

(use-package ace-jump-mode
  ;; jump to a char, can select by 'abcd..'
  :bind
  (
   ("C-c h SPC" . ace-jump-mode)
   ("C-x SPC" . ace-jump-mode-pop-mark)))

(use-package yasnippet
  :init
  (progn
    (defvar yas-snippet-dirs)
    (yas-global-mode 1))
  (use-package yasnippet-snippets)
  :config
  ;; https://guix.gnu.org/manual/en/html_node/The-Perfect-Setup.html
  ;; FIXME tangled code
  (add-to-list 'yas-snippet-dirs "~/git/reading/guix/etc/snippets")
  ;;; use popup menu for yas-choose-value
  ;; it seems to be installed by default. But it is not marked built-in
  (use-package popup
    :disabled
    :config
    ;; (require 'popup)
    ;; add some shotcuts in popup menu mode
    (define-key popup-menu-keymap (kbd "M-n") 'popup-next)
    (global-set-key (kbd "M-n") 'popup-next)
    (define-key popup-menu-keymap (kbd "TAB") 'popup-next)
    (define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
    (define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
    (define-key popup-menu-keymap (kbd "M-p") 'popup-previous)

    (defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
      (when (featurep 'popup)
        (popup-menu*
         (mapcar
          (lambda (choice)
            (popup-make-item
             (or (and display-fn (funcall display-fn choice))
                 choice)
             :value choice))
          choices)
         :prompt prompt
         ;; start isearch mode immediately
         :isearch t
         )))
    (setq yas-prompt-functions '(yas-popup-isearch-prompt yas-ido-prompt yas-no-prompt))))

(use-package smartparens
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disabled
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package string-inflection
  :disabled t
  ;; cycle through CamelCase and under_line
  :bind
  ("C-c m" . string-inflection-cycle))

(use-package visual-regexp
  ;; use
  ;; vr/query-replace
  ;; vr/replace
  :disabled
  :config
  ;; commenting out these keybindings
  ;; need to remember to use vr/xxx when doing replacing
  ;; (define-key global-map (kbd "C-c r") 'vr/replace)
  ;; (define-key global-map (kbd "C-c q") 'vr/query-replace)
  ;; ;; if you use multiple-cursors, this is for you:
  ;; (define-key global-map (kbd "C-c m") 'vr/mc-mark)
  )

;; These two packages are used in fuzzy complete
(use-package fuzzy
  :disabled)
(use-package flx
  :disabled)



;; not sure if these wierd binding is what I want
(use-package expand-region
  :bind
  :disabled
  (("s-e" . er/expand-region)))

(use-package browse-kill-ring
  :defer t
  :disabled
  :config
  (browse-kill-ring-default-keybindings))

(use-package regex-tool
  :defer t
  :disabled)

;; In order to use flycheck, the checkers need to be installed. To
;; verify a checker is properly installed, use
;; flycheck-verify-checker.
;;
;; python: pylint
;;
;; Seems that the xref-find-definitions also rely on pylint (or maybe
;; just virtualenv .. when ipython is not installed system-wise)
(use-package flycheck
  :disabled
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :bind
  (("C-c c" . flycheck-buffer))
  :config
  ;; (setq flycheck-clang-args '"--std=c++11")
  ;; (setq flycheck-clang-args nil)
  ;; --std=c++11 is not working with C code.
  ;; Instead, include this in .dir-locals.el
  ;; ((c++-mode . ((flycheck-clang-args . ("--std=c++11")))))
  (add-hook 'c++-mode-hook '(lambda()
                              (setq flycheck-clang-args "--std=c++11")))
  (add-hook 'c++-mode-hook #'(lambda () (setq flycheck-gcc-language-standard "c++11")))
  (add-hook 'c-mode-hook '(lambda()
                            (setq flycheck-clang-args "")))
  ;; Add include path
  ;; I found this information by:
  ;; M-x describe-checker => found c/c++-clang
  ;; Click on it, goes to the description, along with the configurable part.
  (setq flycheck-clang-include-path (list "..")))
(use-package company
  :disabled
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :bind
  (
   ("C-;" . company-complete)
   ;; ("TAB" . company-indent-or-complete-common)
   )
  :config
  (setq company-idle-delay nil) ; do not automatically give me completion

  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  ;; (setq tab-always-indent 'complete)
  ;; (define-key company-active-map (kbd "TAB") #'company-indent-or-complete-common)
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony))

  ;; tab trigger
  (define-key company-mode-map [remap indent-for-tab-command]
    'company-indent-for-tab-command)

  (setq tab-always-indent 'complete)

  (defvar completion-at-point-functions-saved nil)

  (defun company-indent-for-tab-command (&optional arg)
    (interactive "P")
    (let ((completion-at-point-functions-saved completion-at-point-functions)
          (completion-at-point-functions '(company-complete-common-wrapper)))
      (indent-for-tab-command arg)))

  (defun company-complete-common-wrapper ()
    (let ((completion-at-point-functions completion-at-point-functions-saved))
      (company-complete-common))))


;; require installing aspell-en package
(use-package flyspell
  ;; disabling because Starting new Ispell process ... all the time
  ;; when exporting html
  :disabled
  :init
  :config
  (progn
    ;; (add-hook 'LaTeX-mode-hook '(flyspell-mode t))
    ;; TODO not working for flyspell-mode
    (setq ispell-program-name "aspell")
    (add-hook 'LaTeX-mode-hook #'turn-on-flyspell)
    (add-hook 'latex-mode-hook #'turn-on-flyspell)
    (add-hook 'org-mode-hook #'turn-on-flyspell)))


;; On NixOS, it cannot be built
;; https://github.com/akermu/emacs-libvterm/issues/115 Thus, I'm installing it
;; with Nix. In that case, vterm is automatically available. However, keeping
;; this use-package will download and build another copy and ultimately no
;; commands can be used in that terminal. Thus disabled.
(use-package vterm
  :disabled)
