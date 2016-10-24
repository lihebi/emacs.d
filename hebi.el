;;; hebi.el -- my specific config

;;; Commentary:
;;; Code:

(defface my-face
  '((t :foreground "red"))
  ""
  :group 'hebi)

(defvar my-face 'my-face)

(defun hebi-add-keyword ()
  "Add keyword for current buffer."
  (font-lock-add-keywords nil
                          '(("(HEBI: .*)" 0 'my-face prepend))))

(add-hook 'prog-mode-hook 'hebi-add-keyword)
(add-hook 'latex-mode-hook 'hebi-add-keyword)
(add-hook 'markdown-mode-hook 'hebi-add-keyword)
;; R mode is not a prog-mode ..
(add-hook 'R-mode-hook 'hebi-add-keyword)
(add-hook 'org-mode-hook 'hebi-add-keyword)

(provide 'hebi)
;;; hebi.el ends here
