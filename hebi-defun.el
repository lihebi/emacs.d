;;; hebi-defun.el --- some defuns
;;; Commentary:
;;; Code:
(defun latexmk ()
  "."
  (interactive)
  (shell-command (concat "latexmk -cd -quiet -pdf -pv -shell-escape " buffer-file-name)
                 ))

(defun latexmk-clean ()
  "."
  (interactive)
  (shell-command "latexmk -C"))

(global-set-key (kbd "<f6>") 'latexmk)

(defun timestamp ()
  "."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %a %I:%M:%S %p")))

(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "M-;") 'toggle-comment-on-line)

(provide 'hebi-defun)
;;; hebi-defun.el ends here
