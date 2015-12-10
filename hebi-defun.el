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


(provide 'hebi-defun)
;;; hebi-defun.el ends here
