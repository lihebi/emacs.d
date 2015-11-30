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

(provide 'hebi-defun)
;;; hebi-defun.el ends here
