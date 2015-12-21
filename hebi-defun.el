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

(global-set-key (kbd "C-M-;") 'toggle-comment-on-line)


(defun toggle-window-split ()
  "."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x 9") 'toggle-window-split)

(provide 'hebi-defun)
;;; hebi-defun.el ends here
