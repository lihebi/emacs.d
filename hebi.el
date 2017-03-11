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
  (font-lock-add-keywords
   nil
   '(("(HEBI: .*)" 0 'my-face prepend))))

(add-hook 'prog-mode-hook 'hebi-add-keyword)
(add-hook 'latex-mode-hook 'hebi-add-keyword)
(add-hook 'markdown-mode-hook 'hebi-add-keyword)
;; R mode is not a prog-mode ..
(add-hook 'R-mode-hook 'hebi-add-keyword)
(add-hook 'org-mode-hook 'hebi-add-keyword)
(add-hook 'bibtex-mode-hook 'hebi-add-keyword)



;; highlight title in bib files
(font-lock-add-keywords
 'bibtex-mode
 '(("\\btitle[[:space:]]*=[[:space:]]*{\\(.*\\)}" 1 'my-face prepend)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; list-colors-display
;; list-faces-display
(defvar hebi-red-face 'hebi-red-face)
(defvar hebi-green-face 'hebi-green-face)
(defvar hebi-yellow-face 'hebi-yellow-face)
(defvar hebi-cyan-face 'hebi-cyan-face)
(defface hebi-red-face
  '((t :background "black" :foreground "red"))
  ""
  :group 'hebi-faces)

(defface hebi-green-face
  '((t :background "black" :foreground "green"))
  ""
  :group 'hebi-faces)

(defface hebi-cyan-face
  '((t :foreground "cyan"))
  ""
  :group 'hebi-faces)

(defface hebi-yellow-face
  '((t :foreground "yellow"))
  ""
  :group 'hebi-faces)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some functions useful for leetcode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get the binary format of an integer
;; (defun hebi-get-binary(num)
;;   (reverse (cl-loop
;;    until (= num 0)
;;    collect (logand num 1)
;;    do
;;    (setq num (lsh num -1)))))

;; (hebi-get-binary 6) ; => (1 1 0)



;; copy region as single line
(defun hebi-copy-as-single-line()
  (interactive)
  (kill-ring-save 0 0 t)
  (with-temp-buffer
    (yank)
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (replace-match ""))
    (kill-ring-save (point-min) (point-max))
    ))


;; mouse-1 in dired-file-file
;; click in Dired buffer would open a new window for both dir and files
;; this function would change it to be in the same window
;; the file is copied from dired-mouse-find-file-other-window
;; the only changed part is the find-file-other-window and dired-other-window calls near the end
;; to handle file and directory respectively
(defun dired-mouse-find-file (event)
  "In Dired, visit the file or directory name you click on."
  (interactive "e")
  (let (window pos file)
    (save-excursion
      (setq window (posn-window (event-end event))
	    pos (posn-point (event-end event)))
      (if (not (windowp window))
	  (error "No file chosen"))
      (set-buffer (window-buffer window))
      (goto-char pos)
      (setq file (dired-get-file-for-visit)))
    (if (file-directory-p file)
	(or (and (cdr dired-subdir-alist)
		 (dired-goto-subdir file))
	    (progn
	      (select-window window)
	      ;; (dired-other-window file)
              (dired-find-file)
              ))
      (select-window window)
      (find-file-other-window (file-name-sans-versions file t)))))

;; although the event is mouse-1, the command called is not this
;; it is an "up event" that calls the dired-mouse-find-file
;; and that is bound to mouse-2, not know why, but this works
(define-key dired-mode-map [mouse-2] 'dired-mouse-find-file)


(provide 'hebi)
;;; hebi.el ends here
