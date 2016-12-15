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


(provide 'hebi)
;;; hebi.el ends here
