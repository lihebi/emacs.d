;;; hebi.el --- provide hebi-mode

;;; Commentary:
;; highlight TODO, FIXME, HEBI, STEP

;;; Code:

(defvar hebi-keyword-font-lock)
(defun hebi-register-font-lock-keywords ()
  "."
  (setq hebi-keyword-font-lock
        '(("\\bFIXME\\b" . hebi-red-face)
          ("\\bTODO\\b" . hebi-green-face)
          ("\\bHEBI\\b.*" . hebi-red-face)
          ))
  )

(defun hebi-register-keywords ()
  "."
  ;; (mapc (lambda (mode)
  ;;         (font-lock-add-keywords
  ;;          mode
  ;;          hebi-keyword-font-lock "end"))
  ;;       hebi-modes)
  (font-lock-add-keywords nil hebi-keyword-font-lock "end")
  )

(defun hebi-remove-keywords ()
  "."
  ;; (mapc (lambda (mode)
  ;;         (font-lock-remove-keywords
  ;;          mode
  ;;          hebi-keyword-font-lock))
  ;;       hebi-modes)
  )

(defun hebi-reload-keywords ()
  "."
  (interactive)
  (hebi-register-font-lock-keywords)
  (hebi-register-keywords)
  )

;;;###autoload(defvar hebi-mode nil)
(define-minor-mode hebi-mode
  ""
  :lighter " hebi"
  :global
  :group hebi-mode
  (if hebi-mode
      (hebi-reload-keywords)
  (hebi-remove-keywords))
  )

(hook-into-modes 'hebi-mode '(prog-mode-hook latex-mode-hook))

;; (hebi-reload-keywords)

;; (add-hook 'prog-mode-hook (lambda ()
;;                             (hebi-reload-keywords)
;;                             ))
;; (add-hook 'latex-mode-hook (lambda ()
;;                              (message "hook for latex")
;;                              (hebi-reload-keywords)
;;                              ))


(provide 'hebi-mode)

;;; hebi.el ends here
