;;; Package -- Summary
;;; Commentary:
;;; Code:

(defun helium-get-slice-linums()
  "Get the content of slice file that match current buffer."
  "Return a list of linums in slice."
    (let* ((old-buf (current-buffer))
           (old-buf-name (buffer-name (current-buffer))) ; this may not be different from file name
           ;; (old-name (buffer-file-name old-buf)) ; this is absolute path
           ;; (true-name buffer-file-truename) ; this just contains ~
           (ret ()))
      (with-temp-buffer
        (insert-file-contents
         (read-file-name "file name: "))
        ;; (buffer-substring (point-min) (point-max))
        (message old-buf-name)
        (goto-char (point-min))
        (while (not (eobp))
          ;; (copy-region-as-kill left right)
          ;; (copy-region-as-kill (line-beginning-position) (point))
          ;; (copy-region-as-kill (point) (line-end-position))
          (let* ((whole-line (buffer-substring (line-beginning-position) (line-end-position)))
                 (splits (split-string whole-line))
                 (filename (nth 0 splits))
                 (linum (nth 1 splits))
                 (splits (split-string filename "/"))
                 (filename (nth (- (length splits) 1) splits)))
            (if (string= filename old-buf-name)
                (push (string-to-number linum) ret)))
          ;; (search-forward ":")
          ;; (let* ((filename
          ;;        (progn
          ;;          (backward-char)
          ;;          (buffer-substring (line-beginning-position) (point))))
          ;;       (linum
          ;;        (progn
          ;;          (forward-char)
          ;;          (buffer-substring (point) (line-end-position))))
          ;;       (splits (split-string filename "/"))
          ;;       (filename (nth (- (length splits) 1) splits)))
          ;;   ;; (print (concat "filename: " filename))
          ;;   ;; (print (concat "linum: " linum))
          ;;   (if (string= filename old-buf-name)
          ;;       (push (string-to-number linum) ret)))
          (forward-line))
        (append ret))))


(defun create-overlays(linums)
  "create overlays for each line in slice linums."
  (cl-loop for linum in linums
           collect (progn
                     (goto-char (point-min))
                     (forward-line linum)
                     (make-overlay (point) (line-end-position)))))

(defface slice-highlight-face '((((min-colors 88) (class color))
			   :foreground "white" :background "blue1")
			  (((class color))
			   :foreground "white" :background "blue")
			  (t :slant italic))
  "Face used to highlight slice."
  :group 'helium-faces
  )


(defvar helium-slice-overlay)
(setq helium-slice-overlay ())

;; face to use: custom-changed
(defun helium-highlight-slice()
  "highlight lines in slice."
  (interactive)
  (helium-hightlight-dismiss)
  ;; (make-local-variable 'helium-slice-overlay)
  (let ((slices (helium-get-slice-linums)))
    ;; (print slices)
    (setq helium-slice-overlay (create-overlays slices))
    ;; (print helium-slice-overlay)
    (cl-loop for overlay in helium-slice-overlay do
             (overlay-put overlay 'face 'slice-highlight-face))))

(defun helium-hightlight-dismiss()
  "dismiss the highlight posed by helium"
  (interactive)
  (if (and (boundp 'helium-slice-overlay) (not (null helium-slice-overlay)))
      (cl-loop for overlay in helium-slice-overlay do
               (delete-overlay overlay))))

;; buffer local variable?
;; (defvar helium-slice-overlay)
;; (setq helium-slice-overlay ())
;; (setq helium-slice-overlay (make-overlay (point-min) (point-max)))

;; (let ((overlay (make-overlay (point-min) (point-max)))
;;       )
;;   (overlay-put overlay 'face 'custom-changed)
;;   (delete-overlay overlay)
;;   )

;;; helium-slice-highlighter.el ends here
