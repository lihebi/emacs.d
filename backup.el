;; obsolete codes

(defun toggle-transparency ()
  "Toggle the transparency of the frame."
  ;; Since all interactive commands should have documentation,
  ;; I'm adding this, but transparency for an editor is really a BAD idea.
  (interactive)
  (if (/=
       (cadr (frame-parameter nil 'alpha))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(90 90))))
;; (global-set-key (kbd "C-c t") 'toggle-transparency)
