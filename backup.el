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


;; org configs

(set-face-attribute 'org-level-1 nil
                    :height 1.3 :weight 'bold :overline "#A7A7A7"
                    :foreground "#3C3C3C" :background "#F0F0F0")
(set-face-attribute 'org-level-2 nil
                    :height 1.0 :weight 'bold :overline "#123555"
                    :foreground "#123555" :background "#E5F4FB")
(set-face-attribute 'org-level-3 nil
                    :height 1.0 :weight 'bold
                    :foreground "#005522" :background "#EFFFEF")
(set-face-attribute 'org-level-4 nil
                    :height 1.0 :weight 'bold :slant 'normal
                    :foreground "#EA6300")

(set-face-attribute 'org-block nil :inherit 'shadow
                    :background "#FFFFE0")
(set-face-attribute 'org-block-begin-line nil :inherit 'org-meta-line
                    :underline "#A7A6AA" :foreground "#555555" :background "#E2E1D5")
(set-face-attribute 'org-block-end-line nil :inherit 'org-meta-line
                    :overline "#A7A6AA" :foreground "#555555" :background "#E2E1D5")
(set-face-attribute 'org-document-title nil
                    :family "Sans Serif" :height 1.8 :weight 'bold :foreground "black")
(set-face-attribute 'org-document-info-keyword nil
                    :foreground "#008ED1" :background "#EAEAFF")
