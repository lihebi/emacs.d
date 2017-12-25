;;; scholar.el --- External packages

;;; Commentary:
;; No Comments!

;;; Code:

(use-package tex
  :disabled t
  :ensure auctex
  :config
  (add-hook 'LaTeX-mode-hook
            '(lambda()
               (define-key LaTeX-mode-map (kbd "C-c ]") 'helm-bibtex)))
  ;; (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
  (add-hook 'LaTeX-mode-hook
            '(lambda()
               (add-to-list 'LaTeX-verbatim-environments "lstlisting")))
  ;; (define-key LaTeX-mode-map (kbd "C-c t") 'reftex-toc)
  (setq TeX-open-quote "\"")
  (setq TeX-close-quote "\"")
  (if (string= system-type "darwin")
      (progn
        (setq TeX-view-program-selection '((output-pdf "Skim"))))
    (setq TeX-view-program-selection '((output-pdf "PDF Tools"))))
  ;; supporting indentation of [] in LaTeX mode
  (defun TeX-brace-count-line ()
    "Count number of open/closed braces."
    (save-excursion
      (let ((count 0) (limit (line-end-position)) char)
        (while (progn
                 (skip-chars-forward "^{}[]\\\\" limit)
                 (when (and (< (point) limit) (not (TeX-in-comment)))
                   (setq char (char-after))
                   (forward-char)
                   (cond ((eq char ?\{)
                          (setq count (+ count TeX-brace-indent-level)))
                         ((eq char ?\})
                          (setq count (- count TeX-brace-indent-level)))
                         ((eq char ?\[)
                          (setq count (+ count TeX-brace-indent-level)))
                         ((eq char ?\])
                          (setq count (- count TeX-brace-indent-level)))
                         ((eq char ?\\)
                          (when (< (point) limit)
                            (forward-char)
                            t))))))
        count)))
  )

(load "~/.emacs.d/org-ref-conf")

(use-package pdf-tools
  ;; :disabled t
  ;; :defer t
  :init
  ;; (setq pdf-info-epdfinfo-program "/home/hebi/.emacs.d/straight/build/pdf-tools/epdfinfo")
  :config
  ;; load this on demand!
  (pdf-tools-install)
  (setq pdf-view-resize-factor 1.03)
  (defun pdf-view-fit-paper(number)
    ;; using P for horizontal reading
    ;; using C-u P for vertical reading
    (interactive "p")
    (if (= number 1)
        (progn
          ;; landscape
          (setq pdf-view-display-size 1.53)
          (image-set-window-vscroll 6))
      (progn
        ;; portrait
        (setq pdf-view-display-size 2.05)
        (image-set-window-hscroll 11)))
    (pdf-view-redisplay t))
  (define-key pdf-view-mode-map (kbd "P") 'pdf-view-fit-paper))

;;; scholar.el ends here
