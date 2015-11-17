;;; smart-scholar.el --- Smart Scholar!

;;; Commentary:

;;; Code:

(defvar ss-red-keywords
  '(("^COMMENT\\b\\|^YEAR\\b\\|^AUTHOR\\b\\|^DOWN\\b" . hebi-red-face)))

(defvar ss-green-keywords
  '(("^PROBLEM\\b\\|^CITE\\b\\|^CONF\\b" . hebi-green-face)
    ("^CONTRIB\\b\\|^TECHNIQUE\\b" . hebi-green-face)
    ("^RELATED\\b\\|^POINT\\b" . hebi-green-face)))

(defvar ss-yellow-keywords
  '(("^BENCHMARK\\b\\|^TOOL\\b\\|^UNIV\\b" . hebi-yellow-face)))

(defvar ss-cyan-keywords
  '(("^AWORD\\b\\|^ALIAS\\b\\|^SS_BEGIN\\b" . hebi-cyan-face)))

(defvar ss-link-keywords
  '(("PDF:[[:space:]]*\\([0-9a-zA-Z./:_-]*\\)" . (1 'link))
    ))

(define-minor-mode smart-scholar-minor-mode
  "Smart Scholar."
  :lighter "smart-scholar"

  ;; (font-lock-add-keywords nil ss-kwds)
  (font-lock-add-keywords nil ss-red-keywords "end")
  (font-lock-add-keywords nil ss-green-keywords "end")
  (font-lock-add-keywords nil ss-yellow-keywords "end")
  (font-lock-add-keywords nil ss-cyan-keywords "end")
  (font-lock-add-keywords nil ss-link-keywords "end")

  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer)))))

(add-hook 'bibtex-mode-hook (lambda () (smart-scholar-minor-mode 1)))

;;; smart-scholar.el ends here
