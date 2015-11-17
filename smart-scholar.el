;;; smart-scholar.el --- Smart Scholar!

;;; Commentary:

;;; Code:

;; list-colors-display
;; list-faces-display
(defvar smart-scholar-red 'smart-scholar-red)
(defvar smart-scholar-green 'smart-scholar-green)
(defvar smart-scholar-yellow 'smart-scholar-yellow)
(defvar smart-scholar-cyan 'smart-scholar-cyan)
(defface smart-scholar-red
  '((t :background "black" :foreground "red"))
  ""
  :group 'smart-scholar-faces)

(defface smart-scholar-green
  '((t :background "black" :foreground "green"))
  ""
  :group 'smart-scholar-faces)

(defface smart-scholar-cyan
  '((t :foreground "cyan"))
  ""
  :group 'smart-scholar-faces)

(defface smart-scholar-yellow
  '((t :foreground "yellow"))
  ""
  :group 'smart-scholar-faces)

(defvar ss-red-keywords
  '(("^COMMENT\\b\\|^YEAR\\b\\|^AUTHOR\\b\\|^DOWN\\b" . smart-scholar-red)))

(defvar ss-green-keywords
  '(("^PROBLEM\\b\\|^CITE\\b\\|^CONF\\b" . smart-scholar-green)
    ("^CONTRIB\\b\\|^TECHNIQUE\\b" . smart-scholar-green)
    ("^RELATED\\b\\|^POINT\\b" . smart-scholar-green)))

(defvar ss-yellow-keywords
  '(("^BENCHMARK\\b\\|^TOOL\\b\\|^UNIV\\b" . smart-scholar-yellow)))

(defvar ss-cyan-keywords
  '(("^AWORD\\b\\|^ALIAS\\b\\|^SS_BEGIN\\b" . smart-scholar-cyan)))

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

;; (define-derived-mode smart-scholar-mode bibtex-mode
;;   "Smart Scholar mode"
;;   (font-lock-add-keywords nil ss-red-keywords)
;;   (font-lock-add-keywords nil ss-green-keywords)
;;   (font-lock-add-keywords nil ss-yellow-keywords)
;;   (font-lock-add-keywords nil ss-cyan-keywords)
;;   )

;; (font-lock-add-keywords 'bibtex-mode ss-red-keywords)

(add-hook 'bibtex-mode-hook (lambda () (smart-scholar-minor-mode 1)))

;; (defvar kwds-hebi)
(defvar kwds-hebi
      '(("Sin\\|Cos\\|Sum" . font-lock-function-name-face)
        ("Pi\\|Infinity" . font-lock-constant-face)
        ("hebi\\|lianzhu" . smart-scholar-red)
        ))

(define-minor-mode my-minor-mode
  "need doc"
  :lighter "blah"

  (font-lock-add-keywords nil ss-red-keywords)
  (font-lock-add-keywords nil kwds-hebi)
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer)))))

(defvar hebi-keywords
  '(("\\bHEBI\\b.*" . 'smart-scholar-red)))

(font-lock-add-keywords nil hebi-keywords)

;;; smart-scholar.el ends here
