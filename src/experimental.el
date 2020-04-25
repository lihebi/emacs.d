;;; experimental.el -- my specific config

;;; Commentary:
;;; Code:

;; list-colors-display
;; list-faces-display
(defvar hebi-red-face 'hebi-red-face)
(defvar hebi-green-face 'hebi-green-face)
(defvar hebi-yellow-face 'hebi-yellow-face)
(defvar hebi-cyan-face 'hebi-cyan-face)
(defface hebi-red-face
  '((t :foreground "red"))
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




(defun hebi-add-keyword-to-mode (mode)
  ;; ((HEBI: hello) world)
  ;; multiline (HEBI: sfd)
  ;; jdfsi )
  (font-lock-add-keywords
   mode
   '(("(HEBI: [^)]*\n?[^)]*)" 0 'hebi-red-face prepend))))

(hebi-add-keyword-to-mode 'fundamental-mode)
(hebi-add-keyword-to-mode 'prog-mode)

(defun hebi-add-keyword ()
  (interactive)
  "Add keyword for current buffer. Use in the hook of a mode."
  (hebi-add-keyword-to-mode nil))

(add-hook 'prog-mode-hook 'hebi-add-keyword)
(add-hook 'text-mode-hook 'hebi-add-keyword)
(add-hook 'latex-mode-hook 'hebi-add-keyword)
(add-hook 'markdown-mode-hook 'hebi-add-keyword)
;; R mode is not a prog-mode ..
(add-hook 'R-mode-hook 'hebi-add-keyword)
(add-hook 'org-mode-hook 'hebi-add-keyword)
(add-hook 'bibtex-mode-hook 'hebi-add-keyword)

;; highlight title in bib files
(font-lock-add-keywords
 'bibtex-mode
 '(("\\btitle[[:space:]]*=[[:space:]]*{\\(.*\\)}" 1 'hebi-red-face prepend)))

;; (font-lock-add-keywords
;;  'c-mode
;;  '(("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend)
;;    ("\\<\\(and\\|or\\|not\\)\\>" . font-lock-keyword-face)))

;; (font-lock-add-keywords
;;  'emacs-lisp-mode
;;  '(("Ludovic" 0 'hebi-red-face prepend)))

;; The gnus-summary-mode is very weird, when adding keywords, the
;; default face for read marking is gone. It is not defined using
;; define-derived-mode as usual, that could be the reason
;;
;; (defun hebi-gnus-add-keywords ()
;;   "Add keywords for highlighting."
;;   (font-lock-add-keywords
;;    nil
;;    '(("Ludovic" . 'hebi-red-face))
;;    t))

;; (add-hook 'gnus-summary-mode-hook 'hebi-gnus-add-keywords)
;; (remove-hook 'gnus-summary-mode-hook 'hebi-gnus-add-keywords)

;; Not working
;; (font-lock-add-keywords
;;  'gnus-summary-mode
;;  '(("Ludovic" 0 'hebi-red-face prepend)))



(defun toggle-mode-line ()
  "toggles the modeline on and off"
  (interactive) 
  (setq mode-line-format
    (if (equal mode-line-format nil)
        (default-value 'mode-line-format)) )
  (redraw-display))

(global-set-key [f12] 'toggle-mode-line)

(defun hebi-copy-pdf-to-tmp ()
  (interactive)
  (let ((file (buffer-file-name (current-buffer))))
    (copy-file file "~/tmp/")))

(defun hebi-move-pdf-to-research-manual (newname)
  (interactive
   (let* ((file (buffer-file-name (current-buffer)))
          (filename (file-name-nondirectory file)))
     (list (read-string (format "As filename (%s): " filename)))))
  (when (not (file-exists-p "~/git/smart-scholar-pdfs/manual/"))
    (mkdir "~/git/smart-scholar-pdfs/manual/" t))
  (rename-file (buffer-file-name (current-buffer))
               (concat "~/git/smart-scholar-pdfs/manual/" newname)))

(defun hebi-trans (word)
  "Translate WORD into chinese, return result."
  (interactive
   ;; "sWord: "
   (list
    ;; get current word, use as default
    ;; prompt minibuffer
    (let ((cur (current-word)))
      (read-string (format "Word (%s): " cur)
                            nil nil
                            cur))))
  (let ((res (string-trim
              (shell-command-to-string
               (concat "trans"
                       " -b"
                       " :zh"
                       " " word)))))
    (prin1 res)
    (kill-new res)))

(global-set-key (kbd "C-c h t") 'hebi-trans)



(defun hebi-upload-to-remarkable ()
  ;; According to
  ;; https://support.remarkable.com/hc/en-us/articles/115005700389-7-4-Transfer-files-with-a-USB-cable-to-reMarkable
  ;; I need to enable USB web interface in storage setting. Otherwise
  ;; the curl request will be refused.
  ;;
  ;; I forget where I get this, but some useful scripts:
  ;; https://github.com/reHackable/awesome-reMarkable
  ;;
  ;; TODO I would love to crop the paper before sending it to
  ;; remarkable, so that I don't need to crop on the device. The
  ;; https://github.com/GjjvdBurg/arxiv2remarkable uses pdfcrop. I
  ;; tried it, it works, but the quality of the cropping seems to be
  ;; bad
  (interactive)
  (let ((file (buffer-file-name (current-buffer))))
    (when (member (file-name-extension file) '("pdf" "epub"))
      (shell-command
       ;; TODO remote file name
       (concat "curl 'http://10.11.99.1/upload'"
               " -H 'Origin: http://10.11.99.1'"
               " -H 'Accept: */*'"
               " -H 'Referer: http://10.11.99.1/'"
               " -H 'Connection: keep-alive'"
               " -F \"file=@" file ";filename="
               (file-name-nondirectory file)
               ";type=application/pdf\"")))))
(global-set-key (kbd "C-c h m") 'hebi-upload-to-remarkable)

;; copy region as single line
(defun hebi-copy-as-single-line()
  (interactive)
  (kill-ring-save 0 0 t)
  (with-temp-buffer
    (yank)
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (replace-match ""))
    (kill-ring-save (point-min) (point-max))))
