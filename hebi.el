;;; hebi.el -- my specific config

;;; Commentary:
;;; Code:

(defface my-face
  '((t :foreground "red"))
  ""
  :group 'hebi)

(defvar my-face 'my-face)


(defun hebi-add-keyword-to-mode (mode)
  ;; ((HEBI: hello) world)
  ;; multiline (HEBI: sfd)
  ;; jdfsi )
  (font-lock-add-keywords
   mode
   '(("(HEBI: [^)]*\n?[^)]*)" 0 'my-face prepend))))

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
;; (define-key dired-mode-map [mouse-2] 'dired-mouse-find-file)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hebi-check-git-repo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode hebi-repo-mode org-mode "HebiRepo"
  "A mode for displaying the status of repos I care.")

(defcustom hebi-repo-list ()
  "A list of repos to check."
  :group 'hebi-repo
  :type '(repeat (file)))


(defun hebi-check-git-repos-hard()
  ;; checking of the status of a list of git repos, with update of remote
  (interactive)
  (hebi-check-git-repos-helper t))

(defun hebi-check-git-repos()
  ;; checking of the status of a list of git repos, without update of remote
  (interactive)
  (hebi-check-git-repos-helper))

(defun hebi-check-git-repos-helper(&optional hard)
  ;; check two things
  ;; 1. anything to stage, commmit, push?
  ;; 2. anything to pull?
  ;; (setq repo "~/github/test-dirty")
  ;; TODO make the buffer readonly just like magit buffer
  ;; TODO allow refresh by g
  ;; TODO allow better switch buffer support
  (let ((buf (get-buffer-create "*hebi-repo*")))
    (switch-to-buffer-other-window buf)
    (hebi-repo-mode)
    ;; (read-only-mode)
    (erase-buffer)
    (dolist (repo hebi-repo-list)
      (insert "* Status for repo: " repo "\n")
      (when (file-exists-p repo)
        (insert (shell-command-to-string (concat "cd " repo "&& git status --porcelain")))
        (if hard
            (shell-command (concat "cd " repo " && git remote update")))
        (insert (shell-command-to-string (concat "cd " repo "&& git status -uno | grep -E 'behind|diverge'"))))
      (insert "\n"))))

;; TODO these repos and projectile bookmarks are duplicate, should
;; only get one copy
(setq hebi-repo-list
      '("~/github/note"
        "~/github/wiki"
        ;; "~/github/test-dirty"
        ;; "~/github/test-stage"
        "~/github/bibliography"
        "~/github/helium"
        "~/github/builder-paper"
        "~/github/helium-paper"
        "~/.emacs.d"
        "~/.hebi"
        "~/.stumpwm.d"
        "~/github/leetcode"
        "~/github/docker-files"
        "~/github/benchmark"
        "~/github/papers"
        "~/github/arch-server"
        "~/github/arch-desktop"
        "~/github/arch-helium"
        ))


(defun hebi-build-rate()
  "calculate build rate"
  (interactive)
  (let ((suc (count-matches "Success"))
        (fail (count-matches "Failure")))
    (goto-char (point-min))
    (insert "Build Rate: ")
    (insert (format "(/ %d.0 (+ %d %d))" suc suc fail))
    (insert " => ")
    ;; (insert (/ suc (+ suc fail)))
    (insert (format "%f" (/ (float suc) (+ suc fail))))
    (insert "\n")
    ))


;; (defface scroll-highlight-line-face
;;   '((t (:background "cadetblue4" :foreground "white" :weight bold)))
;;   "Font Lock mode face used to highlight line."
;;   :group 'scroll-screen)

;; (defun scroll-highlight (beg end delay)
;;   "Highlight a region temporarily."
;;   (let ((ov (make-overlay beg end)))
;;     (overlay-put ov 'face 'scroll-highlight-line-face)
;;     (sit-for delay)
;;     (delete-overlay ov)))

;; (defun my-highlight-advice (oldfunc &optional arg)
;;   "highlight first and last line."
;;   ;; get current position
;;   (let ((vscroll (window-vscroll)))
;;     ;; apply the scroll up and down function
;;     (apply oldfunc arg)
;;     ;; temporarily highlight
;;     (scroll-highlight begin end 0.5)))

;; ;; pdf tools scroll first/last line
;; (advice-add 'pdf-view-scroll-up-or-next-page :around
;;             #'my-highlight-advice)
;; (advice-add 'pdf-view-scroll-down-or-previous-page :around
;;             #'my-highlight-advice)

(defun toggle-mode-line ()
  "toggles the modeline on and off"
  (interactive) 
  (setq mode-line-format
    (if (equal mode-line-format nil)
        (default-value 'mode-line-format)) )
  (redraw-display))

(global-set-key [f12] 'toggle-mode-line)


;; Hide the echo area. It seems that it can only work when emacs starts
;; up, and cause a lot of trouble. Thus I'm not using it

;; (setq initial-frame-alist (append '((minibuffer . nil)) initial-frame-alist))
;; (setq default-frame-alist (append '((minibuffer . nil)) default-frame-alist))
;; (setq minibuffer-auto-raise nil)
;; (setq minibuffer-exit-hook '(lambda () (lower-frame)))

(defun hebi-copy-pdf-to-tmp ()
  (interactive)
  (let ((file (buffer-file-name (current-buffer))))
    (copy-file file "~/tmp/")))

(defun hebi-move-pdf-to-research-manual (newname)
  (interactive
   (let* ((file (buffer-file-name (current-buffer)))
          (filename (file-name-nondirectory file)))
     (list (read-string (format "As filename (%s): " filename)))))
  (rename-file (buffer-file-name (current-buffer))
               (concat "~/github/smart-scholar-pdfs/manual/" newname)))


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
  (interactive)
  (let ((file (buffer-file-name (current-buffer))))
    (when (string= (file-name-extension file) "pdf")
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

;; (use-package request)

;; ;;.1/' -H ': ' -F "file=@1710.11469.pdf;filename=my_new_awesome_name.pdf;type=application/pdf" 
;; (request
;;  "http://10.11.99.1/upload"
;;  :type "POST"
;;  :headers '(("Origin" . "http://10.11.99.1")
;;             ("Accept" . "*/*")
;;             ("Referer" . "http://10.11.99.1/")
;;             ("Connection" . "keep-alive"))
;;  :files '(("myfile.pdf" . ("17.pdf" :file "/home/hebi/Downloads/1710.11469.pdf")))
;;  ;; :data '(("file" . "/home/hebi/Downloads/1710.11469.pdf")
;;  ;;         ("filename" . "myname")
;;  ;;         ("type" . "application/pdf"))
;;  :sync t)



(provide 'hebi)
;;; hebi.el ends here
