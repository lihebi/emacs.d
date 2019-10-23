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

;; remove because it cause wired color scheme in dired buffer
;; (setq dired-listing-switches "-alh")

(set-register ?s '(file . "~/git/note/stack.org"))


;; narrow/widen
;; narrow-to-defun
;; narrow-to-region
;; widen


;; C/C++
;; hs-toggle-hiding
;; hs-hide-all
;; hs-show-all
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; Emacs 26 removed make-variable-frame-local, which break
;; perspective.  https://github.com/nex3/perspective-el/issues/64
(when (not (fboundp 'make-variable-frame-local))
  (defun make-variable-frame-local (variable) variable))


;; restore previous layout
(winner-mode)

;; Info-default-directory-list
;; (setq Info-additional-directory-list '("~/.guix-profile/share/info/"))

;; (set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
;; (set-frame-parameter (selected-frame) 'alpha '(90 90))
;; (add-to-list 'default-frame-alist '(alpha 90 90))



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
  ;; (setq repo "~/git/test-dirty")
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

;; Hide the echo area. It seems that it can only work when emacs starts
;; up, and cause a lot of trouble. Thus I'm not using it

;; (setq initial-frame-alist (append '((minibuffer . nil)) initial-frame-alist))
;; (setq default-frame-alist (append '((minibuffer . nil)) default-frame-alist))
;; (setq minibuffer-auto-raise nil)
;; (setq minibuffer-exit-hook '(lambda () (lower-frame)))

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

