;;; org-ref-conf.el --- org-ref and bibtex package load and config

;;; Commentary:
;; This is apart from org-conf.el because emacs 24.3 will not work with org-ref
;; the check if done in init.el

;;; Code:


(use-package helm-bibtex
  ;; this is used by org-ref
  ;; I do not really need to "use" it here,
  ;; since I guess org-ref requries it,
  ;; and it is automatically installed when I install org-ref.
  ;; So maybe I'd better move this into "org-ref"
  ;; I just want to do some configuration for it here.
  
  ;; helm-bibtex-notes-template-one-file
  ;; helm-bibtex-notes-template-multiple-files
  :config
  ;; open pdf with system pdf viewer (works on mac)
  (setq bibtex-completion-pdf-open-function
        (lambda (fpath)
          (start-process "open" "*open*" "open" fpath)))
  ;; alternative
  ;; (setq helm-bibtex-pdf-open-function 'org-open-file)
  ;; (setq helm-bibtex-notes-path "~/github/bibliography/helm-bibtex-notes")
  (defun hebi-bibtex-rehash()
    "Invalidate the helm-bibtex bibliograph cache by clear the hash.
Do it will cause the next C-c ] in org-ref (or helm-bibtex)
to rescan the bib files and update pdf and notes notation."
    (interactive)
    (setq bibtex-completion-bibliography-hash "")
    ))


;; (directory-files "~/github/bibliography/" t ".*\.bib$")

(use-package org-ref
  :config
  ;; (let* ((bib-dir "~/github/bibliography")
  ;;        (bib-files (directory-files bib-dir t ".*\.bib$"))
  ;;        (bib-note-file (concat bib-dir "/notes.org"))
  ;;        (bib-pdf-dir (concat bib-dir "/bibtex-pdfs/"))
  ;;        )
  ;;   (setq reftex-default-bibliography bib-files) ; reftex
  ;;   (setq bibtex-completion-bibliography bib-files) ; bibtex
  ;;   (setq org-ref-default-bibliography bib-files) ; org-ref
  ;;   ;; notes
  ;;   (setq org-ref-bibliography-notes bib-note-file)
  ;;   (setq bibtex-completion-notes-path bib-note-file)
  ;;   ;; pdf
  ;;   (setq org-ref-pdf-directory bib-pdf-dir)
  ;;   (setq bibtex-completion-library-path bib-pdf-dir)
  ;;   )
  ;; (defvar bib-files)
  ;; (define-key bibtex-mode-map (kbd "C-c ]") 'org-ref-helm-insert-cite-link)
  ;; (setq bib-files (directory-files "~/github/bibliography/" t ".*\.bib$"))
  ;; (setq reftex-default-bibliography bib-files)
  ;; (setq bibtex-completion-bibliography bib-files)
  ;; (setq org-ref-default-bibliography bib-files)
  ;; (setq org-ref-bibliography-notes "~/github/bibliography/notes.org")
  ;; (setq bibtex-completion-notes-path "~/github/bibliography/notes.org")
  ;; (setq org-ref-pdf-directory "~/github/bibliography/bibtex-pdfs/")
  ;; (setq bibtex-completion-library-path "~/github/bibliography/bibtex-pdfs")

  ;; (let ((bib-dir "~/github/bibliography"))
  ;;   (setq reftex-default-bibliography (concat bib-dir "/refacotr.bib"))
  ;;   (setq bibtex-completion-bibliography (concat bib-dir "/refactor.bib"))
  ;;   (setq org-ref-default-bibliography (concat bib-dir "/refactor.bib"))
  ;;   (setq org-ref-bibliography-notes (concat bib-dir "/notes.org"))
  ;;   (setq bibtex-completion-notes-path (concat bib-dir "/notes.org"))
  ;;   (setq org-ref-pdf-directory (concat bib-dir "/bibtex-pdfs/"))
  ;;   (setq bibtex-completion-library-path (concat bib-dir "/bibtex-pdfs"))
  ;;   )

  ;; default condfiguration
  (setq reftex-default-bibliography '("~/github/bibliography/refactor.bib"))

  ;; see org-ref for use of these variables
  (setq org-ref-bibliography-notes "~/github/bibliography/notes.org"
        org-ref-default-bibliography '("~/github/bibliography/refactor.bib")
        org-ref-pdf-directory "~/github/bibliography/bibtex-pdfs/")


  (setq bibtex-completion-bibliography "~/github/bibliography/refactor.bib")
  (setq bibtex-completion-library-path "~/github/bibliography/bibtex-pdfs")

  ;; open pdf with system pdf viewer (works on mac)
  (setq bibtex-completion-pdf-open-function
        (lambda (fpath)
          (start-process "open" "*open*" "open" fpath)))

  ;; alternative
  ;; (setq bibtex-completion-pdf-open-function 'org-open-file)

  (setq bibtex-completion-notes-path "~/github/bibliography/bibtex-completion-notes")
  

  ;; my temporary patches
  (defalias 'helm-bibtex-get-value 'bibtex-completion-get-value)
  (defalias 'helm-bibtex-clean-string 'bibtex-completion-clean-string)
  (defalias 'helm-bibtex-shorten-authors 'bibtex-completion-shorten-authors)

  ;; "\n** ${year} - ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :AUTHOR: ${author}\n  :END:\n\n"
  (setq org-ref-note-title-format
        "** %y - %t
 :PROPERTIES:
 :Custom_ID: %k
 :AUTHOR: %a
 :END:
")
  (setq bibtex-completion-notes-template-one-file
        ;; "\n* ${author} (${year}): ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :END:\n\n"
        "\n** ${year} - ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :AUTHOR: ${author}\n  :END:\n\n"
        )

  ;; To display more keywords, look into this defun:
  ;; bibtex-completion-candidates-formatter
  )


(provide 'org-ref-conf)
;;; org-ref-conf.el ends here
