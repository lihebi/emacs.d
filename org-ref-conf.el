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

(require 'cl)

(defun folder-dirs (folder)
  "find the folders inside another folder, except . and .."
  "FIXME throw error if folder does not exist"
  (delete-if-not 'file-directory-p
    (mapcar (lambda(arg) (file-name-as-directory (concat (file-name-as-directory folder) arg)))
      (delete-if (lambda (arg) (or (string= ".." arg) (string= "." arg)))
                 (directory-files folder)))))

;; (folder-dirs "~/Downloads")

(defun find-bib-files-1 (folder)
  "find the folder/*.bib files"
  (if (file-exists-p folder)
      (directory-files folder t ".*\.bib$")))

(defun find-bib-files-2 (folder)
  "level 2 find bib files
  will find folder/*.bib and folder/*/*.bib"
  (let ((l_folders (folder-dirs folder)))
    (-flatten (mapcar 'find-bib-files-1 l_folders))
    ))

;; (find-bib-files "~/github")
;; (folder-dirs "~/github")
;; (file-exists-p "~/github")
  
(defun find-bib-files (folder)
  "find both level 1 and level 2 bib files"
  (if (file-exists-p folder)
      (let*
          ((f1 (find-bib-files-1 folder))
           (f2 (find-bib-files-2 folder))
           )
        (append f1 f2))))

;; (find-bib-files-1 "~/tmptmp")
;; (find-bib-files-2 "~/tmptmp")
;; (find-bib-files "~/tmptmp")

;; (list "dd" "nnn")

;; use your function instead of print
;; (recursively-run-on-every-dir 'print "/your/initial/path/")

(use-package org-ref
  :config
  (let* ((bib-dir "~/github/bibliography")
         ;; (bib-files (if (file-exists-p bib-dir)
         ;;                ;; TODO recursive search sub-dirs for bib files
         ;;              (directory-files bib-dir t ".*\.bib$"))
         ;;            )
         (bib-files (find-bib-files bib-dir))
         (bib-note-file (concat bib-dir "/notes.org"))
         ;; (bib-pdf-dir (list (concat bib-dir "/bibtex-pdfs/") (concat bib-dir "/manual-pdfs/")))
         (bib-pdf-dir (concat bib-dir "/pdfs/"))
         )
    (setq reftex-default-bibliography bib-files) ; reftex
    (setq bibtex-completion-bibliography bib-files) ; bibtex
    (setq org-ref-default-bibliography bib-files) ; org-ref
    ;; notes
    (setq org-ref-bibliography-notes bib-note-file)
    (setq bibtex-completion-notes-path bib-note-file)
    ;; pdf
    (setq org-ref-pdf-directory bib-pdf-dir)
    (setq bibtex-completion-library-path bib-pdf-dir)
    )
  (setq bibtex-completion-pdf-open-function
        (lambda (fpath)
          (start-process "open" "*open*" "open" fpath)))

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

  ;; (require 'org-ref-pdf)
  ;; (require 'org-ref-url-utils)
  )


;; HEBI I'm using it to enlarge the keyword column in helm-bibtex interface
;; The only change is a number in the second from the last line, from 7 to 15
(defun bibtex-completion-candidates-formatter (candidates _source)
  "Formats BibTeX entries for display in results list.
Argument CANDIDATES helm candidates.
Argument SOURCE the helm source.
Adapted from the function in `helm-bibtex' to include additional
fields, the keywords I think."
  (cl-loop
   with width = (with-helm-window (helm-bibtex-window-width))
   for entry in candidates
   for entry = (cdr entry)
   for entry-key = (bibtex-completion-get-value "=key=" entry)
   if (assoc-string "author" entry 'case-fold)
   for fields = '("=key="  "title" "author"  "year" "=has-pdf=" "=has-note=" "=type=")
   else
   for fields = '("=key=" "title" "editor" "year" "=has-pdf=" "=has-note=" "=type=" "=key=")
   for fields = (--map (bibtex-completion-clean-string
                        (bibtex-completion-get-value it entry " "))
                       fields)
   for fields = (-update-at 0 'bibtex-completion-shorten-authors fields)
   for fields = (append fields
                        (list (or (bibtex-completion-get-value "keywords" entry)
                                  "")))
   collect
   (cons (s-format "$0 $1 $2 $3 $4$5 $6 $7" 'elt
                   (-zip-with (lambda (f w) (truncate-string-to-width f w 0 ?\s))
                              fields (list 18 (- width 85) 30 4 1 1 7 15)))
         entry-key)))

(use-package gscholar-bibtex
  :config
  (setq gscholar-bibtex-default-source "Google Scholar")
  (setq gscholar-bibtex-database-file "/Users/hebi/github/bibliography/tmp.bib")
  )



(provide 'org-ref-conf)
;;; org-ref-conf.el ends here
