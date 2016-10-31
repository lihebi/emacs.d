;;; org-ref-conf.el --- org-ref and bibtex package load and config

;;; Commentary:
;; This is apart from org-conf.el because emacs 24.3 will not work with org-ref
;; the check if done in init.el

;;; Code:




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

(defun find-files-by-ext-1 (folder ext)
  "find the folder/*.ext files"
  (if (file-exists-p folder)
      (directory-files folder t (concat ".*\." ext "$"))))

(defun find-files-by-ext-2 (folder ext)
  "level 2 find bib files
  will find folder/*.ext and folder/*/*.ext"
  (let ((l_folders (folder-dirs folder)))
    (-flatten (mapcar (lambda (folder)
                        (find-files-by-ext-1 folder ext))
                      l_folders))
    ))

;; (folder-dirs "~/github")
;; (file-exists-p "~/github")
;; (find-files-by-ext "~/github" "bib")
;; (find-files-by-ext-1 "~/github" "bib")
;; (find-files-by-ext-2 "~/github" "bib")
  
(defun find-files-by-ext (folder ext)
  "find both level 1 and level 2 files"
  (if (file-exists-p folder)
      (let* ((f1 (find-files-by-ext-1 folder ext))
             (f2 (find-files-by-ext-2 folder ext)))
        (append f1 f2))))

(use-package org-ref
  ;; do not automatically download for this, because I want to use the git version
  ;; :ensure nil
  ;; use this path, so that I can test the github version of org-ref
  ;; I think this path is just added to the front of the load path,
  ;; so if it is not there, use-package will still find it in the elpa folder
  ;; :load-path "packages/org-ref"
  :init
  :config
  (use-package helm-bibtex
    :config
    (defun hebi-bibtex-rehash()
      "Invalidate the helm-bibtex bibliograph cache by clear the hash.
Do it will cause the next C-c ] in org-ref (or helm-bibtex)
to rescan the bib files and update pdf and notes notation."
      (interactive)
      (setq bibtex-completion-bibliography-hash "")
      ))
  (let* ((bib-dir "~/github/bibliography")
         (bib-files (find-files-by-ext bib-dir "bib"))
         (bib-note-file (concat bib-dir "/notes.org"))
         (bib-pdf-dir `("~/github/papers/" "~/github/books/" ,@(folder-dirs "~/github/proceeding-papers"))))
    (setq reftex-default-bibliography bib-files) ; reftex
    (setq bibtex-completion-bibliography bib-files) ; bibtex
    (setq org-ref-default-bibliography bib-files) ; org-ref
    ;; notes
    (setq org-ref-bibliography-notes bib-note-file)
    (setq bibtex-completion-notes-path bib-note-file)
    ;; pdf
    (setq org-ref-pdf-directory bib-pdf-dir)
    (setq bibtex-completion-library-path bib-pdf-dir))
  (defun helm-bibtex-candidates-formatter (candidates _)
    (cl-loop
     with width = (with-helm-window (helm-bibtex-window-width))
     for entry in candidates
     for entry = (cdr entry)
     for entry-key = (bibtex-completion-get-value "=key=" entry)
     collect (cons (bibtex-completion-format-entry entry width) entry-key)))
  (defun bibtex-completion-format-entry (entry width)
    "Formats a BibTeX entry for display in results list."
    (let* ((fields (list
                    "=key=" "title"
                    (if (assoc-string "author" entry 'case-fold) "author" "editor")
                    "year" "=has-pdf=" "=has-note=" "=type="))
           (fields (-map (lambda (it)
                           (bibtex-completion-clean-string
                            (bibtex-completion-get-value it entry " ")))
                         fields))
           (fields (-update-at 0 'bibtex-completion-shorten-authors fields)))
      (s-format "$0 $1 $2 $3 $4$5 $6" 'elt
                (-zip-with (lambda (f w) (truncate-string-to-width f w 0 ?\s))
                           fields
                           (mapcar 'floor (list (* width 0.1)
                                                   (* width 0.5)
                                                   (* width 0.25)
                                                   4 1 1 7))
                           ;; (list 20 (- width 108) 46 4 1 1 7)
                           )))))


;; "\n** ${year} - ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :AUTHOR: ${author}\n  :END:\n\n"
;;   (setq org-ref-note-title-format
;;         "** %y - %t
;;  :PROPERTIES:
;;  :Custom_ID: %k
;;  :AUTHOR: %a
;;  :END:
;; ")
;; (setq bibtex-completion-notes-template-one-file
;;       ;; "\n* ${author} (${year}): ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :END:\n\n"
;;       "\n** ${year} - ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :AUTHOR: ${author}\n  :END:\n\n"
;;       ))

;; (use-package gscholar-bibtex
;;   :config
;;   (setq gscholar-bibtex-default-source "Google Scholar")
;;   (setq gscholar-bibtex-database-file "/Users/hebi/github/bibliography/tmp.bib")
;;   )

(provide 'org-ref-conf)
;;; org-ref-conf.el ends here
