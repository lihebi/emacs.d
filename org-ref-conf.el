;;; org-ref-conf.el --- org-ref and bibtex package load and config

;;; Commentary:
;; This is apart from org-conf.el because emacs 24.3 will not work with org-ref
;; the check if done in init.el

;;; Code:


(setq TeX-open-quote "\"")
(setq TeX-close-quote "\"")

;; (directory-files "~/github/bibliography/" t ".*\.bib$")

(require 'cl)

(defun folder-dirs (folder)
  "find the folders inside another folder, except . and .."
  (when (file-exists-p folder)
    (delete-if-not
     'file-directory-p
     (mapcar (lambda(arg)
               (file-name-as-directory
                (concat (file-name-as-directory folder) arg)))
             (delete-if (lambda (arg)
                          (or (string= ".." arg) (string= "." arg)))
                        (directory-files folder))))))

;; (folder-dirs "~/")

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

(use-package helm-bibtex
  :config
  (setq bibtex-completion-cite-prompt-for-optional-arguments nil)
  ;; (setq bibtex-completion-cite-default-as-initial-input t)
  (defun hebi-bibtex-rehash()
    "Invalidate the helm-bibtex bibliograph cache by clear the hash.
Do it will cause the next C-c ] in org-ref (or helm-bibtex)
to rescan the bib files and update pdf and notes notation."
    (interactive)
    (setq bibtex-completion-bibliography-hash "")
    ))
(use-package org-ref
  ;; do not automatically download for this, because I want to use the git version
  ;; :ensure nil
  ;; use this path, so that I can test the github version of org-ref
  ;; I think this path is just added to the front of the load path,
  ;; so if it is not there, use-package will still find it in the elpa folder
  ;; :load-path "packages/org-ref"
  :init
  :config
  (global-set-key (kbd "C-c ]") 'org-ref-helm-insert-cite-link)
  ;; (define-key tex-mode-map (kbd "C-c ]") 'org-ref-helm-insert-cite-link)
  ;; open pdf directly in bib file
  ;; (define-key bibtex-mode-map
  ;;   (kbd "C-c h o")
  ;;   'org-ref-open-bibtex-pdf)
  (define-key bibtex-mode-map
    (kbd "C-c b")
    'org-ref-bibtex)
  (let* ((bib-files (append
                     (find-files-by-ext
                      "~/github/research/bib"
                      "bib")
                     '("~/github/bibliography/book.bib")))
         ;; (bib-note-file (concat bib-dir "/notes.org"))
         (bib-pdf-dir
          `("~/github/research/pdf/auto/"
            "~/github/research/pdf/manual/"
            ;; "~/github/papers/"
            "~/github/books/"
            )))
    (setq reftex-default-bibliography bib-files)    ; reftex
    (setq bibtex-completion-bibliography bib-files) ; bibtex
    (setq org-ref-default-bibliography bib-files)   ; org-ref
    ;; notes
    ;; (setq org-ref-bibliography-notes bib-note-file)
    ;; (setq bibtex-completion-notes-path bib-note-file)
    ;; pdf
    (setq org-ref-pdf-directory bib-pdf-dir)
    (setq bibtex-completion-library-path bib-pdf-dir))

  (when (string= system-type "darwin")
    (setq bibtex-completion-pdf-open-function
          (lambda (fpath)
            (start-process "open" "*open*" "open" fpath)))
    (setq org-ref-open-pdf-function
          (lambda (fpath)
            (start-process "open" "*open*" "open" fpath))))

  ;; disable to work on linux
  ;; (setq bibtex-completion-pdf-open-function
  ;;       (lambda (fpath)
  ;;         (start-process "open" "*open*" "open" fpath)))

  
  ;; (setq org-ref-open-pdf-function
  ;;       (lambda (fpath)
  ;;         (start-process "open" "*open*" "open" fpath)))

  ;; (setq bibtex-completion-pdf-open-function
  ;;       (lambda (fpath)
  ;;         (call-process "open" nil 0 nil "-a" "/Applications/Skim.app" fpath)))

  (setq bibtex-completion-display-formats
        '((t . "${year:4} ${author:36} ${title:*} ${=has-pdf=:1}${=has-note=:1} ${=type=:7} ${keywords:18}")))
  (setq bibtex-completion-additional-search-fields '(keywords))
  )




(provide 'org-ref-conf)
;;; org-ref-conf.el ends here
