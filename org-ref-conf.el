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
  ;; link message will freeze emacs in case of many bib files
  (org-ref-cancel-link-messages)
  (let ((bib-pdf-dir
         '("~/github/research/pdf/auto/"
           "~/github/research/pdf/manual/"
           "~/github/research/pdf/manual/book"
           "~/github/research/pdf/manual/tian"
           "~/github/research/pdf/manual/tmp"
           "~/github/research/pdf/manual/paper"
           "~/github/papers/"
           "~/github/books/")))
    (setq org-ref-pdf-directory bib-pdf-dir)
    (setq bibtex-completion-library-path bib-pdf-dir))

  ;; bibs
  (defun set-bib (v)
    (setq reftex-default-bibliography v)    ; reftex
    (setq bibtex-completion-bibliography v) ; bibtex
    (setq org-ref-default-bibliography v))  ; org-ref
  (defun add-bib (v)
    (setq reftex-default-bibliography
          (remove-duplicates
           (append reftex-default-bibliography v)))
    (setq bibtex-completion-bibliography
          (remove-duplicates
           (append bibtex-completion-bibliography v)))
    (setq org-ref-default-bibliography
          (remove-duplicates
           (append org-ref-default-bibliography v))))
  (defun dir-bib-files (dir)
    (directory-files dir t ".*\\.bib"))
  (defun conf-bib-files (conf)
    (let ((auto-bib-dir "~/github/research/bib/auto/"))
      (dir-bib-files
       (concat (file-name-as-directory auto-bib-dir)
               conf))))
  (defun hebi-load-bib (in)
    (interactive
     (list
      (completing-read "choose one conf: "
                       '("se" "pl" "os" "other" "manual" "unload"))))
    (cond
     ((member in '("se" "pl" "os" "other-conf"))
      (let ((conf
             (cond
              ((string= in "se") '("ASE" "PASTE" "FSE" "ICSE" "ISSTA" "MSR"))
              ((string= in "pl") '("CGO" "ASPLOS" "Onward"
                                   "OOPSLA" "PLDI" "SIGPLAN" "POPL"
                                   "Haskell" "ICFP" "LFP"))
              ((string= in "os") '("OSDI" "SOSP"))
              ((string= in "other") '("KDD" "STOC" "VLDB")))))
        (add-bib (apply #'append (mapcar #'conf-bib-files conf)))))
     ((member in '("manual"))
      (add-bib (append (dir-bib-files "~/github/research/bib/manual/")
                       '("~/github/bibliography/book.bib"))))
     ((member in '("unload"))
      (set-bib nil))))

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
  (defun hebi-gen-bib ()
    (interactive)
    (insert (org-bibliography-complete-link)))
  )




(provide 'org-ref-conf)
;;; org-ref-conf.el ends here
