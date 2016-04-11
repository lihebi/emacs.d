;;; org.el --- org package load and config

;;; Commentary:
;; No Comments!

;;; Code:

(use-package htmlize
  ;; used for fontify code in exporting of org
  :defer t
  )
(use-package org
  :init
  (setq org-plain-list-ordered-item-terminator '?.) ; remove using ?) causing a listing
  :defer t
  :bind
  (("C-c n" . org-capture)
   ;; ("C-c o" . org-open-at-point)
   ("C-c o" . org-open-at-point-global)
   )
  :init
  (progn
    (defvar org-startup-folded)
    (defvar org-directory)
    (defvar org-capture-templates)
    (defvar org-agenda-files)
    (setq org-startup-folded nil)
    (setq org-directory "~/github/org")
    ;; capture templates
    (setq org-capture-templates
          '(("t" "TODO" entry (file+headline (concat org-directory "/scratch.org") "Tasks")
             "* TODO %?\n  %U")
            ("n" "Note" entry (file (concat org-directory "/scratch.org"))
             "* Notes on %U\n%?" :prepend t)
            ("s" "Stack" entry (file (concat org-directory "/stack.org"))
             "* New Stack on %U\n%?" :prepend t)
            ))
    (setq org-agenda-files (list org-directory))
    )
  :config
  (use-package org-plus-contrib)
  ;; highlight
  (setq org-src-fontify-natively t)
  ;; my srcml converter
  (require 'ob-srcml)
  (require 'ob-ctags)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk . t)
     (emacs-lisp . t)
     (python . t)
     (ruby . t)
     (shell . t)
     ;; other babel languages
     (plantuml . t)
     ;; this should be capital C, the same as in #+begin_src C
     (C . t)
     (srcml . t)
     (ctags . t)
     (dot . t)
     (R . t)
     (sqlite . t)
     )
   )

  (defun my-org-confirm-babel-evaluate (lang body)
    (not (string= lang "R")))  ; don't ask for ditaa
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

  ;; to use plantuml in org-mode:
  
  ;; #+begin_src plantuml :file tryout.png
  ;;   Alice -> Bob: synchronous call
  ;;   Alice ->> Bob: asynchronous call
  ;; #+end_src

  ;; #+results:
  ;; [[file:tryout.png]]

  ;; org mode have a babel support for plantuml..., built-in!
  ;; just go to the code, than press C-c C-c to evaluate it. The #+results section is gnerated by org-mode.
  ;; not sure if I can use plantuml command itself instead of setting the following jar path.

  ;; To load the image in to emacs:
  ;; org-toggle-inline-images
  ;; C-c C-x C-v
  (setq org-plantuml-jar-path
        (expand-file-name "~/bin/plantuml.jar"))
  ;; latex templates
  (require 'ox-latex)
  ;; (setq org-export-latex-listings t)
  (add-to-list 'org-latex-classes
               '("fse"
                 "\\documentclass{sig-alternate-05-2015}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("popl"
                 "\\documentclass[preprint]{sigplanconf}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("acmsmall" ;; acm computing survey(CSUR) journal format
                 "\\documentclass{acmsmall}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("beamer" ;; acm computing survey(CSUR) journal format
                 "\\documentclass\[presentation\]\{beamer\}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
                 ))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; pdf code listing options
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; this will add color to pdf output from latex
  ;; needs to install Pygments
  (require 'ox-latex)
  ;; (setq org-latex-packages-alist nil)
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  ;; (add-to-list 'org-latex-packages-alist '("newfloat" "minted"))
  ;; (setq org-latex-listings 'minted)
  (setq org-latex-listings 'listings)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; org to latex
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; from 8.3, it TOC becomes a top-level option
  ;; #+TOC: headlines 2
  ;; #+TOC: tables
  ;; #+TOC: listings
  ;; to have correct bibtex citation, we need to remove the toc completely,
  ;; because it will insert a strange \tableofcontent, which is not recognized by latex
  ;; #+OPTIONS: toc:nil

  ;; the org mode comes with emacs 24.5 is 8.2, but 8.3 introduce some new features.
  ;; also, some community contrib package is not shipped with emacs, which need to be installed by org-plus-contrib
  ;; to make it compitible, I also want to replace the default org with the one in melpa.
  ;; there're two source of org I can install from: gnu and org.
  ;; the org version, for some reason, will not compile for #:TITLE or #:AUTHOR.
  ;; but the gnu version works just fine
  ;; there's another method to install the most recent version of org: homebrew.
  ;; I cannot manage the version of it from emacs, so this is just a backup plan, but it works too.

  ;; this is needed to have #+BIBLIOGRAPHY: buffer-overflow plain works.
  ;; insert it in the end of the page can put the reference at the end.
  ;; it will do something else like change [[cite:xxx]] into \cite{xxx}

  (require 'ox-bibtex)
  ;; org-latex-pdf-process should also be customized, or it can not parse bibtex correctly
  ;; the following works, but it's slow
  ;; (setq org-latex-pdf-process
  ;;       (quote ("texi2dvi --pdf --clean --verbose --batch %f"
  ;;               "bibtex %b"
  ;;               "texi2dvi --pdf --clean --verbose --batch %f"
  ;;               "texi2dvi --pdf --clean --verbose --batch %f")))
  ;; (setq org-latex-pdf-process
  ;;       (list "latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f"))
  ;; (setq org-latex-pdf-process (list "latexmk -pdf %f"))
  ;; (setq org-latex-to-pdf-process
  ;;       '("pdflatex %f" "bibtex %b" "pdflatex %f" "pdflatex %f"))
  ;; (setq org-latex-pdf-process (quote ("texi2dvi -p -b -V %f")))
  ;; I add this one on my own based on my experience, and it seems to work well
  (setq org-latex-pdf-process (list "latexmk -cd -quiet -pdf -shell-escape %f"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; publishing blog
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq org-html-style "<style>
a:visited {color: red;}
</style>
"
        )

  ;; (setq org-publish-project-alist
  ;;       '(
  ;;         ("blog-org"
  ;;          :base-directory "~/github/blog/org/"
  ;;          :base-extension "org"
  ;;          :publishing-directory "~/github/blog/jekyll/"
  ;;          :recursive t
  ;;          :publishing-function org-html-publish-to-html
  ;;          :headline-levels 4
  ;;          :html-extension "html"
  ;;          :body-only t
  ;;          )
  ;;         ("static-assets"
  ;;          :base-directory "~/github/blog/org/"
  ;;          :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
  ;;          :publishing-directory "~/github/blog/"
  ;;          :recursive t
  ;;          :publishing-function org-publish-attachment
  ;;          )
  ;;         ("blog"
  ;;          :components ("blog-org" "static-assets")
  ;;          )
  ;;         )
  ;;       )

  ;; see also: http://orgmode.org/worg/org-tutorials/org-publish-html-tutorial.html
  (setq org-publish-project-alist
        '(
          ("wiki-org"
           :base-directory "~/github/wiki/"
           :base-extension "org"
           :publishing-directory "~/github/wiki-dist/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :html-extension "html"
           ;; experimental
           :auto-sitemap t
           )
          ("wiki-static"
           :base-directory "~/github/wiki/"
           :base-extension "ttf\\|js\\|css"
           :recursive t
           :publishing-directory "~/github/wiki-dist/"
           :publishing-function org-publish-attachment
           )
          ;; this is still TODO
          ;; the C-c C-e Pp cannot publish wiki-static
          ;; I have to use C-c C-e Px and choose wiki-static for css and ttf
          ;; but it's fine for now
          ("wiki" :components ("wiki-org" "wiki-static"))
          ("note"
           :base-directory "~/github/note/"
           :base-extension "org"
           :publishing-directory "~/github/note-dist/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :html-extension "html"
           ;; experimental
           :auto-sitemap t
           )
          )
        )
  ;; default is 'inline-css
  ;; which will output the color inside html tags
  ;; using 'css will only insert the class, and you need to provide you own css file.
  ;; (setq org-html-htmlize-output-type 'css)

  )

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


(provide 'org-conf)
;;; org-conf.el ends here