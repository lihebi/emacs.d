;;; org.el --- org package load and config

;;; Commentary:
;; No Comments!

;;; Code:



(use-package htmlize
  ;; used for fontify code in exporting of org
  :defer t
  )

(defun my/org-inline-css-hook (exporter)
  "Insert custom inline css to automatically set the
background of code to whatever theme I'm using's background"
  (when (eq exporter 'html)
    (let* (
           ;; (my-pre-bg (face-background 'default))
           (my-pre-bg "#01331c")
           (my-pre-fg (face-foreground 'default)))
      (setq
       org-html-head-extra
       (concat
        org-html-head-extra
        (format "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}</style>\n"
                my-pre-bg my-pre-fg))))))

(add-hook 'org-export-before-processing-hook 'my/org-inline-css-hook)

(use-package org
  :init
  (setq org-plain-list-ordered-item-terminator '?.) ; remove using ?) causing a listing
  :defer t
  :bind
  (("C-c n" . org-capture)
   ;; ("C-c o" . org-open-at-point)
   ("C-c o" . org-open-at-point-global)
   ("C-c t" . org-todo)
   ("C-c a" . org-agenda))
  :init
  :config
  (setq org-startup-folded nil)
  ;; the forbidden, by default, is ,'", but I want all of them actually. By the way why these are forbidden?
  (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n")
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  (setq org-export-backends (append '(man) org-export-backends))
  (define-key org-mode-map (kbd "C-j") (lambda()
                                         (interactive)
                                         (join-line -1)))
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
  ;;    ;; other babel languages
     (plantuml . t)
  ;;    ;; this should be capital C, the same as in #+begin_src C
     (C . t)
     (srcml . t)
     (ctags . t)
     (dot . t)
     ;; (R . t)
     (sqlite . t)
     )
   )

  ;; CAUTION The following lines will permit the execution of R code without a confirmation.
  ;; (defun my-org-confirm-babel-evaluate (lang body)
  ;;   (not (string= lang "R")))  ; don't ask for R
  ;; (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

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

  ;; this sets the plantuml.jar path
  ;; (setq org-plantuml-jar-path
  ;;       (expand-file-name "~/bin/plantuml.jar"))
  ;; But I found debian has plantuml in its repo, so why not?
  (setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
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
  ;; set the default export headline toc level
  ;; it can also be set by #+OPTIONS: H:6
  ;; but it can NOT be set by #+OPTIONS: toc:6
  ;; default is 3, but I want to show everything by default
  ;; also note that in org-publish-project-alist, the headline is set to 4
  ;; this may need some change in the future.
  (setq org-export-headline-levels 6)

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
           :base-extension "ttf\\|js\\|css\\|png"
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

(if (and (>= emacs-major-version 24)
	     ;; FIXME 25.x
	     (>= emacs-minor-version 4))
    (load (emacs-d "org-ref-conf")))


(provide 'org-conf)
;;; org-conf.el ends here
