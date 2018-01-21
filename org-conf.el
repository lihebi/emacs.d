;;; org.el --- org package load and config

;;; Commentary:
;; No Comments!

;;; Code:


;; the headerline bullets
;; (use-package org-bullets
;;   :config
;;   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package htmlize
  ;; used for fontify code in exporting of org
  :defer t
  )


(defun setup-plantuml()
  "Set Up plantuml"
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
  ;; this is for arch via AUR
  (setq org-plantuml-jar-path "/opt/plantuml/plantuml.jar")

  (require 'iimage)
  (autoload 'iimage-mode "iimage" "Support Inline image minor mode." t)
  (autoload 'turn-on-iimage-mode "iimage" "Turn on Inline image minor mode." t)
  (add-to-list 'iimage-mode-image-regex-alist '("@startuml\s+\\(.+\\)" . 1))

  ;; Rendering plantuml
  (defun plantuml-render-buffer ()
    (interactive)
    (message "PLANTUML Start rendering")
    ;; (shell-command (concat "java -jar ~/Downloads/plantuml.jar " 
    ;;                        buffer-file-name))
    (shell-command (concat "plantuml "  ; on mac, I use homebrew to install it.
                           buffer-file-name))
    (message (concat "PLANTUML Rendered:  " (buffer-name))))

  ;; Image reloading
  (defun reload-image-at-point ()
    (interactive)
    (message "reloading image at point in the current buffer...")
    (image-refresh (get-text-property (point) 'display)))

  ;; Image resizing and reloading
  (defun resize-image-at-point ()
    (interactive)
    (message "resizing image at point in the current buffer123...")
    (let* ((image-spec (get-text-property (point) 'display))
           (file (cadr (member :file image-spec))))
      (message (concat "resizing image..." file))
      (shell-command (format "convert -resize %d %s %s " 
                             (* (window-width (selected-window)) (frame-char-width))
                             file file))
      (reload-image-at-point))))

(defun setup-latex()
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
               '("acmart"
                 "\\documentclass[sigconf]{acmart}"
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
  (add-to-list 'org-latex-classes
               '("pldi" "\\documentclass[preprint]{sigplanconf}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; pdf code listing options
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; this will add color to pdf output from latex
  ;; needs to install Pygments
  (require 'ox-latex)
  ;; (setq org-latex-packages-alist nil)
  ;; in init.el, I also use customize to add float before hyperef
  ;; the correct order can be:
  ;; 1. float hyperef algorithm
  ;; 2. algorithm hyperref
  ;; But cannot be:
  ;; 3. hyperef algorithm
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

  ;; (require 'ox-bibtex)
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
  ;; (setq org-latex-pdf-process (list "latexmk -cd -quiet -pdf -shell-escape %f")))
  )

(defun setup-publish()
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

  (defvar org-html-head)
  (setq org-html-head
        (concat
         ;; "<style type=\"text/css\"> a:visited {color: red;} </style>"
         ;; "<style type=\"text/css\"> code {color: red;} </style>"
         "<link rel=\"stylesheet\" type=\"text/css\" href=\"assets/hebi.css\" />"
         "<link rel=\"stylesheet\" type=\"text/css\" href=\"assets/test.css\" />"
         ))

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


(defun setup-ob()
  ;; my srcml converter
  (require 'ob-srcml)
  (require 'ob-clang)
  (require 'ob-ctags)
  (setq org-export-use-babel nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk . t)
     (emacs-lisp . t)
     (python . t)
     (ruby . t)
     ;; (shell . t)
     ;;    ;; other babel languages
     
     ;; I don't like that when I was exporting the wiki, it keeps ask
     ;; me to evaluate code or not. And I don't want to enable to
     ;; execute arbitrary code without my grant, so, no evaluating.
     
     ;; (plantuml . t)
     
     ;;    ;; this should be capital C, the same as in #+begin_src C
     (C . t)
     (ctags . t)
     (dot . t)
     ;; (R . t)
     (sqlite . t)
     (lisp . t)
     (srcml . t)
     (clang . t)
     ;; (latex . t)
     )
   )

  ;; CAUTION The following lines will permit the execution of R code without a confirmation.
  ;; (defun my-org-confirm-babel-evaluate (lang body)
  ;;   (not (string= lang "R")))  ; don't ask for R
  ;; (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
  )



;; this org might not be installed automatically, don't know why
;; but install it from elpa manually. There're gnu and org version, use the org one.
(use-package org
  ;; :recipe (:host github
  ;;                :repo "emacsmirror/org"
  ;;                :files ("list/*.el" "contrib/lisp/*.el"))
  :init
  (setq org-plain-list-ordered-item-terminator '?.) ; remove using ?) causing a listing
  ;; :defer t
  :bind
  (
   ("C-c n" . org-capture)
   ("C-c m" . org-capture-goto-target)
   ;; ("C-c o" . org-open-at-point)
   ("C-c o" . org-open-at-point-global)
   ("C-c t" . org-todo)
   ("C-c a" . org-agenda)
   )
  :config
  (setup-ob)
  ;; disabling plantuml
  ;; (setup-plantuml)
  (setup-latex)
  (setup-publish)

  (setq org-file-apps
        (quote
         ((auto-mode . emacs)
          ("\\.mm\\'" . default)
          ;; have to use this to open in browser
          ;; otherwise will open in emacs
          ("\\.x?html?\\'" . "/usr/bin/chromium %s")
          ("\\.pdf\\'" . default))))

  (when (string= system-type "darwin")
    (setq org-file-apps
          (quote
           ((auto-mode . emacs)
            ("\\.mm\\'" . default)
            ;; have to use this to open in browser
            ;; otherwise will open in emacs
            ("\\.x?html?\\'" . "open %s")
            ("\\.pdf\\'" . "open %s")))))

  (setq org-descriptive-links nil)
  
  ;; (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (setq org-log-done 'time)
  (setq org-startup-folded nil)
  (setq org-yank-folded-subtrees nil)
  ;; the forbidden, by default, is ,'", but I want all of them actually. By the way why these are forbidden?
  (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n")
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  (setq org-export-backends (append '(man) org-export-backends))
  (define-key org-mode-map (kbd "C-j") (lambda()
                                         (interactive)
                                         (join-line -1)))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)" "CANCELED(c)")))
  (setq org-todo-keyword-faces
        '(("TODO" . org-todo) ("CANCELED" . org-warning) ("STARTED" . (:foreground "white" :background "red"))))

  (setq org-agenda-files
        '(
          ;; "~/github/note/TODO.org"
          "~/github/note/org"))

  ;; org capture
  (setq org-directory "~/github/note/org")
  (setq org-default-notes-file "~/github/note/org/default.org")

  ;; typical command
  ;; 1. org-capture: C-c n
  ;; 2. org-capture-goto-target C-c m
  ;; 3. org-agenda: C-c a
  (defun gen-template (key title)
    `(,key ,title entry
           (file+headline "~/github/note/org/gtd.org" ,title)
           "* TODO %?\n  %U\n  %i\n"
           :prepend t
           ;; setting one line after does not help at all, it
           ;; basically inserted a new line in the *edit* buffer
           :empty-lines-before 1))
  (setq org-reverse-note-order t)
  (setq org-capture-templates
        `(,(gen-template "t" "Tmp")
          ,(gen-template "s" "Stack")
          ,(gen-template "b" "Buy")
          ,(gen-template "p" "Project")
          ,(gen-template "w" "Wiki")
          ,(gen-template "r" "Research Stack")
          ,(gen-template "l" "Learn")
          ,(gen-template "i" "Idea")
          ,(gen-template "m" "Motto")))

  ;; try to use ATTR_* width
  (setq org-image-actual-width nil)
  ;; (setq org-image-actual-width 300)
  
  ;; (use-package org-plus-contrib)
  ;; highlight
  (setq org-src-fontify-natively t)
  (setq org-fontify-whole-heading-line t)

  ;; (ol1 '(:height 1.3 :weight bold :overline "#A7A7A7" :foreground "#3C3C3C" :background "#F0F0F0"))
  ;; (ol2 '())
  ;; (ol3 '())
  ;; (ol4 '())
  ;; (ol5 '(:height 1.0 :weight bold :slant normal :foreground "#E3258D"))
  ;; (ol6 '(:height 1.0 :weight bold :slant italic :foreground "#0077CC"))

  (set-face-attribute 'org-level-1 nil
                      :height 1.3 :weight 'bold :overline "#A7A7A7" :foreground "#3C3C3C" :background "#F0F0F0")
  (set-face-attribute 'org-level-2 nil
                      :height 1.0 :weight 'bold :overline "#123555" :foreground "#123555" :background "#E5F4FB")
  (set-face-attribute 'org-level-3 nil
                      :height 1.0 :weight 'bold :foreground "#005522" :background "#EFFFEF")
  (set-face-attribute 'org-level-4 nil
                      :height 1.0 :weight 'bold :slant 'normal :foreground "#EA6300")

  (set-face-attribute 'org-block nil :inherit 'shadow :background "#FFFFE0")
  (set-face-attribute 'org-block-begin-line nil :inherit 'org-meta-line
                      :underline "#A7A6AA" :foreground "#555555" :background "#E2E1D5")
  (set-face-attribute 'org-block-end-line nil :inherit 'org-meta-line
                      :overline "#A7A6AA" :foreground "#555555" :background "#E2E1D5")
  (set-face-attribute 'org-document-title nil
                      :family "Sans Serif" :height 1.8 :weight 'bold :foreground "black")
  (set-face-attribute 'org-document-info-keyword nil
                      :foreground "#008ED1" :background "#EAEAFF")
  )


(provide 'org-conf)
;;; org-conf.el ends here
