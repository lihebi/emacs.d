;;; org.el --- org package load and config

;;; Commentary:
;; No Comments!

;;; Code:


;; the headerline bullets
;; (use-package org-bullets
;;   :config
;;   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; used for fontify code in exporting of org
(use-package htmlize)

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
                 "\\documentclass[acmsmall]{acmart}"
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

  (require 'ox-latex)
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  ;; (add-to-list 'org-latex-packages-alist '("newfloat" "minted"))
  ;; (setq org-latex-listings 'minted)
  (setq org-latex-listings 'listings))

(defun setup-publish()
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
         "<link rel=\"stylesheet\" type=\"text/css\" href=\"assets/hebi.css\" />"
         "<link rel=\"stylesheet\" type=\"text/css\" href=\"assets/test.css\" />"
         "<link rel=\"stylesheet\" type=\"text/css\" href=\"../assets/hebi.css\" />"
         "<link rel=\"stylesheet\" type=\"text/css\" href=\"../assets/test.css\" />"))
  (setq org-html-validation-link nil)
  (setq org-export-time-stamp-file nil)
  (setq org-export-with-author nil)
  (setq org-export-with-date nil)
  ;; (setq org-export-with-toc nil)
  (setq org-export-with-timestamps t)
  
  ;; see also: http://orgmode.org/worg/org-tutorials/org-publish-html-tutorial.html
  (setq org-publish-project-alist
        '(("wiki-org"
           :base-directory "~/github/wiki/"
           :base-extension "org"
           :publishing-directory "~/github/wiki-dist/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :html-extension "html"
           ;; experimental
           :auto-sitemap t)
          ("wiki-static"
           :base-directory "~/github/wiki/"
           :base-extension "ttf\\|js\\|css\\|png"
           :recursive t
           :publishing-directory "~/github/wiki-dist/"
           :publishing-function org-publish-attachment)
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
           :auto-sitemap t)
          ("homepage-org"
           :base-directory "~/github/homepage"
           :base-extension "org"
           :publishing-directory "~/github/homepage-dist"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :html-extension "html"
           :auto-sitemap t)
          ("homepage-static"
           :base-directory "~/github/homepage/"
           :base-extension "ttf\\|js\\|css\\|png\\|pdf\\|jpg"
           :recursive t
           :publishing-directory "~/github/homepage-dist/"
           :publishing-function org-publish-attachment)
          ("homepage" :components ("homepage-org" "homepage-static")))))

(defun setup-ob()
  (require 'ob-clang)
  (require 'ob-ctags)
  (setq org-export-use-babel nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk . t)
     (emacs-lisp . t)
     (python . t)
     (ruby . t)
     ;; this should be capital C, the same as in #+begin_src C
     (C . t)
     (ctags . t)
     (dot . t)
     (sqlite . t)
     (lisp . t)
     (srcml . t)
     (clang . t))))

(use-package org
  :straight org-plus-contrib
  :init
  ;; remove using ?) causing a listing
  (setq org-plain-list-ordered-item-terminator '?.)
  ;; :defer t
  :bind (("C-c n" . org-capture)
         ("C-c m" . org-capture-goto-target)
         ("C-c o" . org-open-at-point-global)
         ("C-c t" . org-todo)
         ("C-c a" . org-agenda))
  :config
  (setup-ob)
  (setup-latex)
  (setup-publish)

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
  
  ;; highlight
  (setq org-src-fontify-natively t)
  (setq org-fontify-whole-heading-line t)

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
  )

(use-package org-ref
  :disabled
  :init
  :config
  (use-package helm-bibtex
    :config
    (setq bibtex-completion-cite-prompt-for-optional-arguments nil)
    ;; (setq bibtex-completion-cite-default-as-initial-input t)
    (defun hebi-bibtex-rehash()
      "Invalidate the helm-bibtex bibliograph cache by clear the hash.
Do it will cause the next C-c ] in org-ref (or helm-bibtex)
to rescan the bib files and update pdf and notes notation."
      (interactive)
      (setq bibtex-completion-bibliography-hash "")))

  (global-set-key (kbd "C-c ]") 'org-ref-helm-insert-cite-link)
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
  (setq bibtex-completion-display-formats
        '((t . "${year:4} ${author:36} ${title:*} ${=has-pdf=:1}${=has-note=:1} ${=type=:7} ${keywords:18}")))
  (setq bibtex-completion-additional-search-fields '(keywords))
  (defun hebi-gen-bib ()
    (interactive)
    (insert (org-bibliography-complete-link))))

(provide 'org-conf)
;;; org-conf.el ends here
