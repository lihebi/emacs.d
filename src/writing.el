;;; writing.el --- tex/org package load and config

;;; Commentary:
;; No Comments!

;;; Code:

;; To make the citation works:
;;
;; 1. in orgmode, run hebi-gen-bib. This will insert bib file links at
;; the end of the org file.
;;
;; 2. In latex file, C-c TAB (translated from C-c <tab>) runs the
;; command tex-bibtex-file (found in latex-mode-map).  This is defined
;; in tex-mode instead of auctex.
;;
;; Of course you need to run pdflatex or org-latex-export-to-pdf again

;; (use-package ebib)
(use-package tex
  :straight auctex
  :defer t
  :config
  (setq TeX-open-quote "\"")
  (setq TeX-close-quote "\"")
  (add-hook 'LaTeX-mode-hook
            '(lambda()
               (define-key LaTeX-mode-map (kbd "C-c ]") 'helm-bibtex)))
  ;; (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
  (add-hook 'LaTeX-mode-hook
            '(lambda()
               (add-to-list 'LaTeX-verbatim-environments "lstlisting")))
  ;; (define-key LaTeX-mode-map (kbd "C-c t") 'reftex-toc)
  (setq TeX-open-quote "\"")
  (setq TeX-close-quote "\"")
  (if (string= system-type "darwin")
      (progn
        (setq TeX-view-program-selection '((output-pdf "Skim"))))
    (setq TeX-view-program-selection '((output-pdf "PDF Tools"))))
  ;; supporting indentation of [] in LaTeX mode
  (defun TeX-brace-count-line ()
    "Count number of open/closed braces."
    (save-excursion
      (let ((count 0) (limit (line-end-position)) char)
        (while (progn
                 (skip-chars-forward "^{}[]\\\\" limit)
                 (when (and (< (point) limit) (not (TeX-in-comment)))
                   (setq char (char-after))
                   (forward-char)
                   (cond ((eq char ?\{)
                          (setq count (+ count TeX-brace-indent-level)))
                         ((eq char ?\})
                          (setq count (- count TeX-brace-indent-level)))
                         ((eq char ?\[)
                          (setq count (+ count TeX-brace-indent-level)))
                         ((eq char ?\])
                          (setq count (- count TeX-brace-indent-level)))
                         ((eq char ?\\)
                          (when (< (point) limit)
                            (forward-char)
                            t))))))
        count))))

(when (not (string= system-type "darwin"))
  (use-package pdf-tools
    :init
    ;; FIXME why emacs keeps remove the build directory?
    ;; (setq pdf-info-epdfinfo-program "/home/hebi/.emacs.d/straight/build/pdf-tools/epdfinfo")
    (setq pdf-info-epdfinfo-program "~/.emacs.d/epdfinfo")
    :config
    ;; This seems also sets the default viewing mode of pdf, and it
    ;; seems to honor the pdf-info-epdfinfo-program variable, i.e. put
    ;; the executable there, and don't build if exist
    (pdf-tools-install)
    (setq pdf-view-resize-factor 1.03)
    (defun pdf-view-fit-paper(number)
      ;; using P for horizontal reading
      ;; using C-u P for vertical reading
      (interactive "p")
      (if (= number 1)
          (progn
            ;; landscape
            (setq pdf-view-display-size 1.53)
            (image-set-window-vscroll 6))
        (progn
          ;; portrait
          (setq pdf-view-display-size 2.05)
          (image-set-window-hscroll 11)))
      (pdf-view-redisplay t))
    (defun hebi-pdf-vert-22 ()
      (interactive)
      (setq pdf-view-display-size 2.05)
      (image-set-window-hscroll 11)
      (pdf-view-redisplay t))
    ;; C-c C-r m
    ;; pdf-view-midnight-minor-mode
    (setq pdf-view-midnight-colors
          ;; '("white" . "black")
          ;; '("#839496" . "#002b36")
          '("white" . "#002b36"))
    (define-key pdf-view-mode-map (kbd "P") 'pdf-view-fit-paper)))



;; the headerline bullets
;; (use-package org-bullets
;;   :config
;;   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; used for fontify code in exporting of org
(use-package htmlize)

(defun setup-latex ()
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
               '("naacl"
                 "\\documentclass[11pt,a4paper]{article}
\\usepackage[hyperref]{naaclhlt2019}
\\usepackage{times}
\\usepackage{latexsym}
"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("nips"
                 "\\documentclass{article}
\\usepackage[preprint]{nips_2018}
\\usepackage[utf8]{inputenc} % allow utf-8 input
\\usepackage[T1]{fontenc}    % use 8-bit T1 fonts
\\usepackage{hyperref}       % hyperlinks
\\usepackage{url}            % simple URL typesetting
\\usepackage{booktabs}       % professional-quality tables
\\usepackage{amsfonts}       % blackboard math symbols
\\usepackage{nicefrac}       % compact symbols for 1/2, etc.
\\usepackage{microtype}      % microtypography
"
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

  ;; (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  (add-to-list 'org-latex-packages-alist '("newfloat" "minted"))
  
  (setq org-latex-listings 'minted)
  ;; (setq org-latex-listings 'listings)
  (setq org-latex-minted-options
        '(("frame" "lines")
          ("linenos" "true")
          ("xleftmargin" "0.1\\textwidth")
          ("xrightmargin" "0.1\\textwidth")))
  (setq org-latex-default-figure-position "H")
  (setq org-latex-pdf-process '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

(defun setup-publish()
  ;; set the default export headline toc level
  ;; it can also be set by #+OPTIONS: H:6
  ;; but it can NOT be set by #+OPTIONS: toc:6
  ;; default is 3, but I want to show everything by default
  ;; also note that in org-publish-project-alist, the headline is set to 4
  ;; this may need some change in the future.
  (setq org-export-headline-levels 6)
  (setq org-html-link-home "/")

  (defvar org-html-head)
  (setq org-html-head
        (concat
         ;; FIXME using absolute path would make local file access
         ;; unusable.  I probably want to put it in a webserver.
         "<link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/org.css\" />"
         "<link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/hebi.css\" />"
         ;; "<link rel=\"stylesheet\" type=\"text/css\" href=\"assets/org.css\" />"
         ;; "<link rel=\"stylesheet\" type=\"text/css\" href=\"assets/hebi.css\" />"
         ;; "<link rel=\"stylesheet\" type=\"text/css\" href=\"../assets/org.css\" />"
         ;; "<link rel=\"stylesheet\" type=\"text/css\" href=\"../assets/hebi.css\" />"
         ))
  (setq org-html-validation-link nil)
  (setq org-export-time-stamp-file nil)
  (setq org-export-with-author nil)
  (setq org-export-with-date nil)
  ;; (setq org-export-with-toc nil)
  (setq org-export-with-timestamps t)


  ;; Define a fixed theme for code blocks of html exports, instead of
  ;; inline css based on current theme.
  (setq org-html-htmlize-output-type 'css)
  ;; use org-html-htmlize-generate-css to generate for the current
  ;; theme. But this generated theme is not working, e.g. it does not
  ;; have org-keyword.  I end up using:
  ;;
  ;; https://www.reddit.com/r/emacs/comments/47903g/use_a_different_theme_when_publishing_org_files/
  ;;
  ;; https://cdn.rawgit.com/kaushalmodi/.emacs.d/master/misc/css/leuven_theme.css
  ;;
  ;; This is the default option:
  ;; (setq org-html-htmlize-font-prefix "org-")
  
  ;; see also: http://orgmode.org/worg/org-tutorials/org-publish-html-tutorial.html
  (setq org-publish-project-alist
        '(("wiki-org"
           :base-directory "~/git/wiki/"
           :base-extension "org"
           :publishing-directory "~/git/wiki-dist/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :html-extension "html"
           ;; experimental
           :auto-sitemap t)
          ("wiki-static"
           :base-directory "~/git/wiki/"
           :base-extension "ttf\\|js\\|css\\|png"
           :recursive t
           :publishing-directory "~/git/wiki-dist/"
           :publishing-function org-publish-attachment)
          ;; this is still TODO
          ;; the C-c C-e Pp cannot publish wiki-static
          ;; I have to use C-c C-e Px and choose wiki-static for css and ttf
          ;; but it's fine for now
          ("wiki" :components ("wiki-org" "wiki-static"))
          ("note"
           :base-directory "~/git/note/"
           :base-extension "org"
           :publishing-directory "~/git/note-dist/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :html-extension "html"
           ;; experimental
           :auto-sitemap t)
          ("homepage-org"
           :base-directory "~/git/homepage"
           :base-extension "org"
           ;; CAUTION I need to create the directory and chown to
           ;; user:group
           :publishing-directory "/srv/www"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :html-extension "html"
           :auto-sitemap t)
          ("homepage-static"
           :base-directory "~/git/homepage/"
           :publishing-directory "/srv/www"
           :base-extension "ttf\\|js\\|css\\|png\\|pdf\\|jpg\\|json"
           :recursive t
           :publishing-function org-publish-attachment)
          ("homepage" :components ("homepage-org" "homepage-static")))))

(defun setup-ob ()
  ;; (require 'ob-clang)
  ;; (require 'ob-ctags)
  (setq org-export-use-babel nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk . t)
     (emacs-lisp . t)
     (python . t)
     (ruby . t)
     ;; this should be capital C, the same as in #+begin_src C
     (C . t)
     ;; (ctags . t)
     ;; (clang . t)
     ;; (srcml . t)
     (dot . t)
     (sqlite . t)
     (lisp . t))))

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

  ;; (require 'org-drill)

  (setq org-descriptive-links nil)
  ;; (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (setq org-log-done 'time)
  (setq org-startup-folded nil)
  (setq org-yank-folded-subtrees nil)
  ;; the forbidden, by default, is ,'", but I want all of them actually. By the way why these are forbidden?
  (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n")
  ;; How many newlines allowed inside a marking, defaults to 1
  (setcar (nthcdr 4 org-emphasis-regexp-components) 8)
  ;; Need to evalute this after setting the above
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  ;; original value is t
  ;; (setq org-adapt-indentation t)
  (setq org-export-backends (append '(man) org-export-backends))
  (define-key org-mode-map (kbd "C-j") (lambda()
                                         (interactive)
                                         (join-line -1)))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)" "CANCELED(c)")))
  (setq org-todo-keyword-faces
        '(("TODO" . org-todo) ("CANCELED" . org-warning) ("STARTED" . (:foreground "white" :background "red"))))

  (setq org-agenda-files
        '("~/git/note/org"))

  ;; org capture
  (setq org-directory "~/git/note/org")
  (setq org-default-notes-file "~/git/note/org/default.org")

  (set-face-attribute 'org-level-1 nil
                      :height 2.0 :weight 'bold)
  (set-face-attribute 'org-level-2 nil
                      :height 1.3 :weight 'bold)

  ;; typical command
  ;; 1. org-capture: C-c n
  ;; 2. org-capture-goto-target C-c m
  ;; 3. org-agenda: C-c a
  (defun gen-template (key title)
    `(,key ,title entry
           (file+headline "~/git/note/gtd.org" ,title)
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

  
  ;; Stay the same position after project publishing. The original
  ;; code uses save-window-excursion, and jumps to top eveytime.
  (defun my-org-project-advice (orig-fun &rest args)
    (save-excursion
      (apply orig-fun args)))
  (advice-add 'org-publish-current-file
              :around #'my-org-project-advice)
  (advice-add 'org-publish-current-project
              :around #'my-org-project-advice))

;; FIXME There is a performance problem. In bibtex-completion-init,
;; remove the first mapc for "rm-watch" fixes it.
(use-package org-ref
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

  ;; link message will freeze emacs in case of many bib files
  (org-ref-cancel-link-messages)

  (global-set-key (kbd "C-c ]") 'org-ref-helm-insert-cite-link)
  (define-key bibtex-mode-map
    (kbd "C-c b")
    'org-ref-bibtex)

  (let ((bib-pdf-dir
         '("~/git/research/pdf/manual/"
           ;; "~/git/papers/"
           ;; "~/git/books/"
           )))
    (setq org-ref-pdf-directory bib-pdf-dir)
    (setq bibtex-completion-library-path bib-pdf-dir))
  
  (when (string= system-type "darwin")
    (setq bibtex-completion-pdf-open-function
          (lambda (fpath)
            (start-process "open" "*open*" "open" fpath)))
    (setq org-ref-open-pdf-function
          (lambda (fpath)
            (start-process "open" "*open*" "open" fpath))))
  (setq bibtex-completion-display-formats
        '((t . "${=key=:15}    ${author:36} ${title:*} ${=has-pdf=:1}${=has-note=:1} ${=type=:7} ${keywords:18}")))
  (setq bibtex-completion-additional-search-fields '(keywords))
  (defun hebi-gen-bib ()
    (interactive)
    (insert (org-bibliography-complete-link))))

(use-package smart-scholar
  :straight (smart-scholar :type git :host github
                           :repo "lihebi/smart-scholar.el")
  :config
  (setq smart-scholar-pdf-dir "~/git/smart-scholar-pdfs")
  (setq smart-scholar-bib-dir "~/git/biber-dist")
  (setq smart-scholar-manual-bib-dir "~/git/research/bib/")
  (smart-scholar-load-manual)
  ;; setup initial pdf dirs
  (set-org-ref-pdfdir)
  ;; must be evaluated AFTER org package, because I'm overwriting
  ;; doi-utils-get-bibtex-entry-pdf function
  (defun doi-utils-get-bibtex-entry-pdf ()
    (smart-scholar-bibtex-download-pdf-at-point)))

(provide 'writing)
;;; writing.el ends here
