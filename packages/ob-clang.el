;;; ob-clang.el --- provide org mode execution support for clang AST

;;; Commentary:

;;; Code:


(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:clang '())

;; This function will evaluate the body of the source code and
;; return the results as emacs-lisp depending on the value of the
;; :results header argument
;; - output means that the output to STDOUT will be captured and
;;   returned
;; - value means that the value of the last statement in the
;;   source code block will be returned
;;
;; The most common first step in this function is the expansion of the
;; PARAMS argument using `org-babel-process-params'.
;;
;; Please feel free to not implement options which aren't appropriate
;; for your language (e.g. not all languages support interactive
;; "session" evaluation).  Also you are free to define any new header
;; arguments which you feel may be useful -- all header arguments
;; specified by the user will be available in the PARAMS variable.
;;
;; usage of ob-clang
;; #+BEGIN_SRC clang :lang cpp
;; the content should be c or cpp code
;; use clang to parse it, and output the AST in pretty format
(defun org-babel-execute:clang (body params)
  "Execute a block of Clang code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing Clang source code block")
  (prin1 params t)
  (let* ((processed-params (org-babel-process-params params))
         (lang (if (assoc :lang params)
                   ;; FIXME lang parameter should be one of c or cpp
                   (cdr (assoc :lang params)) "c"))
         (in-file (org-babel-temp-file "clang-" (concat "." lang)))
         (cmd (concat "clang -Xclang -ast-dump -fsyntax-only "
                      (org-babel-process-file-name in-file))))
    (with-temp-file in-file (insert (concat body)))
    (message "%s" cmd)
    (org-babel-eval cmd "")
    ))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:clang (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  )

(defun org-babel-clang-var-to-clang (var)
  "Convert an elisp var into a string of clang source code
specifying a var of the same value."
  (format "%S" var))

(defun org-babel-clang-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  )

(defun org-babel-clang-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
  (unless (string= session "none")
    ))


(provide 'ob-clang)
;;; ob-clang.el ends here
