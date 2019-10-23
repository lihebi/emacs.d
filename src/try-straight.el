;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; melpa should be set up no matter I'm using package.el or
;; straight.el
(setq package-archives
      (append package-archives
              '(("org" . "http://orgmode.org/elpa/"))
              '(("melpa" . "http://melpa.milkbox.net/packages/"))))
(package-initialize)
(setq package-enable-at-startup nil)

;; straight

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; (setq use-package-always-ensure t)
(setq straight-use-package-by-default t)
