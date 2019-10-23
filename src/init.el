;; I'm using the full path to avoid possible naming
;; conflicts. Straight is putting so much staff in 'load-path
;;
;; (add-to-list 'load-path "~/.emacs.d/src/")

(load "~/.emacs.d/src/try-straight.el")

;; (use-package dash)
;; ;; try this with helpful-function then -map
;; (use-package helpful)
;; (use-package request)



(load "~/.emacs.d/src/general.el")
(load "~/.emacs.d/src/layout.el")
(load "~/.emacs.d/src/edit.el")
(load "~/.emacs.d/src/mail.el")
(load "~/.emacs.d/src/prog.el")
(load "~/.emacs.d/src/theme.el")
(load "~/.emacs.d/src/apps.el")
(load "~/.emacs.d/src/writing.el")
(load "~/.emacs.d/src/experimental.el")

;; (load "~/.emacs.d/src/remote.el")
;; (load "~/.emacs.d/src/mac.el")

;; TODO move this to a package
(load "~/.emacs.d/src/lookup.el")
