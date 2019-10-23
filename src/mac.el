(use-package exec-path-from-shell
  ;; when start emacs from desktop env instead of shell, the PATH is aweful.
  ;; :if window-system
  ;;
  ;; I'm disabling it, as it is mostly useful on Mac. On Linux, I can
  ;; now use gdm to load .bash_profile, all applications started from
  ;; there should automatically get those variables
  :disabled
  :config
  (progn
    (exec-path-from-shell-initialize) ;; by default only load $PATH $MANPATH
    ;; (exec-path-from-shell-copy-env "INFOPATH") ;; load $INFOPATH
    (exec-path-from-shell-copy-env "LD_LIBRARY_PATH")
    (exec-path-from-shell-copy-env "LIBRARY_PATH")
    (exec-path-from-shell-copy-env "CPATH")
    (exec-path-from-shell-copy-env "CLASSPATH")
    (exec-path-from-shell-copy-env "ACLOCAL_PATH")
    (exec-path-from-shell-copy-env "PKG_CONFIG_PATH")
    (exec-path-from-shell-copy-env "CMAKE_PREFIX_PATH")
    (exec-path-from-shell-copy-env "PYTHONPATH")
    (exec-path-from-shell-copy-env "C_INCLUDE_PATH")
    (exec-path-from-shell-copy-env "CPLUS_INCLUDE_PATH")
    (exec-path-from-shell-copy-env "GUIX_LOCPATH")
    (exec-path-from-shell-copy-env "GUIX_PACKAGE_PATH")
    (message "%s: %s" "exec-path-from-shell post config" (getenv "PATH"))))

(when (string= system-type "darwin")
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (setq exec-path (append exec-path '("/usr/local/bin"))))


;; Mac Settings
;; use command as meta
(setq mac-command-modifier 'meta)
;; (setq mac-option-modifier 'super)
;; use option as hyper
;; option still use as meta
;; (setq mac-option-modifier 'hyper)
;; (setq ns-function-modifier 'hyper)

