(defun toggle-window-split ()
  "."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x 9") 'toggle-window-split)

;; when split window right, swith to that window
(global-set-key (kbd "C-x 3") (lambda ()
                                (interactive)
                                (split-window-right)
                                (other-window 1)))


(use-package popwin
  ;; use a separate window for buffers like *completion*,
  ;; close them use C-g
  :defer t
  :config
  (popwin-mode 1)
  (push '("*Hebi Output*" :noselect t :tail t) popwin:special-display-config))


(use-package projectile
  :init (progn
          (projectile-global-mode)
          ;; enable catch
          (setq projectile-enable-caching t))
  :bind (("C-c p c" . projectile-compile-project))
  :config
  (setq projectile-switch-project-action 'projectile-dired)
  ;; (setq projectile-track-known-projects-automatically nil)
  ;; when setting this to another file, emacs didn't load it
  ;; (setq projectile-known-projects-file "/home/hebi/.emacs.d/projectile-bookmarks.eld")
  ;; use this to load known projects
  (defun hebi-reload-projectile-known-projects ()
    (interactive)
    (projectile-load-known-projects)))

(use-package perspective
  :init
  :bind (("C-c s" . persp-switch))
  :config (progn
            (persp-mode)
            (setq projectile-switch-project-action 'projectile-dired)))

(use-package persp-projectile
  :bind (("C-c h s" . projectile-persp-switch-project)))

(use-package ace-window
  ;; C-u M-o switch window
  :bind
  (("M-o" . ace-window)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disabled
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package windmove
  :defer t
  :disabled
  :bind
  (("<f2> <right>" . windmove-right)
   ("<f2> <left>" . windmove-left)
   ("<f2> <up>" . windmove-up)
   ("<f2> <down>" . windmove-down)))

