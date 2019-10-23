(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "C-M-;") 'toggle-comment-on-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++

(defun set-ff-directories()
  (defvar cc-search-directories)
  (setq cc-search-directories
        '("." "/usr/include" "/usr/local/include/*" "$PROJECT/*/include"
          "../include/*/*/*" "../../include/*/*/*"
          "../lib" "../../lib/*/*/*" "../../../lib/*/*"
          ;; FIXME the include for the higher order imbeded function is not found
          "../*/src" "../../src/*/*/*" "../../../src/*/*/*/*" "../../../src/*/*/*"
          ".." "...")))

(add-hook 'c++-mode-hook 'set-ff-directories)
(add-hook 'c-mode-hook 'set-ff-directories)

;; (defvar cc-search-directories)
;; (setq cc-search-directories
;;       (append
;;        '("../include/*/*/*" "../../include/*/*/*" "../lib" "../../lib/*/*/*" "../../../lib/*/*" "../*/src" "../../src/*/*/*")
;;        cc-search-directories))


(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))


;; switch between source and header file
(global-set-key (kbd "C-x C-o") 'ff-find-other-file)

