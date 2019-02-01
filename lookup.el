;; lookup a chinese character and output its pinyin


;; read pinyin file pychr.txt, build table

(defvar word-to-py
  (make-hash-table :test 'equal))
;; (setq word-to-py
;;       (make-hash-table :test 'equal))

;; (hash-table-size word-to-py)

(defun add-word (word py)
  (puthash
   word
   ;; assume no duplicate
   (cons py (gethash word word-to-py))
   ;; py
   word-to-py))

(defun query-word (word)
  (gethash word word-to-py))

;; (add-word "别" "bie")
;; (add-word "宾" "bin")
;; (query-word "宾")
;; (query-word "并")
;; (query-word "重")

(defun read-dict-pychr ()
  (with-temp-buffer
    (insert-file-contents "pychr.txt")
    (re-search-forward "\\[Table\\]")
    (forward-char)
    (while (not (= (point) (point-max)))
      (let* ((line (thing-at-point 'line t))
             ;; (split-string "bie 别 憋 瘪 蹩 鳖")
             (py (car (split-string line)))
             (words (cdr (split-string line))))
        (prin1 py)
        (mapc (lambda (word)
                (add-word word py))
              words))
      (next-line))))

;; https://github.com/mozillazg/pinyin-data
;; using kMandarin_8105.txt as it is the most common ones
;; support only one tone for now

(defun read-dict-8105 ()
  (with-temp-buffer
    (insert-file-contents "pinyin-data/kMandarin_8105.txt")
    (while (not (= (point) (point-max)))
      (let ((line (thing-at-point 'line t)))
        (when (not (string-prefix-p "#" line))
          (let ((py (second (split-string line)))
                (word (fourth (split-string line))))
            ;; (prin1 py)
            ;; (prin1 word)
            ;; (prin1 py)
            (add-word word py))))
      (next-line))))

(defun hebi-word-to-py ()
  (interactive)
  (when (hash-table-empty-p word-to-py)
    ;; (read-dict-pychr)
    (read-dict-8105))
  (prin1 (query-word (char-to-string
                      (char-before (point))))))

(global-set-key (kbd "C-c h p") 'hebi-word-to-py)

;; !!!!
(string-to-char "且")
(char-to-string #x4e14)

;; (query-word "且")
