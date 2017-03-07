;;; gnus.el --- Gnus configure file

;;; Commentary:

;;; Code:


(defvar gnus-default-nntp-server)
(defvar gnus-select-method)
(defvar gnus-use-adaptive-scoring)
(defvar gnus-secondary-select-methods)
(setq gnus-select-method
      '(nntp "news.gmane.org"))
(setq gnus-secondary-select-methods
      '((nntp "news.gmane.org")))
(setq gnus-default-nntp-server "news.gmane.org")
(setq gnus-use-adaptive-scoring t)
(add-hook 'gnus-group-mode-hook
          '(gnus-topic-mode hl-line-mode))
(defvar gnus-read-newsrc-file)
(defvar gnus-save-newsrc-file)
(setq gnus-read-newsrc-file nil)
(setq gnus-save-newsrc-file nil)

(defvar gnus-summary-mode-hook)
(setq gnus-summary-mode-hook 'hl-line-mode)

;;; gnus.el ends here
