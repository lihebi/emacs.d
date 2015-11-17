;;; hebi-faces.el --- color and faces

;;; Commentary:

;;; Code:

;; list-colors-display
;; list-faces-display
(defvar hebi-red-face 'hebi-red-face)
(defvar hebi-green-face 'hebi-green-face)
(defvar hebi-yellow-face 'hebi-yellow-face)
(defvar hebi-cyan-face 'hebi-cyan-face)
(defface hebi-red-face
  '((t :background "black" :foreground "red"))
  ""
  :group 'hebi-faces)

(defface hebi-green-face-face
  '((t :background "black" :foreground "green"))
  ""
  :group 'hebi-faces)

(defface hebi-cyan-face
  '((t :foreground "cyan"))
  ""
  :group 'hebi-faces)

(defface hebi-yellow-face
  '((t :foreground "yellow"))
  ""
  :group 'hebi-faces)


;;; hebi-faces.el ends here
