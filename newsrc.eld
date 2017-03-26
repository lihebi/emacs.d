;; -*- mode:emacs-lisp; coding: utf-8-emacs; -*-
;; Gnus startup file.
;; Never delete this file -- if you want to force Gnus to read the
;; .newsrc file (if you have one), touch .newsrc instead.
(setq gnus-newsrc-file-version "Gnus v5.13")

(setq gnus-newsrc-last-checked-date '"Sat, 25 Mar 2017 22:10:56 -0500")

(setq gnus-newsrc-alist '(("nndraft:drafts" 1 nil nil (nndraft "") ((gnus-dummy (gnus-draft-mode)))) ("gmane.comp.compilers.llvm.announce" 6 nil) ("gmane.comp.compilers.llvm.devel" 3 nil) ("gmane.comp.compilers.llvm.klee" 3 nil) ("gmane.comp.compilers.clang.devel" 3 nil) ("gmane.comp.compilers.clang.user" 3 nil) ("gmane.comp.java.clojure.user" 3 nil) ("gmane.linux.kernel" 3 nil)))

(setq gnus-server-alist '(("archive" nnfolder "archive" (nnfolder-directory "~/Mail/archive") (nnfolder-active-file "~/Mail/archive/active") (nnfolder-get-new-mail nil) (nnfolder-inhibit-expiry t))))

(setq gnus-topic-topology '(("Gnus" visible) (("misc" visible))))

(setq gnus-topic-alist '(("misc" "nndraft:drafts") ("Gnus" "gmane.comp.compilers.llvm.announce" "gmane.comp.compilers.llvm.devel" "gmane.comp.compilers.llvm.klee" "gmane.comp.compilers.clang.devel" "gmane.comp.compilers.clang.user" "gmane.comp.java.clojure.user" "gmane.linux.kernel")))
