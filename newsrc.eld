;; -*- mode:emacs-lisp; coding: utf-8-emacs; -*-
;; Gnus startup file.
;; Never delete this file -- if you want to force Gnus to read the
;; .newsrc file (if you have one), touch .newsrc instead.
(setq gnus-newsrc-file-version "Gnus v5.13")

(setq gnus-newsrc-last-checked-date '"Tue, 11 Apr 2017 18:54:20 -0500")

(setq gnus-newsrc-alist '(("nndraft:drafts" 1 nil nil (nndraft "") ((gnus-dummy (gnus-draft-mode)))) ("gmane.comp.compilers.llvm.announce" 6 nil) ("gmane.comp.compilers.llvm.devel" 3 ((1 . 111052) 111082) ((unexist) (seen (110742 . 110759) (110762 . 110768) (110770 . 110786) (110790 . 110793) 110795 (110797 . 110799) (110801 . 110805) (110807 . 110915) (110917 . 110936) (110938 . 110967) (110969 . 111052)))) ("gmane.comp.compilers.llvm.klee" 3 ((1 . 2417)) ((unexist) (seen (2401 . 2417)))) ("gmane.comp.compilers.clang.devel" 3 ((1 . 53246) 53254 53260) ((unexist) (seen (53120 . 53284)))) ("gmane.comp.compilers.clang.user" 3 ((1 . 1004))) ("gmane.comp.java.clojure.user" 3 ((1 . 103722)) ((unexist) (seen (103622 . 103722)))) ("gmane.linux.kernel" 3 ((1 . 2450391)) ((unexist) (seen (2441045 . 2441144) (2449346 . 2450391))))))

(setq gnus-server-alist '(("archive" nnfolder "archive" (nnfolder-directory "~/Mail/archive") (nnfolder-active-file "~/Mail/archive/active") (nnfolder-get-new-mail nil) (nnfolder-inhibit-expiry t))))

(setq gnus-zombie-list '("gwene.com.asana.blog" "gwene.org.opensuse.build.commits.opensuse.factory" "gwene.com.a2central" "gwene.com.codesimplicity" "gwene.com.fireeye.blog.threat-researc.jcr.content" "gwene.com.softpedia.news" "gwene.pt.noticias.acoriano.oriental" "gwene.com.incidental.complexity" "gwene.com.blogspot.jeff-vogel.feeds.posts.default.alt" "gwene.com.johnmenadue" "gwene.com.github.envato.stack.master.commits.master" "gwene.com.foreignpolicy.latest-news" "gwene.com.uhhyeahdude.podcast" "gwene.com.libsyn.billburr" "gwene.com.bealers" "gwene.com.alexvermeer" "gwene.com.ebgconsulting.blog" "gwene.flickr.deutschland" "gwene.com.data.science.central.analytics.courses.discussions" "gwene.phys-all" "gwene.flickr.france.paris" "gwene.io.taiga.blog" "gwene.blog.erikkwakkel" "gwene.com.data.science.central.resources.discussions" "gwene.com.reddit.r.xboxone" "gwene.com.wordpress.patternsinfp" "gwene.com.feedburner.blogspot.tea.masters" "gwene.de.open-access.berlin" "gwene.com.reddit.r.thimbleweedpar" "gwene.com.data.science.central.new.books.and.journals.discussions" "gwene.org.tmplab" "gwene.com.alwaysrainingh" "gmane.org.wikimedia.northern-europe" "gwene.ru.yandex.news" "gwene.org.infrequently.blog" "gwene.uk.co.marcjenkins" "gwene.org.podcast.scotthorton" "gwene.com.data.science.central.research.discussions" "gwene.com.theadhdnerd" "gwene.com.dataartisans" "gwene.fr.cloudfrancois.blog" "gwene.com.podcast.johnbatchelor" "gwene.com.super.mega.comics"))

(setq gnus-topic-topology '(("Gnus" visible) (("misc" visible))))

(setq gnus-topic-alist '(("misc" "nndraft:drafts") ("Gnus" "gmane.comp.compilers.llvm.announce" "gmane.comp.compilers.llvm.devel" "gmane.comp.compilers.llvm.klee" "gmane.comp.compilers.clang.devel" "gmane.comp.compilers.clang.user" "gmane.comp.java.clojure.user" "gmane.linux.kernel")))
