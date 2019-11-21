(use-package guix)

(use-package simple-drill
  :straight (simple-drill :type git :host github
                          :repo "lihebi/simple-drill.el")
  :config
  (setq simple-drill-history-file "~/git/history-files/simple-drill-history.el"))

(use-package repo-tracker
  :straight (repo-tracker :type git :host github
                          :repo "lihebi/repo-tracker.el")
  ;; Although it requires magit, I load magit with :defer, so adding
  ;; this seems to make this package :defer too, thus config not
  ;; loaded.
  ;;
  ;; :requires magit
  :config
  (setq repo-tracker-repos '(
                             ;; notes
                             "~/git/homepage"
                             "~/git/note"
                             "~/git/research"
                             "~/git/scratch/"
                             "~/git/wallpaper"
                             "~/git/wiki2/"
                             ;; hardware
                             "~/git/hbkb"
                             "~/git/rackematic"
                             "~/git/rackenet"
                             "~/git/smart-scholar-pdfs"
                             ;; emane
                             "~/git/emane"
                             "~/git/emane-tutorial"
                             "~/git/heavy-ball"
                             ;; AI papers
                             "~/git/AdvAE-paper"
                             "~/git/AdvAE"
                             "~/git/causal-vae"
                             "~/git/visual-defense-paper"
                             "~/git/deep-causal-paper"
                             "~/git/rackenet"
                             "~/git/robust-itadv"
                             "~/git/anti-rouge"
                             "~/git/rouge-paper"
                             "~/git/"
                             ;; other research
                             "~/git/helium"
                             "~/git/helium2"
                             ;; configs
                             "~/git/history-files"
                             "~/git/guix-channel"
                             "~/.hebi"
                             "~/.stumpwm.d"
                             "~/.emacs.d"
                             ;; emacs packages
                             "~/.emacs.d/straight/repos/smart-scholar.el"
                             "~/git/biber"
                             "~/git/biber-dist"
                             "~/.emacs.d/straight/repos/simple-drill.el"
                             "~/.emacs.d/straight/repos/scribble-mode.el/"
                             "~/.emacs.d/straight/repos/hn.el/"
                             "~/.emacs.d/straight/repos/repo-tracker.el/"
                             "~/.emacs.d/straight/repos/julia-repl/"
                             "~/.emacs.d/straight/repos/elpy-utils.el/"
                             )))

(use-package hn
  :straight (hn :type git :host github
                :repo "lihebi/hn.el")
  :config
  (setq hn-hl-users '("okanesen" "wilsonfiifi"
                      "neilv" "FigBug" "gumby" "skybrian"
                      "coldtea"
                      "rekado" ;; Guix hacker
                      "pjc50" ;; about startup
                      "pg" ;; PG!!!
                      "logicprog" "dreamcompiler"))
  ;; I need to have a list of "bad" users as well
  (setq hn-bad-users '("codegladiator"))
  (setq hn-tags '("pl" "os" "hw" "ai" "space"))
  (setq hn-hl-keywords '("lisp" "racket" "scheme" "clojure"
                         "haskell" "ocaml"
                         "\\brust\\b" "julia"
                         "WebAssembly" "wasm"
                         "functional"
                         "BSD"
                         "emacs" "linux"
                         "vim"
                         "Idris"
                         "RISC"
                         "Guix" "Nix"
                         ;; "Show HN"
                         ;; "Ask HN"
                         "Raspberry Pi"
                         "\\bML\\b" "\\bAI\\b"
                         "nlp"
                         "Machine Learning" "Artificial Intelligence" "Deep Learning"
                         "Neural Network"
                         "Tensorflow" "pytorch" "torch" "lua"
                         "postmarketOS"))
  (setq hn-history-dir (expand-file-name "~/git/history-files/hn"))
  ;; FIXME can I just use the ~ in the path name without manually expanding?
  (setq hn-export-json-file "/srv/www/assets/hn.json")
  (setq hn-top-dir "~/git/hn-top/")
  (setq hn-fields
        ;; '(star time score comment tag user title)
        ;; '(star score comment tag title)
        '(star time score comment title)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; disabled
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emms
  :disabled
  :config
  (emms-all)
  (emms-default-players)
  (setq emms-source-file-default-directory "~/music")
  (setq emms-tag-editor-rename-format "%a - %t")
  ;; this is very buggy, emms-mark-mode will throw error "no first track",
  ;; and never start emms at all
  ;; (setq emms-playlist-default-major-mode 'emms-mark-mode)
  ;; lyrics must be in lrc format
  ;; lyrics can be placed in the same directory as music file
  ;; or this folder: emms-lyrics-dir
  ;; but since it can only be displayed on minibuffer or mode line, I don't want it right now
  ;; (setq emms-lyrics-dir "~/music/lyrics")
  ;; (emms-lyrics 1)

  (emms-mode-line 1)
  (emms-playing-time 1)

  (require 'emms-player-mpd)
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6600")
  (add-to-list 'emms-info-functions 'emms-info-mpd)
  (add-to-list 'emms-player-list 'emms-player-mpd))


(use-package hackernews
  :disabled)


;; M-x set-input-method eim-wb
;; toggle-input-method
;;
;; starting from z, use pinyin as input and show wb code
;; eim-describe-char show code at point
(use-package emacs-eim
  :disabled
  :straight
  (emacs-eim :type git :host github :repo "wenbinye/emacs-eim"
             :files ("*.txt" :defaults))
  ;; no require, otherwise use-package error: cannot load
  :no-require t
  :init
  (autoload 'eim-use-package "eim" "Another emacs input method")
  :config
  ;; 用 ; 暂时输入英文
  ;; (require 'eim-extra)
  ;; (global-set-key ";" 'eim-insert-ascii)
  (setq eim-use-tooltip nil)
  (register-input-method
   "eim-wb" "euc-cn" 'eim-use-package
   "五笔" "汉字五笔输入法" "wb.txt")
  (register-input-method
   "eim-py" "euc-cn" 'eim-use-package
   "拼音" "汉字拼音输入法" "py.txt"))


;; M-x simple-mpc
;; Need mpc client installed.
(use-package simple-mpc
  :disabled)

