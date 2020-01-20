;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/10-progLang-pkg-conf.el
;;
;; This module adds extra Comp Science Programming Language features
;; to turn Emacs into your go-to mad-scientist esoteric programming lab!
;;
;; Resources:
;;  - CL Cookbook: (common lisp)
;;      https://lispcookbook.github.io/cl-cookbook/
;;  - An Introduction to Programming in Emacs Lisp:
;;      http://www.gnu.org/manual/emacs-lisp-intro/emacs-lisp-intro.html
;;  - Writing GNU Emacs Extensions:
;;      http://www.oreilly.com/catalog/gnuext/
;;  - Emacs Lisp Cheat Sheet:
;;      http://wikemacs.org/wiki/Emacs_Lisp_Cheat_Sheet
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Create repositories cache for devOps extras, if required:

(when (not package-archive-contents)
  (package-refresh-contents))

;; Declare a list of required packages for esoteric computer-science programming:

(defvar me--req-proglang-packages
  '(slime
;   slime-autoloads
    helm-slime
    ac-slime
;    clojure-mode
;    cider
    yasnippet
    yasnippet-snippets
    common-lisp-snippets
    auto-yasnippet
    el-autoyas))

;; Install required packages:

(mapc (lambda (p) (package-install p))
      me--req-proglang-packages)

;;;;;;
;; Load SLIME / sbcl IDE Module:
;;;;;;

(load-file "~/.emacs.d/lisp/modules/10-1-SLIME-sbcl-conf.el")

;;;;
;; Load CIDER / Clojure; IDE Module:  (not using on Linux)
;;;;

;(load-file "~/.emacs.d/lisp/modules/10-2-CIDER-Clojure-conf.el")

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/10-progLang-pkg-conf.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
