;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/12-progLang-pkg-conf.el
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

(defvar modular-emacs--req-proglang-packages
  '(slime
;   slime-autoloads
    helm-slime
    ac-slime
    clojure-mode
    cider
    yasnippet
    yasnippet-snippets
    common-lisp-snippets
    auto-yasnippet
    el-autoyas))

;; Install required packages:

(mapc (lambda (p)
        (package-install p))
      modular-emacs--req-proglang-packages)


;;;;;;
;; Enable SLIME / sbcl IDE Setup:
;;;;;;

;; Load  - SLIME / sbcl module:
(load-file "~/.emacs.d/lisp/my-modules/12-1-SLIME-sbcl-pkg-conf.el")

;;;;
;; Enable CIDER / Clojure; IDE Setup:
;;;;

;; Load  - CIDER / Clojure module:
;(load-file "~/.emacs.d/lisp/my-modules/12-2-CIDER-Clojure-pkg-conf.el")


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: 12-progLang-pkg-conf.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
