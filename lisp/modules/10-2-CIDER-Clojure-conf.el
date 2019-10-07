;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/10-2-CIDER-Clojure-pkg-conf.el
;;
;; This module adds CIDER / Clojure support to Modular Emacs...  This is a more
;;  up to date IDE for Clojure than SLIME ever was!  Hopefully we can switch
;;  easily from one to the other with Modular Emacs! :pray:
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

;;;;
;; Enable cider-mode:
;;  cider-mode is normally enabled automatically when you start CIDER, but you
;;  can also enable it explicitly for Clojure(Script) buffers like this:

(add-hook 'clojure-mode-hook #'cider-mode)

;; Note: There’s no need to enable it explicitly for modes derived from
;;       clojure-mode like clojurescript-mode.

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/10-2-CIDER-Clojure-pkg-conf.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
