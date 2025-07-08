;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/10-progLang-pkg-conf.el
;;
;; This module adds extra Programming Language features to turn Emacs into
;; your go-to mad-scientist esoteric programming lab! (and practical stuff too %^)

;; NOTE: I disabled loading CIDER as I am not currently using CIDER but the
;;       code is in place below to get that up and running as well if needed...

;; Resources:
;;  - CL Cookbook: (common lisp)
;;      https://lispcookbook.github.io/cl-cookbook/
;;  - An Introduction to Programming in Emacs Lisp:
;;      http://www.gnu.org/manual/emacs-lisp-intro/emacs-lisp-intro.html
;;  - Writing GNU Emacs Extensions:
;;      http://www.oreilly.com/catalog/gnuext/
;;  - Emacs Lisp Cheat Sheet:
;;      http://wikemacs.org/wiki/Emacs_Lisp_Cheat_Sheet
;;  - How to Configure emacs for multiple language WebDev:
;;      https://github.com/fgallina/multi-web-mode

;;
;; Change Log: (descending chronological order)
;;

;; 2022-009-18 - Alisha Awen, HarmonicAlchemy@proton.me
;;   Not using Slime/SBCL anymore...
;;   ALL OF SBCL for Emacs needs a re-factoring now.
;;   CURRENTLY THIS FILE DOES NOTHING!!!  It is ALL being refactored...

;;   2021-008-09 - Alisha Awen
;;       Multi Web Mode sucks!!! OMG! Switched to web-mode now.  It is much
;;       better/easier and can be customized if needed to your hearts content!

;;   2021-008-03 - Alisha Awen
;;       Added Multi Web Mode Configurations - can't imagine why this stuff
;;       was not already done years ago?  LOL I even remember doing this before!
;;       Must have been another lifetime. %^)
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;; Create repositories cache for devOps extras, if required:

;(when (not package-archive-contents)
;  (package-refresh-contents))

;;; Declare a list of required packages for computer programming:

;(defvar me--req-proglang-packages
;  '(
;    slime
;   slime-autoloads
;    helm-slime
;    ac-slime        ;; Not using Auto Complete Anymore For Anything...
;    clojure-mode
;    cider
;    web-mode
;    multi-web-mode
;    yasnippet
;    yasnippet-snippets
;    common-lisp-snippets
;    auto-yasnippet
;    el-autoyas
;    ))

;;; Install required packages:

;(mapc (lambda (p) (package-install p))
;      me--req-proglang-packages)

;;;;
;;   NOTE:  I am turning all my previous WEB stuff OFF and
;;          trying out xah Lee's WEB mode stuff...
;;          regular web-mode is commented out now...

;;;
;;   Set up Web Mode:
;;   Ref: https://web-mode.org/
;;   Info:
;;
;;      IMHO - This seems to be the best mode for www dev to date...
;;      The mode works well out of box but there are also a slew of
;;      features you can add/customize!  See link above...
;;      All the other (active-semi-active) web related emacs modes
;;      I have looked at seem far too complicated & have bugs!!! OMG!!!

;(require 'web-mode)

;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;;; Set up customizations for Hook:

;; (defun me_web-mode-hook ()
;;   "Hooks for Web mode."
;;   (setq web-mode-markup-indent-offset 2)
;;   (setq web-mode-css-indent-offset 2)
;;   (setq web-mode-code-indent-offset 2))

;; (add-hook 'web-mode-hook 'me_web-mode-hook)

;;; END: web-mode Customizations


;;;
;; Load SLIME / sbcl IDE Module:
;;;

;(load-file "~/.emacs.d/lisp/modules/10-1-SLIME-sbcl-conf.el")


;;;
;; Load CIDER / Clojure; IDE Module:
;; NOTE: Currently not using on Linux but MacOS maybe later...
;;;

;(load-file "~/.emacs.d/lisp/modules/10-2-CIDER-Clojure-conf.el")

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/10-progLang-pkg-conf.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
