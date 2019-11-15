;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/15-Accounting-pkg-conf.el
;;
;; This module provides Accounting Features to Emacs...
;; I am currently experimenting with this.  Looking into using hledger-mode etc.
;; Refs:    https://plaintextaccounting.org/
;;          https://github.com/narendraj9/hledger-mode
;;          https://news.ycombinator.com/item?id=13566147
;;          https://github.com/narendraj9/hledger-mode
;; -TBC-
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Create repositories cache, if required
(when (not package-archive-contents)
  (package-refresh-contents))

;; Declare default modular-emacs list of required packages:
(defvar modular-emacs--acounting-packages
  '(hledger-mode))

;; Install required packages
(mapc (lambda (p) (package-install p))
      modular-emacs--acounting-packages)

;; This module is currently a stub... More will be added later ;-)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/15-Accounting-pkg-conf.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
