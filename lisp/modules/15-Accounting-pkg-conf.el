;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   [modular-emacs]:~/.emacs.d/lisp/modules/15-Accounting-pkg-conf.el
;;
;;   This module provides Accounting Features to Emacs...
;;   I am currently experimenting with this.  Looking into using hledger-mode etc.
;;   Refs:  https://plaintextaccounting.org/
;;          https://github.com/narendraj9/hledger-mode
;;          https://news.ycombinator.com/item?id=13566147
;;          https://github.com/narendraj9/hledger-mode
;; -TBC-
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;;
;; Create repositories cache, if required

(when (not package-archive-contents)
  (package-refresh-contents))

;;;
;; Declare default modular-emacs list of required packages:

(defvar me--acounting-packages
  '(hledger-mode))

;;;
;; Install required packages

(mapc (lambda (p) (package-install p))
      me--acounting-packages)

;;;;
;; Basic HLedger Mode Configuration:
;;;;

(require 'hledger-mode)

;;;
;; To open files with .journal extension in hledger-mode

(add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))

;;;
;; Kindly Provide the path to your local journal file...
;; (a default location would be too opinionated %^)

(setq hledger-jfile "~/path/to/your/HLedger-Accounting/.hledger.journal")

;;;;
;; Auto-completion for account names
;; For company-mode users,

;(add-to-list 'company-backends 'hledger-company)

;;;
;; For auto-complete users,

(add-to-list 'ac-modes 'hledger-mode)
(add-hook 'hledger-mode-hook
    (lambda ()
        (setq-local ac-sources '(hledger-ac-source))))

;;;;
;;  Key BIndings:
;;;;

(global-set-key (kbd "C-c e") 'hledger-jentry)
(global-set-key (kbd "C-c j") 'hledger-run-command)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/15-Accounting-pkg-conf.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
