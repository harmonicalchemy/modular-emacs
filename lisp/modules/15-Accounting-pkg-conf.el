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

(defvar modular-emacs--acounting-packages
  '(hledger-mode))

;;;
;; Install required packages

(mapc (lambda (p) (package-install p))
      modular-emacs--acounting-packages)

;;;;
;; Basic HLedger Mode Configuration:
;;;;

(require 'hledger-mode)

;;;
;; To open files with .journal extension in hledger-mode

(add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))

;;;
;; Provide the path to you journal file.
;; The default location is too opinionated.

(setq hledger-jfile "/Documents/000-Alisha/000-GIT/My-Docs/Alisha-Notes/000-Personal/HR-Finance-Legal/HLedger-Accounting/2019.journal")

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
