;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   [modular-emacs]:~/.emacs.d/lisp/modules/15-Accounting-pkg-conf.el
;;
;;   This module provides Accounting Features to Emacs...
;;   I am experimenting with this. Currently researching hledger-mode and
;;   Timesheets.el...
;;
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
;; Declare default accounting module list of required packages:

(defvar me--acounting-packages
  '(hledger-mode
    timesheet))

;;;
;; Install required packages

(mapc (lambda (p) (package-install p))
      me--acounting-packages)

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;     *** Basic HLedger Mode Configuration ***
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(require 'hledger-mode)

;;;
;; To open files with .journal extension in hledger-mode

(add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))

;;;
;; Kindly Provide the path to your local journal file...
;; (providing a default location and filename would be too
;;  opinionated, IMHO %^)
;;
;;  btw %^) is my default Picasso cubist smiling emoji..
;;  I have been using this same text emoji since the early 80's!
;;  Maybe I should trade mark it... That's opinionated. lol

(when ME--LINUX
  (setq
   hledger-jfile
   "~/000-GIT/My-Docs/Org-Docs/03-Private/.hledger.journal"))

(when ME--DARWIN
  (setq
   hledger-jfile
   "~/Path/To/Your/Personal/HLedger-Accounting/.hledger.journal"))

;;;
;; Auto-completion for account names
;; For company-mode users,

;(add-to-list 'company-backends 'hledger-company)

;;;
;; For auto-complete users,

(add-to-list 'ac-modes 'hledger-mode)
(add-hook 'hledger-mode-hook
    (lambda ()
        (setq-local ac-sources '(hledger-ac-source))))

;;;
;;  Key BIndings:

(global-set-key (kbd "C-c e") 'hledger-jentry)
(global-set-key (kbd "C-c j") 'hledger-run-command)


;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  *** Custom Functions for Timesheet.el ***
;;  These are for setting up templates within my
;;  own Private sub-directory... They are kept
;;  within a private git repository...
;;  I am making a single GP test case first...
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; ensure TEXINPUTs is set
(require 'preview)

;; Require Timesheet bindings
(require 'timesheet)

;;;
;; Helper Function for me_new-timesheet below...
;; Pretty much a clone of timesheet-template-files ()

(defun ts-template-files ()
  "Return a list of pathnames for timesheet template files."
  (let ((share-dir (expand-file-name "TS-share" my-org-templates)))
    (directory-files share-dir t (concat "\\." (regexp-opt '("tex" "pdf" "org"))  "\\'"))))

;;;
;;  Function: me_new-timesheet()
;;    Set up Customer named: " "?  from template.
;;
;;  Initially I cloned this function directly from
;;  the Timesheet share directory and changed its
;;  name...  Now things are a bit different...
;;  This file is now my first extension of the
;;  original, which uses my own Templates Directory
;;  instead of the share directory within the timesheet
;;  repo...

(defun me_new-timesheet ()
  "Setup a timesheet example with a customer called Yoyodyne."
  (interactive)

  (let* ((org-file "yoyodyne.org")  ;; try changing these names and see what heppens...
         (customer "Yoyodyne")      ;; I hope it does not break all the other 
         (share "share")            ;; timesheet functions for a customer...
         (ts-company-dir (file-name-as-directory (expand-file-name "Timesheets" my-org-files)))
         (ts-share-dir (file-name-as-directory (expand-file-name share ts-company-dir)))
         (ts-customer-dir (file-name-as-directory (expand-file-name customer ts-company-dir)))
         (customer-org (expand-file-name org-file ts-customer-dir)))
    (message (format "Making timesheet example with customer: %s" customer))
    (make-directory ts-share-dir t)
    (make-directory ts-customer-dir t)
    (dolist (f (ts-template-files))
      (if (s-ends-with? ".org" f)
          (copy-file f customer-org t)
        (copy-file f ts-share-dir t)))
    ;; open a buffer with customer-org
    (find-file customer-org)))

(provide 'timesheet)

;;; END: me_new-timesheet ()

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/15-Accounting-pkg-conf.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
