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
  '(ledger-mode
    timesheet))

;;;
;; Install required packages

(mapc (lambda (p) (package-install p))
      me--acounting-packages)

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;     *** Basic Ledger Mode Configuration ***
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(require 'ledger-mode)

;;;
;; To open files with .journal extension in hledger-mode

(add-to-list 'auto-mode-alist '("\\.journal\\'" . ledger-mode))

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
