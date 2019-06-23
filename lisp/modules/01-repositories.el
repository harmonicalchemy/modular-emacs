;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/01-repositories.el
;;
;; Change Log:
;;   2019-002-05 - Alisha Awen Sheppard Siren1@disroot.org
;;                 added melpa-stable.  Removed Marmalade which is dead now...
;;   2019-004-22 - Alisha Awen Sheppard Siren1@disroot.org
;;                 Fixed URLs to correct TLS enabled domains...
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Load package.el - Modified 2019-002-05 (Added the 3 Main Emacs package Repositories)
(require 'package)


(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://stable.melpa.org/packages/")
        ("melpa-unstable" . "https://melpa.org/packages/")))

(setq package-archive-priorities
      '(("melpa" . 50)
        ("gnu" . 10)
        ("melpa-unstable" . 0)))

;; Pin required for CIDER compatability: (if it gets installed later)

(add-to-list 'package-pinned-packages '(clj-refactor . "melpa-unstable") t)

;;Add melpa-stable to list of repositories:
;(add-to-list 'package-archives
;            '("melpa-stable" . "https://stable.melpa.org/packages/")
;           t)

;;Add melpa to list of repositories:
;(add-to-list 'package-archives
;	     '("melpa" . "https://melpa.org/packages/") 
;            t)

;;Add GNU elpa to list of repositories:
;(add-to-list 'package-archives
;	     '("elpa" . "https://eelpa.gnu.org/packages/") 
;            t)






;;Initialize package.el:

(package-initialize)
