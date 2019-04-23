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

;;Add melpa-stable to list of repositories:
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/")
             t)

;;Add melpa to list of repositories:
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") 
             t)

;;Initialize package.el
(package-initialize)
