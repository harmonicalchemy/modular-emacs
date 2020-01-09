;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/01-repositories.el
;;
;; Change Log:
;;
;;   2019-006-23 - Alisha Awen Sheppard Siren1@disroot.org
;;      Completely re-wrote this module to improve quality
;;      of package installs to the most stable versions
;;      while allowing installs of systems requiring "pinned"
;;      packages at the same time... It's a balancing act...
;;      Thank God Emacs is Lisp based. We can do this! ;-)
;;
;;   2019-002-05 - Alisha Awen Sheppard Siren1@disroot.org
;;      Added melpa-stable.  Removed Marmalade which is dead now...
;;
;;   2019-004-22 - Alisha Awen Sheppard Siren1@disroot.org
;;      Fixed URLs to correct TLS enabled domains...
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Load package.el - Modified 2019-002-05 (Added the 3 Main Emacs package
;; Repositories)

(require 'package)

(setq package-archives
      '(("GNU-ELPA" . "https://elpa.gnu.org/packages/")
        ("MELPA-Stable" . "https://stable.melpa.org/packages/")
        ("MELPA-Unstable" . "https://melpa.org/packages/")
        ("ORG"   . "https://orgmode.org/elpa/")))

(setq package-archive-priorities
      '(("MELPA-Stable" . 50)
        ("ORG" . 5)
        ("GNU-ELPA" . 5)
        ("MELPA-Unstable" . 0)))

;;;;
;; Some packages are not loading due to a current bug in emacs...
;; This is supposed to be fixed in version 26.3  Here is the workaround...
;; When they fix this you may have to comment this out!  Don't forget... ;-)

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Add your pinned packages to the list below when you encounter an Emacs
;; mode that needs a package to be pinned:

;; Pin clj-refactor package for CIDER compatability:
;; (needed for optional ModEmacs cider-mode Clojure Lisp module)

;(add-to-list 'package-pinned-packages '(clj-refactor . "melpa-unstable") t)

;;;;
;; Always try to load the newest up-to-date packages...

(setq load-prefer-newer t)

;;
;; Initialize package.el:
;;

(package-initialize)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/01-repositories.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
