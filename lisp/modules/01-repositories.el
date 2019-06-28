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
        ("melpa-unstable" . "https://melpa.org/packages/")))

(setq package-archive-priorities
      '(("MELPA-Stable" . 50)
        ("GNU-ELPA" . 5)
        ("MELPA-Unstable" . 0)))

;; Add your pinned packages to the list below when you encounter an Emacs
;; mode that needs a package to be pinned:

;; Pin clj-refactor package for CIDER compatability:
;; (needed for optional ModEmacs cider-mode Clojure Lisp module)

;(add-to-list 'package-pinned-packages '(clj-refactor . "melpa-unstable") t)


;;
;; Initialize package.el:
;;

(package-initialize)
