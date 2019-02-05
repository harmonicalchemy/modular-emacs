;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [machine-name]:~/.emacs.d/lisp/modules/dispatcher.el
;;
;; This is a new file, and not part of my original Super-Emacs fork.
;; This is a new encapsulated `Modular Emacs` init file which is called
;; from your default ~/.emacs.d/init.el file... This dispatcher loads everything
;; else, allowing you to use your normal init.el for local tests/customization.
;; You can use this file to turn off (or back on) any extra features by placing
;; (or removing) comment marks in front of the module's load line below...
;;
;; **NOTE:** Removing some of the default required files below may break your
;; configuration so be careful not to pull everything out from under yourself!
;; Encapsulating like features together into separate modules makes customization
;; easier to do however.
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Load Harmonic Alchemy Modular Emacs - Main system.el configuration module:
(load-file "~/.emacs.d/lisp/modules/00-system.el")

;; Load (default) Super Emacs repositories configuration module:
(load-file "~/.emacs.d/lisp/modules/01-repositories.el")

;; Load Harmonic Alchemy Modular Emacs - Standard Packages module:
(load-file "~/.emacs.d/lisp/modules/02-packages.el")

;; Load Harmonic Alchemy Modular Emacs - Dired Extras module:
(load-file "~/.emacs.d/lisp/modules/03-dired-pkg-conf.el")

;; Load Harmonic Alchemy Modular Emacs - devOps module:
(load-file "~/.emacs.d/lisp/modules/04-devOps-pkg-conf.el")

;; Load Harmonic Alchemy Modular Emacs - pubOps module:
(load-file "~/.emacs.d/lisp/modules/05-pubOps-pkg-conf.el")

;; Load Harmonic Alchemy Modular Emacs - Interface module:
(load-file "~/.emacs.d/lisp/modules/06-interface.el")

;; Load Harmonic Alchemy Modular Emacs - Misc Configs module:
(load-file "~/.emacs.d/lisp/modules/07-misc.el")

;; Load Harmonic Alchemy Modular Emacs - Spelling module:
(load-file "~/.emacs.d/lisp/modules/08-spelling.el")

;; Load Harmonic Alchemy Modular Emacs - Frame "cursor" Move module:
;; (similar to windmove, & integrated with same key commands)
(load-file "~/.emacs.d/lisp/modules/09-framemove.el")

;; Load Harmonic Alchemy Modular Emacs - Key Bindings module:
(load-file "~/.emacs.d/lisp/modules/10-key-bindings.el")

