;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/dispatcher.el
;;
;; This module is an encapsulated `Modular Emacs` init file which is called
;; from your default ~/.emacs.d/init.el file... This dispatcher loads everything
;; else, allowing you to use your normal init.el for local tests/customization.
;; You can use this file to turn off (or back on) extra features by placing
;; (or removing) comment marks in front of the module's load line below...
;;
;; **NOTE:** Removing (commenting out) some of the load lines below could possibly
;; break the default configuration and some key-bindings may cause an error buffer
;; to pop up...  So be careful not to pull everything out from under yourself all
;; at once!  Try removing only a few things and see how it works afterwards for
;; a while before removing more things...  Same thing applies to adding new things
;; of course.  Let developer common sense dictate your choices...
;; Encapsulating like features together into separate modules makes customization
;; easier to do.  Have fun customizing!
;;
;; NOTE: One place where modular-emacs is not yet modular are the key-binding
;; modules...  All key-bindings for everything are in those key-bindings .el files.
;; I plan to modularize key-bindings as well by moving related key commands
;; into the modules they exclusively belong to.  This way, removing any single
;; module will also remove everthing exclusively related to it, seamlessly from
;; Emacs without leaving behind artifacts...  For now I need to test things as
;; they are because I have added a lot of new Xaw-Fly-Keys things and need to
;; get that working stable first... (also learn how to use the damn thing! LOL)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Load Harmonic Alchemy Modular Emacs - Main system.el configuration module:
(load-file "~/.emacs.d/lisp/modules/00-system.el")

;; Load (default) Super Emacs repositories configuration module:
(load-file "~/.emacs.d/lisp/modules/01-repositories.el")

;; Load Harmonic Alchemy Modular Emacs - Standard Packages module:
(load-file "~/.emacs.d/lisp/modules/02-package-conf.el")

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

;; Load Harmonic Alchemy Modular Emacs - Xah Emacs module:
;(load-file "~/.emacs.d/lisp/modules/10-Xah-Fly-Keys.el")

;; Optional: Un-comment the load file line below if you would like
;; to explore MOOs or MUDs using a customized Emacs MOO client! ;-)
;; This option is OFF by default.  Make sure to follow the instructions
;; within the ~/.emacs.d/README.md to install the RMOO package first...
;; Load Harmonic Alchemy Modular Emacs - Games module:
;(load-file "~/.emacs.d/lisp/modules/11-games-pkg-conf.el")

;; Optional: Load Harmonic Alchemy Modular Emacs - Programming Languages module:
;; This is for using Emacs as a full fledged Common Lisp IDE!  Don't un-comment
;; this Load Line unless you are an Egghead...  You have been warned!
;(load-file "~/.emacs.d/lisp/modules/12-progLang-pkg-conf.el")

;; Load Harmonic Alchemy Modular Emacs - Key Bindings module:
(load-file "~/.emacs.d/lisp/modules/13-key-bindings.el")

;; Load Harmonic Alchemy Modular Emacs - Key Bindings module:
;(load-file "~/.emacs.d/lisp/modules/14-progLang-key-bindings.el")

