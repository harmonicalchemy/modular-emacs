;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/dispatcher.el
;;
;; This module is an encapsulated `Modular Emacs` init file which is called
;; from your default ~/.emacs.d/init.el file... This dispatcher loads everything
;; else, allowing you to use your normal init.el for local tests/customization.
;; You can use this file to turn off (or back on) extra features by placing
;; (or removing) comment marks in front of the module's load line below...
;;
;; **NOTE:** Removing (commenting out) some of the default forms could possibly
;; break the default configuration and some key-bindings may cause an error buffer
;; to pop up...  So be careful if you are trying to hack this thing... I am not
;; discouraging you from doing that.. I would do it... Arr!  This is just a heads
;; up about the architecture... Be careful not to pull everything out from under
;; yourself all at once!  Try removing only a few things and see how it works
;; afterwards for a while before removing more things...  Same thing applies to
;; adding new things of course.  Let developer common sense dictate your choices...
;;
;; Encapsulating like features together into separate modules makes customization
;; easier to do.  Have fun hacking this!  Its the best way to learn IMHO ;-) 
;; Make a local topic fork of this so you have a safety net to fall back on...
;;
;; NOTE: One place where modular-emacs is not yet modular are the key-binding
;; modules...  All key-bindings for everything are in those key-bindings .el files.
;; I plan to modularize key-bindings as well by moving related key commands
;; into the modules they exclusively belong to.  This way, removing any single
;; module will also remove everthing exclusively related to it, seamlessly from
;; Emacs without leaving behind artifacts...  For now I need to test things as
;; they are because I have added a lot of new Xaw-Fly-Keys things and need to
;; get that working stable first... (also learn how to use the damn thing! LOL)
;; I'm still cursing myself after accidentally hitting "V" for paste, when I
;; thought I was in insert mode typing plain text!  But I am getting better.
;;
;; This is why it is so important to start out slowly, like learning a new
;; musical instrument... But worse... If you play a wrong note in music, its
;; just a hiccup.. No damage done, except a little embarrassment...
;; On the other hand, if you are typing real fast... (and you will once you
;; start getting cocky with your Xah-Fly-Keys-FU! They are not called Fly
;; for_nuttin!)... If you hit the wrong key you won't realize it until you have
;; typed a whole wrong chord! You won't hear a sound, but the damage will be done
;; and you won't know what the heck happened or how to get back! Slow Down! Drive
;; Safely...
;;
;; Change Log: (descending chronological order)
;;
;;   2019-011-12 - Harmonic Alchemy Modular-Emacs V3.0 [Q4 2019]
;;      This represents mostly updated docs which has been needed for a while!
;;      there were also many bug fixes brought over from the Lisp IDE fork,
;;      where I have been living for most of the summer and fall!  Now the
;;      master and develop branches can benefit the gains...
;;
;;   2019-010-17 - Harmonic Alchemy Modular-Emacs Lisp IDE Fork v2.3.0 [Q3 2019]
;;      This final commit marks the latest sync between Master, Develop, and Lisp
;;      IDE Branches...  All common features and bug fixes have been updated to
;;      all branches.  Lisp IDE branch continues to provide more features with
;;      extra modules enabled...
;;
;;   2019-010-07 - Harmonic Alchemy Modular-Emacs Lisp IDE Fork v2.2.0 [Q4 2019]
;;      I renumbered the module file names to match the exact order in which
;;      they must be evaluated... This is important to prevent some packages
;;      being shadowed by others due to reverse initialization...  This is a
;;      symptom of Lisp being so flexible... "We wanna da flex in da right
;;      places!" (what she said as she turned on the workout exercise machine)
;;      Timing makes all the difference in the world! LOL
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

;;;;
;;              *** Optional Feature ***
;; Load Harmonic Alchemy Modular Emacs - Programming Languages module:
;; This is for using Emacs as a full fledged Common Lisp IDE!
;; This module is enabled by default on the ME lisp-ide branch.  Pull that
;; branch instead if you want to stay up with the programming stuff as
;; it takes time for that to filter into the main branches...

;(load-file "~/.emacs.d/lisp/modules/10-progLang-pkg-conf.el")

;;;;
;;              *** Optional Feature ***
;; Un-comment the load file line below if you would like
;; to explore MOOs or MUDs using a customized Emacs MOO client! ;-)
;; This option is OFF by default.  Make sure to follow the instructions
;; within the ~/.emacs.d/README.md to install the RMOO package first...
;; Load Harmonic Alchemy Modular Emacs - Games module:

;(load-file "~/.emacs.d/lisp/modules/11-games-pkg-conf.el")

;;;;
;;              *** Optional Feature ***
;; Load Harmonic Alchemy Modular Emacs - Xah Emacs module:

;(load-file "~/.emacs.d/lisp/modules/12-Xah-Fly-Keys.el")


;; Load Harmonic Alchemy Modular Emacs - Frame "cursor" Move module:
;; (similar to windmove, & integrated with same key commands)
;;
;; NOTE - Module to be removed!
;;
;; I am going to remove this feature as it is no longer needed
;; and it was causing problems with going into debug mode if
;; the direction had no windoes or frames to move to!
;(load-file "~/.emacs.d/lisp/modules/09-framemove.el")


;;;;
;; Optional/experimental - Load ME S/W Programmer's Key Bindings module:

;(load-file "~/.emacs.d/lisp/modules/14-progLang-key-bindings.el")

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/dispatcher.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

