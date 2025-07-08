;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/dispatcher.el
;;
;; This module is an encapsulated `Modular Emacs` init file which is called
;; from your default ~/.emacs.d/init.el file... This dispatcher loads everything
;; else, allowing you to use your normal init.el for local tests/customization.
;; You can use this file to turn off (or back on) extra features by placing
;; (or removing) comment marks in front of the module's load line below...

;; NOTE: Removing (commenting out) some of the default forms could possibly
;; break the default configuration and some key-bindings may cause an error buffer
;; to pop up...  So be careful if you are trying to hack this thing... I am not
;; discouraging you from doing that.. I would do it... Arr!  This is just a heads
;; up about the architecture... Be careful not to pull everything out from under
;; yourself all at once!  Try removing only a few things and see how it works
;; afterwards for a while before removing more things...  Same thing applies to
;; adding new things of course.  Let developer common sense dictate your choices...

;; Encapsulating like features together into separate modules makes customization
;; easier to do.  Have fun hacking this!  Its the best way to learn IMHO ;-) 
;; Make a local topic fork of this so you have a safety net to fall back on...

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

;; 2022-010-25 - Alisha Awen, HarmonicAlchemy@proton.me
;;   Added a new module: me-abbrev-defs.el
;;   Which contains GLOBAL Abbrev Definitions taken directly from Xah Lee's 
;;   webpage:  http://xahlee.info/emacs/emacs/emacs_abbrev_mode.html
;;   Some new global abbrevs were also added by me. (or rather Modular
;;   Emacs)... IT may be wise NOT to change many of these default abbrevs,
;;   (except personal ones like your name etc.)... Instead, use the section
;;   at the BOTTOM of the list within me-abbrev-defs.el (where noted in the
;;   file), and add your new custom abbrev forms there... (adding to the list)
;;   These default global abbrevs work in all emacs modes... You can make
;;   some exclusive to specific modes as well...
;;
;; 2022-005-15 - Alisha Awen, HarmonicAlchemy@proton.me
;;   Added a new form that invokes new file: 06-XahEmacs-conf.el...
;;   This module is disabled by default (the form is commented out) You won't
;;   be able to use it unless you have purchased XahEmacs...
;;   IF YOU DO HAVE the PREMIUM XahEmacs PACKAGE, you will need to create a
;;   sub-directory: i.e., ~/.emacs.d/lisp/my-modules/XahEmacs and then copy the
;;   individual XahEmacs package directories into there in order to be able to
;;   use them within Harmonic Alchemy Modular Emacs... (see CONFIGURATION notes
;;   within 06-XahEmacs-conf.el for complet instructions...
;;
;;   After doing the above, edit this file and remove the comment characters
;;   from the form titled: *** Optional XahEmacs Premium Features ***
;;   That enables the module...
;;
;;   Currently the only XahEmacs modules enabled by Harmonic Alchemy Modular
;;   Emacs are: xah-elisp-mode and xah-find, but more will be added as I evaluate
;;   them and find a good use-case for them within the context of Harmonic Alchemy
;;   Modular Emacs...

;; 2022-005-15 - Alisha Awen, HarmonicAlchemy@proton.me
;;   Renamed: 02-package-conf.el to: 02-common-conf.el
;;   Removed; 06-interface.el and the form here that loads that file...

;; 2021-004-08 - Updated Modular Emacs to v3.4.0 [Q2 2021]
;;   (still beta - working on updating many docs to org-mode and
;;   publishing as PDFs) This version includes new feature additions &
;;   bugfixes...  A few mentions of version updates are missing below...
;;   Oh Well... All up to date now...

;; 2019-011-12 - Harmonic Alchemy Modular-Emacs V3.0 [Q4 2019]
;;   This represents mostly updated docs which has been needed for a while!
;;   there were also many bug fixes brought over from the Lisp IDE fork,
;;   where I have been living for most of the summer and fall!  Now the
;;   master and develop branches can benefit the gains...

;; 2019-010-17 - Harmonic Alchemy Modular-Emacs Lisp IDE Fork v2.3.0 [Q3 2019]
;;   This final commit marks the latest sync between Master, Develop, and Lisp
;;   IDE Branches...  All common features and bug fixes have been updated to
;;   all branches.  Lisp IDE branch continues to provide more features with
;;   extra modules enabled...

;; 2019-010-07 - Harmonic Alchemy Modular-Emacs Lisp IDE Fork v2.2.0 [Q4 2019]
;;   I renumbered the module file names to match the exact order in which
;;   they must be evaluated... This is important to prevent some packages
;;   being shadowed by others due to reverse initialization...  This is a
;;   symptom of Lisp being so flexible... "We wanna da flex in da right
;;   places!" (what she said as she turned on the workout exercise machine)
;;   Timing makes all the difference in the world! LOL
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;;
;; Load Harmonic Alchemy Modular Emacs Main Config Module:

(if (file-exists-p "~/.emacs.d/lisp/my-modules/00-system.el")
    (load-file "~/.emacs.d/lisp/my-modules/00-system.el")
  (load-file "~/.emacs.d/lisp/modules/00-system.el"))

;;;
;; Load (default) Emacs Package Repositories Config Module:

(if (file-exists-p "~/.emacs.d/lisp/my-modules/01-repositories.el")
    (load-file "~/.emacs.d/lisp/my-modules/01-repositories.el")
  (load-file "~/.emacs.d/lisp/modules/01-repositories.el"))

;;;
;; Load Emacs Common Interface Configurations Module:

(if (file-exists-p "~/.emacs.d/lisp/my-modules/02-common-conf.el")
    (load-file "~/.emacs.d/lisp/my-modules/02-common-conf.el")
  (load-file "~/.emacs.d/lisp/modules/02-common-conf.el"))

;;;;
;;           *** Optional Xah Fly Keys Feature ***
;;
;; NOTE: You will need to clone:
;;       https://github.com/harmonicalchemy/xah-fly-keys.git
;;       or Clone: Xah's source repo (the above is a fork) before
;;       enabling this form... (by un-commenting it)
;;        
;; Load - Xah Fly Keys module:

(if (file-exists-p "~/.emacs.d/lisp/my-modules/12-Xah-Fly-Keys.el")
    (load-file "~/.emacs.d/lisp/my-modules/12-Xah-Fly-Keys.el")
  (load-file "~/.emacs.d/lisp/modules/12-Xah-Fly-Keys.el"))

;;;;
;;           *** Optional XahEmacs Premium Features ***
;;
;;   DON'T ENABLE THIS UNLESS YOU HAVE PURCHASED THIS PACKAGE
;;   If you did purchase the premium XahEmacs package, read
;;   the instructions for getting these packages set up within
;;   file: 06-XahEmacs-conf.el... and then enable this form by
;;   un-commenting it...
;;
;; Load - XahEmacs module:

(if (file-exists-p "~/.emacs.d/lisp/my-modules/06-XahEmacs-conf.el")
    (load-file "~/.emacs.d/lisp/my-modules/06-XahEmacs-conf.el")
  (load-file "~/.emacs.d/lisp/modules/06-XahEmacs-conf.el"))

;;;
;; Load Harmonic Alchemy Modular Emacs - Key Bindings module:

(if (file-exists-p "~/.emacs.d/lisp/my-modules/13-key-bindings.el")
    (load-file "~/.emacs.d/lisp/my-modules/13-key-bindings.el")
  (load-file "~/.emacs.d/lisp/modules/13-key-bindings.el"))

;;;
;; Load - Custom: Abbrevs (Mod Emacs Defaults -AND/OR- Your Personal Overrides)

(if (file-exists-p "~/.emacs.d/lisp/my-modules/me-abbrev-defs.el")
    (load-file "~/.emacs.d/lisp/my-modules/me-abbrev-defs.el")
  (load-file "~/.emacs.d/lisp/modules/me-abbrev-defs.el"))

;;;
;; Load Harmonic Alchemy Modular Emacs - Dired Extras module:

(if (file-exists-p "~/.emacs.d/lisp/my-modules/03-dired-pkg-conf.el")
    (load-file "~/.emacs.d/lisp/my-modules/03-dired-pkg-conf.el")
  (load-file "~/.emacs.d/lisp/modules/03-dired-pkg-conf.el"))

;;;
;; Load Harmonic Alchemy Modular Emacs - devOps module:

(if (file-exists-p "~/.emacs.d/lisp/my-modules/04-devOps-pkg-conf.el")
    (load-file "~/.emacs.d/lisp/my-modules/04-devOps-pkg-conf.el")
  (load-file "~/.emacs.d/lisp/modules/04-devOps-pkg-conf.el"))

;;;
;; Load Harmonic Alchemy Modular Emacs - pubOps module:

(if (file-exists-p "~/.emacs.d/lisp/my-modules/05-pubOps-pkg-conf.el")
    (load-file "~/.emacs.d/lisp/my-modules/05-pubOps-pkg-conf.el")
  (load-file "~/.emacs.d/lisp/modules/05-pubOps-pkg-conf.el"))

;;;
;; Load Harmonic Alchemy Modular Emacs - Misc Configs module:

(if (file-exists-p "~/.emacs.d/lisp/my-modules/07-misc.el")
    (load-file "~/.emacs.d/lisp/my-modules/07-misc.el")
  (load-file "~/.emacs.d/lisp/modules/07-misc.el"))

;;;
;; Load Harmonic Alchemy Modular Emacs - Spelling module:

(if (file-exists-p "~/.emacs.d/lisp/my-modules/08-spelling.el")
    (load-file "~/.emacs.d/lisp/my-modules/08-spelling.el")
  (load-file "~/.emacs.d/lisp/modules/08-spelling.el"))

;;;;
;; Load Harmonic Alchemy Modular Emacs - Org Mode module:

(if (file-exists-p "~/.emacs.d/lisp/my-modules/09-org-mode-pkg-conf.el")
    (load-file "~/.emacs.d/lisp/my-modules/09-org-mode-pkg-conf.el")
  (load-file "~/.emacs.d/lisp/modules/09-org-mode-pkg-conf.el"))

;;;;
;;           *** Optional Programming IDE Features ***
;;
;; Load Harmonic Alchemy Modular Emacs - Programming Languages module:
;; This is for using Emacs as a full fledged Common Lisp IDE!
;; This module is enabled by default on the ME lisp-ide branch.  Pull that
;; branch instead if you want to stay up with the programming stuff as
;; it takes time for that to filter into the main branches...

;; NOTE: These Modules are currently being re-factored so that they don't
;;       have to use emacs packages that add a LOT of Unnecessary FLUF
;;       FLUF = Fluf is Lousy Unnecessary Fuckshit! (recursive acro)

;(if (file-exists-p "~/.emacs.d/lisp/my-modules/10-progLang-pkg-conf.el")
;    (load-file "~/.emacs.d/lisp/my-modules/10-progLang-pkg-conf.el")
;  (load-file "~/.emacs.d/lisp/modules/10-progLang-pkg-conf.el"))

;;;;
;;        *** Optional Games & Multi Media Features ***
;;
;; Un-comment the load file line below if you would like
;; to explore MOOs or MUDs using a customized Emacs MOO client! ;-)
;; This option is OFF by default.  Make sure to follow the instructions
;; within the ~/.emacs.d/README.md to install the RMOO package first...
;; Load Harmonic Alchemy Modular Emacs - Games module:

;(if (file-exists-p "~/.emacs.d/lisp/my-modules/11-external-integrations-conf.el")
;    (load-file "~/.emacs.d/lisp/my-modules/11-external-integrations-conf.el")
;  (load-file "~/.emacs.d/lisp/modules/11-external-integrations-conf.el"))

;;;;
;;              *** Optional Accounting Feature ***
;; Load Modular Emacs Accounting module:

;(if (file-exists-p "~/.emacs.d/lisp/my-modules/15-Accounting-pkg-conf.el")
;    (load-file "~/.emacs.d/lisp/my-modules/15-Accounting-pkg-conf.el")
;  (load-file "~/.emacs.d/lisp/modules/15-Accounting-pkg-conf.el"))

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/dispatcher.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
