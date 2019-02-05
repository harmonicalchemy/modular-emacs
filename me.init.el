;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; File: ~/.emacs.d/init.el - Emacs Configuration for Linux and Mac-OS.
;; Author:   Alisha Awen Sheppard - siren1@disroot.org
;; First Created:  2018-011-13
;; Info:
;;   This is my default Emacs Initialization file for Linux & OSX.  I don't use
;;   any MS Windows machines so I don't know if this works there as well.  If
;;   you try this on Windows and get it to work, please drop me a message and I
;;   may end up working with you as the MS Windows OS test engineer for this emacs
;;   config.  If we get a good version of this working on Windows I will include
;;   it in this repo and credit you as the author of any Windows config files you
;;   provide.  B-) Thanks in advance!
;;
;; Reference: Emacs Manual - 49.4 - The Emacs Initialization File
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html
;;
;; Dependencies / Requirements:
;;   This init file is specifically designed to work with:
;;   https://GitHub.com/harmonicalchemy/modular-emacs.git
;;
;; NOTE: (mostly to self) If you change the Version (as in Change Log: below)
;;       change the Window Title to reflect that so you will know which version
;;       you are running quickly when comparing different emacs windows from
;;       different machines or VPS.
;;
;;       Change title by editing: ~/.emacs.d/lisp/modules/06-interface.el
;;       Also change the Welcome Message at the bottom of this file to the new
;;       version number!
;;
;; Change Log: (descending chronological order)
;;
;;   2019-002-04 - Harmonic Alchemy Modular-Emacs v1.0 (RC2) [Q1 2019] !!!
;;      I believe I am close to having this work well on [MWM-work].  Three are
;;      a few quirks, (shift arrow does not move to another frame if it is present)
;;      so that needs to be fixed.  VMD mode works well on Linux but Org Outline
;;      Bullet headings need to convert to normal markdown headings!!!
;;
;;   2019-001-21 - Harmonic Alchemy Modular-Emacs v1.0 (RC1) [Q1 2019] !!!
;;      Many changes!  All init code went into modules.  This marks the first
;;      release candidate from my original Super-Emacs++ fork, morphed into
;;      an all new **Harmonic Alchemy - Modular Emacs!!!** (the original
;;      super-emacs fork is being maintained within the master branch).
;;
;;      **NOTE:** This Release Candidate is being maintained within the `develop`
;;      branch for the time being... Once I determine that this is stable I will
;;      actually create an entirely new Depot repository for it on GitHub.com.
;;      The new repository name will be called: `modular-emacs.git`.  I may decide
;;      to change all variable and function name refs: `super-emacs` to: `mod-emacs`.
;;      If I do, that note will be appended here...
;;
;;   2019-001-15 - Harmonic Alchemy super-emacs [Q1 2019] v0.5:
;;      With Spelling working well, I corrected some errors and inconsistencies
;;      in this file...  A next step in house cleaning will break sections
;;      out of this file into modules and bring this file back to top level
;;      exec of those modules only... That will make things much more useful
;;      for everyone...
;;
;;   2019-001-14 - Harmonic Alchemy super-emacs [Q1 2019] v0.4:
;;      Cleaned up mess in Spelling: section... See Article:
;;      http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
;;
;;   2019-001-13 - Harmonic Alchemy super-emacs [Q1 2019] v0.3:
;;      Tested and working with my new fork of super-emacs!  Have not tried integration
;;      test of installing completely from scratch on a bare bones AppVM yet...
;;      That acid test comes next... (after updating my documentation etc.)
;;
;;   2019-001-06 - Harmonic Alchemy super-emacs [Q1 2019] v0.0:
;;      This new 2019 version is completely different from the current init.el
;;      committed to my dotfiles repo.  I will push this new modified file as
;;      soon as I have it working and tested.  This version finally gets the
;;      custom.el (for automated internal emacs updates) working correctly so
;;      that the emacs system leaves your init.el (this file) alone for your own
;;      custom manual configurations...
;;
;;   2018-011-13 - Harmonic Alchemy super-emacs [Q4 2019] v0.0
;;      This represents a BIG departure from my previous emacs configurations.
;;      I need to use this file to update my default init.el in my dotfiles
;;      GitHub repository...      
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; *** Load Start time for Harmonic Alchemy Modular Emacs ***
(defvar config-start-time
  (current-time))

;; Load: Harmonic Alchemy Modular Emacs - Dispatcher
(load-file "~/.emacs.d/lisp/modules/dispatcher.el")

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; BEGIN Emacs Manual Configurations: (Add your test scripts below this banner)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





;; Save your testing or custom scripts in here to be persistant across restarts.
;;
;; After testing here, if you want to make your custom scripts more formal, move
;; them to one or more of the ~/.emacs.d/lisp/modules/<module-name>.el files...
;;
;; **Note:** Be sure to be checked out on your local "develop" branch of your local
;; clone of: Harmonic Alchemy - Modular Emacs first! (keep your master branch clean)
;;
;; Choose a module that best fits the category of your new script.  If your new
;; script does not fit with any of the existing modules, create a new one:
;; (e.g., ./modules/NN-your-module-name.el, - where: "NN-" = 10-, 11-, 12-, ...).
;; After creating new module files, be sure to update: ./modules/dispatcher.el
;; with a call to your new modules as well! (don't forget that last step or else
;; your brand new shiny module will just sit there like an orphan! ;-)





;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END Emacs Manual Configurations: (Do not change anything below this banner!)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Print Welcome With Computed Emacs Load Time this configuration took:
(princ (cl-concatenate 'string
                       ";; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
                       ";; Welcome to: [Your-Machine-Name-Here]\n"
                       ";;             Harmonic Alchemy - Modular Emacs!\n"
                       ";; Version:    1.0 (RC2) [Q1 2019]\n"
                       ";; \n"
                       ";; Today's date: " (format-time-string "%Y %B %d") "\n"
                       ";; Startup completed in "
                       (number-to-string (cadr (time-subtract (current-time)
                                                              config-start-time)))
                       " seconds\n"
                       ";; \n"
                       ";;   \"Its name is Public Opinion.\n"
                       ";;    It is held in reverence.\n"
                       ";;    It settles everything.\n"
                       ";;    Some think it is the voice of God.\"\n"
                       ";;                         -- Mark Twain\n"
                       ";; \n"
                       ";; Evaluate lisp expressions below:\n"
                       ";; (if you'd like to do that ;-)\n"
                       ";; This entire message is a lisp comment!\n"
                       ";; Evaluate this entire buffer as a test\n"
                       ";; before committing it as your:\n"
                       ";; \"new-cool-custom-module.el\"\n"
                       ";;               -- Cheers! ;-)\n"
                       ";; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
                       )
       (get-buffer-create (current-buffer)))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END ~/.emacs.d/init.el 
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
