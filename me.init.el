;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; File: ~/.emacs.d/init.el - Emacs Configuration for Linux and Mac-OS.
;; Author:   Alisha Awen Sheppard - siren1@disroot.org
;; First Created:  2018-011-13
;; Info:
;;   This is my default Emacs Initialization file for Linux & OSX.  I don't use
;;   any MS Windows machines so I don't know if this works there as well.  If
;;   you try this on Windows and get it to work, please make a pull request for
;;   your branch and add an issue to HarmonicAlchemy/modular-emacs.  You my also
;;   simply send me an email to siren1@MarketingWebMedia.com to get in touch
;;   as well.  If you put together a nice solution for your Windows setup I
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
;;   2020-005-18 - Harmonic Alchemy Modular-Emacs V3.1 (beta) [Q2 2020]
;;      Org Mode has been heavily customized!  Also some of the Module
;;      files were re-organized. (In particular 12-Xah-Fly-Keys.el
;;      and 13-key-bindings.el) Added some tweaks to improve package
;;      management...  I am prepairing to fork this generic version
;;      that has the optional modules disabled (not loaded) to create
;;      a new branch or branches which have some options turned on...
;;      This will make it easier for anyone wishing to try out some of
;;      the optional features...
;;
;;   2019-011-12 - Harmonic Alchemy Modular-Emacs V3.0 (beta) [Q4 2019]
;;      This represents mostly updated docs which has been needed for a while!
;;      there were also many bug fixes brought over from the Lisp IDE fork,
;;      where I have been living for most of the summer and fall!  Now the
;;      master and develop branches can benefit the gains...
;;
;;   2019-010-17 - Harmonic Alchemy Modular-Emacs - Lisp-IDE fork Release:
;;      V2.3.0. [Q3-2019]  This commitment marks the final update after
;;      merging the Lisp IDE branch back into the Develop branch...
;;      and then modifying that branch so that the extra modules here are
;;      not enabled... Both the Master and Develop branch are up to date now
;;      with all changes from Lisp IDE branch that make sense there...
;;
;;   2019-007-05 - Harmonic Alchemy Modular-Emacs - Lisp-IDE fork Release:
;;      V2.2.0.  Final testing V2.0.1 on Linux done... This fork adds in
;;      all commented out code from the lisp-ide fork to the develop branch.
;;      Updated Docs accordingly... All platforms have been running reasonably
;;      stable for a while now... Compiler warnings noted...  The lisp-ide
;;      branch will be updated frequently and merged into the develop branch
;;      less often when things are looking stable... The develop branch
;;      will be merged into the master branch after docs are all up to date...
;;      None of the extra features will be turned on within the master or
;;      develop branches.  Only the lisp-ide branch has those features on by
;;      default...
;;
;;   2019-006-27 - Harmonic Alchemy Modular-Emacs Release: V2.0.1 RC1
;;      [Q2 2019] Final Testing on Mac OS complete... Final Testing on
;;      Linux begins after this commit.   Release was bumped to V2.0.1
;;      due to Major Long Standing Mac OS Bug fix! Because of the new SBCL
;;      with Slime mode features, I decided to call this a major release
;;      from V1 to V2 as well...  Things are moving along quickly now!
;;      I everything is working reasonably well on the Mac... Need to get
;;      back on Linux and confirm things are still working well, or even
;;      better maybe? That would be nice... Doc updates will be
;;      on-going as well not requiring any point release commits. Possibly
;;      tags only for doc releases...
;;
;;   2019-005-28 - Harmonic Alchemy Modular-Emacs Official Point Release:
;;      v1.0.2 [Q2 2019] Final testing on Mac OS now.  This version includes
;;      the new SBCL Lisp connection with Slime etc.  I need to test on MacOS!
;;
;;   2019-005-15 - Harmonic Alchemy Modular-Emacs Official Point Release:
;;      v1.0.2 [Q2 2019] Final testing a new point release within the develop
;;      branch... After testing period is over a final commit will be made and
;;      merged back into master...  Master currently holds Release: 1.0...
;;
;;   2019-004-24 - Harmonic Alchemy Modular-Emacs Official Point Release:
;;      v1.0.1 [Q2 2019] Final testing a new point release within the develop
;;      branch... After testing period is over a final commit will be made and
;;      merged back into master...  Master still holds Official Release: 1.0...
;;
;;   2019-003-04 - Harmonic Alchemy Modular-Emacs Official Release v1.0 [Q1 2019]
;;      Final test of release candidates is over now... the develop
;;      branch has been merged back into master... (after this commit)...
;;      This commit will be tagged as Harmonic Alchemy Modular Emacs Version 1.0
;;      Official Release [Q1-2019]...
;;
;;   2019-002-21 - Harmonic Alchemy Modular-Emacs v1.0 (RC3) [Q1 2019]
;;      I am now testing/debugging this release candidate on [MWM-work] within
;;      a new local branch named:  mwm-work (of course ;-).  all variable
;;      names no longer reference any old super-emacs ghosts from the past.
;;      this is a completely independent Emacs Project on Github now...
;;      too many things have been changed/introduced/revamped to call this
;;      a fork of Super-Emacs anymore...  Once I have this working well on
;;      [MWM-Work] I will create a remote develop branch on Github and push
;;      local mwm-work branch to origin/develop.  That will be for testing
;;      on other Qubes environments... In particular my [public] VM which
;;      is based on a different template...
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
;;      If I do, that note will be appended here... Retroactive Note: all vars
;;      defined within modular-emacs are now prefixed with me_ instead of super-emacs_.
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

(if (file-exists-p "~/.emacs.d/lisp/my-modules/dispatcher.el")
    (load-file "~/.emacs.d/lisp/my-modules/dispatcher.el")
  (load-file "~/.emacs.d/lisp/modules/dispatcher.el"))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; BEGIN Emacs Manual Configurations: (Add your test scripts below this banner)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Save your testing or custom scripts in here to be persistant across restarts.
;;
;; After testing here, if you want to make your custom scripts more formal, move
;; them to one or more of the ~/.emacs.d/lisp/modules/<module-name>.el files...
;;
;; **Note:** Be sure to be checked out on your own local (un-tracked) branch on
;; your clone of: Harmonic Alchemy - Modular Emacs first! (keep your master 
;; and/or develop branches clean!)  Its much easier to merge your un-tracked 
;; branches into a local tracking branch (which can be destroyed and re-fetched 
;; if anything goes wrong). Your custom changes stay safe on an un-tracked branch 
;; while you recover pull your tracking branches... Trying to do things the other 
;; way around, after the fact, can end up turning into a merge nightmare, wasted 
;; afternoon!
;;
;; Choose a module that best fits the category of your new script.  If your new
;; script does not fit with any of the existing modules, create a new one:
;; (e.g., ./modules/NN-your-module-name.el, - where: "NN-" = 10-, 11-, 12-, ...).
;;
;; After creating a new module file, be sure to update: ./modules/dispatcher.el
;; as well... In order to prevent changes to "dispatcher.el" from being detected
;; by git, you will need to copy it into: ~/.emacs.d/lisp/my-modules/dispatcher.el
;; and update that Instead.  Once you have your own my-modules/dispatcher.el
;; Edit the line ABOVE (within this file) where the dispatcher gets loaded as follows:
;;
;;           Load: Harmonic Alchemy Modular Emacs - Dispatcher
;;           (load-file "~/.emacs.d/lisp/my-modules/dispatcher.el")
;;
;; Any modules you add will then be loaded from your own /my-modules/dispatcher.el
;; from then on...  You will have to check now and again for any new modules
;; that may have been introduced by the remote/master branch etc...
;; But that's easy because you can simply check the difference between your:
;; ./lisp/my-modules/dispatcher.el and ./lisp/modules/dispatcher.el then copy
;; the load lines of any of those new modules you would like to add to your
;; customized installation...
;; Note: There are many ways to skin a rat...
;; btw, I don't skin cats!  My cats do skin rats though. And mice... And birds... %^)





;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END Emacs Manual Configurations: (Do not change anything below this banner!)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Print Welcome With Computed Emacs Load Time this configuration took:
(princ (cl-concatenate
        'string
        ";; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
        ";; Welcome to: [Your-Machine-Name-Here]                    \n"
        ";;             Harmonic Alchemy - Modular Emacs!           \n"
        ";; Version:    3.1.2 (beta) [Q2 2020]                      \n"
        ";;                                                         \n"
        ";; Today's date: " (format-time-string "%Y %B %d") "       \n"
        ";; Startup completed in "
        (number-to-string (cadr (time-subtract (current-time)
                                               config-start-time)))
        " seconds                                                   \n"
        ";;                                                         \n"
        ";;   \"Its name is Public Opinion.                         \n"
        ";;    It is held in reverence.                             \n"
        ";;    It settles everything.                                 \n"
        ";;    Some think it is the voice of God.\"                       \n"
        ";;                         -- Mark Twain                             \n"
        ";;                                                                        \n"
        ";; More Mark Twain quotes can be found here:                                 \n"
        ";; https://www.writerswrite.co.za/literary-birthday-30-november-mark-twain/  \n"
        ";;                                                                           \n"
        ";; If Mark Twain was an 80's New Orleans Lisp Hacker/DJ:                  \n"
        ";;                                                                   \n"
        ";;   \"I yous'ta be a river boat pilot until dis crafty          \n"
        ";;   gambler CONs me out of averyting, including me CAR,     \n"
        ";;   CDR, and me entire Michael Jackson CD collection      \n"
        ";;   mon!!!  Tank god, he didn't find me old IBM 704 in    \n"
        ";;   da basement!!! That's cuz he spied me big bulldog     \n"
        ";;   sleepin back in da corner... He decided his gains     \n"
        ";;   would be NIL down dare after dat...                   \n"
        ";;                                                         \n"
        ";;   Well, dey finally caught em...  Now e's a CONvict...  \n"
        ";;   locked away in a CON'S cell. Being (evaluated) for:    \n"
        ";;   (constructive . rehabilitation)!\"                       \n"
        ";;                    LOL! -- Alisha Awen %^)                   \n"
        ";;                                                                \n"
        ";; Evaluate lisp expressions below within this buffer                \n"
        ";; Check Out: GNU.org's Intro to Programming Emacs Lisp:               \n"
        ";; https://www.gnu.org/software/emacs/manual/html_node/eintr/index.html  \n"
        ";;                                                                      \n"
        ";; This entire message is a lisp comment!                              \n"
        ";; Emacs makes a Great S-exPs (S Expressions) IDE!                    \n"
        ";; Our Universe is a Great Big Sex PistolS IDE ;-)                  \n"
        ";; Evaluate this entire buffer as a test...  Of the universe?    \n"
        ";; Why not? Go for it... You may have super powers!           \n"
        ";; Be sure to debug it well before committing it as your:   \n"
        ";; \"new-way-cool-custom-parallel-world-module.el\" %^)    \n"
        ";;                        -- Cheers!                       \n"
        ";; Alisha Awen: @harmonicalchemy (Github, twitter etc.)    \n"
        ";; PGP Pubkey: 0x922CC456F48355A8D9B2E044C9E6CD44A817E7BF  \n"
        ";; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
       (get-buffer-create (current-buffer)))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END ~/.emacs.d/init.el 
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
