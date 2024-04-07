; -*- coding: utf-8; lexical-binding: t; -*-
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; File:        ~/.emacs.d/init.el - Modular Emacs Config Main Entry Point
;; Ref:         <https://github.com/harmonicalchemy/modular-emacs>
;; Author:      Alisha Awen - harmonicalchemy@proton.me
;; Maintainer:  Alisha Awen
;; Created:     2018-011-13
;; Version:     3.6.2 - 2024-004-07

;; This File is NOT Part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version...

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;;

;;;
;; INFO:
;;   This is the default Emacs Initialization file...
;;   This file does nothing more than load dispatcher.el and finish
;;   with printing a welcome message in the *scratch* buffer...
;;   Doing it this way makes it easy for you to try out other
;;   emacs configrations without touching any of your installed
;;   Modular Emacs configuration...  When done trying out another
;;   configuration simply copy this file to replace the existing
;;   init.el and restart emacs... Your normal Modular Emacs config shall
;;   return as it was before...

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

;;;
;; CHANGE LOG: (descending chronological order)
;;

;;   2024-004-07 - Harmonic Alchemy Modular-Emacs V3.6.2 [Q2 2024]
;;      Update mostly to the pubOps configurations... (Org-Mode Export)
;;      Basic functionality of the eLisp code and loaded modules is MOSTLY
;;      the same as earlier versions...

;;   2022-010-16 - Harmonic Alchemy Modular-Emacs V3.5 [Q3 2022]
;;      Finally have a decent configuration with updated README which is
;;      NOW README.org (instead of Markdown) Still updating docs to reflect
;;      newest changes... When that is done README.org will be official and
;;      README.md will be retired...

;;   2020-007-16 - Harmonic Alchemy Modular-Emacs V3.2 (beta) [Q3 2020]
;;      This has been tested on the develop branch long enough... I am
;;      still calling it "beta" because there is still a lot of work
;;      still unfinished... (mostly peripheral support files, templates, etc.
;;      and the Docs are still being updated... (does documentation ever end? lol)
;;      I will be calling this Beta for a while i guess... But version numbers
;;      will continue to advance...  The nature of Emacs is "always in Beta"
;;      because it is so powerful and flexible, and empowers the user who can
;;      not only use and customize it but can also completely change functionality!

;;   2020-005-18 - Harmonic Alchemy Modular-Emacs V3.1 (beta) [Q2 2020]
;;      Org Mode has been heavily customized!  Also some of the Module
;;      files were re-organized. (In particular 12-Xah-Fly-Keys.el
;;      and 13-key-bindings.el) Added some tweaks to improve package
;;      management...  I am prepairing to fork this generic version
;;      that has the optional modules disabled (not loaded) to create
;;      a new branch or branches which have some options turned on...
;;      This will make it easier for anyone wishing to try out some of
;;      the optional features...

;;   2019-011-12 - Harmonic Alchemy Modular-Emacs V3.0 (beta) [Q4 2019]
;;      This represents mostly updated docs which has been needed for a while!
;;      there were also many bug fixes brought over from the Lisp IDE fork,
;;      where I have been living for most of the summer and fall!  Now the
;;      master and develop branches can benefit the gains...

;;   2019-010-17 - Harmonic Alchemy Modular-Emacs - Lisp-IDE fork Release:
;;      V2.3.0. [Q3-2019]  This commitment marks the final update after
;;      merging the Lisp IDE branch back into the Develop branch...
;;      and then modifying that branch so that the extra modules here are
;;      not enabled... Both the Master and Develop branch are up to date now
;;      with all changes from Lisp IDE branch that make sense there...

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

;;   2019-005-28 - Harmonic Alchemy Modular-Emacs Official Point Release:
;;      v1.0.2 [Q2 2019] Final testing on Mac OS now.  This version includes
;;      the new SBCL Lisp connection with Slime etc.  I need to test on MacOS!

;;   2019-005-15 - Harmonic Alchemy Modular-Emacs Official Point Release:
;;      v1.0.2 [Q2 2019] Final testing a new point release within the develop
;;      branch... After testing period is over a final commit will be made and
;;      merged back into master...  Master currently holds Release: 1.0...

;;   2019-004-24 - Harmonic Alchemy Modular-Emacs Official Point Release:
;;      v1.0.1 [Q2 2019] Final testing a new point release within the develop
;;      branch... After testing period is over a final commit will be made and
;;      merged back into master...  Master still holds Official Release: 1.0...

;;   2019-003-04 - Harmonic Alchemy Modular-Emacs Official Release v1.0 [Q1 2019]
;;      Final test of release candidates is over now... the develop
;;      branch has been merged back into master... (after this commit)...
;;      This commit will be tagged as Harmonic Alchemy Modular Emacs Version 1.0
;;      Official Release [Q1-2019]...

;;   2019-002-21 - Harmonic Alchemy Modular-Emacs v1.0 (RC3) [Q1 2019]
;;      I am now testing/debugging this release candidate on Linux...
;;      Once I have this working well on Linux I will create a remote develop
;;      branch on Github and push local mwm-work branch to origin/develop.
;;      That will be for testing on other environments...

;;   2019-002-04 - Harmonic Alchemy Modular-Emacs v1.0 (RC2) [Q1 2019] !!!
;;      I believe I am close to having this work well on Linux.  Three are
;;      a few quirks, (shift arrow does not move to another frame if it is present)
;;      so that needs to be fixed.  VMD mode works well on Linux but Org Outline
;;      Bullet headings need to convert to normal markdown headings!!!

;;   2019-001-21 - Harmonic Alchemy Modular-Emacs v1.0 (RC1) [Q1 2019] !!!
;;      Many changes!  All init code went into modules.

;;      **NOTE:** This Release Candidate is being maintained within the `develop`
;;      branch for the time being... Once I determine that this is stable I will
;;      actually create an entirely new Depot repository for it on GitHub.com.
;;      The new repository name will be called: `modular-emacs.git`...

;;   2019-001-15 - Harmonic Alchemy super-emacs [Q1 2019] v0.5:
;;      With Spelling working well, I corrected some errors and inconsistencies
;;      in this file...  A next step in house cleaning will break sections
;;      out of this file into modules and bring this file back to top level
;;      exec of those modules only... That will make things much more useful
;;      for everyone...

;;   2019-001-13 - Harmonic Alchemy super-emacs [Q1 2019] v0.3:
;;      Tested and working with my new fork of super-emacs!  Have not tried integration
;;      test of installing completely from scratch on a bare bones AppVM yet...
;;      That acid test comes next... (after updating my documentation etc.)

;;   2019-001-06 - Harmonic Alchemy super-emacs [Q1 2019] v0.0:
;;      This new 2019 version is completely different from the current init.el
;;      committed to my dotfiles repo.  I will push this new modified file as
;;      soon as I have it working and tested.  This version finally gets the
;;      custom.el (for automated internal emacs updates) working correctly so
;;      that the emacs system leaves your init.el (this file) alone for your own
;;      custom manual configurations...

;;   2018-011-13 - Harmonic Alchemy super-emacs [Q4 2019] v0.0
;;      This represents a BIG departure from my previous emacs configurations.
;;      I need to use this file to update my default init.el in my dotfiles
;;      GitHub repository...      
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
;;
;; Note: There are many ways to skin a rat...
;; btw, I don't skin cats!  My cats do skin rats though. And mice... And birds... %^)





;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END Emacs Manual Configurations: (Do not change anything below this banner!)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Print Welcome With Computed Emacs Load Time this configuration took:
(princ (cl-concatenate
        'string
        ";; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
        ";; Welcome to: [Your-Machine-Name-Here]                                    \n"
        ";;             Harmonic Alchemy - Modular Emacs!                           \n"
        ";; Version:    3.6.2 [Q2 2024]                                             \n"
        ";;                                                                         \n"
        ";; Today's date: " (format-time-string "%Y %B %d") "                       \n"
        ";; Startup completed in "
        (number-to-string (cadr (time-subtract (current-time)
                                               config-start-time)))       " seconds \n"
        ";; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
        ";;                                                                         \n"
        ";;      \"Oh my eyes! It otta be illegal to project                        \n"
        ";;       visual Ads right into the brain!  A man has                       \n"
        ";;       a right to close his eyes and see NOTHING if                      \n"
        ";;       he wants to...\" - Sales Pitch by Philip K Dick                   \n"
        ";;                                                                         \n"
        ";; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
        ";;                                                                         \n"
        ";; Evaluate lisp expressions below within this buffer                      \n"
        ";; Check Out: GNU.org's Intro to Programming Emacs Lisp:                   \n"
        ";; https://www.gnu.org/software/emacs/manual/html_node/eintr/index.html    \n"
        ";;                                                                         \n"
        ";; This entire message is a lisp comment! You my compose any lisp          \n"
        ";; statemet below and evaluate it without changing this heading...         \n"
        ";; -OR- simply kill this buffer which will replace this *scratch*          \n"
        ";; buffer with a clean blackboard to geek out on...                        \n"
        ";;                                                                         \n"
        ";; Don't Forget to check out the ./Docs/pubOps section!!!                  \n"
        ";;                                                                         \n"
        ";; Cheers!                                                                 \n"
        ";;                                                                         \n"
        ";; Alisha Awen: @harmonicalchemy (GitHub, GitLab etc.)                     \n"
        ";; PGP Pubkey:  FF3E 38C9 E731 CCEE 17D6  3A92 9D67 B6DC E4F4 7517         \n"
        ";; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")

       (get-buffer-create (current-buffer)))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END ~/.emacs.d/init.el 
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
