;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/06-interface.el
;;
;; This file defines general Emacs interface Customizations.  You may find my
;; comments below a little bit cuemudgin against changing the default Emacs
;; key bindings, or enabling scroll wheels, track pads, etc. in lieu of using
;; the classic Emacs prefix keys & commands that do the same things but much more
;; efficiently.  As a result you will not find much like that in here...
;;
;; Customize this file by adding your own interface tweaks...  You could add CUA
;; mode here if you like...  Not me. ;-)  I have been using C-w, M-w, C-k, & C-y
;; for 30 years and like it that way.  I tried CUA mode for a while once and my
;; fingers got confused and typed other non-emacs keys within Emacs, and also
;; accidentlay typed Emacs key commands when I was in other apps!  OMG!

;; Change Log: (descending chronological order)

;;   2021-004-08 - Updated Modular Emacs to v3.4.0 [Q2 2021]
;;                 (still beta - working on updating many docs to
;;                  org-mode and publishing as PDFs)
;;                 This version includes new feature additions &
;;                 bugfixes...

;;   2021-001-07 - Modified width of initial and default frames to 88.

;;   2020-007-16 - Harmonic Alchemy Modular-Emacs V3.2 (beta) [Q3 2020]
;;      This has been tested on the develop branch long enough... I am
;;      still calling it "beta" because there is still a lot of work
;;      still unfinished... (mostly peripheral support files, templates, etc.
;;      and the Docs are still being updated... (does documentation ever end? lol)
;;      I will be calling this Beta for a while i guess... But version numbers
;;      will continue to advance...  The nature of Emacs is "always in Beta"
;;      because it is so powerful and flexible, and empowers the user who can
;;      not only use and customize it but can also completely change functionality!

;;   2019-007-05 - Harmonic Alchemy Modular-Emacs Lisp IDE Fork v2.2.0 [Q3 2019]
;;      v2.0.1 [Q3 2019] Final testing period over... Version 2.0.1 is stable.
;;      This fork represents new commented out features added from Lisp-IDE
;;      branch/fork back into the develop branch so they can be kept up to
;;      date when the time comes for these features to be turned on within
;;      the develop or master branch at a later date...

;;   2019-007-05 - Harmonic Alchemy Modular-Emacs Official Release:
;;      v2.0.1 [Q3 2019] Final testing period over... Version 2.0.1 is stable.

;;   2019-006-23 - Harmonic Alchemy Modular-Emacs Official Release:
;;      v1.0.0 [Q2 2019] Final testing period over... Version 2 is stable.

;;   2019-004-24 - Harmonic Alchemy Modular-Emacs Official Point Release:
;;      v1.0.0 [Q2 2019] Final testing a new point release within the develop
;;      branch... After testing period is over a final commit will be made and
;;      merged back into master...  Master still holds Official Release: 1.0...

;;   2019-003-04 - Harmonic Alchemy Modular-Emacs Official Release v1.0 [Q1 2019]
;;      Final test of release candidates is over now... the develop
;;      branch has been merged back into master... (after this commit)...
;;      This commit will be tagged as Harmonic Alchemy Modular Emacs Version 1.0
;;      Official Release [Q1-2019]...

;;   2019-002-21 - Updated to Release Candidate 3 for Modular Emacs
;;      Version: 1.0 (Q1-2019)

;;   2019-001-21 - This file marks the first Release Candidate for Version
;;      1 of: Harmonic Alchemy Modular Emacs (displayed in the default Emacs
;;      frame title) 
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Change title-bar text
(setq frame-title-format
      "Harmonic Alchemy Modular Emacs - Version 3.4 (still in beta) [Q4 2021]")

;; Disable tool-bar - I could care less about tool bars in emacs!
;; An oxymoron! But you may feel differently.  Comment this out if you like them.

(tool-bar-mode -1)

;; Disable scroll-bar - I use key commands to navigate in emacs.  Once you get
;; used to the alternate methods you will throw your mouse away! The key commands
;; are way faster and more accurate as well.  Some key commands take you right to
;; the precise spot you want to go!
;; NOTE: I got this to work below by setting the Default Frame alist after
;; trying many other things that were not working...  I am leaving this here
;; commented out to keep the option here in case I need it later, (or you do).

;(scroll-bar-mode -1)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Disable this section if you like using the mouse wheel:
;;
;; On my laptop my palms get in the way and cause all kinds
;; of scrolling crazy stuff. I hate it!!! Seriously. Key commands
;; are best in Emacs.  Right? I Mean Right??? Yup. ;-)
;; 
;; Disable mouse wheel (and two finger swipe) scrolling because
;; it scrolls horribly and I would rather work without it. %^)
;; also disable the middle mouse (mouse-2) pasting text by accident
;; really annoys me!

(mouse-wheel-mode -1)
(global-set-key [wheel-up] 'ignore)
(global-set-key [wheel-down] 'ignore)
(global-set-key [wheel-right] 'ignore)
(global-set-key [wheel-left] 'ignore)
(global-set-key [double-wheel-up] 'ignore)
(global-set-key [double-wheel-down] 'ignore)
(global-set-key [double-wheel-right] 'ignore)
(global-set-key [double-wheel-left] 'ignore)
(global-set-key [triple-wheel-up] 'ignore)
(global-set-key [triple-wheel-down] 'ignore)
(global-set-key [triple-wheel-right] 'ignore)
(global-set-key [triple-wheel-left] 'ignore)
(global-set-key [mouse-2] 'ignore)

;(global-set-key [mouse-yank-at-point] 'ignore)
;(setq mouse-yank-at-point nil)


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Set Initial Startup Frame Dimensions:
;; You may have to play with this depending on your total
;; screen size, etc...

(setq initial-frame-alist
      '( (name . "HA Mod Emacs v3.4 - Coder's Frame")
         (font . "Hermit")
         (height . 38)
         (width . 92)
         (menu-bar-lines . 1)
         (vertical-scroll-bars . nil)
         (horizontal-scroll-bars . nil)
         (left-fringe . 1)
         (right-fringe . 1)))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Set Default Frame Dimensions:
;; You may have to play with this depending on your total
;; screen size, etc...

(setq default-frame-alist
      '( (name . "HA Mod Emacs v3.4 - Coder's Frame")
         (font . "Hermit")
         (height . 38)
         (width . 92)
         (menu-bar-lines . 1)
         (vertical-scroll-bars . nil)
         (horizontal-scroll-bars . nil)
         (left-fringe . 1)
         (right-fringe . 1)))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Activate Blackboard theme:

(load-theme 'blackboard t)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/06-interface.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
