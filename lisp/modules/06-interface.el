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
;;
;; Change Log: (descending chronological order)
;;
;;   2019-004-24 - Harmonic Alchemy Modular-Emacs Official Point Release:
;;      v1.0.0 [Q2 2019] Final testing a new point release within the develop
;;      branch... After testing period is over a final commit will be made and
;;      merged back into master...  Master still holds Official Release: 1.0...
;;
;;   2019-003-04 - Harmonic Alchemy Modular-Emacs Official Release v1.0 [Q1 2019]
;;      Final test of release candidates is over now... the develop
;;      branch has been merged back into master... (after this commit)...
;;      This commit will be tagged as Harmonic Alchemy Modular Emacs Version 1.0
;;      Official Release [Q1-2019]...
;;
;;   2019-002-21 - Updated to Release Candidate 3 for Modular Emacs
;;      Version: 1.0 (Q1-2019)
;;
;;   2019-001-21 - This file marks the first Release Candidate for Version
;;      1 of: Harmonic Alchemy Modular Emacs (displayed in the default Emacs
;;      frame title) 
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Change title-bar text
(setq frame-title-format
      "Modular Emacs v1.0.2 (Official Point Release) [Q2 2019]")

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
;;
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
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Set default Frame Dimensions:
;; You may have to play with this depending on your total screen size etc...
(setq default-frame-alist
      '( (width . 101)
         (height . 38)
         (menu-bar-lines . 1)
         (vertical-scroll-bars . nil)
         (horizontal-scroll-bars . nil) ))

;; Set default font to Hermit Medium (my favorite mono font for everything)...
(set-face-attribute 'default nil
                    :family "Hermit"
                    :foundry "PfEd"
                    :slant 'normal
                    :height 113
                    :weight 'normal
                    :width 'normal)

;; Disable scrollbar on new frame (all frames no matter what)
;; Scroll bars in Emacs are strange animals.. They don't belong here.
;; There seems to be a bug in the standard Emacs way to do this as
;; I originally tried at the top with: (scroll-bar-mode -1)
;;
;; Here is a custom function that takes care of that problem...
;; Oops! Well that does not work for me either... It may be needed on
;; Windows platforms however...  If you are trying this on Windows,
;; you may try un-commenting the function and add-hook below if
;; you are seeing scroll bars on a second frame...
;(defun me-disable-scroll-bars (frame)
;  (modify-frames-parameters frame
;                            '((vertical-scroll-bars . nil)
;                              (horizontal-scroll-bars . nil))))

;(add-hook 'after-make-frame-functions 'me-disable-scroll-bars)

;; Activate blackbord theme...
(load-theme 'blackboard t)
