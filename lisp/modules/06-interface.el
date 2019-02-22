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
;; Update 2019-002-21:
;;    Updated to Release Candidate 3 for Modular Emacs Version: 1.0 (Q1-2019)
;;
;; Update 2019-001-21:
;;    This file marks the first Release Candidate for Version 1 of:
;;    Harmonic Alchemy Modular Emacs (displayed in the default Emacs frame title) 
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Change title-bar text
(setq frame-title-format
      "Modular Emacs v1.0 (RC3) [Q1 2019]")

;; Disable tool-bar - I could care less about tool bars in emacs!
;; An oxymoron! But you may feel differently.  Comment this out if you like them.
(tool-bar-mode -1)

;; Disable scroll-bar - I use key commands to navigate in emacs.  Once you get
;; used to the alternate methods you will throw your mouse away! The key commands
;; are way faster and more accurate as well.  Some key commands take you right to
;; the precise spot you want to go!
(scroll-bar-mode -1)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Disable this section if you like using the mouse wheel:
;;
;; On my laptop my palms get in the way and cause all kinds
;; of scrolling crazy stuff. I hate it!!! Seriously. Key commands
;; are best in Emacs.  Right? I Mean Right??? Yup. ;-)
;; 
;; Disable mouse wheel (and two finger swipe) scrolling because
;; it scrolls horribly and I would rather work without it. %~)
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
      '((width . 101)
        (height . 38)
        (menu-bar-lines . 1)))

;; Set default font to Hermit Medium (my favorite mono font for everything)...
(set-face-attribute 'default nil
                    :family "Hermit"
                    :foundry "PfEd"
                    :slant 'normal
                    :height 113
                    :weight 'normal
                    :width 'normal)

;; Activate blackbord theme...
(load-theme 'blackboard t)
