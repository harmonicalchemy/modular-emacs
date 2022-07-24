;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/02-common-conf.el
;;
;; This module provides the common modules needed by all use-cases...
;; Many default parameters are set here, as well as different frame creation
;; functions... All of the dimension settings for frames / windows etc. are
;; defined within this module...
;;
;;; Change Log:

;; 2022-005-15 - Alisha Awen, siren1@disroot.org
;;   Renamed this file from; 02-package-conf.el to: 02-common-conf.el The
;;   previous name was misleading and in fact incorrect as you would think
;;   it is about configuring melpa / elpa packages etc. That stuff is
;;   configured within module: 01-repositories.el... Also all the code from
;;   06-interface.el has been moved into this file as well...
;;
;;   06-interface.el will be removed...
;;
;;   ALSO: In order to make all this work, dispatcher.el will need to be
;;   modified (i.e., the old name of this module within dispatcher.el needs to
;;   be changed to this new file name and the form that loads 06-interface.el
;;   removed alltoogether...

;; 2020-005-16 - Alisha Awen, siren1@disroot.org
;;   disabled powerline mode-line stuff...  I got tired of it...  Too busy...
;;   I found smart-mode-line to be better for my needs... That is the new
;;   default going forward.  I left the powerline code in, (disabled) in case
;;   you like it and would like to switch back...

;;; OLD CHANGE LOG of 06-interface.el:
;;      (before merging all its code into here)
;;      (descending chronological order)
;;
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
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;;
;; Create repositories cache, if required

(when (not package-archive-contents)
  (package-refresh-contents))

;;;
;; Declare default modular-emacs list of required packages:

(defvar me--required-packages
  '(exec-path-from-shell
    gnu-elpa-keyring-update
    helm
;    powerline
;    smart-mode-line
    imenu-list
    olivetti
    auto-complete
    which-key
    flyspell-correct-helm))

;;;
;; Install required packages

(mapc (lambda (p) (package-install p))
      me--required-packages)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Load Environment Vars from shell:
;; If we are using unix in a POSIX compliant shell...
;; (e.g., OS X, Linux, BSD, with POSIX: Bash, or Zsh etc.)
;; Reference: GitHub:Purcell/exec-path-from-shell
;; Install: from MELPA exec-path-from-shell

(when ME--POSIX (exec-path-from-shell-initialize))


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

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; imenu-list configuration:
;;                 (DISABLED)
;;  Note:  I am currently experiencing problems
;;         with imenu-list.  It is disabled until
;;         that can be sorted out...
;
;(require imenu-list)
;(setq imenu-list-focus-after-activation t)
;(setq imenu-list-auto-resize nil)

;; imenu tweaks:

(setq imenu-auto-rescan t)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Load default auto-complete configs

(ac-config-default)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Start which-key-mode

(which-key-mode)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Default Mode Line Tweaks:
;; Here are some nice tweaks that work fine with
;; Default mode lines as well as fancy mode-line
;; packages... currently I am just using the built
;; in mode-like package that comes with Emacs...

(setq size-indication-mode nil
      column-number-mode t
      line-number-mode t)

;;
;; Trim some minor mode’s display to a single unicode icon:

(dolist (mim '((auto-revert-mode   . "♺")
               (auto-fill-function . "⤶")
               (visual-line-mode   . "⤵")
               (isearch-mode       . "⁇")
               (paredit-mode       . "⁐")
               (xah-fly-keys       . "∑fk")
               (smartparens-mode   . "⦅⦆")))

  (let ((mode (car mim))
        (repl (list (concat " " (cdr mim)))))

    (when (assq (car mim) minor-mode-alist)
      (setf (cdr (assq (car mim) minor-mode-alist)) repl))))



;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Activate Blackboard theme:

(load-theme 'blackboard t)



;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Set Initial Startup Frame Dimensions:
;; You may have to play with this depending on your total
;; screen size, etc...

(setq initial-frame-alist
      '( (name . "HA Mod Emacs v3.4 - Default Frame")
         (font . "Hermit")
         (height . 38)
         (width . 88)
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
      '( (name . "HA Mod Emacs v3.4 - Default Frame")
         (font . "Hermit")
         (height . 38)
         (width . 88)
         (menu-bar-lines . 1)
         (vertical-scroll-bars . nil)
         (horizontal-scroll-bars . nil)
         (left-fringe . 1)
         (right-fringe . 1)))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Toggle Olivetti Mode with default width: (86 col)
;; The extra 6 chars over standard 80 col allows
;; for code and org-mode indention (a compromise)
;; My standard frame width is set to 88 columns)
;; so this should just fit...

(require 'olivetti)

(defun me_toggle-olivetti-mode ()
  "Toggles olivetti-mode and sets default width back to 86 columns..."
  (interactive)
  (progn
    (olivetti-mode 'toggle)
    (olivetti-set-width 82)))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Set up helm-mode:

(helm-mode 1)
(helm-autoresize-mode 1)
(setq helm-split-window-in-side-p t)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Modular Emacs - Make NEW Writer's Frame:
;;  This makes a brand new frame to match
;;  HA Modular Emacs Default Writing Frame Specs...
;;  The new frame opens with initial buffer set
;;  to: Bookmark Menu List

(defun me_make-writing-frame ()
  "Create NEW Frame using ME default writer's face
   and ME default writer's frame dimensions
   Frame opens with buffer set to Bookmark Menu List"
  (interactive)
  (progn
    ;; Set buffer to *Bookmark List*

    (bookmark-bmenu-list)

    ;; Make New Writer's Frame:

    (make-frame
     (quote
      ((name . "HA Mod Emacs v3.4 - Writer's Frame"))))

    ;; Select this new frame:

    (select-frame-by-name "HA Mod Emacs v3.4 - Writer's Frame")

    ;; Set Default Face to Courier Prime:
    (set-face-attribute 'default (selected-frame)
                        :family "Courier Prime"
                        :height 135)))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Modular Emacs - Make NEW Coder's Frame:
;;  This makes a brand new frame to match
;;  HA Modular Emacs Default Coding Frame Specs...
;;  The new frame opens with initial buffer set
;;  to: Bookmark Menu List

(defun me_make-default-frame ()
  "Create NEW Frame using ME default coding face
   and ME default coding frame dimensions
   Frame opens with buffer set to Bookmark Menu List"
  (interactive)
  (progn
    ;; Set buffer to *Bookmark List*
    (bookmark-bmenu-list)

    ;; Make NEW Coder's Frame:

    (make-frame
     (quote
      ((name . "HA Mod Emacs v3.4 - Default Frame"))))

    ;; Select this NEW Frame:
    
    (select-frame-by-name "HA Mod Emacs v3.4 - Default Frame")

    ;; Set Default Face for Coding:
    (set-face-attribute 'default (selected-frame)
                        :family "Hermit"
                        :height 120)))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Modular Emacs - Make Frame Function:
;;  This makes NEW frames to match HA Modular Emacs
;;  current me--def-face flag setting...
;;  If the flag is set to: 1
;;     me_make-writing-frame is called...
;;  If the flag is set to: 2
;;     me_make-default-frame is called...

(defun me_make-frame ()
  "Create Frames depending on the current setting of
   the me--def-face flag. If flag = 2 make writer's
   frame. If flag = 1, make coder's frame..."
  (interactive)
  (progn
    (cond
     ((= me--def-face 2)
      (message "Creating Writer's Frame")
      (me_make-writing-frame))
     ((= me--def-face 1)
      (message "Creating Default Frame")
      (me_make-default-frame)))))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Modular Emacs - Set Default Face Functions:
;;
;;  Purpose:
;;
;;    I like to use a serif mono font for writing paragraphs...
;;    but I use Hermit or other similar font for Coding...
;;    This provides a way to go back and fourth from one
;;    face (which is Emacs Default) to another depending on
;;    my current work mode (writing or coding)...
;;
;;  Usage:
;;
;;    Adjust face dimensions and weight within forms below as needed.
;;    Note: Linux vs Mac, Big screen vs Laptop, may require
;;          sub cases to handle... %^)
;;
;;    Xah Fly Key Assigned: Command Mode "p"
;;
;;  NOTE1:  Currently there is no check to see if these fonts are
;;          installed on your system! This is still alpha test stage..."
;;
;;  NOTE2:  It appears that Courier Prime is required to prevent
;;          D o u b l e  W i d e rendering of certain Emacs faces! This
;;          was a problem on Linux.  Not sure about Mac OS yet as I
;;          installed Courier Prime there first and it "just worked" ;-)
;;          This is the reason I have two cases below in the function.
;;          Currently the cases produce redundant results, but i left them in
;;          in case I need to play around with this later...
;;
;;  NOTE3:  If I get fancy with fonts, I will provide them in this repo for easy
;;          installations. In fact I have already gotten fancy with fonts eh? LOL
;;

;;;
;; Set Modular Emacs Writing Frame Function:
;; NOTE: This only sets frame parameters
;;       for the currently Selected Frame...
;;        (other frames are not affected)

(defun me_set-writing-frame ()
  "Set Frame Font & Frame Dimensions for Writing"
  (interactive)
  (progn
    (set-face-attribute 'default (selected-frame)
                        :family "Courier Prime"
                        :height 135)

    (modify-frame-parameters
     nil
     (quote
      ((name . "HA Mod Emacs v3.4 - Writer's Frame")
       (height . 38)
       (width  . 84))))))


;;;
;; Set Modular Emacs Fancy Org-Mode WIDE Frame Function:
;; NOTE: This only sets frame parameters
;;       for the currently Selected Frame...
;;        (other frames are not affected)

(defun me_set-org-wide-frame ()
  "Set Frame Font & Frame Dimensions for Modular Emacs Fancy WIDESCREEN Org-Mode"
  (interactive)
  (progn
    (set-face-attribute 'default (selected-frame)
                        :family "Courier Prime"
                        :height 135)

    ;; Modify Frame dimensions for Fancy WIDE Org-Mode:
    (modify-frame-parameters
     nil
     (quote
      ((name   . "HA Mod Emacs v3.4 - Fancy Wide Org-Mode Frame")
       (height . 38)
       (width  . 142))))))


;;;
;; Set Modular Emacs Default Frame Function:
;; NOTE: This only sets the frame parameters
;;       for the currently Selected Frame...
;;        (other frames are not affected)

(defun me_set-default-frame ()
  "Set Font & Frame Parameters back to HA Modular Emacs Default"
  (interactive)
  (progn

    ;; Ensure default Coding Face:

    (set-face-attribute 'default (selected-frame)
                        :family "Hermit"
                        :height 120)

    ;; Change Frame Dimensions back to Normal Coding Frame:
    
    (modify-frame-parameters
     nil
     (quote
      ((name . "HA Mod Emacs v3.4 - Default Frame")
       (height . 38)
       (width  . 88))))))


;;;
;; Set Modular Emacs WIDE Coding Frame Function:
;; NOTE: This only sets the frame parameters
;;       for the currently Selected Frame...
;;        (other frames are not affected)

(defun me_set-coding-wide-frame ()
  "Set Frame Font & Frame Dimensions for Coding"
  (interactive)
  (progn
    (set-face-attribute 'default (selected-frame)
                        :family "Hermit"
                        :height 120)

    (modify-frame-parameters
     nil
     (quote
      ((name . "HA Mod Emacs v3.4 - WIDE Coder's Frame")
       (height . 38)
       (width  . 184))))

    ;; Split Window Side by Side for Comparing code etc.

    (split-window-right)))


;;;
;; Set Modular Emacs Default Coding Frame Function:
;; NOTE: This only sets the frame parameters
;;       for the currently Selected Frame...
;;        (other frames are not affected)

(defun me_set-coding-default-frame ()
  "Set Frame Font & Frame Dimensions to Coding Default"
  (interactive)
  (progn

    ;; Remove Split Windows (remaining window is where cursor is)

    (delete-other-windows)

    ;; Ensure Font is correct for Coding:
    
    (set-face-attribute 'default (selected-frame)
                        :family "Hermit"
                        :height 120)

    ;; Set Frame Parameters to Default Coder's Frame:
    
    (modify-frame-parameters
     nil
     (quote
      ((name . "HA Mod Emacs v3.4 - Default Frame")
       (height . 38)
       (width  . 88))))))


;;;
;;  Toggle Default Face Function:
;;
;;  Bound to Xah Fly Command Key:  "p"
;;
;;  NOTE1: This only sets the face for the currently Selected Frame...
;;         (other frames are not affected)
;;
;;  This function calls one of the two above depending on test variable:  me--default
;;    if me--default is t
;;      Switch to Writer's Frame Style
;;      Change me--default to nil
;;    Otherwise
;;      Switch to Coder's Frame Style
;;      Change me--default to t
;;
;;  NOTE2: The flag that gets set to check whether coding or writing mode
;;         DOES effect other frames!  This is a side effect, but the remedy
;;         is to simply toggle the key twice.  A silly fix is too complicated imho!
;;

(defvar me--def-face 1 "Test variable for me_toggle-default-face")

(defun me_toggle-default-face ()
  "Toggle default frame parameters for selected frame only,
   depending on current editing needs...
   Purpose: I like to use a serif mono font for writing
   paragraphs, but I need to use Hermit etc. for Coding
   This provides a way to toggle from one to the other"
  ;; NOTE: The flag me--def-face DOES effect other frames!
  ;;       Therefore, For example: If you are editing code in one frame
  ;;       and writing a screenplay in another frame, if you switch to
  ;;       writing mode in the coding frame, that will also reset the
  ;;       flag in your screenplay frame.  It will not effect the display
  ;;       of the screenplay frame, only the flag.  This results in you
  ;;       needing to hit the toggle key twice if, for instance, you needed
  ;;       to change from writing a screenplay to doing some coding...
  ;;                             No Problemo! %^)
  ;;       The complicated code needed to have a local flag for each frame
  ;;       is too much worry over the silly fix it would provide...
  ;;       Simply toggle the key a second time if you do not see the font
  ;;       change etc. Note, this will reset the flag in your coding frame,
  ;;       but no worries there either.. (the woes of multitasking lol)
  ;;
  ;;       UPDATE: Thinking on this further... It makes sense to do this
  ;;               in a let form...  Then local variables will not effect
  ;;               flag state in other frames... I was being lazy above. %~)
  ;;               Still being lazy cause I have not put that let form in
  ;;               below yet... LOL
  (interactive)
  (cond
   ((= me--def-face 1)
    (message "Setting default face to Courier Prime for Writing")
    (me_set-writing-frame)
    (setq me--def-face 2))
   ((= me--def-face 2)
    (message "Setting default face to Hermit for Coding")
    (me_set-default-frame)
    (setq me--def-face 1))))

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/02-package-conf.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
