;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modula-emacs]:~/.emacs.d/lisp/modules/02-package-conf.el
;;
;; This module provides the basic modules that most likely will be needed
;; by most all use-cases...  Modular Emacs default packages so-to-speak...
;;
;; Change Log:
;;
;; 2020-005-16 - Alisha Awen, siren1@disroot.org
;;   disabled poserline mode-line stuff...  I got tired of it...  Too busy...
;;   I found smart-mode-line to be better for my needs... That is the new
;;   default going forward.  I left the powerline code in, (disabled) in case
;;   you like it and would like to switch back...
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

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Configure & Enable Smart-mode-line:
;; DISABLED - I no longer wish to mess with fancy
;;            mode lines... The default works fine
;;            for me. ;-) Your mileage may vary...
;
;;  Choose SML Theme:
;;  NOTE:   THIS IS DISABLED
;;    (I am no longer using mode-line packages)
;;    (pick one and enable it if you like but also
;;    load the theme at top first!)
;
;(setq sml/theme 'dark)
;(setq sml/theme 'light)
;(setq sml/theme 'respectful)
;
;; Enable Smart Mode Line after Emacs Startup:
;; NOTE: THIS IS DISABLED (see above note)
;(add-hook 'after-init-hook 'sml/setup)
;
;;; POWERLINE MODE
;;                          (DISABLED)
;;  If you would rather use powerline, enable the three forms below
;;  and disable the above smart-mode-line section if you enabled it
;;  previously...
;;  Also make sure to load the mode at the top in the package install
;;  section!
;;
;; Enable powerline:
;(require 'powerline)
;(powerline-center-theme)
;(setq powerline-default-separator 'slant)
;
;
;; Platform Specific SML directory abbreviations:
;; NOTE: THIS IS DISABLED - Just as above you need to
;;       un-comment this section and make sure other smart-mode-line
;;       sections above are enabled first... (also load the package
;;       at the very top in the package install section)...
;;
;;  This is a demo list... It probably works for your .emacs.d directory
;;  and standard Docs directory, but you will need to fill in the path to
;;  the last element in the list to a real directory on your system
;;  (one which you would like to make a shortcut abbreviation for...
;;  Add more just like that to the end of the list (as instructed in the
;;  comment at the end of the list)
;
;; Platform Specific SML directory abbreviations:
;;                  (DISABLED)
;(when ME--DARWIN
;  (add-to-list 'sml/replacer-regexp-list '("^~/\\.emacs\\.d/" ":EMACS:"))
;  (add-to-list 'sml/replacer-regexp-list '("^~/Documents/" ":DOCS:"))
;  ;; Add more platform specific directory shortcut abbreviations to this list here as needed....
;  ;; When you are done adding new abbreviations, get rid of this comment and pull up the
;  ;; final parenthesis below to tidy up %^)...
;  )
;
;(when ME--LINUX
;  (add-to-list 'sml/replacer-regexp-list '("^~/\\.emacs\\.d/" ":EMACS:"))
;  (add-to-list 'sml/replacer-regexp-list '("^~/Documents/" ":DOCS:"))
;  ;; Add more platform specific directory shortcut abbreviations to this list here as needed....
;  ;; When you are done adding new abbreviations, get rid of this comment and pull up the
;  ;; final parenthesis below to tidy up %^)...
;  )
;
;; Platform Independent SML directory abbreviations:
;
;  (add-to-list 'sml/replacer-regexp-list '("^:DOCS:/Path/To/Your/Other/Docs/" ":My-Other-Docs:"))
;
;; Add more platform independent directory shortcut abbreviations just like the last form above...
;; These are invoked independently as complete forms here... No cleanup or closing paren needed when
;; adding to the end of this list...


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
    (let ((frame-height ME--DOC-FRAME-HEIGHT)
          (frame-width ME--DOC-FRAME-WIDTH))
      ;; Set Frame Dimensions for Writing:
      (make-frame
       (quote
        ((name   . "HA Mod Emacs v3.4 - Writer's Frame")
         (height . 42)
         (width  . 92)))))

    ;; Select this new frame:
    (select-frame-by-name "HA Mod Emacs v3.4 - Writer's Frame")

    ;; Set Default Face to Courier Prime:
    (set-face-attribute 'default (selected-frame)
                        :family "Courier Prime"
                        :height 130)

    ;; Set Olivetti Default Width
    (olivetti-set-width ME--DOC-OLIV-WIDTH)

    ;; Call Xah-Fly-Keys (resets some face attributes)
    (xah-fly-keys 1)))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Modular Emacs - Make NEW Coder's Frame:
;;  This makes a brand new frame to match
;;  HA Modular Emacs Default Coding Frame Specs...
;;  The new frame opens with initial buffer set
;;  to: Bookmark Menu List

(defun me_make-coding-frame ()
  "Create NEW Frame using ME default coding face
   and ME default coding frame dimensions
   Frame opens with buffer set to Bookmark Menu List"
  (interactive)
  (progn
    ;; Set buffer to *Bookmark List*
    (bookmark-bmenu-list)

    ;; Make NEW Coder's Frame:
    (let ((frame-height ME--CODE-FRAME-HEIGHT)
          (frame-width ME--CODE-FRAME-WIDTH))
      ;; Set Frame Dimensions for Coding:
      (make-frame
       (quote
        ((name . "HA Mod Emacs v3.4 - Coder's Frame")
         (height . 42)
         (width . 92)))))

    ;; Select this NEW Frame:
    (select-frame-by-name "HA Mod Emacs v3.4 - Coder's Frame")

    ;; Set Default Face for Coding:
    (set-face-attribute 'default (selected-frame)
                        :family "Hermit"
                        :height 120)

    ;; Enable Olivetti Mode - Default Coding Width:
    (olivetti-mode)
    (olivetti-set-width ME--CODE-OLIV-WIDTH)

    ;; Call Xah-Fly-Keys (resets some face attributes)
    (xah-fly-keys 1)))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Modular Emacs - Make Frame Function:
;;  This makes NEW frames to match HA Modular Emacs
;;  current me--def-face flag setting...
;;  If the flag is set to: 1
;;     me_make-writing-frame is called...
;;  If the flag is set to: 2
;;     me_make-coding-frame is called...

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
      (message "Creating Coder's Frame")
      (me_make-coding-frame)))))

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
                        :height 130)

    ;; Modify Frame dimensions for Writing...
    (let ((frame-height ME--DOC-FRAME-HEIGHT)
          (frame-width ME--DOC-FRAME-WIDTH))
      (modify-frame-parameters nil
                               (quote
                                ((name   . "HA Mod Emacs v3.4 - Writer's Frame")
                                 (height . 42)
                                 (width  . 92)))))))


;;;
;; Set Modular Emacs Coding Frame Function:
;; NOTE: This only sets the frame parameters
;;       for the currently Selected Frame...
;;        (other frames are not affected)

(defun me_set-coding-frame ()
  "Set Frame Font & Frame Dimensions for Coding"
  (interactive)
  (progn
    (set-face-attribute 'default (selected-frame)
                        :family "Hermit"
                        :height 120)

    ;; Modify Frame dimensions for Coding...
    (let ((frame-height ME--CODE-FRAME-HEIGHT)
          (frame-width ME--CODE-FRAME-WIDTH))
      (modify-frame-parameters nil
                               (quote
                                ((name   . "HA Mod Emacs v3.4 - Coder's Frame")
                                 (height . 42)
                                 (width  . 92)))))))

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
    (me_set-coding-frame)
    (setq me--def-face 1))))

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/02-package-conf.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
