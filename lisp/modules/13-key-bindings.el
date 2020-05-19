;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;;; [Modular-Emacs]:~/.emacs.d/lisp/modules/13-key-bindings.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Map Modular Emacs General Keys:

(defvar me--my-keyboard-bindings
  '(("C-c l" . org-store-link)
    ("C-c a" . org-agenda)
    ("C-c c" . org-capture)
    ("C-x C-g" . deft-find-file)
    ("C-c m" . org-md-export-to-markdown)
    ("C-c s" . flyspell-auto-correct-word) 
    ("C-c v" . vmd-mode)
    ("C-c r" . view-mode)
    ("C-c '" . imenu-list-smart-toggle)
    ("C-c b" . other-frame)
    ("C-c u" . me_toggle-letter-case)
    ("M-x" . helm-M-x)
    ("C-x b" . helm-mini)
    ("C-x C-f" . helm-find-files)
    ("C-x C-r" . helm-recentf)
    ("<f5>" . me-reload-current-file)))

(defun me-apply-keyboard-bindings (pair)
  "Apply keyboard-bindings for supplied list of key-pair values"
  (interactive)
  (global-set-key (kbd (car pair))
                  (cdr pair)))

(mapc 'me-apply-keyboard-bindings
      me--my-keyboard-bindings)


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Flyspell Correct Previous - Helm key binding:

(require 'flyspell-correct-helm)
(define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous)


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Modular Emacs - Set Default Face Functions:
;;
;; Purpose:
;;
;;  I like to use a serif mono font for writing paragraphs...
;;  but I use Hermit or other similar font for Coding...
;;  This provides a way to go back and fourth from one
;;  face (which is Emacs Default) to another depending on
;;  my current work mode (writing or coding)...
;;
;; Usage:
;;
;;   Adjust face dimensions and weight within forms below as needed.
;;   Note: Linux vs Mac, Big screen vs Laptop, may require
;;         sub cases to handle... %^)
;;
;;   Xah Fly Key Assigned: Command Mode "p"
;;
;; NOTE: Currently there is no check to see if these fonts are 
;;       installed on your system! This is still alpha test stage..."
;;

(defun me_set-org-face ()
  ;; Set default face to Go Mono for Powerline (A nice mono serif for writing)...
  (interactive)
  (progn
    (set-face-attribute 'default nil
                        :family "Go Mono for Powerline"
                        :slant 'normal
                        :height 123
                        :weight 'normal
                        :width 'normal)
    (setq-default 'me--default nil)))

(defun me_set-default-face ()
  ;; Set default font to Hermit Medium (my favorite mono font for everything)...
  (interactive)
  (progn
    (set-face-attribute 'default nil
                        :family "Hermit"
                        :foundry "PfEd"
                        :slant 'normal
                        :height 120
                        :weight 'normal
                        :width 'normal)
    (setq-default 'me--default t)))

;;;
;; Toggle Default Face... This one gets bound to Xah Fly Command Key:  "p"
;; This one calls one of the two above depending on test variable:  me--default
;; if me--default is t,
;;   Switch to Org Mode;
;;   Change me--default to nil;
;; Otherwise
;;   Switch back to default face;
;;   Change me--default to t;
;;   

(defvar me--default t "Test variable for me_toggle-default-face")

(defun me_toggle-default-face ()
  "Toggle default face, depending on current need...
   Purpose: I like to use a serif mono font for writing
   paragraphs, but I need to use Hermit etc. for Coding
   This provides a way to toggle from one to the other"
  (interactive)
  (if (eq me--default t)
      'me_set-org-face
    'me_set-default-face))


;;;
;; Map Linux Alt keys to Emacs META:
;; Don't enable this next form as it is not working yet...
;; On Linux, I swapped Caps-Lock and Home keys using xmodmap to accommodate using
;; Xah Fly Keys in an efficient manner...  However, I tried many ways, including
;; full blown custom xkb keymap to swap my control and alt keys, (so it would be
;; the same keystrokes for me on both Mac and Linux ;-).  All that sweat to no
;; avail so far! ...but I will get it.. I was stuck and needed a good break...
;; The Mac has its quirks as well but Karabiner Elements saved the day!
;;
;; You may not need any of this anyway... Unless you also want to enable and use
;; Xah Fly Keys... Then you are in the same Linux boat as I am... ARR!!! Mate! %^)
;; This form will be enabled, (with the right settings) once I get all this working!
;; Stay Tuned...

;(when *is-linux*
  ;(define-key key-translation-map (kbd "Control_L") (kbd "Alt_L"))
  ;(define-key key-translation-map (kbd "Alt_L") (kbd "Ccontrol_L"))
  ;(define-key key-translation-map (kbd "Control_R") (kbd "Alt_R"))
  ;(define-key key-translation-map (kbd "Alt_R") (kbd "Ccontrol_R"))
;  (setq x-alt-keysym 'control)
;  (setq x-ctrl-keysym 'meta)
;  (setq x-super-keysym 'meta))

;;;
;; For Mac OS - Set Mac Command key (both sides of space bar) to work as Emacs
;; Control Key...  Set normal keyboard control key to work as Emacs META:
;; Note: Don't enable this unless you like to have the control key on both
;; sides right next to your space bar (for Emacs only)...
;;
;; In addition to above, you have to install and configure Karabiner-Elements on
;; your Mac to remap some of your keys. I did all the above and also enable
;; Xah-Fly-Keys to make Emacs behave as a modal editor similar to the way Vi
;; works. After using this setup over my hands and fingers are now much less
;; stressed, and I can edit code, compose documents or write fiction faster than
;; ever!

(when *is-darwin*
  (setq mac-command-modifier 'control)
  (setq mac-control-modifier 'meta))

;;;
;; Make Escape Key Do C-g:
;; This is a goodie I learned while setting up Xah-Fly-Keys.  It is not for
;; Xah Fly Keys users specifically, but rather a general purpose aid...

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;;;
;; Bind M-h key to Invoke Slime Doc Lookup:
;; You don't need this unless you have enabled the optional ProgLang module
;; and its associated sub modules... But leaving this enabled does not hurt
;; if you are not using those modules... Best to leave this alone... It
;; does not activate until slime-mode is loaded...

(eval-after-load 'slime
  `(define-key slime-prefix-map (kbd "M-h") 'slime-documentation-lookup))

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/13-key-bindings.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
