;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/00-system.el
;;
;; Note:  I changed everything in this file because it was causing issues with
;;        newer versions of emacs!  Everything below has been re-written based
;;        on Emacs Manual: 49.1.4 Saving Customizations...
;;        Also, all my work environments now have the latest GnuPG installed.
;;        When GPG is invoked, it will (and should) be at least V2.2 or higher...
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;;
;; Set custom-file so that Emacs does not use init.el

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;;
;; Use GPG Version 2 (gpg2) instead of gpg.
;; Note: If you are using GnuPG later than V2.2 you may have to change
;; below from "gpg2" back to "gpg". (After version 2.2, the default GnuPG
;; command defaults back to "gpg" (like GnuPG was before V2).
;; However some systems have both gpg & gpg2 installed so it is best to
;; leave it below the way it is. (you could also create an alias in .bashrc
;; .profile, etc.)

(setq epg-gpg-program "gpg")

;;;
;; System Constants: (Sonnet XVIII rebuttal - no rhyming yet. maybe later)
;;
;;   There are no constants in life,
;;   Nothing to compare,
;;   They are like the dead,
;;   Frozen in place,
;;   Indelible stasis,
;;   Even our dead contain life not ours,
;;   Only timeless skeletons remain,
;;   Fading and soon forgotten,
;;   But the living, they move on,
;;   Engaged in the flux,
;;   Intoxicated from fair to fair,
;;   New memories emerge,
;;   Old memories lost,
;;   forever now new...
;;                  - Alisha Awen

(defconst ME--DARWIN (eq system-type 'darwin))
(defconst ME--LINUX (eq system-type 'gnu/linux))
(defconst ME--POSIX (memq window-system `(ns x)))

;;;
;; Load Harmonic Alchemy Productions - Modular Emacs CONSTANTS
;; NOTE: Override The Defaults set in the load file below to
;;       better fit your particular platform needs... Do this
;;       by copying the file below into "my-modules" and then
;;       editing it...

(if (file-exists-p "~/.emacs.d/lisp/my-modules/me-constants.el")
    (load-file "~/.emacs.d/lisp/my-modules/me-constants.el")
  (load-file "~/.emacs.d/lisp/modules/me-constants.el"))

;;;
;; Configure custom elisp library load path.

(add-to-list 'load-path "~/.emacs.d/lisp/")

(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;;;
;; Load path for your custom Emacs themes:

(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/themes/")

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Enable, Force UTF8 encoding EVERYWHERE in Emacs:
;; Set the default encoding system to UTF8
;; Explicitly & Redundantly for all these below...
;; (even though Emacs may already have done it)
;; (even though you put those directives at the top of files)
;; I BELIEVE ALL OF THIS TAKES CARE OF utf8 NOW EH?  
;; If I see any warnings about f---ing UTF8 within emacs
;; or exported PDFs etc., I WILL BE SO PISSED,,, LOL

(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)

(prefer-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
   (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; disable CJK coding/encoding - not needed (yet)
;; (Chinese/Japanese/Korean characters)

(setq utf-translate-cjk-mode nil)

;; backwards compatibility as default-buffer-file-coding-system
;; is deprecated in 23.2.
(if (boundp buffer-file-coding-system)
    (setq buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

;; END: [modular-emacs]:~/.emacs.d/lisp/modules/00-system.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
