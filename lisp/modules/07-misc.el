;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/07-misc.el
;;
;; This module is a catch-all for things that don't seem to fit in a specific
;; category.  LOL "miscellaneous" is the category you use when YOU HAVE NO
;; IDEA what category to use... That's This Module for HAP Modular Emacs...

;;
;; CHANGE LOG: (descending chronological order)
;;

;; 2023-003-25 - Alisha Awen, HarmonicAlchemy@proton.me
;;    Added ORG Emphasis Functions:

;; 2022-010-06 - Alisha Awen, HarmonicAlchemy@proton.me
;;    Updated Comments, etc.  Nothing special... This module started out as an
;;    afterthought... But it is an important place to keep odds-and-ends that
;;    Don't fit anywhere else...
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ORG Emphisis Functions:
;; These are quick ways to format the look of text within org-mode 
;; paragraphs etc...  The selection characters for different emphesis are:
;; "*" "~" "=" "/" "_" and "+" if called on Selected Text the entire selection
;; will automagially change to proper emphisis... If executed on a blank line
;; you simply get the delimiter charadters and may start writing the text inside
;; which will change to the specified emphisis once you begin typing...

(defun me_make-org-text-bold ()
  "This function sets selected text to bold emphisis."
  (interactive)
  (org-emphasize '?*))

(defun me_make-org-text-code ()
  "This function sets selected text to monospace code emphisis."
  (interactive)
  (org-emphasize '?~))

(defun me_make-org-text-hilight ()
  "This function sets selected text to HIGHLIGHTED monospace emphisis."
  (interactive)
  (org-emphasize '?=))

(defun me_make-org-text-italic ()
  "This function sets selected text to italic emphisis."
  (interactive)
  (org-emphasize '?/))

(defun me_make-org-text-underlined ()
  "This function sets selected text to underlined emphisis."
  (interactive)
  (org-emphasize '?_))

(defun me_make-org-text-strikethrough ()
  "This function sets selected text to strikethrough emphisis."
  (interactive)
  (org-emphasize '?+))

(defun me_make-org-text-plain ()
  "This function sets selected text back to NO Emphisis. This simply opens up the selection... You still have to remove the delimiter characters..."
  (interactive)
  (org-emphasize '? ))
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Insert Date Function: (without TIME component)
;;  Insert Date at point using format:  yyyy-mmm-dd
;;  If a region has been selected, delete that first...

(defun me_insert-date ()
  "Insert current date in this format: 2020-012-25.
   Uses three digit months to distinguish from days."
  (interactive)
  (when (use-region-p) (delete-region (region-beginning) (region-end)))
  (insert (format-time-string "%Y-%03m-%d"))) ;; "2022-010-06"


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Reload File Function:

(defun me_reload-current-file ()
  "Reload the file loaded in current buffer from the disk"
  (interactive)
  (cond (buffer-file-name (progn (find-alternate-file buffer-file-name)
                                 (message "File reloaded")))
        (t (message "You're not editing a file!"))))

;; Disable splash message, start *scratch* buffer by default

(setq initial-buffer-choice t)

(setq initial-scratch-message "")

;; Enable winner-mode

(winner-mode t)

;; Highlight current line

(global-hl-line-mode +1)

;; Turn off highlight long lines

(setq whitespace-line-column 10000)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Parenthesis / Bracket HIGHLIGHTING
;;  Matching Pairs Are Highlighted Blue: ( ) [ ]
;;  Un-Matching & Singletons are Highlighted dark
;;  RED when cursor is ON either one: (  ] or single [

;;; TURN ON Highlight Matching Parenthesis/Brackets Mode:

(show-paren-mode 1)

;;; Option 1: ONLY Highlight Bracket Characters: ( ) [ ]
;(setq show-paren-style 'parenthesis)

;;; Option 2: HIGHLIGHT ENTIRE Matching Pair EXPRESSION:
;(setq show-paren-style 'expression)

;;; MY CHOICE: Highlight Bracket Chars ONLY If BOTH Visible:
;;             ELSE, Highlight The ENTIRE EXPRESSION:
;;
;; This one is helpful when trying to find matching pair of
;; real long forms, (using my split windows technique) to get
;; Begin And End Parens In View At Same Time...
;; BUT it WON'T HIGHLIGHT YOU IN YOUR FACE for short forms...

(setq show-paren-style 'mixed)

;; Change all prompts to y or n

(fset 'yes-or-no-p 'y-or-n-p)

;; Turn on Visual Line Mode for text modes only

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Enable Generic Modes:

(require 'generic-x)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Set Olivetti Mode DEFAULT WIDTH: (from constant)

(setq olivetti-body-width ME--CODE-OLIV-WIDTH)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; The default visible bell actually startles me when it occurs!
;; This is a much better way... Only the mode line flashes!  Great.
;; I found this clever snippit in a comment by: Phil@disqus_COwPSAc69c
;; on: Pragmatic Emacs.

(defun my-visible-bell ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(define-minor-mode my-visible-bell-mode
  "Use `my-visible-bell' as the `ring-bell-function'."
  :global t
  (let ((this 'my-visible-bell-mode))
    (if my-visible-bell-mode
        (progn
          (put this 'visible-bell-backup visible-bell)
          (put this 'ring-bell-function-backup ring-bell-function)
          (setq visible-bell nil
                ring-bell-function #'my-visible-bell))
      ;; Restore the original values when disabling.
      (setq visible-bell (get this 'visible-bell-backup)
            ring-bell-function (get this 'ring-bell-function-backup)))))

(setq visible-bell t)
(my-visible-bell-mode 1)

;; Do not invoke Debugger on Errors: key mistakes, etc...
;; (I prefer to do this manually only when there are real problems)
(setq debug-on-error nil)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/07-misc.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
