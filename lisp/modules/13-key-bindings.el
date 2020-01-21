;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;;; [modular-emacs]:~/.emacs.d/lisp/modules/13-key-bindings.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;(defun me-insert-backtick-quote () (interactive) (xah-insert-bracket-pair "`" "`"))

;;;
;; Map Modular Emacs General Keys:

(defvar me--my-keyboard-bindings
  '(("C-'" . imenu-list-smart-toggle)
    ("C-\," . neotree-toggle)
    ("C-c d" . deft)
    ("C-x C-g" . deft-find-file)
    ("C-c m" . org-md-export-to-markdown)
    ("C-c v" . vmd-mode)
    ("C-c r" . view-mode)
    ("C-c ," . other-frame)
    ("M-c" . toggle-letter-case)
    ("M-x" . helm-M-x)
    ("C-x b" . helm-mini)
    ("C-x C-f" . helm-find-files)
    ("C-x C-r" . helm-recentf)
    ("<f5>" . me-reload-current-file)))

(defun me-apply-keyboard-bindings (pair)
  "Apply keyboard-bindings for supplied list of key-pair values"
  (global-set-key (kbd (car pair))
                  (cdr pair)))
 
(mapc 'me-apply-keyboard-bindings
      me--my-keyboard-bindings)

;;;
;; Add new key(s) to xah fly command mode keys:
(defun me-xfk-cmd-keys-add ()
  "Add or Modify xah fly keys - Command Mode Keys
To be added to `xah-fly-command-mode-activate-hook'. Note, it appears
you have to toggle from command mode to insert mode and back the first
time you open Emacs.  For some reason this hook is not being called the
very first time..."
  (interactive)
  ;; I need easy keys to create and switch frames, (not just windows)
  ;; Add more key definitions here if needed.
  ;; Options not used by xfkeys:  ~  `  1  2  0  \  - and =
  (define-key xah-fly-key-map (kbd "b") 'other-frame)
  (define-key xah-fly-key-map (kbd "2") 'make-frame))

(add-hook 'xah-fly-command-mode-activate-hook 'me-xfk-cmd-keys-add)

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

;;;
;; Function slime-space() is called from SPC key when in slime-mode!
;; This cripples my Xah Fly Keys Global SPC Leader key which needs to
;; be established while in COMMAND mode! I use Xah-Fly-Keys (both modes)
;; pretty much all the time.  The xah SPC leader key cannot get shadowed
;; by other modes when I am in xah-fly-keys-command-mode.
;;
;; You care about this if:  You use the extra Common Lisp Prog Lang modules,
;; AND you also wish to enable Xah-Fly-Keys...
;;
;; If True, Enable ALL forms below:
;; Otherwise, Leave everything below commented out as it is...
;;
;; Set slime SPC key to xah-fly-leader-key when activating xah fly command mode:

;(defun override-slime-space-key-binding ()
;  (define-key slime-mode-indirect-map (kbd "SPC") 'xah-fly-leader-key-map))

;(add-hook 'xah-fly-command-mode-activate-hook 'override-slime-space-key-binding)

; Set Slime SPC key back to slime-space() when activating xah fly insert mode:

;(defun restore-slime-space-key-binding ()
;  (define-key slime-mode-indirect-map (kbd "SPC") 'slime-space))

;(add-hook 'xah-fly-insert-mode-activate-hook 'restore-slime-space-key-binding)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/13-key-bindings.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
