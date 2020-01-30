;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;;; [Modular-Emacs]:~/.emacs.d/lisp/modules/13-key-bindings.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;;
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
    ("C-c ," . other-frame)
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

;;;
;; Flyspell Correct Previous - Helm key binding:

(require 'flyspell-correct-helm)
(define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous)

;;;
;; Add New key(s) to xah fly command mode keys:

(defun me-xfk-cmd-keys-add ()
  "Add or Modify xah fly keys - Command Mode Keys
  To be added to `xah-fly-command-mode-activate-hook'. 
  NOTE: It appears you have to toggle from command mode to insert mode 
    and back at least one time after closing and reopening Emacs before
    these custom keys take hold...  I need to troubleshoot this hook to
    discover the cause.  For some reason this hook is not being called 
    the very first time... Also, there seem to be some quirks with 
    deleting frames, causing Emacs to loose focus requiring clicking 
    mouse outside, and then back to window frame to get it back in focus."
  (interactive)
  ;; Add more key definitions here if needed.
  ;; Options not used by xfkeys:  ~  `  1  2  0  \  -  =
  ;; I need easy keys to create and switch frames, (not just windows)
  (define-key xah-fly-key-map (kbd "b") 'other-frame)
  (define-key xah-fly-key-map (kbd "2") 'make-frame)
  ;; Added olivetti-mode key since I have a new custom other-frame key...
  (define-key xah-fly-key-map (kbd "`") 'olivetti-mode)
  ;; Added neotree key to primary KFKeys Command Mode Map...
  (define-key xah-fly-key-map (kbd "'") 'neotree-toggle)
  ;; Added key to primary KFKeys Command Mode Map to invoke deft-mode...
  (define-key xah-fly-key-map (kbd "0") 'deft)
  ;; I need easy keys for HLedger Mode:
  (define-key xah-fly-key-map (kbd "s") 'hledger-jentry)
  (define-key xah-fly-key-map (kbd "=") 'hledger-run-command)
  ;; Added KFKeys Leader Sequence to expand and shrink olivetti...
  (define-key xah-fly-leader-key-map (kbd "]") 'olivetti-expand)
  (define-key xah-fly-leader-key-map (kbd "[") 'olivetti-shrink)
  ;; Added KFKeys Leader Sequence to toggle case (three choices)...
  (define-key xah-fly-leader-key-map (kbd "u") 'me_toggle-letter-case)
  ;; leader-key delete-frame key mirrors direct make-frame key...
  (define-key xah-fly-leader-key-map (kbd "2") 'delete-frame)
  ;; Added VMD mode leader key sequence: SPC "v" ("k" Dvorak)
  ;; since I already have that paste key in normal Command mode...
  (define-key xah-fly-leader-key-map (kbd "v") 'vmd-mode))

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

;;;
;; Set Slime SPC key back to slime-space() when activating xah fly insert mode:

;(defun restore-slime-space-key-binding ()
;  (define-key slime-mode-indirect-map (kbd "SPC") 'slime-space))

;(add-hook 'xah-fly-insert-mode-activate-hook 'restore-slime-space-key-binding)

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/13-key-bindings.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
