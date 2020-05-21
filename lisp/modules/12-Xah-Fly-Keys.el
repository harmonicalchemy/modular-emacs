;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/12-Xah-Fly-Keys.el
;;
;; This module turns Emacs into a Modal Editor (like VI) but this is NOT a
;; VI emulation..  The keys are arranged to improve efficiency and limit finger
;; fatigue and the famous EPF syndrome.
;; Ref:  http://ergoemacs.org/emacs/elisp_insert_brackets_by_pair.html
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Set Keyboard Layout for xah-fly-keys:
;; possible layout values:
;;    "azerty"
;;    "azerty-be"
;;    "colemak"
;;    "colemak-mod-dh"
;;    "dvorak"
;;    "programer-dvorak"
;;    "qwerty"
;;    "qwerty-abnt"
;;    "qwertz"
;;    "workman"

;; Disable Xah's Control & Meta Key Functions:
;; I don't need these as I am learning leader (SPACE) key
;; commands that are much better eh?
;; Also, I have some of my own Ctrl Key's of my own that
;; must not be overwritten!

(setq xah-fly-use-control-key nil)
(setq xah-fly-use-meta-key nil)

;; Enable Xah Fly Keys:

(require 'xah-fly-keys)
(xah-fly-keys-set-layout "qwerty") ; My keyboard layout...
(xah-fly-keys 1)

;; Enable Xah eLisp Mode in eLisp files:

(add-to-list 'auto-mode-alist '("\\.el\\'" . xah-elisp-mode))

;; Load Xah Find functions:

(autoload 'xah-find-text "xah-find" "find replace" t)
(autoload 'xah-find-text-regex "xah-find" "find replace" t)
(autoload 'xah-find-replace-text "xah-find" "find replace" t)
(autoload 'xah-find-replace-text-regex "xah-find" "find replace" t)
(autoload 'xah-find-count "xah-find" "find replace" t)




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
  ;; Set default face to Courier Prime Emacs (A nice mono serif for writing)...
  (interactive)
  (progn
    (set-face-attribute 'default nil
                        :family "Courier Prime Emacs"
                        :slant 'normal
                        :height 131
                        :weight 'normal
                        :width 'normal)))

(defun me_set-default-face ()
  ;; Set default font to Hermit Medium (my favorite mono font for everything)...
  (interactive)
  (progn
    (set-face-attribute 'default nil
                        :family "Hermit"
                        :slant 'normal
                        :height 118
                        :weight 'normal
                        :width 'normal)))

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

(defvar me--def-face 1 "Test variable for me_toggle-default-face")

(defun me_toggle-default-face ()
  "Toggle default face, depending on current need...
   Purpose: I like to use a serif mono font for writing
   paragraphs, but I need to use Hermit etc. for Coding
   This provides a way to toggle from one to the other"
  (interactive)
  (cond
   ((= me--def-face 1)
    (message "Set default face to Courier Prime Emacs")
    (me_set-org-face)
    (setq me--def-face 2))
   ((= me--def-face 2)
    (message "Set default face back to normal code font")
    (me_set-default-face)
    (setq me--def-face 1))))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;           CUSTOM Xah Fly KEY BINDINGS
;;  Add Personalized keybindings to Xah fly Keys:
;;  Both command and insert mode keys...

(with-eval-after-load 'xah-fly-keys
  ;;
  ;; Options not used by Xah fly keys:  ~  `  1  2  0  \  -  =
  ;;
  ;; In addition you could use "p" as a command mode key.  I never use
  ;; it to insert a space before.  typing p is harder to reach than
  ;; going into insert mode "f" and hitting the space bar.  IMHO

  ;;;
  ;; Command mode keybindings:
  ;; Add more key definitions to this list when needed:

  ;; Set easy keys to create and switch frames, (not just windows)
  (define-key xah-fly-command-map (kbd "b") 'other-frame)
  (define-key xah-fly-command-map (kbd "2") 'make-frame)

  ;; Set easy key to toggle neotree in left window pane:
  ;; Note: This disables (default Xfk to run command delete-char)
  ;;       I don't need that as the "D" key works fine for me...
  (define-key xah-fly-command-map (kbd "5") 'neotree-toggle)

  ;; Change default olivetti-mode key (because I have conflicting other-frame key)
  (define-key xah-fly-command-map (kbd "`") 'olivetti-mode)

  ;; Set global key to toggle imenu (pops up in Helm window)...
  (define-key xah-fly-command-map (kbd "'") 'imenu)

  ;; Set Invoke Daft key to primary KFKeys Command Mode Map...
  (define-key xah-fly-command-map (kbd "0") 'deft)

  ;; Set Key to Hide Org Tree Heading Bullets:
  (define-key xah-fly-command-map (kbd "p") 'me_toggle-default-face)

  ;; Set Keys to open Org Tree Element(s) in Right window pane...
  ;; This first key also moves cursor to right window
  ;;   TODO: Position cursor for instant writing, (i.e., continuing
  ;;         at last edited cursor location)
  (define-key xah-fly-command-map (kbd "1") 'me_org-tree-open-in-right-win)

  ;; This key opens Elements(s) in Right window Pane But leaves the
  ;; cursor in the left outline window pane (it stays where it was)
  (define-key xah-fly-command-map (kbd "s") 'org-tree-to-indirect-buffer)

  ;; Set key to run HLedger Mode command:
  (define-key xah-fly-command-map (kbd "=") 'hledger-run-command)

  ;;;
  ;; Insert mode keybindings:
  ;; Add more key definitions to this list when needed:

  ;; Set KFKeys Leader Sequence to expand and shrink olivetti...
  (define-key xah-fly-leader-key-map (kbd "]") 'olivetti-expand)
  (define-key xah-fly-leader-key-map (kbd "[") 'olivetti-shrink)

  ;; Set KFKeys Leader Sequence to toggle case (three choices)...
;  (define-key xah-fly-leader-key-map (kbd "u") 'me_toggle-letter-case)

  ;; This leader-key delete-frame key mirrors direct make-frame key above...
  (define-key xah-fly-leader-key-map (kbd "2") 'delete-frame)

  ;; Added VMD mode leader key sequence: SPC "v" ("k" Dvorak)
  ;; since I already have that paste key in normal Command mode...
  (define-key xah-fly-leader-key-map (kbd "v") 'vmd-mode))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

;;; Set slime SPC key to xah-fly-leader-key when activating xah fly command mode:

(defun override-slime-space-key-binding ()
  (define-key slime-mode-indirect-map (kbd "SPC") 'xah-fly-insert-map))

(add-hook 'xah-fly-command-mode-activate-hook 'override-slime-space-key-binding)

;;; Set Slime SPC key back to slime-space() when activating xah fly insert mode:

(defun restore-slime-space-key-binding ()
  (define-key slime-mode-indirect-map (kbd "SPC") 'slime-space))

(add-hook 'xah-fly-insert-mode-activate-hook 'restore-slime-space-key-binding)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/12-Xah-Fly-Keys.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
