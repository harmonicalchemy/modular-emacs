;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/12-Xah-Fly-Keys.el
;;
;; This module turns Emacs into a Modal Editor (like VI) but this is NOT a
;; VI emulation..  The keys are arranged to improve efficiency and limit finger
;; fatigue and the famous EPF syndrome.
;; Ref:  http://ergoemacs.org/emacs/elisp_insert_brackets_by_pair.html
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;;
;; Create repositories cache, if required

(when (not package-archive-contents)
  (package-refresh-contents))

;;;
;; Declare default modular-emacs list of required packages:

(defvar me--fly-keys-packages
  '(xah-fly-keys
    xah-elisp-mode
    xah-find))

;;;
;; Install required packages

(mapc (lambda (p) (package-install p))
      me--fly-keys-packages)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Disable Xah's Control & Meta Key Functions:
;;  I don't need these as I am learning leader (SPACE) key
;;  commands that are much better eh?
;;  Also, I have some of my own Ctrl Key's of my own that
;;  must not be overwritten!

(setq xah-fly-use-control-key nil)
(setq xah-fly-use-meta-key nil)

;;;
;;  Enable Xah Fly Keys:

(require 'xah-fly-keys)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Set Keyboard Layout for xah-fly-keys:
;;  possible layout values:
;;     "azerty"
;;     "azerty-be"
;;     "colemak"
;;     "colemak-mod-dh"
;;     "dvorak"
;;     "programer-dvorak"
;;     "qwerty"
;;     "qwerty-abnt"
;;     "qwertz"
;;     "workman"

(xah-fly-keys-set-layout "qwerty") ; My keyboard layout...

;;;
;;  Call xah-fly-keys Function:

(xah-fly-keys 1)

;; Enable Xah eLisp Mode in eLisp files:

(add-to-list 'auto-mode-alist '("\\.el\\'" . xah-elisp-mode))

;; Load Xah Find functions:

(autoload 'xah-find-text "xah-find" "find replace" t)
(autoload 'xah-find-text-regex "xah-find" "find replace" t)
(autoload 'xah-find-replace-text "xah-find" "find replace" t)
(autoload 'xah-find-replace-text-regex "xah-find" "find replace" t)
(autoload 'xah-find-count "xah-find" "find replace" t)


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

  ;; Set key to run a ztree-diff session:
  (define-key xah-fly-command-map (kbd "z") 'ztree-diff)

  ;; Set key to run HLedger Mode command:
  (define-key xah-fly-command-map (kbd "=") 'hledger-run-command)

  ;; Save the current location as bookmark:  SAVED (overwrites last setting)
  (define-key xah-fly-command-map (kbd "-") '(bookmark-set "SAVED"))

  ;; Set key to toggle case (three choices)...
  (define-key xah-fly-command-map (kbd "\\") 'me_toggle-letter-case)

  ;;;
  ;; Insert mode keybindings:
  ;; Add more key definitions to this list when needed:

  ;; Jump back to previously SAVED bookmark: (see: last command map key above)
  (define-key xah-fly-command-map (kbd "-") '(bookmark-jump "SAVED"))

  ;; Set KFKeys Leader Sequence to expand and shrink olivetti...
  (define-key xah-fly-leader-key-map (kbd "]") 'olivetti-expand)
  (define-key xah-fly-leader-key-map (kbd "[") 'olivetti-shrink)

  ;; This leader-key delete-frame key mirrors direct make-frame key above...
  (define-key xah-fly-leader-key-map (kbd "2") 'delete-frame)

  ;; Added VMD mode leader key sequence: SPC "v" ("k" Dvorak)
  ;; since I already have that paste key in normal Command mode...
  (define-key xah-fly-leader-key-map (kbd "v") 'vmd-mode))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/12-Xah-Fly-Keys.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
