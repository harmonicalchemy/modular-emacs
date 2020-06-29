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
;;
;;  Options not used by Xah fly keys:  ~  `  1  2  0  \  -  =
;;
;;  In addition you could use "p" as a command mode key.  I never use
;;  it to insert a space before.  typing p is harder to reach than
;;  going into insert mode "f" and hitting the space bar.  IMHO

(with-eval-after-load 'xah-fly-keys

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; Xah-Fly-Keys "Command Mode" keybindings:
  ;; (Xah-Fly-Keys Command Map)
  ;; Add more key definitions to this list when needed:

  ;; Set easy key to switch frames:
  ;; This enhances functionality of Xah-Fly-Keys
  ;; default xah-next-window-or-frame: "," key.
  (define-key xah-fly-command-map (kbd "b") 'other-frame)

  ;; Make Frame:
  ;; This "Command Mode" make-frame key mirrors "Insert Mode"
  ;; delete-frame "2" key below... (which does the opposite)
  (define-key xah-fly-command-map (kbd "2") 'me_make-default-frame)

  ;; Set easy key to toggle neotree in left window pane:
  ;; Note: This disables (default Xfk to run command delete-char)
  ;;       I don't need that as the "D" key works fine for me...
  (define-key xah-fly-command-map (kbd "5") 'neotree-toggle)

  ;; Change default olivetti-mode key:
  ;; (back tick)
  ;; (because I have conflicting other-frame key)
  (define-key xah-fly-command-map (kbd "`") 'olivetti-mode)

  ;; Set global key to toggle imenu:
  ;; (single quote)
  ;; (pops up in Helm window)...
  ;; NOTE: this used to run: imenu-list but I am
  ;;       experiencing a problem with imenu-list right now.
  ;;       When that is fixed... This key will run that
  ;;       function instead...
  (define-key xah-fly-command-map (kbd "'") 'imenu)
  ;;  (define-key xah-fly-command-map (kbd "'") 'imenu-list)

  ;; Invoke Daft:
  (define-key xah-fly-command-map (kbd "0") 'deft)

  ;; Recenter top to bottom:
  ;; (Standard Emacs "C-l" does this as well):
  (define-key xah-fly-command-map (kbd "p") 'recenter-top-bottom)

  ;; TOGGLE Org Sidebar Tree:
  ;; (opens or closes tree window on Left Side)
  ;; Note: This is not Sidebar (i.e., on right side)
  ;;       "Insert Mode" "1" (below) toggles sidebar (not tree).
  (define-key xah-fly-command-map (kbd "1") 'org-sidebar-tree-toggle)

  ;; This key opens Elements(s) in Right window Pane But leaves the
  ;; cursor in the left outline window pane (it stays on outline
  ;; heading where it was) i.e. for navigation (Browse Mode)...
  ;; NOTE: To Jump to the expanded element in right (or middle) window,
  ;;       (for editing the expanded content) simply hit the RET key
  ;;       while sitting on any heading in the left tree window.
  (define-key xah-fly-command-map (kbd "s") 'me_org-tree-open-in-right-no-focus)

  ;; Run a ztree-diff session:
  ;; (you will be prompted for right and left directories)
  (define-key xah-fly-command-map (kbd "z") 'ztree-diff)

  ;; Set key to run HLedger Command:
  (define-key xah-fly-command-map (kbd "=") 'hledger-run-command)

  ;; Save the current location as bookmark:  SAVED
  ;; (overwrites the last saved setting)
  ;; This "Command Mode" bookmark-set mirrors "Insert Mode"
  ;; bookmark-jump "-" key below... (which jumps to the bookmark)
  (define-key xah-fly-command-map (kbd "-") 'bookmark-set)

  ;; TOGGLE Case of Character, Word, or Selection:
  ;; (There are three possibilities. Typing "\" key multiple
  ;; times cycles through them)... This is important as I have
  ;; disabled the Caps-Lock key (which is mapped to HOME now)
  (define-key xah-fly-command-map (kbd "\\") 'me_toggle-letter-case)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; Xah-Fly-Keys "Insert Mode" keybindings:
  ;; (Xah-Fly-Keys Leader Key Map)
  ;; Add more key definitions to this list when needed:

  ;; Jump back to previously SAVED bookmark:
  ;; This "Insert Mode" bookmark-jump mirrors "Command Mode"
  ;; bookmark-set "-" key above... (which sets the bookmark)
  (define-key xah-fly-leader-key-map (kbd "-") 'bookmark-jump)

  ;; Set KFKeys Insert Mode Sequence to expand and shrink olivetti...
  (define-key xah-fly-leader-key-map (kbd "]") 'olivetti-expand)
  (define-key xah-fly-leader-key-map (kbd "[") 'olivetti-shrink)

  ;; TOGGLE Org Sidebar (opens or closes window on Right Side)
  ;; Note: This is not Sidebar Tree (i.e., on left side)
  ;;       "Command Mode" "1" (above toggles sidebar tree)
  (define-key xah-fly-leader-key-map (kbd "1") 'org-sidebar-toggle)

  ;; Delete Frame:
  ;; This "Insert Mode" delete-frame mirrors "Command Mode"
  ;; make-frame "2" key above... (which does the opposite)
  (define-key xah-fly-leader-key-map (kbd "2") 'delete-frame)

  ;; Export Org file to Markdown File:
  (define-key xah-fly-leader-key-map (kbd "m") 'org-md-export-to-markdown)

  ;; TOGGLE Default face between coding mode and writing mode:
  (define-key xah-fly-leader-key-map (kbd "p") 'me_toggle-default-face)

  ;; VMD-Mode "Insert Mode" key sequence: SPC "v" ("k" Dvorak)
  ;; (Since there already is a default paste "v" key in Command
  ;; mode and it's easy enough to switch to Command Mode to paste)
  (define-key xah-fly-leader-key-map (kbd "v") 'vmd-mode))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/12-Xah-Fly-Keys.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
