;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/12-Xah-Fly-Keys.el
;;
;; This module turns Emacs into a Modal Editor (like VI) but this is NOT a
;; VI emulation..  The keys are arranged to improve efficiency and limit finger
;; fatigue and the famous EPF syndrome.
;; Ref:  http://ergoemacs.org/emacs/elisp_insert_brackets_by_pair.html
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Disable Xah's Control & Meta Key Functions:
;;  I don't need these as I am learning leader (SPACE) key
;;  commands that are much better eh?
;;  Also, I have some of my own Ctrl Key's of my own that
;;  must not be overwritten!

(setq xah-fly-use-control-key nil)
(setq xah-fly-use-meta-key nil)

;;;
;; Load xah-fly-keys - now a cloned repo in my-modules

(add-to-list 'load-path "~/.emacs.d/lisp/my-modules/xah-fly-keys")

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

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Call xah-fly-keys Function:

(xah-fly-keys 1)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Xah Fly Keys Command and Insert Mode Hook Functions:

(defun me_xfk-command-color () (set-background-color "#280028"))
(defun me_xfk-insert-color () (set-background-color "#180028"))

;(custom-theme-set-faces
; 'user
; `(text-cursor ((t (:background "red" :foreground "white")))))

(add-hook 'xah-fly-command-mode-activate-hook 'me_xfk-command-color)
(add-hook 'xah-fly-insert-mode-activate-hook  'me_xfk-insert-color)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;                      Xah Fly KEY BINDINGS
;;
;;  Add Personalized keybindings to Xah fly Keys:
;;  For:
;;       DIRECT Command Mode keybindings,
;;       PRIMARY Leader-Key keybindings,
;;       -and-
;;       SECONDARY Leader-Key keybindings...
;;
;;  Options not used by Xah fly keys:  ~  `  1  2  0  \  -  =
;;  (I have commandeered some of these already if not all of them!)
;;  In addition I have also remapped the "p" command mode key.
;;  I never used it to insert a space so no loss for me.
;;  Typing p is harder to reach than going into insert mode "f"
;;  and hitting the space bar.  IMHO %^) But of course you will have
;;  to type Caps_Lock (HOME) once more if you needed to stay in Command Mode...
;;  This does not bother me... I need that p for repositioning the cursor
;;  which is much more important to me...

(with-eval-after-load 'xah-fly-keys

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; Xah-Fly-Keys "Command Mode" keybindings:
  ;; (Xah-Fly-Keys Command Map) (NO Leader key)
  ;; Add more key definitions to this list when needed:

  ;; Set easy key to switch frames:
  ;; This enhances functionality of Xah-Fly-Keys
  ;; default xah-next-window-or-frame: "," key.

  (define-key xah-fly-command-map (kbd "b") 'other-frame)

  ;; Make Frame:
  ;; This DIRECT Command Mode make-frame key mirrors
  ;; PRIMARY Leader-Key keybinding delete-frame "2" key below...
  ;; (which does the opposite)

  (define-key xah-fly-command-map (kbd "2") 'me_make-frame)

  ;; Switch to Modular Emacs Org Mode Wide Screen View:
  ;; This DIRECT Command Mode "5" key mirrors
  ;; PRIMARY Leader-Key "5" key below...
  ;; (which Switches to Org Mode back to Normal Screen View)

  (define-key xah-fly-command-map (kbd "5") 'me_org-wide-screen)

  ;; Switch to Modular Emacs Coding WIDE Screen View:
  ;; This DIRECT Command Mode "6" key mirrors
  ;; PRIMARY Leader-Key "6" key below...
  ;; (which modifies coding frame back to Normal Screen View)

  (define-key xah-fly-command-map (kbd "6") 'me_set-coding-wide-frame)

  ;; Change default olivetti-mode key:
  ;; (back tick)
  ;; (because I have conflicting other-frame key)

  (define-key xah-fly-command-map (kbd "`") 'me_toggle-olivetti-mode)

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
  ;; Note: This is Sidebar TREE not Sidebar (i.e., on right side)
  ;;       PRIMARY Leader-Key keybinding "1" (below) toggles
  ;;       sidebar (not the same as tree).

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
  ;; This DIRECT Command Mode bookmark-set key mirrors
  ;; PRIMARY Leader-Key keybinding bookmark-jump "-" key below...
  ;; (which jumps to this saved bookmark)

  (define-key xah-fly-command-map (kbd "-") 'bookmark-set)

  ;; TOGGLE Case of Character, Word, or Selection:
  ;; (There are three possibilities. Typing "\" key multiple
  ;; times cycles through them)... This is important as I have
  ;; disabled the Caps-Lock key (which is mapped to HOME now)

  (define-key xah-fly-command-map (kbd "\\") 'me_toggle-letter-case)

  ;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; Xah-Fly-Keys PRIMARY Leader-Key keybindings:
  ;; (xah-fly-leader-key-map) (the SPACE key)
  ;; Add more key definitions to this list when needed:

  ;; Jump back to previously SAVED bookmark:
  ;; This PRIMARY Leader-Key bookmark-jump "-" mirrors
  ;; DIRECT Command Mode bookmark-set "-" key above...
  ;; (which sets the bookmark)

  (define-key xah-fly-leader-key-map (kbd "-") 'bookmark-jump)

  ;; Set KFKeys Primary Leader-Key Sequence to expand and shrink olivetti...

  (define-key xah-fly-leader-key-map (kbd "]") 'olivetti-expand)
  (define-key xah-fly-leader-key-map (kbd "[") 'olivetti-shrink)

  ;; TOGGLE Org Sidebar:
  ;; (opens or closes special SIDEBAR window on Right Side)
  ;; You can configure the SIDEBAR to display agenda items etc.
  ;; Note: This is NOT Sidebar TREE (i.e., on left side)
  ;;       DIRECT Command Mode "1" (above toggles sidebar TREE)
  ;;       SIDEBAR-TREE is not the same as "SIDEBAR"...

  (define-key xah-fly-leader-key-map (kbd "1") 'org-sidebar-toggle)

  ;; Delete Frame:
  ;; This PRIMARY Leader-Key delete-frame "2" mirrors
  ;; DIRECT Command Mode make-frame "2" key above...
  ;; (which does the opposite)

  (define-key xah-fly-leader-key-map (kbd "2") 'delete-frame)

  ;; Switch to Modular Emacs Normal Org Mode View:
  ;; This PRIMARY Leader-Key "5" key mirrors
  ;; DIRECT Command Mode "5" key above...
  ;; (which Switches to Org Mode Wide Screen View)

  (define-key xah-fly-leader-key-map (kbd "5") 'me_org-narrow-screen)

  ;; Switch to Modular Emacs Normal Coder's Mode View:
  ;; This PRIMARY Leader-Key "6" key mirrors
  ;; DIRECT Command Mode "6" key above...
  ;; (which Switches to Coding Wide Screen View)

  (define-key xah-fly-leader-key-map (kbd "6") 'me_set-coding-default-frame)

  ;; Org Toggle Hidden Formatting Characters:
  ;; (emphasized text and hyperlink display)
  ;; This makes it easy to edit them and then toggle back
  ;; afterwards to hide the formatting and show Fontification.
  ;; I overrode (stole) this from xah-toggle-previous-letter-case
  ;; which I am willing to exchange for this more needed function.
  ;; I have my own toggle letter case function and could incorporate
  ;; toggling camel case characters to it as well if I really need that.
  ;;
  ;; NOTE: This Does NOT work if you are editing an org file within
  ;;       an INDIRECT buffer. (i.e., you are using Modular Emacs
  ;;       W I D E S C R E E N Org Mode - me_org-wide-screen and have
  ;;       used the left outline panel to create indirect buffers
  ;;       containing a subsets of your entire .org file.)  I am
  ;;       looking into figuring out a way to fix this as it kind
  ;;       of cripples things.  (I edit in small indirect buffers
  ;;       a lot!!!)

  (define-key xah-fly-leader-key-map (kbd "b") 'org-toggle-link-display)

  ;; Export Org file to Markdown File:

  (define-key xah-fly-leader-key-map (kbd "m") 'org-md-export-to-markdown)

  ;; TOGGLE Default face between coding mode and writing mode:

  (define-key xah-fly-leader-key-map (kbd "p") 'me_toggle-default-face)

  ;; VMD-Mode Primary Leader-Key Sequence: SPC "v" ("k" Dvorak)
  ;; (Since there already is a default paste "v" key in Command
  ;; mode and it's easy enough to switch to Command Mode to paste)

  (define-key xah-fly-leader-key-map (kbd "v") 'vmd-mode)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; Xah-Fly-Keys SECONDARY Leader-Key keybindings:
  ;; For secondary leader key: QWERTY "d" (dvorak "e")
  ;; (xah-fly-e-keymap Leader Key Map)
  ;; Add more key definitions to this list when needed:
  ;; Some of these default "insert" keybindings are not needed.

  ;; Override default xah-insert-date () func with my own
  ;; simpler version that simply formats the date as I like to do it:
  ;; (i.e., "yyyy-mmm-dd" digits)

  (define-key xah-fly-Lp2p0-key-map (kbd "f") 'me_insert-date))

;; remove end paren (above line) after uncomment below... otherwize error parsing!

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; Xah-Fly-Keys SECONDARY Leader-Key keybindings:
  ;; For secondary leader key: QWERTY "e" (dvorak "." dot)
  ;; (xah-fly-e-keymap Leader Key Map)
  ;; Add more key definitions to this list when needed:
  ;; None of these keys are used by xah-fly-keys. You can use
  ;; anything here...

  ;; Override default xah-insert-date () func with my own
  ;; simpler version that simply formats the date as I like to do it:
  ;; (i.e., "yyyy-mmm-dd" digits)

;  (define-key xah-fly-dot-keymap (kbd "p") 'org-latex-export-to-pdf)

  ;; Set Easy Keybinding to Insert Org README Drawer:

;  (define-key xah-fly-dot-keymap (kbd "r") 'me_org-insert-readme-drawer)

  ;; Set Easy Keybinding to Open Org File Link Other Frame:

;  (define-key xah-fly-dot-keymap (kbd "o") 'me_org-open-other-frame))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/12-Xah-Fly-Keys.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
