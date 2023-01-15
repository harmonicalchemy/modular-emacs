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

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Change Mode Indicator In Mode Line:

(setq xah-fly-command-mode-indicator "🔺")
(setq xah-fly-insert-mode-indicator "✍" )

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Load xah-fly-keys - now a cloned repo in my-modules

(add-to-list 'load-path "~/.emacs.d/lisp/my-modules/xah-fly-keys")

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

(set-background-color "#280028")

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Set different Modeline Background Colors:
;; For a nice visual reference of ALL available Emacs Colors
;; See: http://www.raebear.net/computers/emacs-colors/
;; (Jessica's Corner of Cyberspace)

(defun me_modeline-color-on ()
  (set-face-background 'mode-line "orange")
  (set-face-foreground 'mode-line "#280028"))

(defun me_modeline-color-off ()
  (set-face-background 'mode-line "LightPink")
  (set-face-foreground 'mode-line "#280028"))

(add-hook 'xah-fly-command-mode-activate-hook 'me_modeline-color-on)
(add-hook 'xah-fly-insert-mode-activate-hook  'me_modeline-color-off)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Use Current Line Highlight in Command Mode ONLY: 

(defun me_xfk-command-mode-tweaks ()
  (global-hl-line-mode 1)
  (set-background-color "Grey5"))

(defun me_xfk-insert-mode-tweaks ()
  (global-hl-line-mode 0)
  (set-background-color "Grey15"))

(add-hook 'xah-fly-command-mode-activate-hook 'me_xfk-command-mode-tweaks)
(add-hook 'xah-fly-insert-mode-activate-hook  'me_xfk-insert-mode-tweaks)

;; Remove Above XFKey Mode Tweeks If you Don't Like them...
;; (remove-hook 'xah-fly-command-mode-activate-hook 'me_xfk-command-mode-tweaks)
;; (remove-hook 'xah-fly-insert-mode-activate-hook  'me_xfk-insert-mode-tweaks)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;                     CUSTOMIZED Xah Fly KEY BINDINGS
;;
;;  (Personalized keybindings to Xah fly Keys:
;;       DIRECT Command Mode bindings,
;;       PRIMARY Leader-Key bindings,
;;       -and-
;;       SECONDARY Leader-Key keybindings...
;;
;;  Options not used by Xah fly keys:  ~  `  1  2  0  \  -  =
;;  Options not used by Customizations:  ~
;;  (I have commandeered some of these already if not all of them!)
;;  In addition I have also remapped the "p" command mode key.
;;  I never used it to insert a space so no loss for me.
;;  Typing p is harder to reach than going into insert mode "f"
;;  and hitting the space bar.  IMHO %^) But of course you will have
;;  to type Caps_Lock (HOME) once more if you needed to stay in Command Mode...
;;  This does not bother me... I need that p for repositioning the cursor
;;  which is much more important to me...
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(with-eval-after-load 'xah-fly-keys

  ;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; Xah-Fly-Keys COMMAND MODE BINDINGS:
  ;; (Xah-Fly-Keys Command Map) (NO Leader key)
  ;; Add more key definitions to this list when needed:
  ;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

  ;; Set global key to toggle imenu-list: (SINGLE QUOTE CHAR)
  ;; (displays list window on right side)

  (define-key xah-fly-command-map (kbd "'") 'imenu-list)
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

  
  ;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; Xah-Fly-Keys PRIMARY LEADER-KEY BINDINGS:
  ;; (xah-fly-leader-key-map) SPACEBAR
  ;; Add more key definitions to this list when needed:
  ;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

  
  ;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; Xah-Fly-Keys SECONDARY LEADER-KEY BINDINGS:
  ;; (xah-fly-Lp2p0-key-map) QWERTY "d"
  ;; Add more key definitions to this list when needed:
  ;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ;; Set Easy Keybinding to Insert Org README Drawer:
  ;; Overrides default binding: xah-insert <> bracket pair ()

  (define-key xah-fly-Lp2p0-key-map (kbd "r") 'me_org-insert-readme-drawer)

  ;; ME INSERT DATE:
  ;; Overrides default xah-insert-date () 
  ;; A simpler version that simply formats the date as I like to do it:
  ;; (i.e., "yyyy-mmm-dd" digits)

  (define-key xah-fly-Lp2p0-key-map (kbd "f") 'me_insert-date)

  
  ;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; Xah-Fly-Keys SECONDARY LEADER-KEY BINDINGS:
  ;; (xah-fly-Lp2p1-key-map) QWERTY "e"
  ;; Add more key definitions to this list when needed:
  ;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ;; OPEN FILE OR DIR LINK IN NEW FRAME:
  ;; (xah-fly-Lp2p1-key-map) QWERTY "o"

  (define-key xah-fly-Lp2p1-key-map (kbd "o") 'me_org-open-other-frame)

  ;; Org-Mode Export/Publish to PDF File:
  ;; (xah-fly-Lp2p1-key-map) QWERTY "p"

  (define-key xah-fly-Lp2p1-key-map (kbd "p") 'org-latex-export-to-pdf)
  
  ;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; Xah-Fly-Keys SECONDARY LEADER-KEY BINDINGS:
  ;; (xah-fly-Rp2p1-key-map) QWERTY "i"
  ;; Add more key definitions to this list when needed:
  ;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ;; TOGGLE FRAME FULL SCREEN:
  ;; (xah-fly-Rp2p1-key-map) QWERTY "2"
  ;; This binding pairs well with other frame related key commands
  ;; key commands on numbers: i.e., 1, 2, 3, 4, 5, & 6

  (define-key xah-fly-Rp2p1-key-map (kbd "2") 'toggle-frame-fullscreen)

) ;; <--WARNING! DO NOT REMOVE THIS END PARENTHESIS!!! It terminates
  ;; the list above... I put it here on a line by itself so I won't
  ;; accidentally break this config if I have to add a new binding to the
  ;; end of the list (just above) and forget to relocate the terminating
  ;; parenthesis... (Murphy's Law... it's a long long list that is easy
  ;; to mess up).  Normal lisp style guides recommend putting end parenthesis
  ;; immediately at the end of a line that may also contain a stack of other
  ;; end parenthesis terminating sub-forms within the form...
  ;; LISP = Lost Inside Spaghetti Parenthesis %^)

  ;; END: CUSTOMIZED Xah Fly KEY BINDINGS
  ;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/12-Xah-Fly-Keys.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
