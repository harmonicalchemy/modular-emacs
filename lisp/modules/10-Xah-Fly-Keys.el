;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/13-Xah-Emacs-pkg.conf.el
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

;; Disable Xah's Control Key Functions:
;; I don't need these as I am learning leader (SPACE) key
;; commands that are much better eh?
;; Also, I have some of my own Ctrl Key's of my own that
;; must not be overwritten!

(setq xah-fly-use-control-key nil)

;; Enable Xah Fly Keys:

(require 'xah-fly-keys)
(xah-fly-keys-set-layout "qwerty") ; My keyboard layout...
(xah-fly-keys 1)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: 13-Xah-Emacs-pkg.conf.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

