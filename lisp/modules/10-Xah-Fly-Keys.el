;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/10-Xah-Fly-Keys.el
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

;; Enable Xah eLisp Mode in eLisp files:

(add-to-list 'auto-mode-alist '("\\.el\\'" . xah-elisp-mode))

;; Load Xah Find functions:

(autoload 'xah-find-text "xah-find" "find replace" t)
(autoload 'xah-find-text-regex "xah-find" "find replace" t)
(autoload 'xah-find-replace-text "xah-find" "find replace" t)
(autoload 'xah-find-replace-text-regex "xah-find" "find replace" t)
(autoload 'xah-find-count "xah-find" "find replace" t)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/10-Xah-Fly-Keys.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


