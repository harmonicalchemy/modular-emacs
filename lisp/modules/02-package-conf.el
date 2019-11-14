;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/02-package-conf.el
;;
;; This module provides the basic modules that most likely will be needed
;; by most all use-cases...  Modular Emacs default packages so-to-speak...
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Create repositories cache, if required
(when (not package-archive-contents)
  (package-refresh-contents))

;; Declare default modular-emacs list of required packages:
(defvar modular-emacs--required-packages
  '(exec-path-from-shell
    helm
    multiple-cursors
    ace-jump-mode
    ace-window
    imenu-list
    powerline
    buffer-move
    auto-complete
    which-key
    meta-presenter
    xah-fly-keys
    xah-elisp-mode
    xah-find
    flyspell-correct-helm))

;; Install required packages
(mapc (lambda (p) (package-install p))
      modular-emacs--required-packages)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Load Environment Vars from shell:
;; If we are using unix in a POSIX compliant shell...
;; (e.g., OS X, Linux, BSD, with POSIX: Bash, or Zsh etc.)
;; Reference: GitHub:Purcell/exec-path-from-shell
;; Install: from MELPA exec-path-from-shell

(when *is-posix* (exec-path-from-shell-initialize))

;; Load default auto-complete configs

(ac-config-default)

;; Start which-key-mode

(which-key-mode)

;; Set up ace-jump-mode

(autoload 'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

(autoload 'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-"
  t)

;; Enable iMenu List minor mode globally at startup:
;(imenu-list-minor-mode)

;; Enable powerline:

(powerline-center-theme)
(setq powerline-default-separator 'slant)

;; Set up helm-mode:

(helm-mode 1)
(helm-autoresize-mode 1)
(setq helm-split-window-in-side-p t)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/02-package-conf.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
