;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [machine-name]:~/.emacs.d/lisp/modules/02-packages.el
;;
;; This file has been modified from my original Super-Emacs fork.
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Create repositories cache, if required
(when (not package-archive-contents)
  (package-refresh-contents))

;; Declare default super-emacs list of required packages:
(defvar super-emacs--required-packages
  '(
    helm
    sublimity
    multiple-cursors
    ace-jump-mode
    ace-window
    powerline
    buffer-move
    auto-complete
    which-key
    meta-presenter
    exec-path-from-shell
    )
  )

;; Install required packages
(mapc (lambda (p)
        (package-install p))
      super-emacs--required-packages)

;; Configure and enable sublimity-mode
(require 'sublimity-scroll)
(sublimity-mode)

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

;; Enable powerline
(powerline-center-theme)
(setq powerline-default-separator 'slant)

;; Set up helm-mode
(helm-mode 1)
(helm-autoresize-mode 1)
(setq helm-split-window-in-side-p t)
