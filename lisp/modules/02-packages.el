;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/02-packages.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Create repositories cache, if required
(when (not package-archive-contents)
  (package-refresh-contents))

;; Declare default modular-emacs list of required packages:
(defvar modular-emacs--required-packages
  '(
    helm
    sublimity
    multiple-cursors
    ace-jump-mode
    ace-window
    imenu-list
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
      modular-emacs--required-packages)

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

;; Enable iMenu List minor mode globally at startup:
;(imenu-list-minor-mode)

;; Enable powerline
(powerline-center-theme)
(setq powerline-default-separator 'slant)

;; Set up helm-mode
(helm-mode 1)
(helm-autoresize-mode 1)
(setq helm-split-window-in-side-p t)
