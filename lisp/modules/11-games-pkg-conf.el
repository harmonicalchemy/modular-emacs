;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/11-games-pkg-conf.el
;;
;; About:     This module adds some Emacs gaming features. For a start it 
;;            integrates a new fork of the 'rmoo' MOO client for Emacs.
;;            No other games related things have been added here yet...
;;
;; Useful Reference:
;;    RMOO:   Github.com/lisdude/rmoo.git   V1.2 (Nov 13, 2018)
;;                                          Emacs MOO Client...
;;    MOO:    https://www.lisdude.com/moo/  A curated MOO Archive of everything!
;;                                          This is great news for die-hard Text
;;                                          based Virtual Reality Authors!
;;                                          Thanks! lisdude!
;;
;;    Stunt LambdaMOO:   GitHub.com/toddsundsted/stunt (stunt.io - main site)
;;                       This is an updated LambdaMOO with multiple inheritance,
;;                       anonymous objects, HTTP, JSON <-> MOO translations
;;                       better crypto, a map datatype and a RESTful interface.
;;                           (A great place to start a MOO server in 2019)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Create repositories cache for games, if required:
(when (not package-archive-contents)
  (package-refresh-contents))

;; Declare a list of required packages for extra dired features:
(defvar modular-emacs--req-games-packages
  '(xterm-color))

;; Install required packages:
(mapc (lambda (p)
        (package-install p))
      modular-emacs--req-games-packages)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; RMOO - A MOO client for Emacs:
;; Reference: Github.com/lisdude/rmoo.git
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(add-to-list 'load-path "~/.emacs.d/lisp/my-modules/rmoo")
(require 'rmoo-autoload)
(require 'moocode-mode)
(global-set-key (kbd "C-c C-r") 'rmoo)
(add-to-list 'auto-mode-alist '("\\.moo$" . moocode-mode))
(add-hook 'rmoo-interactive-mode-hooks (lambda ()
(linum-mode -1)                  ;; ... no line numbers
(olivetti-mode)
(olivetti-set-width 102)
(goto-address-mode t)))          ;; ... clickable links

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END 11-games-pkg-conf.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
