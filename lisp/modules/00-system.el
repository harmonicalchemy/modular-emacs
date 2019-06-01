;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/00-system.el
;;
;; Note:  I changed everything in this file because it was causing issues with
;;        newer versions of emacs!  Everything below has been re-written based
;;        on Emacs Manual: 49.1.4 Saving Customizations...
;;        Also, all my work environments now have the latest GnuPG installed.
;;        When GPG is invoked, it will (and should) be at least V2.2 or higher...
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Set custom-file so that Emacs does not use init.el

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Use GPG Version 2 (gpg2) instead of gpg.
;; Note: If you are using GnuPG later than V2.2 you may have to change
;; below from "gpg2" back to "gpg". (After version 2.2, the default GnuPG
;; command defaults back to "gpg" (like GnuPG was before V2).
;; However some systems have both gpg & gpg2 installed so it is best to
;; leave it below the way it is. (you could also create an alias in .bashrc
;; .profile, etc.)

(setq epg-gpg-program "gpg")

;; System Constants:

(defconst *is-darwin* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))

(when (memq window-system `(ns x))
  (defconst *is-posix* t)
  )

;; Configure custom elisp library load path.

(add-to-list 'load-path "~/.emacs.d/lisp/")

(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Load path for your custom Emacs themes:

(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/themes/")

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Load Environment Vars from shell:
;; If we are using unix in a POSIX compliant shell...
;; (e.g., OS X, Linux, BSD, with POSIX: Bash, or Zsh etc.)
;; Reference: GitHub:Purcell/exec-path-from-shell
;; Install: from MELPA exec-path-from-shell

(when *is-posix*
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("NVM_BIN"))
  )

