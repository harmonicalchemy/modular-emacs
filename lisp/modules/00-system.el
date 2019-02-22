;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/00-system.el
;;
;; Note:  I changed everything in this file because it was causing issues with
;;        newer versions of emacs!  Everything below has been re-written based
;;        on Emacs Manual: 49.1.4 Saving Customizations...
;;        Also, all my work environments now have the latest GnuPG installed.
;;        no need to play games with defining gpg to gpg2 anymore...
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

;; Configure custom elisp library load path.
(add-to-list 'load-path "~/.emacs.d/lisp/")

(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Load path for your custom Emacs themes:
(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/themes/")

