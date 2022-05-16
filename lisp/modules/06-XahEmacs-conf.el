;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/06-XahEmacs-conf.el
;;
;; This module adds support for XahEmacs premium package (if you have purchased
;; that from: http://xahlee.info/emacs/emacs/xah_emacs_modes.html
;; These Pro features expand on configurations made in: 04-devOps-pkg-conf.el
;; and possibly other Harmonic Alchemy "Vanilla" package configurations...

;; STATUS:
;;   Currently the only modules enabled here are xah-elisp-mode and xah-find,
;;   but more will be added as I evaluate them and find a good use-case for
;;   them within the context of Harmonic Alchemy Modular Emacs...
;;   Therefore: Don't add any of the other XahEmacs packages (as explained in
;;   CONFIGURATION section below)
;;
;;   If you are already using some of these modules in your Emacs...
;;   and would like to configure them to work with Harmonic Alchemy Modular Emacs
;;   you will have to do that integration on your own. It is far beyond the scope
;;   here to help you integrate your already integrated init.el of course)

;; CONFIGURATION:  
;;   If you have purchased XahEmacs you will need to create a sub-directory
;;   within my-modules sub-directory of your Harmonic Alchemy modular Emacs
;;   repository to enable them... (~/.emacs.d/lisp/my-modules is NOT tracked by
;;   git so you can put things in there without upsetting the repo)
;;
;;   Run the following command within a terminal: (to create XahEmacs Home Dir)
;;
;;       mkdir ~/.emacs.d/lisp/my-modules/XahEmacs
;;
;;   Next; Create sub-directories within your XahEmacs home directory with the
;;   same name as the .el files found within the "packages" directory of your
;;   extracted XahEmacs package. (i.e., from the downloaded zip file)
;;
;;   Example: (run command below for every XahEmacs package you wish to use)
;;            (i.e., run command below.. Then change xah-elisp-mode to xah-find
;;            and run the command again.  Keep changing the package name for
;;            each time you run the command until sub-directories for all
;;            packages are all created...)
;;
;;       mkdir ~/.emacs.d/lisp/my-modules/XahEmacs/xah-elisp-mode   ## etc...
;;
;;   Then copy the individual xah-[package-name].el files into the sub-directories
;;   named after them (which you just created)
;;
;;   Example:  (do this from within the XahEmacs packages sub-directory)
;;
;;       cp xah-elisp-mode.el ~/.emacs.d/lisp/my-modules/XahEmacs/xah-elisp-mode
;;
;;       Run the command above for every XahEmacs package you wish to enable just
;;       as before changing the package name at the end of the command above
;;       until all "xah-package-name.el" files are in their own sub-directories.
;;
;;   Then: Go edit dispatcher.el and remove the comment characters from the form
;;   that invokes this file... That's it...

;;
;; Change Log: (descending chronological order)
;;

;; 2022-005-15 - Alisha Awen, siren1@disroot.org
;;   Created this file: 06-XahEmacs-conf.el which is disabled by default within
;;   dispatcher.el...  
;;
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Load xah-elisp-mode (premium package)

(add-to-list 'load-path "~/.emacs.d/lisp/my-modules/XahEmacs/xah-elisp-mode")

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Enable Xah eLisp Mode in eLisp files:

(require 'xah-elisp-mode)

(add-to-list 'auto-mode-alist '("\\.el\\'" . xah-elisp-mode))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Load xah-find (premium package)

(add-to-list 'load-path "~/.emacs.d/lisp/my-modules/XahEmacs/xah-find")

(require 'xah-find)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Load Xah Find functions:

(autoload 'xah-find-text "xah-find" "find replace" t)
(autoload 'xah-find-text-regex "xah-find" "find replace" t)
(autoload 'xah-find-replace-text "xah-find" "find replace" t)
(autoload 'xah-find-replace-text-regex "xah-find" "find replace" t)
(autoload 'xah-find-count "xah-find" "find replace" t)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  More XahEmacs premium packages may be added here later as I try them out...
;;  Stay tuned!  (there is a LOT of stuff to look at and try out first)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/06-XahEmacs-conf.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
