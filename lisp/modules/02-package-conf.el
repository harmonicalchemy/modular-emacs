;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/02-package-conf.el
;;
;; This module provides the basic modules that most likely will be needed
;; by most all use-cases...  Modular Emacs default packages so-to-speak...
;;
;; Change Log:
;;
;; 2020-005-16 - Alisha Awen, siren1@disroot.org
;;   disabled poserline mode-line stuff...  I got tired of it...  Too busy...
;;   I found smart-mode-line to be better for my needs... That is the new
;;   default going forward.  I left the powerline code in, (disabled) in case
;;   you like it and would like to switch back...
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;;
;; Create repositories cache, if required

(when (not package-archive-contents)
  (package-refresh-contents))

;;;
;; Declare default modular-emacs list of required packages:

(defvar me--required-packages
  '(exec-path-from-shell
    gnu-elpa-keyring-update
    helm
;    powerline
;    smart-mode-line
    auto-complete
    which-key
    xah-fly-keys
    xah-elisp-mode
    xah-find
    flyspell-correct-helm))

;;;
;; Install required packages

(mapc (lambda (p) (package-install p))
      me--required-packages)


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Load Environment Vars from shell:
;; If we are using unix in a POSIX compliant shell...
;; (e.g., OS X, Linux, BSD, with POSIX: Bash, or Zsh etc.)
;; Reference: GitHub:Purcell/exec-path-from-shell
;; Install: from MELPA exec-path-from-shell

(when *is-posix* (exec-path-from-shell-initialize))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Load default auto-complete configs

(ac-config-default)


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Start which-key-mode

(which-key-mode)


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Default Mode Line Tweaks:
;; Here are some nice tweaks that work fine with
;; Default mode lines as well as fancy mode-line
;; packages... currently I am just using the built
;; in mode-like package that comes with Emacs...

(setq size-indication-mode nil
      column-number-mode t
      line-number-mode t)

;;
;; Trim some minor mode’s display to a single unicode icon:

(dolist (mim '((auto-revert-mode   . "♺")
              (auto-fill-function . "⤶")
              (visual-line-mode   . "⤵")
              (isearch-mode       . "⁇")
              (paredit-mode       . "⁐")
              (xah-fly-keys       . "∑fk")
              (smartparens-mode   . "⦅⦆")))

  (let ((mode (car mim))
        (repl (list (concat " " (cdr mim)))))

    (when (assq (car mim) minor-mode-alist)
      (setf (cdr (assq (car mim) minor-mode-alist)) repl))))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Configure & Enable Smart-mode-line:
;; DISABLED - I no longer wish to mess with fancy
;;            mode lines... The default works fine
;;            for me. ;-) Your mileage may vary...

;;  Choose SML Theme:
;;  NOTE:   THIS IS DISABLED
;;    (I am no longer using mode-line packages)
;;    (pick one and enable it if you like but also
;;    load the theme at top first!)

;(setq sml/theme 'dark)
;(setq sml/theme 'light)
;(setq sml/theme 'respectful)

;; Enable Smart Mode Line after Emacs Startup:
;; NOTE: THIS IS DISABLED (see above note)
;(add-hook 'after-init-hook 'sml/setup)

;;; POWERLINE MODE DISABLED
;;  If you would rather use powerline, enable the three forms below
;;  and disable the above smart-mode-line section if you enabled it
;;  previously...
;;  Also make sure to load the mode at the top in the package install
;;  section!
;;
;; Enable powerline:
;(require 'powerline)
;(powerline-center-theme)
;(setq powerline-default-separator 'slant)


;; Platform Specific SML directory abbreviations:
;; NOTE: THIS IS DISABLED - Just as above you need to
;;       un-comment this section and make sure other smart-mode-line
;;       sections above are enabled first... (also load the package
;;       at the very top in the package install section)...
;;
;;  This is a demo list... It probably works for your .emacs.d directory
;;  and standard Docs directory, but you will need to fill in the path to
;;  the last element in the list to a real directory on your system
;;  (one which you would like to make a shortcut abbreviation for...
;;  Add more just like that to the end of the list (as instructed in the
;;  comment at the end of the list)

;; Platform Specific SML directory abbreviations:

;(when *is-darwin*
;  (add-to-list 'sml/replacer-regexp-list '("^~/\\.emacs\\.d/" ":EMACS:"))
;  (add-to-list 'sml/replacer-regexp-list '("^~/Documents/" ":DOCS:"))
;  ;; Add more platform specific directory shortcut abbreviations to this list here as needed....
;  ;; When you are done adding new abbreviations, get rid of this comment and pull up the
;  ;; final parenthesis below to tidy up %^)...
;  )

;(when *is-linux*
;  (add-to-list 'sml/replacer-regexp-list '("^~/\\.emacs\\.d/" ":EMACS:"))
;  (add-to-list 'sml/replacer-regexp-list '("^~/Documents/" ":DOCS:"))
;  ;; Add more platform specific directory shortcut abbreviations to this list here as needed....
;  ;; When you are done adding new abbreviations, get rid of this comment and pull up the
;  ;; final parenthesis below to tidy up %^)...
;  )

;; Platform Independent SML directory abbreviations:

;  (add-to-list 'sml/replacer-regexp-list '("^:DOCS:/Path/To/Your/Other/Docs/" ":My-Other-Docs:"))

;; Add more platform independent directory shortcut abbreviations just like the last form above...
;; These are invoked independently as complete forms here... No cleanup or closing paren needed when
;; adding to the end of this list...


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Set up helm-mode:

(helm-mode 1)
(helm-autoresize-mode 1)
(setq helm-split-window-in-side-p t)

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/02-package-conf.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
