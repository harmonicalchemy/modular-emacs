;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/10-1-SLIME-sbcl-pkg-conf.el
;;
;; This module adds SLIME sbcl support to Modular Emacs...  The older
;; SLIME mode is still useful for older Common Lisp projects (which abound)
;;
;; Resources:
;;  - Emacs Wiki Slime Mode:
;;      https://www.emacswiki.org/emacs/SlimeMode
;;  - SLIME User Manual (v2.22)
;;      https://common-lisp.net/project/slime/doc/html/
;;  - CL Wiki/SLIME Features:
;;      https://www.cliki.net/SLIME%20Features
;;  - CL Wiki/SLIME Tips:
;;      https://www.cliki.net/SLIME%20Tips
;;  - CL Cookbook: (common lisp)
;;      https://lispcookbook.github.io/cl-cookbook/
;;  - An Introduction to Programming in Emacs Lisp:
;;      http://www.gnu.org/manual/emacs-lisp-intro/emacs-lisp-intro.html
;;  - Writing GNU Emacs Extensions:
;;      http://www.oreilly.com/catalog/gnuext/
;;  - Emacs Lisp Cheat Sheet:
;;      http://wikemacs.org/wiki/Emacs_Lisp_Cheat_Sheet

;;
;; Change Log: (descending chronological order)
;;

;; 2022-009-18 - Alisha Awen, HarmonicAlchemy@proton.me
;;   Not using Slime or this module anymore...
;;   ALL OF SBCL for Emacs needs a re-factoring now.
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Enable slime-mode within lisp-mode:

(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

;; Define Default Lisp Environment:

(load (expand-file-name "~/quicklisp/slime-helper.el"))

;;;;
;; BEGIN: Platform Specific Rules:

(when ME--DARWIN
  (setq inferior-lisp-program "/opt/local/bin/sbcl"))

(when ME--LINUX
  (setq inferior-lisp-program "/usr/bin/sbcl"))

;; END: Platform Specific Rules...

;; Load Slime Autoloads package:

(require 'slime-autoloads)

;;;;
;; Set up Emacs Lisp IDE with some contribs:
;; (See: Section 8.1 Loading Contrib Packages - Slime.PDF)
;; NOTE: We used ~/quicklisp/slime-helper.el so load path and autoloads
;; are all set... (don't duplicate it here as shown in Slime.PDF examples)
;;;;

;; First: Start with only these slime-contribs loaded:

(setq slime-contribs
      '(slime-fancy
        slime-asdf
        slime-quicklisp
        slime-tramp
        helm-slime
        slime-repl))

(slime-require :swank-listener-hooks)

;; Tell auto-complete to use ac-slime specific completions when sime-mode is active:
;; NOTE:  I am no longer using Auto Complete mode or any Completion mode..  Using
;;        Emacs Default Abbriv instead...

;(add-hook 'slime-mode-hook 'set-up-slime-ac)
;(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

;; Bind ac-modes and slime-repl-mode to Slime:

;(eval-after-load "auto-complete"
;  '(add-to-list 'ac-modes 'slime-repl-mode))

;;;;
;; Modify the way lisp-mode buffers behave:

(defun lisp-hook-fn ()
  (interactive)
  ;; Start slime mode:
  (slime-mode)
  ;; Set TAB key-binding the Slime Complete Symbol:
  (local-set-key [tab] 'slime-complete-symbol)
  ;; Set Meta "q" to Re Indent Lisp Block: (used to be TAB)
  (local-set-key (kbd "M-q") 'slime-reindent-defun)
  (set (make-local-variable lisp-indent-function) 'common-lisp-indent-function)
  ;; Tell slime to NOT load failed compiled code:
  (setq slime-load-failed-fasl 'never))
 
 ;; Run above lisp-hook function on startup:
 (add-hook 'lisp-mode-hook 'lisp-hook-fn)


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Function slime-space() is called from SPC key when in slime-mode!
;; This cripples my Xah Fly Keys Global SPC Leader key which needs to
;; be established while in COMMAND mode! I use Xah-Fly-Keys (both modes)
;; pretty much all the time.  The xah SPC leader key cannot get shadowed
;; by other modes when I am in xah-fly-keys-command-mode.
;;
;; You care about this if:  You use the extra Common Lisp Prog Lang modules,
;; AND you also wish to enable Xah-Fly-Keys...
;;
;; If Both Cases above are True, Enable ALL forms below:
;; Otherwise, Leave everything below commented out as it is...
;;

;;; Set slime SPC key to xah-fly-leader-key when activating xah fly command mode:

; (defun override-slime-space-key-binding ()
;   (define-key slime-mode-indirect-map (kbd "SPC") 'xah-fly-insert-map))

; (add-hook 'xah-fly-command-mode-activate-hook 'override-slime-space-key-binding)

;;; Set Slime SPC key back to slime-space() when activating xah fly insert mode:

; (defun restore-slime-space-key-binding ()
;   (define-key slime-mode-indirect-map (kbd "SPC") 'slime-space))

; (add-hook 'xah-fly-insert-mode-activate-hook 'restore-slime-space-key-binding)


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; EXPERIMENTAL STUFF:
;;   The code below is mostly for performance tuning and smart tabs for languages...
;;   So far nothing is working for me here... I need to consult Stack Overflow etc...
;;   Maybe I don't even need the smart tabs stuff below...
;;   Seems like tabs are working ;)
;;
;;;;
;; Load Swank Faster by using custom core file with socket support and POSIX
;; bindings included: (advise from slime.pdf doc)
;;
;; One Time Setup from shell:
;;   $> sbcl
;;      * (mapc 'require '(sb-bsd-sockets sb-posix sb-introspect sb-cltl2 asdf))
;;      * (save-lisp-and-die "sbcl.core-for-slime")
;;
;; Corresponding Emacs Lisp code:
;(setq slime-lisp-implementations
;'((sbcl ("sbcl" "--core" "sbcl.core-for-slime"))))


;; Append New Programming languages to smart-tabs-insinuate list:
;; NOTE:  I have a bug here... Trying to include "lisp" into the list if it is
;;        not already in there... (my lisp juggling is incorrect! - fix it!) 
;(let* ((newlangs '("lisp"))
;       (omit (delete-dups (append newlangs smart-tabs-insinuate))))
;  (setq smart-tabs-insinuate omit))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/10-1-SLIME-sbcl-pkg-conf.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
