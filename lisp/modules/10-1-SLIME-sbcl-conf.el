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
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Enable slime-mode within lisp-mode:

(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

;; Define Default Lisp Environment:

(load (expand-file-name "~/quicklisp/slime-helper.el"))

(when *is-darwin*
  (setq inferior-lisp-program "/usr/local/bin/sbcl"))

(when *is-linux*
  (setq inferior-lisp-program "/usr/bin/sbcl"))

;;;;
;; Set up Emacs Lisp IDE with some contribs:
;; (See: Section 8.1 Loading Contrib Packages - Slime.PDF)
;; NOTE: We used ~/quicklisp/slime-helper.el so load path and autoloads
;; are all set... (don't duplicate it here as shown in Slime.PDF examples)
;;;;

;; First: Start with only these slime-contribs loaded:

(setq slime-contribs
      '(slime-fancy
        slime-quicklisp
        helm-slime
        slime-repl
;        slime-contribs
        slime-asdf))

;; Tell auto-complete to use ac-slime specific completions when sime-mode is active:

(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

;; Bind ac-modes and slime-repl-mode:

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

;; Append New Programming languages to smart-tabs-insinuate list:
;; NOTE:  I have a bug here... Trying to include "lisp" into the list if it is
;;        not already in there... (my lisp juggling is incorrect! - fix it!) 
;(let* ((newlangs '("lisp"))
;       (omit (delete-dups (append newlangs smart-tabs-insinuate))))
;  (setq smart-tabs-insinuate omit))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/10-1-SLIME-sbcl-pkg-conf.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
