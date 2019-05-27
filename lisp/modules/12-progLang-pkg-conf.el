;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/12-progLang-pkg-conf.el
;;
;; This module adds extra Comp Science Programming Language features
;; to turn Emacs into your go-to mad-scientist esoteric programming lab!
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

;; Create repositories cache for devOps extras, if required:

(when (not package-archive-contents)
  (package-refresh-contents))

;; Declare a list of required packages for esoteric computer-science programming:

(defvar modular-emacs--req-proglang-packages
  '(slime
;   slime-autoloads
    helm-slime
    ac-slime))

;; Install required packages:

(mapc (lambda (p)
        (package-install p))
      modular-emacs--req-proglang-packages)

;; Enable slime-mode within lisp-mode:

(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

;; Define Default Lisp Environment:

(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/bin/sbcl")

;; Set your lisp system and some contribs...
;; Start with these slime-contribs loaded first:

(setq slime-contribs '(slime-scratch slime-editing-commands helm-slime slime-repl))

;; NOTE: slime-fancy mode loads the REPL and almost all of the popular contribs...
;; If you like what you see so far, go ahead and load everything with this instead:
;(setq slime-contribs '(slime-fancy)) ; loads almost everything!

;; Tell auto-complete to use ac-slime specific completions when sime-mode is active:

(add-hook 'slime-mode-hook 'set-up-slime-ac)
 (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
 (eval-after-load "auto-complete"
   '(add-to-list 'ac-modes 'slime-repl-mode))

;; Append New Programming languages to smart-tabs-insinuate list:
;; NOTE:  I have a bug here... Trying to include "lisp" into the list if it is
;;        not already in there... (my lisp juggling is incorrect! - fix it!) 
;(let* ((newlangs '("lisp"))
;       (omit (delete-dups (append newlangs smart-tabs-insinuate))))
;  (setq smart-tabs-insinuate omit))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: 04-devOps-pkg-conf.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
