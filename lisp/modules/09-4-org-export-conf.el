;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   [modular-emacs]:~/.emacs.d/lisp/modules/09-4-org-export-conf.el
;;
;;   This is a sub module of 09-org-mode-pkg-conf.el which adds extra tools for
;;   exporting org-mode with specific features to many different output
;;   publishing formats...  Consider this file to be the control panel for your
;;   Emacs Org Mode PubOps engine...
;;
;;   You now hove in your hands Publishing Abilities Way Beyond Scrivener!
;;
;;   Override this file by placing a copy of it into "my-modules" then change
;;   settings below to suit your org mode export needs...
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;;
;;  Exporters:

(require 'ox-md)
(require 'ox-latex)

;;;
;;  Hide the Emphasis Markup:
;;  (e.g., /.../ for italics, *...* for bold, etc.)
;;
;;  Disable this form if you want to see them...
;;
;;  TIP: The invisible characters are easy enough to remove
;;       as there will be a small space in its place...
;;       You can backspace over it to remove it and then
;;       you will be able to see and remove the other one as well...

(setq org-hide-emphasis-markers t)

;;;
;;  Position tags right after last char of headline:
;;  (This is mostly to fix problems with variable-pitch
;;  It does not seem to be a problem with fixed-pitch)
;;  NOTE: I disabled this thing... It was causing an
;;        error on load!  I need to look into this
;;        later and fix it, or get rid of it...
;;        I am not even sure if I need it as I am
;;        not using Emacs in global scaled mode, or
;;        what ever that's called... lol...
;;        There may be more to this than I realized! %^)
;;        I will get to it later when I start messing with org tags...
;(org-tags-column 0)

;;;
;;  speed keys for quick navigation:

(setq org-use-speed-commands 1)

;;;
;;  set maximum indentation for org-mode description lists:

(setq org-list-description-max-indent 5)

;;;
;;  prevent org-mode demoting heading also shifting text inside sections:

(setq org-adapt-indentation nil)

;;;
;;  Stop Inline Images Being Too Big:

(setq org-image-actual-width '(500))

;;;
;;  Automatically Refresh Inline Images:
;;  REF: http://emacs.stackexchange.com/questions/3302/live-refresh-of-inline-images-with-org-display-inline-images

(defun shk-fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(add-hook 'org-babel-after-execute-hook 'shk-fix-inline-images)


;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   Org Mode Export Options:

;;;
;;  syntax highlight code blocks:

(setq org-src-fontify-natively t)

;;;
;;  put caption below in tables:

(setq org-export-latex-table-caption-above nil)
(setq org-latex-table-caption-above nil)

;;;
;;  don't export tags:
;;
;;  Note: You may want to change this later if tags are important
;;        to your exported docs...

(setq org-export-with-tags nil)

;;;
;;  Enable extra org-babel language-specific packages:

(require 'ob-lisp)

;;;
;;  Make sure TEXINPUTS is set to: elpa/auctex-nn.nn.n/latex
;;  require 'preview below should set this as long as auctex is installed...

(unless (require 'latex nil t)
  (message "MELPA package 'latex not yet installed..."))

(require 'preview)


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Org Babel Active Language Configurations:
;;       t = enable     nil = disable
;;
;;  Prerequisites:
;;    ABC requires: abc-mode and the following
;;    external programs:
;;      abcm2ps (Install with Macports on MacOS)
;;      ps2pdf (bundled with GhostScript)
;;  Usage:
;;     Babel adds some new elements to code blocks.
;;     The basic structure becomes:
;;
;;       #+BEGIN_SRC language  org-switches header-arguments
;;       ,body
;;       #+END_SRC
;;
;;     Compile Babel Code blocks in the standard way using
;;     C-c C-c while the cursor is within the code block.
;;     For Example:
;;       C-c C-c within an ABC block will compile the block.
;;       into nice music notation...
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (abc . t)
   (asymptote . t)
   (awk . t)
   (calc . t)
   (css . t)
   (ditaa . t)
   (dot . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (haskell . nil)
   (java . t)
   (js . t)
   (latex . t)
   (ledger . t) ;This adds support for hledger hopefully...
   (lilypond . t)
   (lua . nil)
   (ocaml . nil)
   (octave . t)
   (org . t)
   (perl . t)
   (python . t)
   (ruby . t)
   (sass . t)
   (sed . t)
   (screen . nil)
   (shell . t)
   (sql . nil)
   (sqlite . t)))

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   END: [modular-emacs]:~/.emacs.d/lisp/modules/09-4-org-export-conf.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
