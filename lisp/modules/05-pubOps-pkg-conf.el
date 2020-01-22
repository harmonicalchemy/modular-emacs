;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/05-pubOps-pkg-conf.el
;;
;; This module adds extra tools for writing, viewing, and publishing to give you
;; a jump start at using Emacs as a great publishing platform.  Just as good or
;; in-my-opinion, better than using something like Open Office, MS Word, or even
;; something fancier like Scrivener!
;;
;; using vmd-mode in combination with olivetti, etc. provide you with both
;; WYSIWYG live (like standard word processors) but also very powerful macro
;; commands, ease of fast typing everything (no mouse needed) etc.  Which is
;; why I strongly feel Emacs is the best publishing tool out there for anything!
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;;
;; Create repositories cache for pubOps extras, if required:

(when (not package-archive-contents)
  (package-refresh-contents))

;;;
;; Declare a list of required packages for writers, bloggers, publishers, etc.:

(defvar me--req-pubops-packages
  '(markdown-mode
    markdown-mode+
    markdown-toc
    deft
    auctex
    pandoc-mode
    fountain-mode
    olivetti
    graphviz-dot-mode
    org-bullets
    vmd-mode))

;;;
;; Install required packages:

(mapc (lambda (p)
        (package-install p))
      me--req-pubops-packages)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  AucTeX: LaTeX configuration:
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(setq reftex-plug-into-AUCTeX t)

;;;
;; Set default compile to PDF:

(setq TeX-PDF-mode t)

;;;
;; If the above did’t work, try this instead:

;    (require 'tex)
;    (TeX-global-PDF-mode t)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; mmd-mode - Multimarkdown extensions to markdown-mode:
;; Reference: GitHub:jmquigley/mmd-mode
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(add-to-list 'load-path "{~/.emacs.d/lisp/my-modules/mmd-mode}")
(require 'mmd-mode)

(add-to-list 'auto-mode-alist '("\\.md\\'" . mmd-mode))
(add-to-list 'auto-mode-alist '("\\.mdwn\\'" . mmd-mode))
(add-to-list 'auto-mode-alist '("\\.mdt\\'" . mmd-mode))
(add-to-list 'auto-mode-alist '("\\.mmd\\'" . mmd-mode))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Use Deft mode for quickly browsing, filtering, and
;; editing directories of plain text notes...
;; Deft was created by Jason Blevins. What a clever dude who
;; besides being an Emacs wizard. He is also a wizard in the
;; fine art of statistics! And looks to also dabble with
;; economics, money, crypotcurrency, etc...
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(require 'deft)

;;;;
;; Enable/edit these next several lines to Customize items in
;; the deft group to change default functionality...
;;
;; Adjust path to the place where much of your Markdown docs live:
;; Change the placeholder paths below to a location where you keep your library
;; of markdown files. If you don't have something like that set up already,
;; create a place for that now.  DEFT will give you a nice for navigating your
;; markdown and/or org docs...  There are options below for both Mac OS and Linux
;; systems.  If you Emacs on both systems set the paths below for both of them...
;;;;

;;;
;; Path to My-Docs on Mac OS:

(when *is-darwin*
  (setq deft-directory "~/change-this-path/to/your-Docs"))

;;;
;; Path to My-Docs on Linux:

(when *is-linux*
  (setq deft-directory "~/change-this-path/to/your-Docs"))

(setq deft-recursive t)
(setq deft-use-filename-as-title t)
(setq deft-file-naming-rules
      '((noslash . "-")
        (nospace . "-")))
(setq deft-markdown-mode-title-level 1)
(setq deft-org-mode-title-prefix t)

;;;
;; Associate file types for deft to process:
;; The default is "txt" "md" "org".  I have added Tex (LaTex files)
;; You may add more here if you work with other formats.

(setq deft-extensions '("txt" "md" "tex" "org" "fountain"))

;;;
;; Set deft to do regexp search instead of incremental:

;(setq deft-org-mode-title-prefix nil)

;;;
;; I don't want auto save because I open many notes for viewing only.
;; (Not to edit them but to read as reference, etc.  If this is not
;; disabled, I end up with a bunch of un-tracked files in my git
;; repository that holds these notes!

(setq deft-auto-save-interval 0)

;;;
;; Also, I had to add this deft-mode-hook function because lines were wrapping.
;; I tried everything else to no avail... This is a band-aid.  I need
;; to consult with Jason Blevins about this one %^)

(defun me-deft-init ()
  (setq truncate-lines t))

(add-hook 'deft-mode-hook 'me-deft-init)

;;;; ~~~~~~~~~~~~
;;   Pandoc Mode:
;;;; ~~~~~~~~~~~~

;;;
;; NOTE: On Mac OS, Depending on how you installed PanDoc,
;; you may need one of the following: (I enabled the MacPorts form for my setup)

(when *is-darwin*
  ;; You may need to enable the first form if you used HomeBrew
  ;; to install Pandoc...  If you enable the first form, be sure to comment
  ;; out (disable) the second form below it and visa-versa!!
  ;; Your Pandoc was installed by HomeBrew.  Ensure Emacs gets the path...
  ;(setq pandoc-binary "/usr/local/bin/pandoc"))
  ;; Your Pandoc was installed by MacPorts.  Ensure Emacs gets the path...
  (setq pandoc-binary "/opt/local/bin/pandoc"))

;;;
;; Add Pandoc Mode to all Markdown Files:
;; Ref: https://joostkremers.github.io/pandoc-mode/

(add-hook 'markdown-mode-hook 'pandoc-mode)

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~
;;   Org-Mode Configurations:
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~

;;;
;; Org Mode Exporters:

(require 'ox-md)
(require 'ox-latex)

;;;
;; speed keys for quick navigation:

(setq org-use-speed-commands 1)

;;;
;; set maximum indentation for org-mode description lists:

(setq org-list-description-max-indent 5)

;;;
;; prevent org-mode demoting heading also shifting text inside sections:

(setq org-adapt-indentation nil)

;;;
;; Stop Inline Images Being Too Big:

(setq org-image-actual-width '(500))

;;;
;; Automatically Refresh Inline Images:
;; REF: http://emacs.stackexchange.com/questions/3302/live-refresh-of-inline-images-with-org-display-inline-images

(defun shk-fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(add-hook 'org-babel-after-execute-hook 'shk-fix-inline-images)

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~
;;   Org Mode Export Options:
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~

;;;
;; syntax highlight code blocks:

(setq org-src-fontify-natively t)

;;;
;; put caption below in tables:

(setq org-export-latex-table-caption-above nil)
(setq org-latex-table-caption-above nil)

;;;
;; don't export tags:

(setq org-export-with-tags nil)

;;;; ~~~~~~~~~~~~~~~~~
;;   Org-Bullets Mode:
;;;; ~~~~~~~~~~~~~~~~~

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;;;
;; Make windmove work in org-mode: 
;; The point must not be on an outline heading for this to work...
;; (move your point to a blank space or normal text first)

(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;;;; ~~~~~~~~~
;;   VMD Mode:
;;;; ~~~~~~~~~

;;;
;; Require GitHub flavored Markdown preview minor-mode:

(require 'vmd-mode)

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   Graphviz-dot-mode Customizations:
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;;
;; I only changed the default output from .png to .svg...
;; That is my personal prefrence... Comment out if you like .png better...
;; Also add other custom features here to this hook as well if you like...
;; If there are any custom keys set for graphviz-dot-mode, you will find
;; those defined within 10-key-bindings.el...

(defun my-graphviz-tweaks ()
  (setq graphviz-dot-preview-extension "svg"))

(add-hook 'graphviz-dot-mode-hook 'my-graphviz-tweaks)

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   END: [modular-emacs]:~/.emacs.d/lisp/modules/05-pubOps-pkg-conf.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
