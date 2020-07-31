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
    vmd-mode))

;;;
;; Install required packages:

(mapc (lambda (p)
        (package-install p))
      me--req-pubops-packages)


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Fountain Mode Tweaks:

(custom-set-faces
 '(fountain ((t (
                 :height 130
                 :family "Courier Prime Emacs")))))

(custom-set-faces
 '(fountain-dialog ((t (
                        :height 130
                        :family "Courier Prime Emacs"
                        :inherit
                        (font-lock-string-face))))))

;;;
;;  ME Fountain Mode Hook Function:

(defun me_fountain-mode-hook ()
  "Customize some things to do when fountain mode starts up"
  (interactive)
  (imenu-add-menubar-index)
  (fountain-completion-update)
  (fountain-outline-hide-custom-level)
  (olivetti-mode)
  ;; Add olivetti mode options for fountain mode here:
  (olivetti-set-width 80))

(add-hook 'fountain-mode-hook 'me_fountain-mode-hook)


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  AucTeX: LaTeX configuration:
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(require 'tex)

(setq TeX-auto-save t)
(setq TeX-fold-mode 1)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq reftex-plug-into-AUCTeX t)

;;;
;; Set default compile to PDF:

(setq TeX-PDF-mode t)

;;;
;; LaTeX Mode Hook tweaks:

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)


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

(when DARWIN
  (setq deft-directory "~/change-this-path/to/your-Docs"))

;;;
;; Path to My-Docs on Linux:

(when LINUX
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

;; Mac OS Case:

(when DARWIN
  ;; You may need to enable the first form if you used HomeBrew
  ;; to install Pandoc...  If you enable the first form, be sure to comment
  ;; out (disable) the second form below it and visa-versa!!
  ;; Your Pandoc was installed by HomeBrew.  Ensure Emacs gets the path...
  ;(setq pandoc-binary "/usr/local/bin/pandoc"))
  ;; Your Pandoc was installed by MacPorts.  Ensure Emacs gets the path...
  (setq pandoc-binary "/opt/local/bin/pandoc"))

;; Linux Case:

(when LINUX
  (setq pandoc-binary "/usr/bin/pandoc"))

;;;
;; Add Pandoc Mode to all Markdown Files:
;; Ref: https://joostkremers.github.io/pandoc-mode/

(add-hook 'markdown-mode-hook 'pandoc-mode)

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

(defun me_graphviz-tweaks ()
  (setq graphviz-dot-preview-extension "svg"))

(add-hook 'graphviz-dot-mode-hook 'me_graphviz-tweaks)

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   END: [modular-emacs]:~/.emacs.d/lisp/modules/05-pubOps-pkg-conf.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
