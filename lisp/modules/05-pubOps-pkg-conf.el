;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Create repositories cache for pubOps extras, if required:
(when (not package-archive-contents)
  (package-refresh-contents))

;; Declare a list of required packages for writers, bloggers, publishers, etc.:
(defvar modular-emacs--req-pubops-packages
  '(markdown-mode
    markdown-mode+
    markdown-toc
    deft
    pandoc-mode
    fountain-mode
    olivetti
    graphviz-dot-mode
    org-bullets    
    vmd-mode))

;; Install required packages:
(mapc (lambda (p)
        (package-install p))
      modular-emacs--req-pubops-packages)

;; Toggle olivetti minor mode (for writing) on and off:
(global-set-key (kbd "C-`") #'olivetti-mode)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; mmd-mode - Multimarkdown extensions to markdown-mode:
;; Reference: GitHub:jmquigley/mmd-mode
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(add-to-list 'load-path "{~/.emacs.d/lisp/my-modules/mmd-mode}")
(require 'mmd-mode)

(add-to-list 'auto-mode-alist '("\\.md\\'" . mmd-mode))
(add-to-list 'auto-mode-alist '("\\.mdwn\\'" . mmd-mode))
(add-to-list 'auto-mode-alist '("\\.mdt\\'" . mmd-mode))
(add-to-list 'auto-mode-alist '("\\.mmd\\'" . mmd-mode))


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Use Deft mode for quickly browsing, filtering, and
;; editing directories of plain text notes...
;; Deft was created by Jason Blevins. What a clever dude who
;; besides being an Emacs wizard. He is also a wizard in the
;; fine art of statistics! And looks to also dabble with
;; economics, money, crypotcurrency, etc...
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(require 'deft)
;; Enable/edit these next several lines to Customize items in
;; the deft group to change default functionality...
(setq deft-directory "~/000-GIT/Gen-Dat/My-Docs")
(setq deft-recursive t)
(setq deft-use-filename-as-title t)
(setq deft-file-naming-rules
      '((noslash . "-")
        (nospace . "-")))
(setq deft-markdown-mode-title-level 1)
(setq deft-org-mode-title-prefix t)
;; Associate file types for deft to process.  The default
;; is "txt" "md" "org".  I have added Tex (LaTex files)
;; You may add more here if you work with other formats.
(setq deft-extensions '("txt" "md" "tex" "org"))
;; Set deft to do regexp search instead of incremental...
;(setq deft-org-mode-title-prefix nil)

;; I don't want auto save because I open many notes for viewing only.
;; (Not to edit them but to read as reference, etc.  If this is not
;; disabled, I end up with a bunch of un-tracked files in my git
;; repository that holds these notes!
(setq deft-auto-save-interval 0)

;; Also, I had to add this deft-mode-hook function because lines were wrapping.
;; I tried everything else to no avail... This is a band-aid.  I need
;; to consult with Jason Blevins about this one %^)
(defun me-deft-init ()
  (setq truncate-lines t))

(add-hook 'deft-mode-hook 'me-deft-init)


;; Add Pandoc Mode to all Markdown Files:
;; Ref: https://joostkremers.github.io/pandoc-mode/
(add-hook 'markdown-mode-hook 'pandoc-mode)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Org-Mode Configurations...
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Org Mode Exporters
(require 'ox-md)
(require 'ox-latex)

;; speed keys for quick navigation
(setq org-use-speed-commands 1)

;; set maximum indentation for org-mode description lists
(setq org-list-description-max-indent 5)

;; prevent org-mode demoting heading also shifting text inside sections
(setq org-adapt-indentation nil)

;; stop inline images being too big
(setq org-image-actual-width '(500))

;; automatically refresh inline images
;; http://emacs.stackexchange.com/questions/3302/live-refresh-of-inline-images-with-org-display-inline-images
(defun shk-fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(add-hook 'org-babel-after-execute-hook 'shk-fix-inline-images)

;; export options
;; syntax highlight code blocks
(setq org-src-fontify-natively t)

;; put caption below in tables
(setq org-export-latex-table-caption-above nil)
(setq org-latex-table-caption-above nil)

;; don't export tags
(setq org-export-with-tags nil)

;; Org-Bullets Mode:
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Make windmove work in org-mode: 
;; The point must not be on an outline heading for this to work...
;; (move your point to a blank space or normal text first)
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; GitHub flavored Markdown preview minor-mode.
(require 'vmd-mode)


;; Graphviz-dot-mode Customizations:
(defun my-graphviz-tweaks ()
  (setq graphviz-dot-preview-extension "svg"))

(add-hook 'graphviz-dot-mode-hook 'my-graphviz-tweaks)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: 05-pubOps-pkg-conf.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
