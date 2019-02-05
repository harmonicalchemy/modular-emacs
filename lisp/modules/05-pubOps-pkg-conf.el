;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [machine-name]:~/.emacs.d/lisp/modules/05-pubOps-pkg-conf.el
;;
;; This is a new file which is not part of the original Super-Emacs project.
;; This adds extra tools for writing, viewing, and publishing which super-emacs
;; alone does not provide...
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Create repositories cache for pubOps extras, if required:
(when (not package-archive-contents)
  (package-refresh-contents))

;; Declare a list of required packages for writers, bloggers, publishers, etc.:
(defvar super-emacs--req-pubops-packages
  '(markdown-mode
    markdown-mode+
    markdown-toc
    fountain-mode
    olivetti
    org-bullets    
    vmd-mode))

;; Install required packages:
(mapc (lambda (p)
        (package-install p))
      super-emacs--req-pubops-packages)

;; Toggle olivetti minor mode (for writing) on and off:
(global-set-key (kbd "C-`") #'olivetti-mode)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; mmd-mode - Multimarkdown extensions to markdown-mode:
;; Reference: GitHub:jmquigley/mmd-mode
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(add-to-list 'load-path "{~/.emacs.d/lisp/mmd-mode/}")
(require 'mmd-mode)

(add-to-list 'auto-mode-alist '("\\.md\\'" . mmd-mode))
(add-to-list 'auto-mode-alist '("\\.mdwn\\'" . mmd-mode))
(add-to-list 'auto-mode-alist '("\\.mdt\\'" . mmd-mode))
(add-to-list 'auto-mode-alist '("\\.mmd\\'" . mmd-mode))


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

;; Global Keybindings:

(global-set-key (kbd "M-N") 'org-md-export-as-markdown)
(global-set-key (kbd "M-n") 'vmd-mode) 


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: 05-pubOps-pkg-conf.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
