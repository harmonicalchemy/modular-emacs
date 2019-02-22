;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/03-dired-pkg-conf.el
;;
;; This module adds extra dired features that are not part of the default dired.
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Create repositories cache for dired extras, if required:
(when (not package-archive-contents)
  (package-refresh-contents))

;; Declare a list of required packages for extra dired features:
(defvar modular-emacs--req-dired-packages
  '(helm-core
    dired-launch
    dired-imenu))

;; Install required packages:
(mapc (lambda (p)
        (package-install p))
      modular-emacs--req-dired-packages)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Dired Extras - (added 2018-011-04 by Alisha)

;; Load dired-x.el when dired is first invoked (e.g., when you first type C-x d)
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            ;; Set dired-x global variables here:
            ;; For example:
            ;;   (setq dired-guess-shell-gnutar "gtar")
            ;;   (setq dired-x-hands-off-my-keys nil)
            ;;
            ;; Specify default ls switches for dired to use:
            ;; NOTE: 2019-001-26 - Something here is not working...
            ;; I am troubleshooting this on Mac OS...
            ;; Not sure if this is a problem on Linux yet...
;            (setq-default dired-omit-files-p t)
;            (setq dired-listing-switches
;                  "-la --ignore='#*' --ignore='.DS_Store' --ignore='Icon*' --group-directories-first")
;            ;; Specify which files get omitted in Dired mode:
;            (setq dired-omit-files "^\\.?#\\|\\.DS_STORE\\|Icon*")
            ))

;; Load dired-x mode hook (dired-omit-mode, etc.)
(add-hook 'dired-mode-hook
          (lambda ()
            ;; Set dired-x buffer-local variables here:
            ;; For example:
            ;;   (dired-omit-mode 1)
            ;;
            'dired-launch-mode
            ;; Begin new dired sessions with dired-omit-mode `on` by default...
            (dired-omit-mode 1)
	    (define-key dired-mode-map (kbd "h") #'dired-omit-mode)
            ))


;; Auto load dired-jump and dired-jump-other-window:
(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)

(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)

(define-key global-map "\C-x\C-j" 'dired-jump)
(define-key global-map "\C-x4\C-j" 'dired-jump-other-window)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END 03-dired-pkg-conf.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
