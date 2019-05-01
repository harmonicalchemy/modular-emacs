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


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Dired Extras - (first added: 2018-011-04 - Alisha)

;; On Mac OS, (darwin) the hooks below break because Mac OS no longer supports
;; standard ls command!  This next line fixes that.
;; NOTE: Requires: $> brew install coreutils
(if (eq system-type 'darwin)
    (setq insert-directory-program "gls" dired-use-ls-dired t))

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
            ;; NOTE 2019-001-26:
            ;;       Something here is not working...
            ;;       I am troubleshooting this on Mac OS...
            ;;       Was not a problem on Linux...
            ;; Update 2019-005-01:
            ;;       I finally have this working well on Mac OS...
            ;;       We needed:  insert-directory-program "gls" Doh!
            ;; NOTE: I removed the group-directories-first option from
            ;;       dired-listing-switches here, because I have a new function
            ;;       below that so far looks like it does a better job...
            ;;       Although I still notice .DS_Store show up after hiting "g".
            ;;       (you have to toggle it off by hitting "h" twice!)
            (setq dired-listing-switches
                 "-la --ignore='#*' --ignore='.DS_Store' --ignore='Icon*' ")
            ;;
            (setq-default dired-omit-files-p t)
            ;;
            ;; Specify which files get omitted in Dired mode:
            (setq dired-omit-files "^\\.?#\\|\\.DS_STORE\\|Icon*")
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

;; Dired Sort Directories First... This function works and does not break after
;; adding marks and pressing 'g'.
;; From: https://www.emacswiki.org/emacs/DiredSortDirectoriesFirst
(defun mydired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header 
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defadvice dired-readin
    (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding marks."
  (mydired-sort))


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
