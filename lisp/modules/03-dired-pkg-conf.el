;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/03-dired-pkg-conf.el
;;
;; This module adds extra dired features that are not part of the default dired.
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Create repositories cache for dired extras, if required:

(when (not package-archive-contents)
  (package-refresh-contents))

;; Declare a list of required packages for extra dired features:

(defvar me--req-dired-packages
  '(helm-core
    neotree
    dired-launch))

;; Install required packages:

(mapc (lambda (p)
        (package-install p))
      me--req-dired-packages)

;;;
;; Load xah-dired.el - from my-modules/XahEmacs/xah-dired

(add-to-list 'load-path "~/.emacs.d/lisp/my-modules/XahEmacs/xah-dired")

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Dired Extras - (first added: 2018-011-04 - Alisha)

;; On Mac OS, (darwin) the hooks below break because Mac OS no longer supports
;; standard ls command!  This next line fixes that.
;; NOTE: Requires: $> brew install coreutils

(if (eq system-type 'darwin)
    (setq insert-directory-program "gls" dired-use-ls-dired t))

;; Load dired-x.el when dired is first invoked (e.g., when you first type C-x d)
;; NOTE: Newer versions of Emacs do not require doing this in a hook anymore. ;-)
;; Plus I learned a fancy trick for dired-omit-extensions as well...

(require 'dired-x)
(setq-default dired-omit-files-p t)

;; Specify which files get omitted in Dired mode:

(setq dired-omit-files (concat dired-omit-files
                               "^\\.?#\\|^.gitkeep$\\|^.DS_STORE$\\|^.SSH_AGENT\\|Icon*\\|^.git$\\|^transient$\\|^auto-save-list$\\|^recentf$"))

;; Ignore files with these extensions:

(let* ((exts '(".dat" ".temp" ".out"))
       (omit (delete-dups (append exts dired-omit-extensions))))
  (setq dired-omit-extensions omit))

;;
;; Modular Emacs dired Mode Hook Function:
;;

(defun me_dired-mode-hook-tweaks ()
  ;; Set dired-x buffer-local variables here:
  ;; For example:
  ;;   (dired-omit-mode 1)
  ;;
  'dired-launch-mode
  ;; Begin new dired sessions with dired-omit-mode `on` by default...
  (dired-omit-mode 1)
  ;; Truncate long lines... Don't mess up my nice dired columns!
  ;; You can always C-e, C-a to jump back and forth... (rarely needed)
  (setq truncate-lines t)
  ;; Map 'h' key to dired omit mode:
  (define-key dired-mode-map (kbd "h") #'dired-omit-mode))

;; Load Custom Dired Mode Tweaks:

(add-hook 'dired-mode-hook 'me_dired-mode-hook-tweaks)        

;; Dired Sort Directories First... This function works and does not break after
;; adding marks and pressing 'g'.
;; From: https://www.emacswiki.org/emacs/DiredSortDirectoriesFirst

(defun me_dired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defadvice dired-readin
    (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding marks."
  (me_dired-sort))

;; Auto load dired-jump and dired-jump-other-window:

(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)

(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)

(define-key global-map "\C-x\C-j" 'dired-jump)
(define-key global-map "\C-x4\C-j" 'dired-jump-other-window)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/03-dired-pkg-conf.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

