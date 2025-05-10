;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/04-devOps-pkg-conf.el
;;
;; This module adds extra sysAdmin, programming, develop/test, and deployment
;; features to make Emacs the center of your development operations...

;;
;; Change Log: (descending chronological order)
;;

;; 2022-009-18 - Alisha Awen, siren1@disroot.org
;;   Changed Harmonic Alchemy Modular Emacs TO: v3.5...
;;   I am no longer using magit... It requires emacs packages
;;   I don't want this extra FLUF in my Emacs...
;;   I have my own GIT macros,aliases, etc. set up for ZSH and BASH...
;;   DOING GIT within ZSH IS MY AWESOME WORKFLOW... (Magit Take a HIKE Thanks!)

;; 2022-005-15 - Alisha Awen, siren1@disroot.org
;;   Removed Premium XahEmacs package loads and created a dedicated conf.el file
;;   for that: 06-XahEmacs-conf.el which is disabled by default within
;;   dispatcher.el...  If you purchase XahEmacs you will need to create a
;;   sub-directory: i.e., ~/.emacs.d/lisp/my-modules/XahEmacs and then copy the
;;   individual XahEmacs package directories into there...  Currently the only
;;   modules enabled are xah-elisp-mode and xah-find...  

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Create repositories cache for devOps extras, if required:

(when (not package-archive-contents)
  (package-refresh-contents))

;;;
;; Declare a list of required packages for programming, build/test, deploy, etc.:
;; NOTE: I removed: gitconfig-mode, gitignore-mode & gitattributes-mode
;;       No longer supported... 

(defvar me--req-devops-packages
  '(ztree
    ssh-config-mode
    yaml-mode
    nginx-mode
    php-mode          ;; SOON to be REPLACED by xah-php-mode (when it is done)
    lua-mode          ;; Mode for editing .lua files...
    logview))
;    smart-tabs-mode  ;; DISABLED - Using Xah Code Functions now...
;    indent-tools     ;; Removed becuase it rwquires s- Package (Insulting eLisp code)

;; Install required packages:

(mapc (lambda (p)
        (package-install p))
      me--req-devops-packages)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Load ssh-config-mode:

(autoload 'ssh-config-mode "ssh-config-mode" t)

;; Automatically sense SSH config files and set ssh-config-mode:

(add-to-list 'auto-mode-alist '("/\\.ssh/config\\'"     . ssh-config-mode))
(add-to-list 'auto-mode-alist '("/sshd?_config\\'"      . ssh-config-mode))
(add-to-list 'auto-mode-alist '("/known_hosts\\'"       . ssh-known-hosts-mode))
(add-to-list 'auto-mode-alist '("/authorized_keys2?\\'" . ssh-authorized-keys-mode))
(add-hook 'ssh-config-mode-hook 'turn-on-font-lock)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  TABS Configuration:

;; Set Default Tab size to three spaces:

(setq-default tabs-width 3)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Add Language Support for Web Dev: (HTML, CSS, PHP, etc...)

(setq c-basic-offset 3)
(setq web-mode-markup-indent-offset 3)
(setq web-mode-css-indent-offset 3)
(setq web-mode-code-indent-offset 3)
(setq web-mode-sql-indent-offset 3)
(setq css-indent-offset 3)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Enable Smart Tabs for all supported languages:
;; DISABLED - Using Xah Code Functions now...

;; (smart-tabs-insinuate 'c
;;                       'c++
;;                       'javascript
;;                       'java
;;                       'cperl
;;                       'python
;;                       'ruby
;;                       'nxml)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Set C Lang Auto Mode for specific file extensions:

(add-to-list 'auto-mode-alist '("\\.xkb\\'" . c-mode))


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Ediff Customizations:
;; From: https://oremacs.com/2015/01/17/setting-up-ediff/
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Macro for setting custom variables (not just useful for eDiff:

(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))

;; Don't use the weird setup with the control panel in a separate frame.

(csetq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Split the windows horizontally instead of vertically.
;; (easier to follow changes)

(csetq ediff-split-window-function 'split-window-horizontally)

;; Ignore white space. (can cause problems with python)
;; If you need to view diffs of Python code you need to disable this!

(csetq ediff-diff-options "-w")

;; Assign j to move down, and k to move up.

(defun ora-ediff-hook ()
  (ediff-setup-keymap)
  (define-key ediff-mode-map "k" 'ediff-next-difference)
  (define-key ediff-mode-map "i" 'ediff-previous-difference))

(add-hook 'ediff-mode-hook 'ora-ediff-hook)

;; Restore Original Window Configuration after Ediff quits:

(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/04-devOps-pkg-conf.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
