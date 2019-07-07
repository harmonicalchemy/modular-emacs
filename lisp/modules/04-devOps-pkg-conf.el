;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/04-devOps-pkg-conf.el
;;
;; This module adds extra sysAdmin, programming, develop/test, and deployment
;; features to make Emacs the center of your development operations...
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Create repositories cache for devOps extras, if required:
(when (not package-archive-contents)
  (package-refresh-contents))

;; Declare a list of required packages for programming, build/test, deploy, etc.:
(defvar modular-emacs--req-devops-packages
  '(ztree
    neotree
    smart-tabs-mode
    ssh-config-mode
    gitconfig-mode
    gitignore-mode
    gitattributes-mode
    yaml-mode
    indent-tools
    hydra
    magit))

;; Install required packages:
(mapc (lambda (p)
        (package-install p))
      modular-emacs--req-devops-packages)

;; Load ssh-config-mode:
(autoload 'ssh-config-mode "ssh-config-mode" t)

;; Automatically sense SSH config files and set ssh-config-mode:
(add-to-list 'auto-mode-alist '("/\\.ssh/config\\'"     . ssh-config-mode))
(add-to-list 'auto-mode-alist '("/sshd?_config\\'"      . ssh-config-mode))
(add-to-list 'auto-mode-alist '("/known_hosts\\'"       . ssh-known-hosts-mode))
(add-to-list 'auto-mode-alist '("/authorized_keys2?\\'" . ssh-authorized-keys-mode))
(add-hook 'ssh-config-mode-hook 'turn-on-font-lock)

;; Enforce spaces for indentation, instead of tabs
(setq-default indent-tabs-mode nil)

;; Enable Smart Tabs for all supported languages:
(smart-tabs-insinuate 'c
                      'c++
                      'javascript
                      'java
                      'cperl
                      'python
                      'ruby
                      'nxml)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Set F1 key to load man page for keyword at current cursor position (woman):
;; This key is universal...  Nice to have it in Emacs for Man Pages! Cool! 
(global-set-key (kbd "<f1>")
                (lambda ()
                  (interactive)
                  (let ((woman-use-topic-at-point t))
                    (woman))))


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Magit Customisation Section:
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; So far I only set a global key for git status: (more may come)
(global-set-key (kbd "C-x g") 'magit-status)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Ediff Customizations:
;; From: https://oremacs.com/2015/01/17/setting-up-ediff/
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Macro for setting custom variables:
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
  (define-key ediff-mode-map "j" 'ediff-next-difference)
  (define-key ediff-mode-map "k" 'ediff-previous-difference))

(add-hook 'ediff-mode-hook 'ora-ediff-hook)

;; Restore Original Window Configuration after Ediff quits:
(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/04-devOps-pkg-conf.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
