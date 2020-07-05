;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/11-games-pkg-conf.el
;;
;; About:     This module adds some Emacs gaming features. For a start it 
;;            integrates a new fork of the 'rmoo' MOO client for Emacs.
;;            No other games related things have been added here yet...
;;
;; Useful Reference:
;;    RMOO:   Github.com/lisdude/rmoo.git   V1.2 (Nov 13, 2018)
;;                                          Emacs MOO Client...
;;    MOO:    https://www.lisdude.com/moo/  A curated MOO Archive of everything!
;;                                          This is great news for die-hard Text
;;                                          based Virtual Reality Authors!
;;                                          Thanks! lisdude!
;;
;;    Stunt LambdaMOO:   GitHub.com/toddsundsted/stunt (stunt.io - main site)
;;                       This is an updated LambdaMOO with multiple inheritance,
;;                       anonymous objects, HTTP, JSON <-> MOO translations
;;                       better crypto, a map datatype and a RESTful interface.
;;                           (A great place to start a MOO server beyond 2019)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Create repositories cache for games, if required:
(when (not package-archive-contents)
  (package-refresh-contents))

;; Declare a list of required packages for extra dired features:
(defvar me--req-games-packages
  '(emms
    xterm-color))

;; Install required packages:
(mapc (lambda (p)
        (package-install p))
      me--req-games-packages)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Configure RMOO - A MOO client for Emacs:
;; Reference: Github.com/lisdude/rmoo.git
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(add-to-list 'load-path "~/.emacs.d/lisp/my-modules/rmoo")
(require 'rmoo-autoload)
(require 'moocode-mode)
(global-set-key (kbd "C-c C-r") 'rmoo)
(add-to-list 'auto-mode-alist '("\\.moo$" . moocode-mode))
(add-hook 'rmoo-interactive-mode-hooks (lambda ()
(linum-mode -1)                  ;; ... no line numbers
(olivetti-mode)
(olivetti-set-width 102)
(goto-address-mode t)))          ;; ... clickable links


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Setup & Configure GNU EMMS (Emacs Multimedia System)
;; Reference: https://www.gnu.org/software/emms
;; EMMS Manual: https://www.gnu.org/software/emms/manual/
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(autoload 'emms-smart-browse "emms-browser.el" "Browse with EMMS" t)
(global-set-key [(f7)] 'emms-smart-browse)

(with-eval-after-load 'emms
  (emms-standard) ;; or (emms-devel) if you want all features
  ;; NOTE: Change this next Path definition to the location of your own Media Library:
  (setq emms-source-file-default-directory "~/Documents/DATA/000-HAP-Media-Library/010-MP3-Library"
        emms-info-asynchronously t
        emms-show-format "♪ %s")

  ;; Might want to check `emms-info-functions',
  ;; `emms-info-libtag-program-name',
  ;; `emms-source-file-directory-tree-function'
  ;; as well.

  (require 'emms-setup)
  (emms-all)
  (emms-mode-line nil)
  (require 'helm)

  ;; Determine which player to use.
  ;; If you don't have strong preferences or don't have
  ;; exotic files from the past (wma) `emms-default-players`
  ;; is probably all you need.
  (if (executable-find "mpd")
      (progn
        (require 'emms-player-mpd)
        (add-to-list 'emms-player-list 'emms-player-mpd)
        (require 'emms-volume)
        (setq emms-volume-change-function 'emms-volume-mpd-change))
    (emms-default-players))

  ;; For libre.fm see `emms-librefm-scrobbler-username' and
  ;; `emms-librefm-scrobbler-password'.
  ;; Future versions will use .authoinfo.gpg.
  )

(global-set-key (kbd "C-c C-p") 'emms-pause)
(global-set-key (kbd "C-c C-s") 'emms-stop)
(global-set-key (kbd "C-c <up>") 'emms-previous)
(global-set-key (kbd "C-c <down>") 'emms-next)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/11-games-pkg-conf.el
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
