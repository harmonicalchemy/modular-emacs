;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/11-external-integrations-conf.el
;; NOTE: NAME CHANGE - "games" was too specific and limiting...
;;
;; About:     This module adds EXTERNAL APPS and LIBRARY INTEGRATIONS to
;;            Harmonic Alchemy Modular Emacs Configuration.
;;            External apps include Lilypond, Frescobaldi, GNU EMMS
;;            (Emacs Multimedia System) LambdaMOO servers (via rmoo)
;;            Converters for Org Mode to use during Export... etc...
;;            Basically this is the EXTRAS Stuff that is OPTIONAL but
;;            POTENTIALLY POWERFUL...

;;            NOTE: I created my own fork of the old 'rmoo' MOO client
;;            for Emacs and hopefully am trying my best to keep it working
;;            at least for my own limited "text based VR adventures" YMMV...

;; Useful LambdaMOO Reference:
;;
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
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Create repositories cache for games, if required:
(when (not package-archive-contents)
  (package-refresh-contents))

;; Declare a list of required packages for extra dired features:
(defvar me--req-external-packages
  '(emms
    xterm-color))

;; Install required packages:
(mapc (lambda (p)
        (package-install p))
      me--req-external-packages)

;; This may help to connect rMOO (below) via TLS
(setq network-security-level 'high)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;               LILYPOND CONFIGURATIONS
;;
;; WELL now in 2025 it appears that EMACS comes built with
;; lilypond-mode out of box so WE DON'T need to mess with that 
;; anymore... YAY! You guys know to add the important stuff eh?
;;
;; NOTHING TO DO HERE ANYMORE...
;; (but maybe some extra stuff will be needed at some point)
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;                         R-MOO
;; Configure RMOO - A MOO client for Emacs:
;; Reference: Github.com/lisdude/rmoo.git
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(require 'xterm-color)
(add-to-list 'auto-mode-alist '("\\.moo$" . moocode-mode))
(add-to-list 'load-path "~/.emacs.d/lisp/my-modules/rmoo")
(require 'rmoo-autoload)
(require 'moocode-mode)
(global-set-key (kbd "C-c C-r") 'rmoo)

;; RMOO Hook:
(add-hook 'rmoo-interactive-mode-hooks
          (lambda ()
            (linum-mode -1)            ;; no line numbers
            (olivetti-mode)
            (olivetti-set-width 80)
            (goto-address-mode t)))    ;; clickable links

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;;             GNU - E. M. M. S. with M P D
;; Setup & Configure GNU EMMS (Emacs Multimedia System)
;; Reference: https://www.gnu.org/software/emms
;; EMMS Manual: https://www.gnu.org/software/emms/manual/
;; Helpful Youtube Video: https://youtu.be/xTVN8UDScqk  
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; (autoload 'emms-smart-browse "emms-browser.el" "Browse with EMMS" t)
;; (global-set-key [(f7)] 'emms-smart-browse)

;; (with-eval-after-load 'emms
;;   (emms-standard) ;; or (emms-devel) if you want all features

;;   ;; Yes... Do Change This Next Path Definition to the
;;   ;;        Location of: Your Own Media Library. ;-)

;;   ;; And please don't try to hack my music library, lol!
;;   ;; I would share it with the world if I could do so without
;;   ;; getting in trouble though... lol   Beautiful art should
;;   ;; be free access like clean clear water and air...
;;   ;; We desperately need it to survive and stay peaceful!
;;   ;; Yeah.. that second part especially!  Make dance not war!
;;   ;; Be warriors of the dance rather than warriors of blood...
;;   ;; Doing it that way keeps us strong...
;;   ;; (just in case attacked from elsewhere)
;;   ;; nevermind, I'm rambling now... in a Lisp comment at that...
;;   ;; lol (who will read this except Lisp nerds? that counts though)

;;   ;; For My Mac OS (Your path is different. Change this...)
;;   (when ME--DARWIN
;;     (setq emms-source-file-default-directory
;;           "~/Documents/DATA/000-HAP-Media-Library/010-MP3-Library"))

;;   ;; For My Linux (Your path is different. Change this...)
;;   (when ME--LINUX
;;     (setq emms-source-file-default-directory
;;           "~/000-GIT/Media-Archive/Music-Audio"))

;;   (emms-info-asynchronously t)
;;   (emms-show-format "♪ %s")

;;   ;; You might want to check out:
;;   ;;   emms-info-functions
;;   ;;   emms-info-libtag-program-name
;;   ;;   emms-source-file-directory-tree-function
;;   ;;   as well.

;;   (require 'emms-setup)

;;   (emms-all)
;;   (emms-mode-line nil)
;;   (require 'helm)

;;   (setq emms-seek-seconds 5)
;;   (setq emms-player-mpd-server-name "localhost")
;;   (setq emms-player-mpd-server-port "6601")

;;   ;; Determine which player to use.
;;   ;; If you don't have strong preferences or don't have
;;   ;; exotic files from the past (wma) `emms-default-players`
;;   ;; is probably all you need.

;;   (if (executable-find "mpd")
;;       (progn
;;         (require 'emms-player-mpd)
;;         (add-to-list 'emms-player-list 'emms-player-mpd)
;;         (require 'emms-volume)
;;         (setq emms-info-functions '(emms-info-mpd))
;;         (setq emms-volume-change-function 'emms-volume-mpd-change)
;;         (emms-default-players)))

;;   ;; For libre.fm see `emms-librefm-scrobbler-username' and
;;   ;; `emms-librefm-scrobbler-password'.
;;   ;; Future versions will use .authoinfo.gpg.
;;   ) ;; END: with-eval-after-load 'ems
;;     ;; NOTE: (move this closing parenthesis to its proper
;;     ;;        position after all elements are placed...
;;     ;;        Still coding this)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Start MPD from within Emacs...

;; (defun me_mpd-start-music-daemon ()
;;   "Start MPD, connects to it and syncs the metadata cache."
;;   (interactive)
;;   (shell-command "mpd")
;;   (mpd/update-database)
;;   (emms-player-mpd-connect)
;;   (emms-cache-set-from-mpd-all)
;;   (message "MPD Started!"))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Keybindings: 
;;
;; TODO: Make this some of these xah-fly-key bindings if
;;       that will help.  The top bar transport keys are
;;       universal however...)

;; Start Music Player Daemon: (good xah-fly-key candidate)

;(global-set-key (kbd "C-c f") 'me_mpd-start-music-daemon)

;; EMMS Media Transport / Playlist Keys:

;; (global-set-key (kbd "<f9>") 'emms-pause)
;; (global-set-key (kbd "C-c C-s") 'emms-stop)      ; good xah-fly-key candidate
;; (global-set-key (kbd "<C-left>") 'emms-previous)
;; (global-set-key (kbd "<C-right>") 'emms-next)

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; END: [modular-emacs]:~/.emacs.d/lisp/modules/11-external-integrations-conf.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

