;; -*- coding: utf-8; lexical-binding: t; -*-
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; File: ~/.emacs.d/lisp/modules/me-abbrev-defs.el
;; Refs:
;;   http://xahlee.info/emacs/emacs/emacs_abbrev_mode.html
;;   http://xahlee.info/emacs/emacs/emacs_abbrev_mode_tutorial.html
;;
;; This File Contains Modular Emacs DEFAULT GP Emacs Abbrev Definitions...
;; Override this default by copying this file to:  ~/.emacs.d/lisp/my-modules/.
;; and then change anything below to suit your particular abbreviation needs...

;; NOTE1: Many of these below match Xah Lee's abbrevs making it easy for you to
;;        follow along with him during tutorials, and or Live Stream sessions...
;;        Consider that when modifying this file... (For the above pedagogical
;;        reason, it may be smarter to add your own new abbrevs to the bottom
;;        of the GLOBAL List and keep the existing definitions as they are)...

;; NOTE2: ME = Modular Emacs (used in comments below to distinguish between
;;        Modular Emacs Default Abbrevs and Xah Lee's Definitions from his
;;        Emacs Abbrevs Help Page)... ME used in file names, functions, and
;;        symbol names, means "Modular Emacs" or could also mean the word "me"
;;        (if you like to think of it that way)... i.e. me=you in your point
;;        of view ;-) 
;;
;; Once this file is loaded typing any of the abbreviations below and then
;; typing a space will automatically expand them.  If you DON'T WANT an abbrev
;; to expand for any reason you can stop expansion by pressing: C-q AFTER 
;; typing the Abbrev AND BEFORE typing a space or punctuation. Therefore,
;; You are NOT TRAPPED or Forced into using this speedy tool...
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(
    ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ;; NOTE: These are GLOBAL Abbrev Definitions taken directly from this page:
    ;;          http://xahlee.info/emacs/emacs/emacs_abbrev_mode.html
    ;;       IT may be wise NOT to change many of these (except personal ones)
    ;;       and instead add Your own section at the BOTTOM of this list
    ;;       where noted... Closing Parenthesis for this list are on a blank line
    ;;       to make it easy to add more forms with out breaking this list. ;-)

    ;; English Word Abbrevs:

    ("zarg" "argument")
    ("zbc" "because")
    ("zbg" "background")
    ("zbt" "between")
    ("zparen" "parenthesis") ;; ME added this one... (not xah-lee)

    ;; English Abridged Words:

    ("zcnt" "can't")
    ("zddnt" "didn't")
    ("zdnt" "don't")

    ;; Phrase Abbrev:
    
    ("zafaik" "as far as i know")
    ("zatm" "at the moment")
    ("zty" "thank you")
    ("zbtw" "by the way")

    ;; Computing:
    
    ("zcfg" "context-free grammar")
    ("zcs" "computer science")

    ;; Tech Company:
    
    ("zgc" "Google Chrome")
    ("zmacos" "macOS")
    ("zmsw" "Microsoft Windows")
    ("zglx" "GNU Linux")            ;; ME added this one... (not xah-lee)
    ("zbsx" "BSD Unix")             ;; ME added this one... (not xah-lee)
    ("zonx" "Other Unix")           ;; ME added this one... (not xah-lee)

    ;; Programing:
    
    ("zipa" "IP address")
    ("zjvm" "Java Virtual Machine")
    ("zrsi" "Repetitive Strain Injury")
    ("zsubdir" "subdirectory")
    ("zdb" "database")

    ("zevp" "environment variable")
    ("zguip" "graphical user interface")
    ("zoopp" "object oriented programing")
    ("zosp" "operating system")

    ("zeq" "==")
    ("zr" "return")
    ("zutf8" "-*- coding: utf-8 -*-")

    ;; Regex:
    
    ("zazt" "\\([A-Za-z0-9]+\\)")
    ("zbrackett" "\\[\\([^]]+?\\)\\]")
    ("zcurlyt" "“\\([^”]+?\\)”")
    ("zdigitst" "\\([0-9]+\\)")
    ("zdatet" "\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)")
    ("zstrt" "\\([^\"]+?\\)")

    ;; Unicode:
    
    ("zhr" "--------------------------------------------------")
    ("zbu" "•")
    ("zcatface" "😸")
    
    ;; ME added this one... 
    ("zocto" "🐙")
    
    ;; ME added this one...
    ("zaa" "Alisha Awen 👩‍💻 HarmonicAlchemy@Proton.me")  
    
    ("zhearts" "♥💕💓💔💖💗💘💝💞💟💙💚💛💜")
    ("zra" "→")

    ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ;; ADD YOUR OWN ABBREV DEFINITIONS HERE:

    )) ;; END: global-abbrev-table

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Turn Abbrev Mode ON Globally...

(set-default 'abbrev-mode t)

(setq save-abbrevs nil)

;; END File: ~/.emacs.d/lisp/modules/me-abbrev-defs.el
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
