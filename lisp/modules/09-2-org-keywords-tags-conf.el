;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   [modular-emacs]:~/.emacs.d/lisp/modules/09-2-org-keywords-tags-conf.el
;;
;;   This is a sub module of 09-org-mode-pkg-conf.el which takes care of GLOBAL
;;   Org mode Keywords and TAGS...
;;
;;   The TAGS are defined in: $Org-Docs/HAP-Controlled-Vocabulary.org
;;
;;   Override THIS file by placing a copy of it into "my-modules" then change
;;   your copy to suit your personal org-mode keywords and tags needs...
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Setup Modular Emacs Default Org-Mode Keywords:
;;  The above are based on general GTD schemes I am using...
;;  Note: "PHONE" has no key assignment. It gets handled elsewhere...
;;  (adjust this list to fit your own planning style) 

(setq org-todo-keywords
      (quote
       ((sequence "TODO(-)" "NEXT(+)" "|" "DONE(d!/!)")
	
        (sequence "ACTIVE(a)" "REPEATING(r)" "WAITING(w@/!)" "HOLD(h@/!)"
                  "|" "CANCELLED(q@/!)" "PHONE")
	
        ;; 2024-008-13: Added META (for Meta INFO: HOWTO, Cheatsheets, etc...)
	(sequence  "NEW(n)" "NOW(!)" "SOMEDAY(s)" "META(m)" "|" "ARCHIVED(v)")
	
        ;; 2023-005-15: Added these 4 new (for Software Dev - NEW & DONE are used here too)
	(sequence  "CODED(c)" "TESTING(t)" "DEBUGGED(b)" "ORIGINAL(o)"))))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Set Org TODO keyword faces:
;;  (don't settle for boring red, green, blue... Be creative!)
;;  INFO:  To lookup the list of Emacs colours that can be used by name use:
;;
;;                         M-x list-colors-display
;;
;;  NOTE: You can also do this for tag faces:
;;        Copy this form when you are ready to do that and add in your tags
;;        as new elements in the list...
;;
;;   (setq org-tag-faces
;;      (quote
;;       (
;;        ("TAG-NAME" . (:family "Hermit" :height 100 :foreground "red" :weight bold))
;;        Add More Tag Elements to this list using above form as example member...
;;        )))

(setq org-todo-keyword-faces
      (quote
       (("TODO" . (:family "Hermit" :height 100 :foreground "red" :weight bold))
        ("NEXT" . (:family "Hermit" :height 100 :foreground "BlueViolet" :weight bold))
        ("DONE" . (:family "Hermit" :height 100 :foreground "green2" :weight bold))
        ("ACTIVE" . (:family "Hermit" :height 100 :foreground "chocolate1" :weight bold))
        ("REPEATING" . (:family "Hermit" :height 100 :foreground "DeepSkyBlue" :weight bold))
        ("WAITING" . (:family "Hermit" :height 100 :foreground "lavender" :weight bold))
        ("HOLD" . (:family "Hermit" :height 100 :foreground "gray62" :weight bold))
        ("CANCELLED" . (:family "Hermit" :height 100 :foreground "SlateGray" :weight bold))
        ("PHONE" . (:family "Hermit" :height 100 :foreground "DarkOrange" :weight bold))
        ("NEW" . (:family "Hermit" :height 100 :foreground "DodgerBlue" :weight bold))
        ("NOW" . (:family "Hermit" :height 100 :foreground "HotPink" :weight bold))
        ("SOMEDAY" . (:family "Hermit" :height 100 :foreground "gold" :weight bold))
        ("META" . (:family "Hermit" :height 100 :foreground "gold" :weight bold))
	("ARCHIVED" . (:family "Hermit" :height 100 :foreground "AntiqueWhite" :weight bold))
        ;; 2023-005-15: Added these 3 new (for Software Dev - NEW & DONE are used here too)
        ("CODED" . (:family "Hermit" :height 100 :foreground "purple" :weight bold))
        ("TESTING" . (:family "Hermit" :height 100 :foreground "DarkOrange" :weight bold))
        ("DEBUGGED" . (:family "Hermit" :height 100 :foreground "cyan" :weight bold))
        ("ORIGINAL" . (:family "Hermit" :height 100 :foreground "yellow" :weight bold)))))

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   SPECIFIC UNIVERSAL TAGS & Other Custom Global Configurations Section:
;;   (change theses as needed for your own specific global tags)
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; TAGGING BEST PRACTICE:
;;
;; Ref: https://karl-voit.at/2022/01/29/How-to-Use-Tags/
;;
;; 1.  Use as few tags as possible. 100 tags Max - Much less tags are better...
;; 2.  Limit yourself to a self-defined set of tags. (use a controlled vocabulary)
;; 3.  Tags within your set must not overlap.
;; 4.  By convention, tags are in plural.
;; 5.  Tags are lower-case.
;; 6.  Tags are single words.
;; 7.  Keep tags on a general level. (e.g., sports instead of vollyball etc.)
;; 8.  Omit tags that are obvious:
;;     e.g., No Tags which can be derived from file extensions as follows:
;;      Bad Examples: images, spreadsheets, photographs, PDFs, etc...
;;      Recommended:  presentations instead of powerpoint, etc...
;; 9.  Use one tag language.
;; 10. Explain your tags.
;;
;; EXTRA:
;;
;;   Use mutually exclusive tags or datestamps instead of version numbers, etc.
;;   Mutually Exclusive means entering a tag will remove another tag that is set
;;   to be mutually exclusive of it. (and visa-versa) Bad Examples:  almost-final,
;;   final, finished, etc... Recommended Mutually Exclusive: draft|final,
;;   confidential|public|secret etc...
;;   There is ALSO TAG Hierarchy... (not implemented here YET)
;;   which looks quite useful for searching and filtering later with agendas.
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; DEFINE PERSISTENT GLOBAL TAGS for GENERAL USE:
;; (org-tag-persistent-alist)
;;
;; NOTE:
;;
;;   2025-006-07 - The TAG LIST BELOW has been greatly  
;;   shortened from previous iterations of this file...
;;
;;   For this GLOBAL LIST only Tags of the MOST GENERAL 
;;   NATURE and represent the Top Level of their category. 
;;   These tags must also be useful for most common cases.
;;
;;   More Specific Tags will be placed within xxx-tags.org 
;;   files which may be imported by any .org projects that 
;;   need specific tags from specific categories. The result 
;;   will be a much shorter choice of tags when assigning 
;;   them for headings in your docs...
;;
;;   The Tags below are available (persistent) to ALL 
;;   .ORG files... YOU MAY DISABLE These Tags on a per
;;   file basis by PUTTING: "#+STARTUP: noptag" at the top 
;;   of the file... (and then add a custom set of tags to
;;   that file alone)
;;
;; WARNING:
;;
;;   Do NOT use any keyboard keys which are already defined 
;;   below to define any NEW local TAGS for any specific 
;;   .org files... Your KEYS will NOT WORK as they were 
;;   already defined globally HERE
;;   (SEE: "Keys Used So Far:" BELOW)...
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; FIRST CLEAR ANY PREVIOUS LIST before Initializing below:
(setq org-tag-persistent-alist nil)

(setq
 org-tag-persistent-alist 
 (quote
   ;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ;; BEGIN: MUTUALLY EXCLUSIVE TAGS:
   ;; DEFAULT (persistent)  org mode tags for Mod Emacs

   ((:startgroup  . nil) ;; BEGIN: ME Group:
    ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~
    ;; Special Purpose EXPORT / NOEXPORT Flags
    ("noexport"   . ?n)  ;; EXCLUDE TREE from Export
    ("export"     . ?x)  ;; EXPORT This TREE
    (:endgroup    . nil) ;; END ME Group:
    ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~

    (:newline        . nil) ;; Put next tags on fresh new line...

    ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~
    ;; TOC-ORG TAGS:
    ;; (for Table Of Contents in Org Files, GitHub, etc.)
  
    (:startgroup) ;; BEGIN: ME Group
    ("TOC")        ;; TOC-Org DEFAULT 2 levels
    ("TOC_3")      ;; FLAG: TOC-Org (3 levels deep)
    (:endgroup)    ;; END: ME Group
    ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~

    ;; ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~
    ;; ;; SPECIAL TOC-ORG tags:
    ;; ;;
    ;; ;; NOTE: These Were REMOVED from PERSISTENT TAGS... They will
    ;; ;; Be placed within the default-tags.org SETUPFILE instead...
    ;; ;;
    ;; ;; Headings tagged with :noexport: will be excluded from the
    ;; ;; TOC. HOWEVER, If you want to preserve the heading, but
    ;; ;; strip its children (for changelog entries, for example),
    ;; ;; you can tag it :noexport_1: (by analogy, you can use
    ;; ;; :noexport_2:, :noexport_3:, etc. for children of deeper
    ;; ;; levels). Note, though, :noexport: has a similar meaning in
    ;; ;; org-mode, which I hope is a Good Thing (tm). However,
    ;; ;; :noexport_1: and friends won’t be recognized by org-mode
    ;; ;; as anything special. Look at org-export-exclude-tags
    ;; ;; variable for more detail...

    ;; (:startgroup)   ;; BEGIN: ME Group
    ;; ("noexport_1")  ;; FLAG: "TOC ORG 1 heading deep"
    ;; ("export_2")    ;; FLAG: "TOC ORG 2 headings deep"
    ;; ("export_3")    ;; FLAG: "TOC ORG 3 headings deep"
    ;; (:endgroup)     ;; END: ME Group
    ;; ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~

    ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~
    ;; GENERAL PURPOSE MUTUALLY EXCLUSIVE TAGS:
    ;; (Select one of these for each file's use-case)

    (:startgroup)     ;; BEGIN: ME Group
    ("home"     . ?h) ;;
    ("personal" . ?p) ;;
    ("work"     . ?w) ;;
    (:endgroup)       ;; END: ME Group

    (:startgroup)         ;; BEGIN: ME Group
    ("confidential" . ?C) ;; NON Encrypted CONFIDENTIAL
    ("public"       . ?P) ;; AVAILABLE to the public, etc...
    ("secrets"      . ?S) ;; ENCRYPTED SECRET Materials
    (:endgroup)           ;; END: ME Group

    ;; END: MUTUALLY EXCLUSIVE TAGS:
    ;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ;; BEGIN: PERSISTENT SINGLETON TAGS:
    ;; (USED by All HAP Modular Emacs org-mode files)
    ;;
    ;; These general purpose TAGS should be useful for 
    ;; most any use-case...

    ("archived"        . ?a) 
    ("default"         . ?d) 
    ("diy"             . ?y)
    ("installed"       . ?i)
    ("transportation"  . ?t)
    ("tools"           . ?T)
    (:newline          . nil))))

    ;; END: PERSISTENT SINGLETON TAGS
    ;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; KEYS USED SO FAR (above):
;;  a d h i n p t w x y C P S T   
;;
;; END: GLOBAL TAGS for GENERAL USE: (org-tag-persistent-alist)
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   END: [modular-emacs]:~/.emacs.d/lisp/modules/09-2-org-keywords-tags-conf.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
