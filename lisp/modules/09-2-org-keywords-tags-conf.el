;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   [modular-emacs]:~/.emacs.d/lisp/modules/09-2-org-keywords-tags-conf.el
;;
;;   This is a sub module of 09-org-mode-pkg-conf.el which takes care of all
;;   Org mode Keywords and TAGS...
;;
;;   Override this file by placing a copy of it into "my-modules" then change
;;   it to suit your personal org-mode keywords and tags needs...
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Setup my Default Org-Mode Keywords:
;;  The above are based on general GTD schemes I am using...
;;  Note: "PHONE" has no key assignment.  It gets handled elsewhere...
;;  (adjust this list to fit your own planning style)

(setq org-todo-keywords
      (quote
       ((sequence "TODO(t)" "NEXT(x)" "|" "DONE(d!/!)")
        (sequence "ACTIVE(a)" "REPEATING(r)" "WAITING(g@/!)" "HOLD(h@/!)"
                  "|" "CANCELLED(q@/!)" "PHONE")
	(sequence  "NEW(n)" "NOW(w)" "SOMEDAY(m)" "|" "ARCHIVED(v)")
        ;; 2023-005-15: Added these 4 new (for Software Dev - NEW & DONE are used here too)
	(sequence  "CODED(c)" "TESTING(s)" "DEBUGGED(b)" "ORIGINAL(o)"))))

;;;
;; Setup my Default Org-Mode Keywords: (with fancy UTF 8 symbols)
;; NOTE: I decided this was cluttering things up to much so I disabled it...
;(setq org-todo-keywords
;      (quote
;       ((sequence "☞ TODO(t)" "⚡ NEXT(x)" "|" "✔ DONE(d!/!)")
;        (sequence "ACTIVE(a)" "↺ REPEATING(r)" "⚑ WAITING(w@/!)" "⨂ HOLD(h@/!)"
;                  "|" "✘ CANCELLED(c@/!)" "PHONE(p)")
;	(sequence  "NEW(n)" "⦾ NOW(o)" "SOMEDAY(s)"  "|" "ARCHIVED(a)"))))

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
        ("ARCHIVED" . (:family "Hermit" :height 100 :foreground "AntiqueWhite" :weight bold))
        ;; 2023-005-15: Added these 3 new (for Software Dev - NEW & DONE are used here too)
        ("CODED" . (:family "Hermit" :height 100 :foreground "purple" :weight bold))
        ("TESTING" . (:family "Hermit" :height 100 :foreground "DarkOrange" :weight bold))
        ("DEBUGGED" . (:family "Hermit" :height 100 :foreground "cyan" :weight bold))
        ("ORIGINAL" . (:family "Hermit" :height 100 :foreground "yellow" :weight bold)))))

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   My Specific Universal TAGS & Other Custom Global Configurations Section:
;;   (change theses as needed for your own specific individual setup)
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Tagging Best Practice:
;;
;; Ref: https://media.ccc.de/v/GLT18_-_321_-_en_-_g_ap147_004_-_201804281550_-_the_advantages_of_file_name_conventions_and_tagging_-_karl_voit#t=1480
;;
;; - Use a controlled vocabulary and TAB-completion
;; - Not more than 100 tags maximum - the less the better
;; - Tags are always plural
;; - Keep Tags General  (e.g., sports instead of vollyball etc.)
;; - No Tags which can be derived from file extensions as follows:
;;      Bad Examples: images, spreadsheets, photographs, PDFs, etc...
;;      Recommended:  presentations instead of powerpoint, etc...
;; - No Tags that are obvious. (as above)
;; - Use mutually exclusive tags or datestamps instead of version numbers, etc.
;;      Mutually Exclusive means entering a tag will remove another tag that
;;      is set to be mutually exclusive of it. (and visa-versa)
;;      Bad Examples:  almost-final, final, finished, etc...
;;      Recommended Mutually Exclusive: draft|final, confidential|public|secret etc.

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Configure top-level universal tags with quick assign letters:
;;
;;    These are your quick universal tags that can be set for the 
;;    current headline within an .org file by entering:   C-c C-q  
;;    (and then typing one of the single letter keys listed below)
;;    Also:  You will be prompted after typing a colon ":" at the end
;;    of any headline, followed by typing M-TAB
;;
;;  Note 1:
;;    If you need to override this global org-tag-alist, for a specific
;;    file, and instead use a dynamic tag list, add an empty ‘TAGS’ keyword
;;    to that file:
;;      Example:
;;        #+TAGS:
;;
;;  Note 2:
;;    Also, to set specific tags assigned to a single file alone, use the
;;      "TAGS" keyword within the file itself to assign local tags...
;;      Example:
;;        #+TAGS: @work @home @tennisclub
;;        #+TAGS: laptop car pc sailboat
;;
;;  Note 3:
;;    If you have a preferred set of tags that you would like to use in every
;;    file, in addition to those defined on a per-file basis by the TAGS
;;    keyword, then you may specify a list of tags with the variable:
;;                          org-tag-persistent-alist
;;    You may turn this off on a per-file basis by adding a "STARTUP" keyword
;;    to that file:
;;      Example:
;;        #+STARTUP: noptag
;;    Making tags persistant does not seem that useful to me, but it is another option...
;;

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; DEFINE General AND GLOBAL Tags for EVERYTHING ELSE:
;;
;; NOTE: 2021-003-10 - This general tag list has been greatly shortened...
;;       Instead, I am now using Org Categories with Tag Files of specific
;;       categories instead.  These xxx-tags.org files are imported by .org
;;       files that need specific tags from specific categories. The result
;;       is a much shorter general-tags list below... "Keep it Simple Stupid"
;;
;;       The Tags below MUST be available to ALL .ORG files for proper
;;       operation of specialized features of Harmonic Alchemy Modular
;;       Emacs "fancy org mode" kinds of things...
;;
;;       WARNING: Do NOT use any keyboard keys which are already defined
;;                below to define any NEW local TAGS for any specific .org
;;                files... Your KEYS will NOT WORK as they were already
;;                defined globally HERE (see Keys Used So Far: below)...

(defvar me--general-tags
  (quote

   ;; Mutually Exclusive Tags:

   ((:startgroup     . nil) ;; BEGIN: Mutually Exclusive Group:
    ("noexport"      . ?n) ;; FLAG: "Exclude Outline Tree from Export"
    ("export"        . ?x) ;; FLAG: "Export Outline Tree"
    (:endgroup       . nil) ;; END Group:

    (:startgroup     . nil) ;; BEGIN: Mutually Exclusive Group:
    ("@home"         . ?H) ;; FLAG: (personal stuff, banking, health, etc.)
    ("@work"         . ?W) ;; FLAG: (devOps, sysAdmin, clients, community, sales)
    (:endgroup       . nil) ;; END Group:

    (:startgroup     . nil) ;; BEGIN: Mutually Exclusive Group:
    ("not_in_ed_cal" . ?0)  ;; FLAG: "NOT In Editorial Calendar"
    ("in_ed_cal"     . ?1)  ;; FLAG: "In Editorial Calendar"
    (:endgroup       . nil) ;; END Group:

    (:startgroup     . nil) ;; BEGIN: Mutually Exclusive Group:
    ("TOC"           . ?2)  ;; FLAG: TOC-Org (DEFAULT 2 levels deep)
    ("TOC_3"         . ?3)  ;; FLAG: TOC-Org (3 levels deep)
    (:endgroup       . nil) ;; END Group:

    (:startgroup     . nil) ;; BEGIN: Mutually Exclusive Group:
    ("published"     . ?+)  ;; FLAG: "IS Published"
    ("unpublished"   . ?-)  ;; FLAG: "NOT Published"
    (:endgroup       . nil)))) ;; END Group:

    ;; Keys Used So Far:
    ;; n x H W 0 1 2 3 + -

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Append all TOP LEVEL tag lists together for org-mode quick set tags....

;(setq org-tag-alist (append me--general-tags nil))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Reset Org Tag Persistent alist with General Tags: 

(setq org-tag-persistent-alist nil) ;; Start with empty list...

(setq org-tag-persistent-alist (append me--general-tags nil))

;;;
;; Here is some code I snarfed from the web that may help simplify the above...
;; The code below is not finished... It is supposed to save org-tag-alist to a file.
;; I am not sure if I need to save to a file, as I am doing the opposite; i.e.,
;; reading this file and then loading org-tags-alist with data stored here...
;; This function may be modified to do something else though... Don't delete it yet...

;; (defun me_org-persist-new-tags ()
;;   (interactive)
;;   (let ((known-tags (append org-tag-persistent-alist org-tag-alist))
;;         (item-tags (split-string (org-get-tags))))
;;     (setq org-tag-alist
;;           (append org-tag-alist
;;                   (-filter (lambda (tag) (assoc tag known-tags)) item-tags)))
;;     ;; TODO: write out to a file the org-tag-alist
;;     ))

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   END: [modular-emacs]:~/.emacs.d/lisp/modules/09-2-org-keywords-tags-conf.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
