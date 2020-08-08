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
        (sequence "ACTIVE(a)" "REPEATING(r)" "WAITING(w@/!)" "HOLD(h@/!)"
                  "|" "CANCELLED(c@/!)" "PHONE")
	(sequence  "NEW(n)" "NOW(o)" "SOMEDAY(s)"  "|" "ARCHIVED(v)"))))

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
;;        as a new element in the list...
;;
;(setq org-tag-faces
;      (quote (
;              ( "TAG-NAME" . (:family "Hermit" :height 100 :foreground "red" :weight bold))
;              ;; Add More Tag Elements to the list...
;              ;;
;              )))

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
        ("ARCHIVED" . (:family "Hermit" :height 100 :foreground "AntiqueWhite" :weight bold)))))

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   My Specific Universal TAGS & Other Custom Global Configurations Section:
;;   (change theses as needed for your own specific individual setup)
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
;; Tag Hierarchy EXAMPLE:
;;
;;  (setq org-tag-alist
;;      (quote
;;       ((:startgrouptag)
;;        ("GTD")
;;        (:grouptags)
;;        ("Control")
;;        ("Persp")
;;        (:endgrouptag)
;;        (:startgrouptag)
;;        ("Control")
;;       )))

;; My Global TAGS: (hierarchical list)
;;
;; NOTE: Single keys are assigned to the following
;;       TOP LEVEL Group Tags for quick access:
;;       (see dotted pairs (cons cells) below in org-tag-alist)
;;
;;    @Qubes              "q"
;;    @Alisha             "a"
;;    Family              "f"
;;    @LIBRARY            "l"

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Create separate lists for TOP Level Tags
;;  These will be appended later to create the full
;;  org-tags-alist...

;;;
;; Create TOP LEVEL TAGS (specific tags for different parts of my life)
;; Note:  These tags are mutually exclusive...

(defvar me--top-tags
  (quote
   (("@DOCS" . ?d)    ;; General Docs
    ("@Qubes" . ?q)   ;; Qubes OS - VM compartmentalization management...
    ("@Alisha" . ?a)  ;; Me %^)  (change to your name)
    ("@Family" . ?f)  ;; My Relatives...  (or yours - keep it)
   )))                ;; END me--top-tags

;;;
;; Create TOP LEVEL General Tags for everything else:
;;
;; NOTE: There are way too many tags below... And that is my public list!
;;       I have overridden this file in /my-modules/ with even more tags!
;;       I don't expect to use all of these in any practical way via any
;;       pop-up HELM views etc.  There are just too many to make that useful!
;;
;;       If you find any of the tags below useful to you, it may be wise to break
;;       them up into categories or use them as file tags for specific groups of
;;       files, or load them via eLisp code blocks, etc. That way you will have
;;       a way to use different agendas designed to match a subset group
;;       containing no more than say 10 or 15 tags for each category, max.
;;
;; The List below has a long history!  I started this list by using tags in
;; Firefox to organize bookmarks maybe 15 or so years ago... I think it was
;; still called Netscape back then actually...  The list migrated later to
;; Evernote, (which was way cool because the tags were hierarchical).  I wrote
;; an article about that wishing for a file system to designed like that.  No...
;; Apple did not do it... They have a tag system in the Finder, but it is lame...
;;
;; Later, I ditched Evernote for simple Markdown Notes in Emacs, using DEFT
;; for organization.  That lasted for about a year... I knew I would eventually
;; be doing this in org-mode but I had a lot to learn first... And I still have
;; a LOT to learn... This stuff is vast! OMG!
;;
;; The real reason I put all these tags in here is to maintain my master list
;; of tags, #Hashtags, etc. for everything I do, with reasonable consistency in
;; one single place where I have programmatic control over them to help keep them
;; unique and not duplicated with different versions/spellings of the same tag!
;;
;; Lisp is the best place, for things like that. Eventually I will come up with an
;; efficient scheme in Org-Mode to manage all of them with the ability to do
;; quick lookups and assignments.  Many of which will of course be automatic,
;; based on work processes...

(defvar me--general-tags
  (quote

   ;; Major Categories:

   (("GenRef")
    ("Gen💡Ops")
    ("Pub✎Ops")
    ("Dev§Ops")
    ("git")
    ("Sys§Admin")
    ("Research")
    ("Blog_Ideas")
    ("New_Ideas")
    ("Projects")
    ("GTD")
    ("Autofocus")

    ;; Personal:

    ("Friends")
    ("Personal")
    ("photos")

    ;; Reference - Research

    ("Science")
    ("emotional_intelligence")
    ("Nikola_Tesla")
    ("permaculture")
    ("seasteading")
    ("seed_freedom")
    ("sustainability")
    ("economics")
    ("cryptocurrency")
    ("government")
    ("politics")

    ;; Tech Stuff:

    ("progLangs")
    ("Lisp")
    ("eLisp")
    ("apps_tools")
    ("Emacs")
    ("email")
    ("Email_SMTP_IMAP")
    ("hosting")
    ("hosts")
    ("hypervisor")
    ("Mac_OS")
    ("MultiMarkdown")
    ("NODE")
    ("NPM")
    ("NVM")
    ("open_source_cloud")
    ("open_source_ISP")
    ("Org_Mode")
    ("SBCL")
    ("sh")
    ("shell")
    ("unix")
    ("www")
    ("VPS")
    ("Xah_Fly_Keys")
    ("zsh")

    ;; Security Related:

    ("Info✴Sec")
    ("GnuPG")
    ("OpenSSL")
    ("passwords")
    ("OpenSSH")

    ;; Business - Marketing:

    ("Market$Ops")
    ("sales")
    ("invoices")
    ("bills")
    ("transactions")
    ("timesheets")
    ("banking")
    ("legal")

    ;; Creative - Design:

    ("Graphic_Arts")
    ("animation")
    ("Blender")
    ("images")
    ("svg")
    ("inkscape")
    ("gimp")

    ;; Writing Publishing:

    ("Writing_Resources")
    ("journalism")
    ("copyright")
    ("drafts")
    ("synopsis")
    ("published")
    ("unpublished")
    ("not_in_ed_cal")
    ("in_ed_cal")
    ("screenwriting")
    ("fonts")
    ("Fountain")
    ("symbolism")

    ;; Productions - Games:

    ("Productions")
    ("pre_production")
    ("post_production")
    ("audio")
    ("music_audio_tech")
    ("codecs")
    ("audio_equipment")
    ("audio_theatre")
    ("podcast")
    ("VR")

    ;; Music - Sound Design:

    ("Composing")
    ("arranging")
    ("orchestration")
    ("notation")
    ("film_scoring")
    ("sound_design")
    ("Music_Apps")
    ("music_instruments")

    ;; General Purpose:

    ("@TAGS")
    ("hashtags")
    ("cheatsheets")
    ("diagrams")
    ("how_to")
    ("logs")
    ("scratchpads")
    ("quotes")
    ("README")
    ("templates")
    ("tools")
    ("presets")
    ("tutorials")
    ("videos")

    ;; Culinary Arts:

    ("Culinary")
    ("baking")
    ("bread")
    ("brewing")
    ("chicken")
    ("falafel")
    ("frying")
    ("herbs_spices")
    ("lamb")
    ("pastries")
    ("probiotics")
    ("recipes")
    ("sourdough")
    ("stews")
    ("wild_levain")))) ;; END GENERAL TAGS

;;  NOTE: The TOP LEVEL GENERAL TAGS above stands alone...
;;        It does not depend on any of the lists above or below...

;; Create TOP LEVEL @LIBRARY TAG GROUP:
;; NOTE:  Possibly put these directly within "Books & Library"
;;        related .org files instead...

(defvar me--library-tags
  (quote
   (("NEWS")
    ("fiction")
    ("reference")
    ("biography")
    ("humorous")
    ("comic_books")
    ("graphic_novels")
    ("stories")
    ("mystery")
    ("sci_fi")

    (:startgrouptag)          ;; BEGIN TOP LEVEL - @LIBRARY Tag Group:
    ("@LIBRARY" . ?l)
    (:grouptags)

    (:startgroup)             ;; BEGIN Mutually Exclusive Group Tags
    ("Books")
    ("Abstracts")
    ("Articles")
    ("Papers")
    ("Periodicals")
    (:endgroup)               ;; END Mutually Exclusive Group Tags
    
    (:startgrouptag)              ;; BEGIN @LIBRARY Sub Group - Authors Tag Group:
    ("Authors")
    (:grouptags)
    (:startgroup)                 ;; BEGIN Mutually Exclusive Group Tags
    ("Mark_Twain")
    ("Charles_Dickens")
    ("Jonathan_Swift")
    ("Arthour_C_Clark")
    ("William_Butler_Yeats")
    ("George_Bernard_Shaw")
    ("T_S_Eliot")
    ("Isaac_Asimov")
    ("Ray_Bradbury")
    ("George_Orwell")
    ("Aldous_Huxley")
    ("Philip_K_Dick")
    ("Robert_Heinlein")
    ("Orson_Welles")
    ("Alfred_Hitchcock")
    (:endgroup)                   ;; END Mutually Exclusive Group Tags
    (:endgrouptag)                ;; END Authors Tag Group

    (:endgrouptag)         ;; END TOP LEVEL @LIBRARY Tag Group:
   ))) ;; END me--library-tags

;;  NOTE: The TOP LEVEL @LIBRARY Tag Group above stands alone...
;;        It does not depend on any of the lists above or below...

;;;
;;  Append all TOP LEVEL tag lists together for org-mode quick set tags....

(setq org-tag-alist (append
                     me--top-tags
                     nil))

;;;
;;  Append these general tags to Org Tag Persistent alist

(setq org-tag-persistent-alist (append
                                me--general-tags
                                me--library-tags
                                nil))

;;;
;; Here is some code I snarfed from the web that may help simplify the above...
;; The code below is not finished... It is supposed to save org-tag-alist to a file.
;; I am not sure if I need to save to a file, as I am doing the opposite; i.e.,
;; reading this file and then loading org-tags-alist with data stored here...
;; This function may be modified to do something else though... Don't delete it yet...

(defun me_org-persist-new-tags ()
  (interactive)
  (let ((known-tags (append org-tag-persistent-alist org-tag-alist))
        (item-tags (split-string (org-get-tags))))
    (setq org-tag-alist
          (append org-tag-alist
                  (-filter (lambda (tag) (assoc tag known-tags)) item-tags)))
    ;; TODO: write out to a file the org-tag-alist
    ))

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   END: [modular-emacs]:~/.emacs.d/lisp/modules/09-2-org-keywords-tags-conf.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
