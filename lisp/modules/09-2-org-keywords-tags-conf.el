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
;;  NOTE:  You can also do this for tag faces:
;;        Copy this form when you are ready to do that and add in your tags as a new element in the list...
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
;;    @HA                 "h"
;;    @HAP                "p"
;;    @EA                 "e"
;;    @OMAN               "o"
;;    @MWM                "m"
;;    @SOI                "s"
;;    @MCM                "c"
;;    @DOCS               "d"
;;    @Qubes              "q"
;;    @Alisha             "a"
;;    Family              "f"
;;    @HAP-Clients        "i"
;;    @MWM-Clients        "n"
;;    @LIBRARY            "l"

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Create separate lists for TOP Level Tags
;;  These will be appended later to create the full
;;  org-tags-alist...

;; Create TOP LEVEL TAGS (specific tags for different parts of my life)
;; Note:  These tags are mutually exclusive...

(defvar me--top-tags
  (quote
   (("@DOCS" . ?d)    ;; General Docs
    ("@Qubes" . ?q)   ;; Qubes OS - VM compartmentalization management...
    ("@Alisha" . ?a)  ;; Me %^)  (change to your name)
    ("@Family" . ?f)  ;; My Relatives...  (or yours - keep it)
    (:startgroup)     ;; BEGIN mutually exclusive tags:
    ("@HA" . ?h)         ;; Harmonic Alchemy
    ("@HAP" . ?p)        ;; Harmonic Alchemy Productions
    ("@EA" . ?e)         ;; Emergent Anomalies eZine, podcast
    ("@OMAN" . ?o)       ;; Open Media Arts Metwork
    ("@MWM" . ?m)        ;; Marketing Web Media
    ("@SOI" . ?s)        ;; Sustainable Orcas Island
    ("@MCM" . ?c)        ;; Marketing Content Media
    (:endgroup)       ;; END mutually exclusive tags...
   )))                ;; END me--top-tags

;;  NOTE: TOP LEVEL TAGS above stands alone...
;;        It does not depend on any of the lists below...


;;  Create @HAP_Clients Top Level Tags Group:

(defvar me--hap-clients-tags
  (quote
   ((:startgrouptag)          ;; BEGIN TOP LEVEL @HAP_Clients Tag Group:
    ("@HAP_Clients" . ?i)
    (:grouptags)
    (:startgroup)                 ;; BEGIN Mutually Exclusive @HAP_Clients group members:
    ("Carolyn_Cruso")
    ("Turtleback")
    ("Samara_Shaw")
    ("Elder_Pritchard")
    ("Matthew_Sheppard")
    ("Randy_Smith")
    ("Fun_House")
    (:endgroup)                   ;; END Mutually Exclusive @HAP_Clients group members:
    (:endgrouptag))))         ;; END HAP_Clients Tag Group

;;  NOTE: The TOP LEVEL @HAP_Clients Tag Group above stands alone...
;;        It does not depend on any of the lists below...


;;  Create @MWM_Clients Top Level Tags Group:

(defvar me--mwm-clients
  (quote
   ((:startgrouptag)          ;; BEGIN TOP LEVEL @MWM_Clients Tag Group:
    ("@MWM_Clients" . ?n)
    (:grouptags)
    (:startgroup)                 ;; BEGIN Mutually Exclusive @MWM_Clients group members:
    ("Heather_Wolfe")
    ("Jim_Litch")
    ("Tom_Ely")
    ("UNCA")
    ("Kyria_Mystica")
    (:endgroup)                   ;; END Mutually Exclusive @MWM_Clients group members:
    (:endgrouptag))))         ;; END @MWM_Clients Tag Group

;;  NOTE: The TOP LEVEL @MWM_Clients Tag Group above stands alone...
;;        It does not depend on any of the lists above or below...


;;  Create TOP LEVEL General Tags for everything else:
;; NOTE:   Possibly break these up into separate categories
;;         to use selectively directly within related .org files
;;         where they may be loaded via eLisp code blocks at the
;;         top of the .org files...

(defvar me--general-tags
  (quote
   (("Gen💡Ops")
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

    ("Info✴Sec")
    ("GnuPG")
    ("OpenSSL")
    ("passwords")
    ("OpenSSH")

    ("Market$Ops")

    ("Graphic_Arts")

    ("Writing_Resources")
    ("Esoteric")
    ("HarmonicAlchemy")
    ("Shamanism")
    ("sound_healing")
    ("fringe_science")
    ("paranormal")
    ("parallel_worlds")
    ("simulation_hypothesis")
    ("seti")
    ("ufo")
    ("time_travel")
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
    ("cheatsheets")
    ("diagrams")

    ("GenRef")

    ("Productions")
    ("pre_production")
    ("post_production")
    ("audio")
    ("music_audio_tech")
    ("music_instruments")
    ("codecs")
    ("audio_equipment")
    ("audio_theatre")
    ("podcast")
    ("animation")
    ("Blender")
    ("VR")
    ("Kitely")
    ("Second_Life")
    ("OpenSim")
    ("Sinespace")
    ("Unity")

    ("Science")
    ("fringe_science")
    ("emotional_intelligence")
    ("progLangs")
    ("Lisp")
    ("eLisp")
    ("Nikola_Tesla")
    ("permaculture")
    ("seasteading")
    ("seed_freedom")
    ("sustainability")
    ("economics")
    ("cryptocurrency")
    ("government")
    ("politics")
    
    ("Composing")
    ("arranging")
    ("orchestration")
    ("notation")
    ("film_scoring")
    ("sound_design")
    ("Music_Apps")

    ("@TAGS")
    ("hashtags")
    ("how_to")
    ("logs")
    ("scratchpads")
    ("photos")
    ("quotes")
    ("README")
    ("templates")
    ("tools")
    ("transactions")
    ("tutorials")
    ("videos")

    ("Friends")
    ("Personal")

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


;; Create TOP LEVEL TAROT TAG GROUP for my Tarot Guide.
;; NOTE:  Possibly put these directly within Tarot & Mystical
;;        related .org files instead...

(defvar me--tarot-tags
  (quote
   ((:startgrouptag)          ;; BEGIN TOP LEVEL - @Tarot Tag Group:
    ("@Tarot")
    (:grouptags)
    (:startgroup)             ;; BEGIN TOP LEVEL @Tarot mutually exclusive groups:

    ("tarot_general")

    ("tarot_readings")

    (:startgrouptag)              ;; BEGIN @Tarot sub group - Major_Arcana Tag Group:
    ("Major_Arcana")
    (:grouptags)
    (:startgroup)                     ;; BEGIN Major_Arcana mutually exclusive group:
    ("Majarc_00_Fool")
    ("Majarc_01_Magician")
    ("Majarc_02_High_Priestess")
    ("Majarc_03_Empress")
    ("Majarc_04_Emperor")
    ("Majarc_05_Hierophant")
    ("Majarc_06_Lovers")
    ("Majarc_07_Chariot")
    ("Majarc_08_Justice_Strength")
    ("Majarc_09_Hermit")
    ("Majarc_10_Wheel_of_Fortune")
    ("Majarc_11_Strength_Justice")
    ("Majarc_12_Hanged_Man")
    ("Majarc_13_Death")    
    ("Majarc_14_Temperance")
    ("Majarc_15_Devil")
    ("Majarc_16_Tower")
    ("Majarc_17_Star")
    ("Majarc_18_Moon")
    ("Majarc_19_Sun")
    ("Majarc_20_Judgment")
    ("Majarc_21_World")
    (:endgroup)                       ;; END mutually exclusive group:
    (:endgrouptag)                ;; END Major-Arcana Tag Group

    (:startgrouptag)              ;; BEGIN @Tarot sub group -  Minor_Arcana Tag Group:
    ("Minor_Arcana")
    (:grouptags)
    (:startgroup)                     ;; BEGIN Minor_Arcana mutually exclusive group:
    ("minArc_Cups")
    ("minArc_Pentacles")
    ("minArc_Swords")
    ("minArc_Wands")
    (:endgroup)                       ;; END mutually exclusive group:
    (:startgroup)                     ;; BEGIN Minor_Arcana mutually exclusive group:
    ("minArc_King")
    ("minArc_Queen")
    ("minArc_Knight")
    ("minArc_Page")
    ("minArc_Ace")
    ("minArc_02")
    ("minArc_03")
    ("minArc_04")
    ("minArc_05")
    ("minArc_06")
    ("minArc_07")
    ("minArc_08")
    ("minArc_09")
    ("minArc_10")
    (:endgroup)                       ;; END mutually exclusive group:
    (:endgrouptag)                ;; END Minor Arcana Tag Group

    (:endgroup)               ;; END TOP LEVEL @Tarot mutually exclusive groups:
    (:endgrouptag)            ;; END TOP LEVEL @Tarot Tag Group
   ))) ;; END Tarot

;;  NOTE: The TOP LEVEL Tarot Tag Group above stands alone...
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
    ("Arthour_C_Clark")
    ("Isaac_Asimov")
    ("Ray_Bradbury")
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
                     me--hap-clients-tags
                     me--mwm-clients
                     nil))

;;;
;;  Append these general tags to Org Tag Persistent alist

(setq org-tag-persistent-alist (append
                                me--general-tags
                                me--tarot-tags
                                me--library-tags
                                nil))


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
