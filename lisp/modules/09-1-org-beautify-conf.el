;;
;; Change Log: (descending chronological order)
;;

;; 2023-006-14 - Alisha Awen, siren1@disroot.org
;;   Cleaned up some old code and made some slight tweaks...

;; 2022-009-28 - Alisha Awen, siren1@disroot.org
;;   Removed Org-Bullets... This has been commented out for a while...
;;   I am NOT using org-bullets and only used them for a short while in
;;   the beginning BEFORE I set up my Fancy Org Mode which HIDES all bullets
;;   anyway... (except for the top most level)... Therefore org-bullets has
;;   always been a mute thing here...
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(require 'org-faces)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Set Fallback Font:
;;  This is critical to ensure unicode symbols always
;;  display correctly...
;;  Note: Setting 'nil' below makes it the fallback
;;        font.  Other values allow you to set a range
;;        of gliphs...
;;  Reference:
;;  http://endlessparentheses.com/manually-choose-a-fallback-font-for-unicode.html

(set-fontset-font "fontset-default" nil
                  (font-spec :size 20 :name "Symbola"))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  NOTE: MY HOME SPUN MINIMAL ORG-BULLETS BELOW
;;        HAS BEEN REPLACED BY ORG-SUPERSTAR...
;;        This is kept here commented out for reference only...
;;
;;  Use Fancy Stars for Outline Headings in Org mode:
;;
;;  This function turns normal text stars (asterisks)
;;  into alternate fancy characters (different on each level)
;;
;;  However, by default leading stars are hidden so you will
;;  most likely see ONLY the Main Heading bullet (which is
;;  currently a solid circle (rather than *).
;;
;;  This function is run by the Modular Emacs org-mode hook 
;;  function that gets executed during the org-mode start up loop...
;;
;;  Because I don't really care about leading stars (most of the
;;  time) This function was created as a kind of poor-mans
;;  "org-bullets"... (AND ITS WAY MUCH SIMPLER TOO)
;;
;;  If you want to see all the other pretty stars (OMG!) then
;;  manually DISABLE INDENT while in org mode, then trailing
;;  stars will automagically appear...
;;
;;  NOTE: Sometimes you will see the DEFAULT * for some reason...
;;        This only happens when emacs is first started...
;;        The work-around is simply perform C-c C-c on the first
;;        startup block in the first org file you open after starting
;;        Emacs... All other org files opened after your first fix,
;;        will be fine...
;;
;;  Final Note: IMHO, Trailing Stars and Headings that are NOT indented
;;              (as OUTLINES are SUPPOSED TO BE) makes reading .org files
;;              much harder... In the spirit of "Elements of Style" Lets
;;              NOT do that...
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;; THIS FUNCTION IS NO LONGER USED...
;(defun me_org-fancy-stars ()
;  "Hide the Org Bullets..."
;  (interactive)
;  (font-lock-add-keywords
;   'org-mode `(("\\(?:^\\(?1:\\*+\\)[[:blank:]]\\)"
;              (0 (progn (compose-region
;                         (match-beginning 1) (match-end 1)
;                         (pcase (length (match-string 1))
;                           (1 ?\u2219)
;                           (2 ?\u2022)
;                           (3 ?\u25c9)
;                           (_ ?\u25CB)))
;                        nil))))))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ORG SUPERSTAR to the rescue...  My ad-hoc replacement
;; to the retired org-bullets was not so great...
;; Then I discovered org-superstar (a complete rewrite
;; of org-bullets... Lets see how that works...
;; WOW.. I finally got my outline and list views looking NICE!

(require 'org-superstar)

;; CUSTOMIZE Headline Leading Bullets for each level: (10 levels!)

(setq org-superstar-headline-bullets-list
      (quote ((?➀) (?➁) (?➂) (?➃) (?➄) (?➅) (?➆) (?➇) (?➈) (?➉))))

(setq org-superstar-cycle-headline-bullets t)

;; Automatically demote / promote list items with different characters...
;; As above, You can change (replace) bullet glifs (left column) to others
;; that you may prefer instead...

(setq org-superstar-item-bullet-alist
      (quote
       ((?* . ?☲)
	(?+ . ?⛮)
	(?- . ?⚛))))

;; Bullets to Try:
;; ☆ ☘ ☛ ☞ ☟ ☩ ☯ ⛛
;; ☰ ☲ ☷ ☵ ☸ ☼ ♲
;; ♳ ♻ ♾ ⚛ ⛦ ⛮ ⛯ ⛋
;; ⬘ ⬙ ⭓ ⮕ ★ ☣ ⚑ ⛿ ⚒ 

(setq org-superstar-prettify-item-bullets t)

;; Set Maximum Indentation For Org-Mode Description Lists
;; to get them lined up nicely under their ITEM BULLETS...
;; You Need Space to push them beyond a checkbox (when you add those)
;; (I tried 5 first... did not feel like enough space, but changing it
;; does not do anything???)

(setq org-list-description-max-indent 5)

;; Increase offset a wee bit more between plain list levels...
;; I increased it to help checklists sub text line up better...
;; 2025-003-03: Changed offset to 5 (to see if it is better or worse)

(setq-default org-list-indent-offset 3)

;; You have to call: org-superstar-restart
;; after changing any of the bullets above...

(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Org ellipsis options, other than the default Go to Node...
;; not supported in common font, but supported in Symbola font. ⬎, ⤷, ⤵
;;

;; I LOVE This One:

(setq org-ellipsis "⤵")

;; Open Org Files INITIALLY FOLDED in OVERVIEW Mode:
(setq org-startup-folded 'overview)

;; TELL org-mode to AUTO INSERT SINGLE BLANK LINES AROUND HEADINGS...
;; I disabled this... I don's remember why I did it.. it may be
;; useless... Will remove later...
;(setq org-ascii-headline-spacing (quote (1 . 1)))

;; Open Org files with HEADINGS INDENTED BY DEFAULT...
(setq org-startup-indented t)

;; Prevent Org-Mode Demoting Heading Also Shifting Text Inside Sections:
(setq org-adapt-indentation nil)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; More TWEAKS... (See the Manual, The Symbol Names Tell All)

;;  Hide the Emphasis Markup:
;;  (e.g., /.../ for italics, *...* for bold, etc.)
(setq org-hide-emphasis-markers t)

;; Turn ON Source Block Syntax highlighting and adjust tabs as per language...
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(setq org-cycle-separator-lines 0)

;; Insert Blank lines before or after headings?
;; auto flag = Do the right thing (depending on context)
;; This may actually be org-mode default... (oh well)

(setq org-blank-before-new-entry
      (quote
       ((heading . auto)
	(plain-list-item . auto))))

(setq org-insert-heading-respect-content nil)

(setq org-reverse-note-order nil)

(setq org-show-following-heading t)

(setq org-show-hierarchy-above t)

(setq org-show-siblings (quote (default)))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Show nice chosen symbols instead of a dash in bulleted lists.
;; If you don't like these, either disable the next two forms or
;; change the characters at the end of each expression to something
;; that fits your style...

;(font-lock-add-keywords
; 'org-mode
; '(("^ *\\([-]\\) "
;    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "►" ))))))

;(font-lock-add-keywords
; 'org-mode
; '(("^ *\\([+]\\) "
;    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "🞜"))))))

;(setq org-list-demote-modify-bullet
;      (quote (("+"  . "-")
;              ("-"  . "+")
;              ("🞜"  . "-")
;              ("1." . "-")
;              ("1)" . "-"))))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Use Unicode Symbols To Display Org-Mode Checkboxes:

(add-hook 'org-mode-hook
	  (lambda ()
	    (setq prettify-symbols-alist
		  '(("->"     . ?➔)
		    ("/="     . ?≠)
		    ("!="     . ?≠)
		    ("=="     . ?≡)
		    ("<="     . ?≤)
		    (">="     . ?≥)
		    ("sqrt"   . ?√)
		    ("sqrt"   . ?√)
		    ("[ ]"    . ?☐)
		    ("[X]"    . ?☑)
		    ("[-]"    . ?☢))))
	  (prettify-symbols-mode 1))

;; Push to list...
(defun me_set-unicode-checkboxes ()
  "Use Unicode Symbols To Display Org-Mode Checkboxes"
  (interactive)
  (progn
    (push '("[ ]" .  "☐") prettify-symbols-alist)
    (push '("[X]" . "☑") prettify-symbols-alist)
    (push '("[-]" . "☢") prettify-symbols-alist)
    (prettify-symbols-mode)))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Customize look of emphasized text in org-mode 
;; This is for how markup will look in an org-mode
;; buffer.  Not what it may or may not look like
;; after exported to HTML or LaTeX etc...
;;
;; Experiment with this until you have something
;; you like... so far it's angry fruit salad! ;-)
;; It took me so long to get around to fixing
;; this... So now I will live with these settings
;; until they drive me crazy!  LOL
;;
;; For a nice visual reference of ALL available Emacs Colors
;; See: http://www.raebear.net/computers/emacs-colors/
;; (Jessica's Corner of Cyberspace) Thanks! Jessica

(setq org-emphasis-alist
      '(
	;; *ORG-BOLD*
	("*" (bold
              :foreground "DeepSkyBlue" ))          

	;; /ORG-ITALIC/
        ("/" (italic
              :family "Courier Prime"
              :slant italic
              :foreground "PaleGreen")) 

	;; _ORG-UNDERLINE_
        ("_" (
              :underline (
                          :color foreground-color
                          :style line)))            

	;; =ORG-VERBATIM=
        ("=" (
              :family "Hermit"
              :background "aquamarine4"
              :foreground "GhostWhite"))

        ;; ~ORG-CODE~
        ;;
        ;; Backgrounds: GhostWhite honeydew Ivory
        ;;
        ;; Foregrounds: red red4 DarkRed firebrick DarkGreen chocolate sienna
        ;;              SaddleBrown blue MidnightBlue aquamarine4
	;;              OrangeRed, DarkOrange
        ("~" (
              :family "Hermit"
              :background "honeydew"
              :foreground "firebrick"))             ;; ~org-code~

        ("+" (:strike-through t))))                 ;; +strike-through+

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Customize Org headings:
;; The fonts listed below will be tried in sequence,
;; and the first one found will be used...
;;
;; The inspiration for this rather complicated eLisp code comes from:
;;   "Beautifying Org Mode in Emacs"
;;      https://zzamboni.org/post/beautifying-org-mode-in-emacs/

(let* ((variable-tuple
        (cond
         ((x-list-fonts "Averia Libre")
          '(:font "Averia Libre"))
         ((x-list-fonts "Averia Libre Light")
          '(:font "Averia Libre Light"))
         ((x-list-fonts "Averia Sans Libre Regular")
          '(:font "Averia Sans Libre Regular"))
         ((x-list-fonts "Averia Sans Libre Light")
          '(:font "Averia Sans Libre Light"))
         ((x-list-fonts "Averia Serif Libre Regular")
          '(:font "Averia Serif Libre Regular"))
         ((x-list-fonts "Averia Serif Libre Light")
          '(:font "Averia Serif Libre Light"))
         (nil (warn "Averia Fonts Not Found! Did you install them?"))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit 'default )))

  (custom-theme-set-faces
   'user
   `(org-level-10 ((t (,@headline ,@variable-tuple :height 1.11 :foreground "AntiqueWhite"))))
   `(org-level-9 ((t (,@headline ,@variable-tuple :height 1.11 :foreground "AntiqueWhite"))))
   `(org-level-8 ((t (,@headline ,@variable-tuple :height 1.13 :foreground "AntiqueWhite" ))))
   `(org-level-7 ((t (,@headline ,@variable-tuple :height 1.17 :foreground "AntiqueWhite" ))))
   `(org-level-6 ((t (,@headline ,@variable-tuple :height 1.20 :foreground "AntiqueWhite" ))))
   `(org-level-5 ((t (,@headline ,@variable-tuple :height 1.23 :foreground "AntiqueWhite" ))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.28 :foreground "AntiqueWhite" ))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.38 :foreground "AntiqueWhite" ))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.62 :foreground "AntiqueWhite" ))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.88 :foreground "AntiqueWhite" ))))
   
   `(org-document-title
     ((t
       (,@headline ,@variable-tuple :height 1.88 :foreground "AntiqueWhite" :underline nil))))))

;; Use this to get Org Tables aligned properly...

;; Add any needed code outside os customize - HERE

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   END: [modular-emacs]:~/.emacs.d/lisp/modules/09-1-org-beautify-conf.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
