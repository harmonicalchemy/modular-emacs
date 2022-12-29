;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/09-org-mode-pkg-conf.el
;;
;; This module adds extra tools for customizing org-mode for specific purposes
;; which can be vast!!! For starters, my current logging (which I currently do
;; in markdown mode, is very limited.  org-mode offers a total improvement that
;; makes using traditional PIM Apps look like toys! Org-Mode is the true answer
;; to Knowledge Management (KM) as we discussed and defined the term back at
;; Lotus Research in the 90s...  The solution was right under our noses in Emacs!
;; Secondly, I will be using org-mode for a total Business Accounting System...
;; Thirdly, I will be using org-mode for Writing, Publishing, Scrivener Style...
;; Also, I may end up using org-mode to organize all my System Admin stuff,
;; including the code and scripts...  More about that later....

;;
;; Change Log: (descending chronological order)
;;

;; 2022-009-18 - Alisha Awen, siren1@disroot.org
;;   Added ABC-MODE Back in... (package loads fine now)

;; 2022-009-18 - Alisha Awen, siren1@disroot.org
;;   Removed (commented out) abc-mode.. This was causing some problems
;;   Still troubleshooting this...
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;;
;; Create repositories cache for Org Mode extras, if required:

(when (not package-archive-contents)
  (package-refresh-contents))

;;;
;; Declare a list of required packages for my fancy org-mode setup:
;;

(defvar me--req-org-packages
  '(gnuplot-mode
    toc-org
    sass-mode
    abc-mode
    org-sidebar
    org-contrib
    org-mind-map))

;;;
;; Install required packages:

(mapc (lambda (p) (package-install p))
      me--req-org-packages)

;; Summon the Org-mode Gods to be ready to grant your Mod requests...

(require 'org)
;(require 'org-tempo)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Define LOCAL USER MASTER ORG FILES DIRECTORY:
;; The place where you will be keeping all your private org files and
;; sub-directories... You may already have a special directory for your
;; org files.  In that case be sure to read all the notes below!
;;
;; IMPORTANT NOTE: Create a symlink in your home directory: ~/.OD which
;; points to the top level directory where you keep all your .ORG files...
;; NOTE: If you don't have a master org-files directory on your file system
;; yet, then go ahead and create that directory now... Make sure your
;; newly created symlink:  ~/.OD points to your new org-files directory path
;; organize your important org-files in this master directory... master agenda
;; etc...
;;;

;; Set Path to ORG Docs from CONSTANT:

(setq org-directory ME--ORG-DOC-DIR)


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Configure LilyPond-mode for editing .ly Music Notation files in Emacs
;; NOTE: This is not for org-mode but I only use LilyPond to make files
;;       for org-mode to export eventually to PDF Docs, Books, etc...

(autoload 'LilyPond-mode "lilypond-mode")
(setq auto-mode-alist
      (cons '("\\.ly$" . LilyPond-mode) auto-mode-alist))

(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Configure Org Mode Beautify Settings: (constantly under revision %^)
;;  Override the file below by putting it into "my-modules" then change it to
;;  suit your needs and planning style...

(if (file-exists-p "~/.emacs.d/lisp/my-modules/09-1-org-beautify-conf.el")
    (load-file "~/.emacs.d/lisp/my-modules/09-1-org-beautify-conf.el")
  (load-file "~/.emacs.d/lisp/modules/09-1-org-beautify-conf.el"))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Setup my Default Org-Mode Keywords and Tags:
;;  Schemes are based on the general GTD process I am using...
;;  Override the file below by putting it into my-modules then change it to
;;  suit your needs and planning style...

(if (file-exists-p "~/.emacs.d/lisp/my-modules/09-2-org-keywords-tags-conf.el")
    (load-file "~/.emacs.d/lisp/my-modules/09-2-org-keywords-tags-conf.el")
  (load-file "~/.emacs.d/lisp/modules/09-2-org-keywords-tags-conf.el"))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Setup my Org-Mode Project Management (Capture, GTD, TODO, Tasks, Agenda, etc.
;;  Schemes are based on the "Autofocus GTD Process" I use along with the
;;  Timesheets emacs package and a few other things like mind maps etc...
;;  Override the file below by putting it into my-modules then change it to
;;  suit your needs and planning style...

(if (file-exists-p "~/.emacs.d/lisp/my-modules/09-3-org-PM-conf.el")
    (load-file "~/.emacs.d/lisp/my-modules/09-3-org-PM-conf.el")
  (load-file "~/.emacs.d/lisp/modules/09-3-org-PM-conf.el"))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Configure Org-Mode Export Settings:
;;  Override the file below by putting it into my-modules then change it to
;;  fit your desired Org Mode Export features...

(if (file-exists-p "~/.emacs.d/lisp/my-modules/09-4-org-export-conf.el")
    (load-file "~/.emacs.d/lisp/my-modules/09-4-org-export-conf.el")
  (load-file "~/.emacs.d/lisp/modules/09-4-org-export-conf.el"))


;;;
;;  Speed Keys For Quick Navigation:

(setq org-use-speed-commands 1)

;;;
;;  Set Maximum Indentation For Org-Mode Description Lists:

(setq org-list-description-max-indent 5)

;;;
;;  Prevent Org-Mode Demoting Heading Also Shifting Text Inside Sections:

(setq org-adapt-indentation nil)


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   Alisha's Advanced Org-Mode Configurations: (EXTRA STUFF)
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;             Org Mind Map Configuration

(require 'ox-org)

(setq org-mind-map-engine "dot")       ; Default. Directed Graph
;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
;; (setq org-mind-map-engine "twopi")  ; Radial Layout
;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
;; (setq org-mind-map-engine "twopi")  ; Radial layouts
;; (setq org-mind-map-engine "circo")  ; Circular Layout

;; Set Mobile Org Directory from GLOBAL CONSTANT:

(setq org-mobile-directory ME--MOBILE-ORG-DIR)

;; Set Name of file where new notes will be stored:

(setq org-mobile-inbox-for-pull (expand-file-name  "flagged.org" org-directory))

;; Enable MobileOrg Encryption:

(setq org-mobile-use-encryption t)

;; Set MobileOrg Password:
;;   TODO: How secure is this??? Plain Text? Find out...
;;         Phones are NOT secure by design however...
;;         so only good for shopping-list.org and stuff?
;;         You may see adds for food, etc... LOL

(setq org-mobile-encryption-password ME--MOBILE-ORG-PW)


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Defined Variable:  ME--YT-iFrame-format
;;  For embedding YouTube Links in Org Files:
;;
;;  Ref:
;;     https://endlessparentheses.com/embedding-youtube-videos-with-org-mode-links.html
;;
;;  Usage:
;;     To use this, just write your org links in the following way
;;     (optionally adding a description).
;;
;;                     [[yt:A3JAlWM8qRM]]
;;
;; When you export to HTML, the above will produce an inline snippet that Youtube
;; specifies. The advantage (over simply writing out the iframe) is that this link
;; can also be clicked in org-mode, and can be exported to other formats as well.

(defvar me--yt-iframe-format
  ;; You may want to change your width and height.
  (concat "<iframe width=\"440\""
          " height=\"335\""
          " src=\"https://www.youtube.com/embed/%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe>"))

(org-add-link-type
 "yt"
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/embed/"
            handle)))
 (lambda (path desc backend)
   (cl-case backend
     (html (format me--yt-iframe-format
                   path (or desc "")))
     (latex (format "\href{%s}{%s}"
                    path (or desc "video"))))))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Modular Emacs Quick Insert Org :README: Drawer  
;;  Function...

(defun me_org-insert-readme-drawer ()
  "Simple But Faster Than Typing. The Func Name Is The Doc... %^)"
  (interactive)
  (org-insert-drawer nil "README"))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Modular Emacs Org Mode Open File-link in Other
;;  Frame Function...

(defun me_org-open-other-frame ()
  "Jump to bookmark in another frame. See `bookmark-jump' for more."
  (interactive)
  (let ((org-link-frame-setup
         (acons 'file 'find-file-other-frame org-link-frame-setup)))
    (org-open-at-point)))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Modular Emacs Quick Insert CUSTOM_ID for links
;;  with Helper Functions...
;;  Ref: https://writequit.org/articles/emacs-org-mode-generate-ids.html
;;       This implements the initial set of functions etc. from Lee's
;;       article.  He has revised his original due to some user input
;;       etc...  I have not implemented that but if problems arise
;;       as mentioned in the above article, go back and implement
;;       those updates at the bottom of his article...

;; Require the org-id library, which contains
;; helpers. Use a CUSTOM_ID for links:

(require 'org-id)
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

;; Define custom Modular Emacs version of org-custom-id-get
;; that calls org-id-new and creates a new property if one
;; doesn't already exist...

(defun me_org-custom-id-get (&optional pom create prefix)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.
   If POM is nil, refer to the entry at point. If the entry does
   not have an CUSTOM_ID, the function returns nil. However, when
   CREATE is non nil, create a CUSTOM_ID if none is present
   already. PREFIX will be passed through to `org-id-new'. In any
   case, the CUSTOM_ID of the entry is returned."
  (interactive)
  (org-with-point-at pom
    (let ((id (org-entry-get nil "CUSTOM_ID")))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (create
        (setq id (org-id-new (concat prefix "h")))
        (org-entry-put pom "CUSTOM_ID" id)
        (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
        id)))))

;; Add a helper function that's interactive to add custom ids to all
;; headlines in the buffer if they don't already have one. 

(defun me_org-add-ids-to-headlines-in-file ()
  "Add CUSTOM_ID properties to all headlines in the
   current file which do not already have one."
  (interactive)
  (org-map-entries (lambda () (me_org-custom-id-get (point) 'create))))

;; Add a helper function that's interactive to add a custom id to 
;; the headline at the current cursor location in buffer
;; (if there isn't a custom ID set allready)...

(defun me_org-add-id-to-headline ()
  "Add CUSTOM_ID property to headline nearest to current cursor
   location if the heading does not already have one."
  (interactive)
  (me_org-custom-id-get (point) 'create))

;; Optional helper to add a CUSTOM_ID to headlines
;; created when using an org-capture template: 

(add-hook 'org-capture-prepare-finalize-hook
          (lambda () (me_org-custom-id-get (point) 'create)))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Org Outline Tree on left | content on 
;;  right Functions...
;;
;; Open sub elements in right window pane,
;; move cursor focus to right window pane...
;; NOTE: I am not using this anymore because
;;       I changed to using org-sidebar which
;;       does this by defult when you hit RET
;;       while your cursor is on an outline
;;       heading within the leftmost
;;       org-sidebar-tree window pane.

(defun me_org-tree-open-in-right-win ()
  (interactive)
  (org-tree-to-indirect-buffer)
  (windmove-right))

;; Open sub elements in right window pane,
;; Leave cursor in left outline window pane...

(defun me_org-tree-open-in-right-no-focus ()
  (interactive)
  (org-sidebar-tree-jump-indirect)
  (windmove-left))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Set up Normal Org View in Single Window Frame
;;  NO Org Sidebar Tree Left navigation pane...

(defun me_org-narrow-screen ()
  "Set up Org View in Single Window Frame with
   dimensions set to work fine as a single window only
   session"
  (interactive)
  ;; Test to make sure we are in Org Mode, if not print a warning...
  (if (equal 'org-mode major-mode)
      (progn
        ;; Make sure we are in Split Window Fancy Org View First!
        (when (= (length (window-list)) 2)
          (if (eq 'selected-window 'window-left-child)
              ;; Remove Split Sidebar Tree Window:
              (progn
                (org-sidebar-tree-toggle))
            (progn
              ;; Else Move to left most window:
              (windmove-left)
              ;; And now Remove Split Sidebar Tree Window...
              (org-sidebar-tree-toggle))))

        ;; Make NORMAL ORG MODE Frame size for writing/composition:

        (me_set-writing-frame)

        ;; Set Olivetti Width
        (olivetti-mode)
        (olivetti-set-width ME--ORG-OLIV-WIDTH))

    ;; ELSE print warning about buffer not being an Org File...
    (message "Warning: You are NOT visiting a .ORG file!")))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Set up Fancy Org View in W I D E  S C R E E N
;;  mode with Org Sidebar Tree in Left pane and
;;  Content window on right... (i.e., Like Scrivener)

(defun me_org-wide-screen ()
  "Set up Fancy Org View in W I D E  S C R E E N view with frame
   dimensions set to work nicely with split windows:  A navigation
   window pane on left side showing the Org Tree headings only, and a content
   window on the right side showing everything (for editing)"
  (interactive)
  ;; Test to make sure we are in Org Mode, if not print a warning...
  (if (equal 'org-mode major-mode)
      (progn
        (me_set-org-wide-frame)

        ;; Split Windows with org outline tree in narrow left window...

        (org-sidebar-tree-toggle)
        ;(org-sidebar-tree)

        ;; Set Olivetti Width
        (olivetti-mode)
        (olivetti-set-width ME--WIDE-ORG-OLIV-WIDTH))

    ;; ELSE print warning about buffer not being an Org File...
    (progn
      (message "Warning: You are NOT visiting a .ORG file!"))))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Modular Emacs - me_org-toggle-blocks Function:
;;
;;  With this in place, org-mode collapses SRC blocks by default and lets you
;;  toggle visibility of all SRC blocks by hitting C-c t.

(defvar me--org-blocks-hidden nil)

(defun me_org-toggle-blocks ()
  (interactive)
  (if me--org-blocks-hidden
      (org-show-block-all)
    (org-hide-block-all))
  (setq-local me--org-blocks-hidden (not me--org-blocks-hidden)))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Modular Emacs - Fancy Org View - org-mode Hook Function:
;;  This Fancy Org View hook takes care of setting your writing/publishing
;;  environment nicely...
;;
;;  Using this mode you can have two different Screen Layouts via a Toggle key..
;;  Normal Layout for general purpose Org mode work in a single window...  and
;;  Fancy Org View W I D E  S C R E E N layout with navigation tree window on
;;  left side and content window on right side for adding content.  The wide screen
;;  based publishing environment is working just as well as my Scrivener Projects
;;  were in the past!  It will soon be exceeding anything that I could do with
;;  Scrivener and be future proof to boot!  Combined with Fountain mode for
;;  screenwriting, audio and VR scripting etc... When this project is done
;;  you will be glad to know "all your base are belong to" Emacs Org Mode! %^)

(defun me_fancy-org-mode-hook ()
  "Harmonic Alchemy Modular Emacs Fancy Org Mode Hook Function.
   This function takes care of setting up a nice org-mode writing
   environment for both planning and / or writing-publishing"

  ;; NOTE: All settings below change the currently selected frame only...
  ;;       (other existing and future frames are not affected)

  (me_set-writing-frame)

  ;; Collapse all source blocks;

  (org-hide-block-all)

  ;; Hide bullets...

  (me_hide-org-bullets)

  ;; Set Up Fancy Unicode Checkboxes...
  
  (me_set-unicode-checkboxes)

  ;; Include toc-org...

  (if (require 'toc-org nil t)
      (progn
        (toc-org-mode))
    (warn "toc-org not found"))

  ;; Enable Olivetti Mode...

  (olivetti-mode)
  (olivetti-set-width ME--ORG-OLIV-WIDTH)

  ;; Call Xah-Fly-Keys (resets some face attributes)
  (xah-fly-keys 1))

;; END Fancy Org View - org-mode Hook function...


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Modular Emacs - Org Mode Hook function For EDIFF:
;; (EXPIRIMENTAL for Debugging only - Don't Use this)
;;
;;  This EDIFF Org Mode hook puts Org Mode in bare
;;  bones, single pane mode, which works fine for
;;  using tools that operate on .org files and also
;;  need to controll splitting of windows etc...
;;
;;  USEAGE Notes:
;;
;;    1. No Notes... Just use as is...  It just works...
;;       What it says on the tin...  %^)
;;
;;       Oh... Right....  Don't forget to switch forms
;;       below to register THIS hook instead of the Fancy
;;       Org Mode Hook (above) Right? "That's how you turn
;;       it on honey...  And make sure it's plugged in!"
;;       LOL - OK... ready eddy?  let's dance...

(defun me_normal-org-mode-hook ()
  "Harmonic Alchemy Modular Emacs Simple Org Mode Hook Function.
   This function configures org-mode to work better while using
   diff tools like eDiff to compare .org files.  Fancy Org Mode
   breaks eDiff and other tools that split windows..."

  ;; All this Function does is Set default face to Courier Prime

  (set-face-attribute 'default (selected-frame)
                      :family "Courier Prime"
                      :height 130))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;;                      FANCY ORG MODE INIT
;;  Add Fancy Org Mode Hook Function to Org Mode startup list:

(add-hook 'org-mode-hook 'me_fancy-org-mode-hook)

;;;              NORMAL ORG MODE INIT
;;  Add Normal Org Mode Hook Function to Org Mode startup list:
;;  NOTE:  Don't enable this unless you are troubleshooting Org Mode or something...
;;         And if you do enable this form, you need to disable the form above it...
;(add-hook 'org-mode-hook 'me_normal-org-mode-hook)

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   END: [modular-emacs]:~/.emacs.d/lisp/modules/09-org-mode-pkg-conf.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
