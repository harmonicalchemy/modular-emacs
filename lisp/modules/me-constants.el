;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/me-constants.el
;;   Harmonic Alchemy Productions - Modular Emacs CONSTANTS
;;
;;   USAGE: Copy this file to ~/.emacs.d/lisp/my-modules/me-constants.el
;;          and edit it to best fit your particular platform environment...
;;
;;   NOTE: These DEFAULT Constants are the ones I CHOOSE for PERSONAL USE
;;         on my MAC... SORRY about that... THIS is my right as the PROJECT
;;         OWNER... HOWEVER... You can Just as EASILY HAVE IT YOUR WAY AS WELL
;;
;;         YES! You May and CAN SIMPLY OVERRIDE this config file by placing
;;         a copy of it in: ~/.emacs.d/lisp/my-modules/me-constants.el
;;         THEN Change Any or EVERY CONSTANT definition below to suit your
;;         OWN needs...
;;
;;         You can ALSO OVERRIDE my Blackboard Emacs Theme in a similar way...
;;         (that is another repo hosted on GitHub/harmonicalchemy)
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; DEFINE Main Documents Directory.
;; Adjust Path below to fit your use-case:
;; Note: You could also leave this alone and
;;       INSTEAD define symlink with same name
;;       defined below pointing to your Docs
;;       Directory...

(defconst ME--DOC-DIR "~/git/My-Docs"
  "Master Documents Folder Defined for this Platform")

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; DEFINE Main ORG Documents Directory.
;; Adjust Path below to fit your use-case:
;; Note: You could also leave this alone and
;;       INSTEAD define symlink with same name
;;       defined below pointing to your ORG Docs
;;       Directory...

(defconst ME--ORG-DOC-DIR "~/git/My-Docs/Org-Docs"
  "MASTER ORG Docs Folder Defined for this Platform")

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Define USER ORG FILES SUB DIRECTORY NAMES:
;;
;; NOTE: These CONSTANTS are used by:
;;       09-3-org-PM-conf.el & 15-Accounting-pkg-conf.el
;;       (which get loaded much later in the module dispatch)

(defconst ME--ORG-AGENDA-FILES
  (file-name-as-directory
   (expand-file-name "00-Agenda-files" ME--ORG-DOC-DIR))
  "ORG Agenda Files Folder Defined for this Platform")

(defconst ME--ORG-SANDBOX
  (file-name-as-directory
   (expand-file-name "01-Sandbox" ME--ORG-DOC-DIR))
  "ORG Sandbox Files Folder Defined for this Platform")

(defconst ME--ORG-TEMPLATES
  (file-name-as-directory
   (expand-file-name "02-Templates" ME--ORG-DOC-DIR))
  "ORG Template Files Folder Defined for this Platform")

(defconst ME--ORG-FILES
  (file-name-as-directory
   (expand-file-name "03-Private" ME--ORG-DOC-DIR))
  "ORG PRIVATE Files Folder Defined for this Platform")

(defconst ME--ORG-RTFM
  (file-name-as-directory
   (expand-file-name "10-RTFM" ME--ORG-DOC-DIR))
  "ORG RTFM Files Folder Defined for this Platform")

(defconst ME--ORG-PIM
  (file-name-as-directory
   (expand-file-name "11-PIM" ME--ORG-DOC-DIR))
  "ORG PIM Files Folder Defined for this Platform")

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; DEFAULT COLOR CONSTANTS:
;; For a nice visual reference of ALL available Emacs Colors
;; See: http://www.raebear.net/computers/emacs-colors/
;; (Jessica's Corner of Cyberspace)

;; FRAME BACKGROUND COLOR (See: Blackboard Theme)
(defconst ME--DEFAULT-BACKGROUND "#180028"
  "Mod-Emacs Background Color for ALL Frames All Contexts")

;; DARKER TEXT COLOR
;; (for mode line and other places where background is a lighter color)
(defconst ME--DARKER-TEXT "#080018"
  "Mod-Emacs Darker Text Color for mode line etc.")

;; Xah-Fly-Keys INSERT MODE FRAME BACKGROUND COLOR:
(defconst ME--XFK-INSERT-BACKGROUND "Grey15"
  "Mod-Emacs Xah-Fly-Keys insert-mode Frame Background Color")

;; Xah-Fly-Keys COMMAND MODE-LINE BACKGROUND COLOR: 
(defconst ME--XFK-CMD-MODELINE-BACKGROUND "orange"
  "Mod-Emacs Xah-Fly-Keys command-mode-line BG Color")

;; Xah-Fly-Keys INSERT MODE-LINE BACKGROUND COLOR: 
(defconst ME--XFK-INSERT-MODELINE-BACKGROUND "LightPink"
  "Mod-Emacs Xah-Fly-Keys command-mode-line BG Color")

;; Xah-Fly-Keys COMMAND MODE HI-LINE BACKGROUND COLOR: 
(defconst ME--XFK-CMD-HILINE-BACKGROUND "Grey5"
  "Mod-Emacs Xah-Fly-Keys command-mode-hi-line BG Color")

;; Xah-Fly-Keys INSERT MODE HI-LINE BACKGROUND COLOR: 
(defconst ME--XFK-INSERT-HILINE-BACKGROUND "Grey15"
  "Mod-Emacs Xah-Fly-Keys command-mode-hi-line BG Color")

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; DEFAULT TEXT SIZE/WIDTH DIMENSION CONSTANTS: 
;; NOTE: These Settings were made to look best
;; on a 16" Mac Book Pro with Built-In Display
;; set to next Larger Text after DEFAULT size...
;; MY EYESIGHT (with glasses) requires this %^)
;; Adjust Your Settings to FIT YOUR DEVICE & NEEDS...
;; DO THIS FIRST BEFORE Changing and Frame Sizes BELOW
;; Otherwise you will have to do all that over again. 

;; Set HEIGHT for Coder's mono text: (Hermit)
;; (previous VALUE: 123)

(defconst ME--DEFAULT-CODE-TEXT-HEIGHT (round 131)
  "Default Height for mono Source Code Text. Hermit. me-constants.el")

;; Set HEIGHT for Writer's Org-Mode normal text: (Courier Prime)
;; (previous VALUE: 147)

(defconst ME--DEFAULT-ORG-TEXT-HEIGHT (round 155)
  "Default Height for Org Mode Normal Text. Courier Prime. me-constants.el")

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; DEFAULT FRAME DIMENSIONS: (width, height)
;;
;; Change values below to suit your platforms screen size and resolution...
;; The defaults below work well on a standard size laptop computer...
;; Definitions below are for coding screens, writing screens, and
;; fancy org-mode screens...

;; Coder's (DEFAULT & INITIAL) Frame Dimensions:
;; This is the standard frame used at startup
;; It is also the frame to use when editing code
;; or conf files...
;; Typing: "SPACE p" (Xah-Fly-Key command mode) Invokes It...
;; Frame Title:  "HA MOD Emacs Vx.x - Default Frame"
;; Adjust Numbers Below To Fit Best On Your Screen:

(defconst ME--CODE-FRAME-HEIGHT (round 42)
  "Default Frame Height for Codeing. me-constants.el")
(defconst ME--CODE-FRAME-WIDTH (round 100)
  "Default Frame Width for Codeing. me-constants.el")
(defconst ME--CODE-OLIV-WIDTH (round 97)
  "Default Olivetti width for Codeing. me-constants.el")

;; WIDE CODE FRAME Dimensions:
;; This is for Source CODE or "mono" frames that need to be W I D E...
;; (Useful for things like ediff buffers between adjacent windows, etc.)
;; Typing: "6" (Xah-Fly-Key command mode) Invokes It...
;; Adjust Numbers Below To Fit Best On Your Screen:

(defconst ME--WIDE-CODE-FRAME-HEIGHT (round 42)
  "Default Frame Height for two column coding. me-constants.el")
(defconst ME--WIDE-CODE-FRAME-WIDTH (round 200)
  "Default Frame Width for two column coding. me-constants.el")
(defconst ME--WIDE-CODE-OLIV-WIDTH (round 97)
  "Default Olivetti Width two column coding. me-constants.el")

;; Normal ORG Mode "Docs" Frame Dimensions:
;; This is the frame used when visiting a
;; .org file or whenever you toggle to it
;; via key command "SPACE p"
;; Frame Title: "HA MOD Emacs Vx.x - Writer's Frame"
;; Adjust Numbers Below To Fit Best On Your Screen:

(defconst ME--ORG-FRAME-HEIGHT (round 42)
  "Default Frame Height for Writing. me-constants.el")
(defconst ME--ORG-FRAME-WIDTH (round 114)
  "Default Frame Width for Writing. me-constants.el")
(defconst ME--ORG-OLIV-WIDTH (round 110)
  "Default Olivetti width for Writing. me-constants.el")

;; ORG WIDE SCREEN Frame Dimensions:
;; (accommodates Scrivener like side bar windows) 
;; This frame is ONLY used when you are visiting a .org file...
;; Typing: "5" (Xah-Fly-Key command mode) Invokes It...
;; Frame Title: "HA MOD Emacs Vx.x - Fancy Wide Org-Mode Frame"
;; Adjust Numbers Below To Fit Best On Your Screen:

;; NOTE:  I Don't use this much at all... I set up this ORG MODE
;;        WIDE Screen thing years ago when I was trying to emulate
;;        the Layout/Workflow of Scrivener within EMACS org-mode...
;;        I have SINCE found BETTER ways for my Book Composing/Editing,
;;        LaTeX Configuration, Publishing to PDF, HTML, etc. etc.
;;        (i.e. my writer's IDE within emacs org-mode) to be much
;;        better than what I used to painfully do in Scrivener...
;;        THEREFORE this wide thing is not much useful... but
;;        I keep it all around anyway scratching my head on whether
;;        to finally remove it all...
;;        (along with my STUPID Org-Mode Side Bar thing which I
;;        also DON'T USE...) all of this removal will make modular
;;        emacs much more compliant with good e-lisp practice as well! 
;;        STAY TUNED...

(defconst ME--WIDE-ORG-FRAME-HEIGHT (round 42)
  "Default Frame Height for Fancy Org Mode. me-constants.el")
(defconst ME--WIDE-ORG-FRAME-WIDTH (round 200)
  "Default Frame Width for Fancy Org Mode. me-constants.el")
(defconst ME--WIDE-ORG-OLIV-WIDTH (round 197)
  "Default Olivetti width for Fancy Org Mode. me-constants.el")

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; DEFAULT OLIVETTI WIDTH:
;; (this is the olivetti width for NEW DEFAULT Frames ONLY

(defconst ME--DEFAULT-OLIV-WIDTH ME--CODE-OLIV-WIDTH
  "Default Olivetti width for Codeing. me-constants.el")

(defvar me--current-oliv-width ME--DEFAULT-OLIV-WIDTH
  "Default Olivetti width (for toggle) changes depending on context")

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;            Mobile Org Configuration

;; Set up MobileOrg Staging Area Mac OS:

(when ME--DARWIN
  (defconst ME--MOBILE-ORG-DIR
    (file-name-as-directory
     (expand-file-name "~/Path/To/Your-MacOS/Dropbox/Apps/MobileOrg"))))


;; Set up MobileOrg Staging Area Linux:

(when ME--LINUX
  (defconst ME--MOBILE-ORG-DIR
    (file-name-as-directory
     (expand-file-name "~/Path/To/Your-Linux/Dropbox/Apps/MobileOrg"))))

;; MOBILE ORG Password...
;; I am not comfortable with passwords in plain text but some
;; Emacs Modes require them. (like Mobile Org)
;; Override this file by putting it in my-modules and add
;; your PASSWORDS below as constants... (ONLY IN HERE)
;; Any code that needs passwords will pick them up ONLY
;; from THIS FILE HERE. (making it easy to manage and stay safe)

(defconst ME--MOBILE-ORG-PW "w1sIw1m2.0r9"
  "Default Height for Org Mode Normal Text. Courier Prime. me-constants.el")

;; END: [modular-emacs]:~/.emacs.d/lisp/modules/me-constants.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
