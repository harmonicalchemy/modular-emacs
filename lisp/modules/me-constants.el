;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [modular-emacs]:~/.emacs.d/lisp/modules/me-constants.el
;;   Harmonic Alchemy Productions - Modular Emacs CONSTANTS
;;
;;   USAGE: Copy this file to ~/.emacs.d/lisp/my-modules/me-constants.el
;;          and edit it to best fit your particular platform environment...
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; DEFINE Main Documents Directory.
;; Adjust Path below to fit your use-case:
;; Note: You could also leave this alone and
;;       INSTEAD define symlink with same name
;;       defined below pointing to your Docs
;;       Directory...

(defconst ME--DOC-DIR "~/.MD"
  "Master Documents Folder Defined for this Platform")

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; DEFINE Main ORG Documents Directory.
;; Adjust Path below to fit your use-case:
;; Note: You could also leave this alone and
;;       INSTEAD define symlink with same name
;;       defined below pointing to your ORG Docs
;;       Directory...

(defconst ME--ORG-DOC-DIR "~/.OD"
  "Master ORG Docs Folder Defined for this Platform")

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Define USER ORG FILES SUB DIRECTORY NAMES:
;;
;; NOTE: These CONSTANTS are used by:
;;       09-3-org-PM-conf.el & 15-Accounting-pkg-conf.el
;;       (which gets loaded later in this module chain)

(defconst ME--ORG-AGENDA-FILES
  (file-name-as-directory
   (expand-file-name "00-Agenda-files" ME--ORG-DOC-DIR)))

(defconst ME--ORG-SANDBOX
  (file-name-as-directory
   (expand-file-name "01-Sandbox" ME--ORG-DOC-DIR)))

(defconst ME--ORG-TEMPLATES
  (file-name-as-directory
   (expand-file-name "02-Templates" ME--ORG-DOC-DIR)))

(defconst ME--ORG-FILES
  (file-name-as-directory
   (expand-file-name "03-Private" ME--ORG-DOC-DIR)))

(defconst ME--ORG-RTFM
  (file-name-as-directory
   (expand-file-name "10-RTFM" ME--ORG-DOC-DIR)))

(defconst ME--ORG-BRAIN
  (file-name-as-directory
   (expand-file-name "brain" ME--ORG-DOC-DIR)))


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
(defconst ME--CODE-FRAME-WIDTH (round 92)
  "Default Frame Width for Codeing. me-constants.el")
(defconst ME--CODE-OLIV-WIDTH (round 88)
  "Default Olivetti width for Codeing. me-constants.el")

;; WIDE CODE FRAME Dimensions:
;; This is for Source CODE or "mono" frames that need to be W I D E...
;; (Useful for things like ediff buffers between adjacent windows, etc.)
;; Typing: "6" (Xah-Fly-Key command mode) Invokes It...
;; Adjust Numbers Below To Fit Best On Your Screen:

(defconst ME--WIDE-CODE-FRAME-HEIGHT (round 42)
  "Default Frame Height for Fancy Org Mode. me-constants.el")
(defconst ME--WIDE-CODE-FRAME-WIDTH (round 188)
  "Default Frame Width for Fancy Org Mode. me-constants.el")
(defconst ME--WIDE-CODE-OLIV-WIDTH (round 88)
  "Default Olivetti width for Fancy Org Mode. me-constants.el")

;; Normal ORG Mode "Docs" Frame Dimensions:
;; This is the frame used when visiting a
;; .org file or whenever you toggle to it
;; via key command "SPACE p"
;; Frame Title: "HA MOD Emacs Vx.x - Writer's Frame"
;; Adjust Numbers Below To Fit Best On Your Screen:

(defconst ME--ORG-FRAME-HEIGHT (round 38)
  "Default Frame Height for Writing. me-constants.el")
(defconst ME--ORG-FRAME-WIDTH (round 92)
  "Default Frame Width for Writing. me-constants.el")
(defconst ME--ORG-OLIV-WIDTH (round 88)
  "Default Olivetti width for Writing. me-constants.el")

;; ORG WIDE SCREEN Frame Dimensions:
;; (accommodates Scrivener like side bar windows) 
;; This frame is ONLY used when you are visiting a .org file...
;; Typing: "5" (Xah-Fly-Key command mode) Invokes It...
;; Frame Title: "HA MOD Emacs Vx.x - Fancy Wide Org-Mode Frame"
;; Adjust Numbers Below To Fit Best On Your Screen:

(defconst ME--WIDE-ORG-FRAME-HEIGHT (round 62)
  "Default Frame Height for Fancy Org Mode. me-constants.el")
(defconst ME--WIDE-ORG-FRAME-WIDTH (round 148)
  "Default Frame Width for Fancy Org Mode. me-constants.el")
(defconst ME--WIDE-ORG-OLIV-WIDTH (round 82)
  "Default Olivetti width for Fancy Org Mode. me-constants.el")


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; DEFAULT TEXT SIZE/WIDTH DIMENSION CONSTANTS:

;; Set HEIGHT for Coder's mono text: (Hermit)

(defconst ME--DEFAULT-CODE-TEXT-HEIGHT (round 120)
  "Default Height for mono Source Code Text. Hermit. me-constants.el")

;; Set HEIGHT for Writer's Org-Mode normal text: (Courier Prime)

(defconst ME--DEFAULT-ORG-TEXT-HEIGHT (round 135)
  "Default Height for Org Mode Normal Text. Courier Prime. me-constants.el")


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

(defconst ME--MOBILE-ORG-PW "your-password-here"
  "Default Height for Org Mode Normal Text. Courier Prime. me-constants.el")

;; END: [modular-emacs]:~/.emacs.d/lisp/modules/me-constants.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
