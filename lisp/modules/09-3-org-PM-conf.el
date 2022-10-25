;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   [modular-emacs]:~/.emacs.d/lisp/modules/09-3-org-PM-conf.el
;;
;;   This is a sub module of 09-org-mode-pkg-conf.el which sets up my Org-Mode
;;   Project Management Process:
;;        (e.g., Capture, GTD, TODO, Tasks, Agenda, etc.)
;;   Schemes are based on the "Autofocus GTD Process" I use along with the
;;   Timesheets emacs package and a few other things like mind maps etc...
;;
;;   Override this file by placing a copy of it into "my-modules" then change
;;   settings below to suit your needs and planning style...
;;
;; NOTE:  The definitions below depend on Global Constant: ME--ORG-DOC-DIR and
;;        Sub Directory CONSTANTS being defined properly within your cloned:
;;        my-modules/me-constants.el (which overrides the same file within modules.
;;
;;        The above CONSTANTS need to be set to fit the structure defined below
;;        for proper operation of HAP Modular Emacs custom functionality within
;;        org-mode.
;;
;; NEW ORG USERS NOTE: If you don't allready have an org-files system structure
;; set up, it is highly recommended to create your directory structure exactly as
;; below, (replacing "Your-Org-Docs-dir" with the directory path where you will
;; be setting up the structure below it... The symbol defined above:
;; "ME--ORG-DOC-DIR" must match the path to Your Org Docs Master directory.
;;
;;   Your-Org-Docs-dir/           # This must match (me--org-dir) set above!
;;     |__00-Agenda-files/        # All others must match exactly as shown!
;;     |__01-Sandbox/             # This is a Sandbox for trying org tests etc. 
;;     |__02-Templates/           # This is for capture templates and other things. 
;;         |____t-log-entry.txt      # Capture Template for General Log Notebook.
;;         |____t-private-todo.txt   # Capture Template for TODO List (eventually autofocus)
;;
;;     |__03-Private/          # This is your private Org Files Directory:
;;         |____diary.org      # Important for this directory:  
;;         |____refile.org     #     Create these files following this diagram.
;;         |____Timesheet/     #     They are hard coded within Mod Emacs org config.
;;         |____refile.org     
;;         |____Autofocus-notebook.org
;;     
;;     |__04-Mobile/        # Staging Area for encrypted MobileOrg Sync via Dropbox. 
;;
;; EXISTING ORG USERS NOTE! For existing org-files directory structures,
;; unfortunately you redefine ALL the SUB Folder CONSTANTS defined within:
;; me-constants.el...
;;
;; It's probably best to try doing all this (as is) outside of your normal Org Files
;; directory first...  Once you have a good handle on this it will be easier to
;; adjust the code to fit into your existing org-mode configuration...
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


;;;
;; The following list combines the above local directory
;; names with org-directory (me--org-dir) including
;; me--org-dir (parent) as its members (elements)...
;;
;;      org-directory          = ME--DOC-DIR (Top Level Org Directory)
;;      ME--ORG-AGENDA-FILES   = org-directory/00-Agenda-files
;;      me--org-sandbox        = org-directory/01-Sandbox
;;      me--org-templates      = org-directory/02-Templates
;;      me--org-files          = org-directory/03-Private
;;      me--org-rtfm           = org-directory/10-RTFM
;;      me--org-brain          = org-directory/brain

;; Load Default Org Agenda Files (directories) that are permanent...
;; Any .org files located within the directories of this list
;; are collected for use within the agenda view... 
;;
;; NOTE1: This list is also updated from within some org files
;;        or by manually inserting them while you use org-mode...
;;        If later, you manually add a file that already exists
;;        within this permanent list, it will be moved to the HEAD
;;        of the list... (it does not get duplicated so no problems)
;;        You can also use "<" in the dispatcher to limit the scope
;;        of your agenda view to the file you are currently visiting...
;;        
;; Note2: This is a LIST not a single file.. If org-agenda-files is set to
;;        point to a single .org file you need to add your list of directories
;;        to that file instead... Note... I may switch to that as this is
;;        a public Emacs project...  That way you don't have to tweak my
;;        hard-coded settings below... For now... I am still testing all this...


;;; CURRENTLY DISABLED - Agenda files and Capture Templates configurations are ALL
;;                       being refactored...
;;
;; (setq org-agenda-files
;;       (list
;;        (file-name-as-directory (expand-file-name ME--ORG-DOC-DIR))
;;        (file-name-as-directory (expand-file-name ME--ORG-AGENDA-FILES))
;;        (file-name-as-directory (expand-file-name ME--ORG-SANDBOX))
;;        (file-name-as-directory (expand-file-name ME--ORG-TEMPLATES))
;;        (file-name-as-directory (expand-file-name ME--ORG-FILES))
;;        (file-name-as-directory (expand-file-name ME--ORG-RTFM))
;;        (file-name-as-directory (expand-file-name ME--ORG-BRAIN))
;;        (file-name-as-directory (expand-file-name "~/.emacs.d/Docs/pubOps"))
;;        (file-name-as-directory (expand-file-name "~/.emacs.d/Docs/pubOps/Audio-Drama"))))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Define Custom Org-file Name CONSTANTS:


;;; CURRENTLY DISABLED - Agenda files and Capture Templates configurations are ALL
;;                       being refactored...
;;
;; (defconst ME--DEFAULT-ORG-NOTE 
;;   (expand-file-name "refile.org" ME--ORG-FILES))

;; (defconst ME--DEFAULT-ORG-DIARY
;;   (expand-file-name "diary.org" ME--ORG-FILES))

;; (defconst ME--PRIVATE-ORG-NOTE
;;   (expand-file-name "private.org" ME--ORG-FILES))

;; (defconst ME--WORK-LOG
;;   (expand-file-name "logbook-work.org" ME--ORG-FILES))

;; (defconst ME--PERSONAL-LOG
;;   (expand-file-name "logbook-personal.org" ME--ORG-FILES))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Define Custom Capture Template Files:


;;; CURRENTLY DISABLED - Agenda files and Capture Templates configurations are ALL
;;                       being refactored...
;;
;; (defconst ME--ORG-TODO-TEMPLATE
;;   (expand-file-name "t-private-todo.txt" ME--ORG-TEMPLATES))

;; (defconst ME--ORG-LOG-TEMPLATE
;;   (expand-file-name "t-log-entry.txt" ME--ORG-TEMPLATES))



;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Set Up Capture templates for: TODO tasks, Notes,
;; appointments, phone calls, and org-protocol


;;; CURRENTLY DISABLED - Agenda files and Capture Templates configurations are ALL
;;                       being refactored...
;;
;; (setq
;;  org-capture-templates
;;  (quote
;;   (("p" "Private Templates")

;;    ("pt"
;;     "ToDo Entry"
;;     entry
;;     (file+headline ME--PRIVATE-ORG-NOTE "Capture")
;;     (file ME--ORG-TODO-TEMPLATE)
;;     :empty-lines-before 1)

;;    ("pl" "Log Notebook Entry" entry
;;     (file+headline buffer-file-name "Begin Log Notebook:")
;;     (file ME--ORG-LOG-TEMPLATE)
;;     :unnarrowed t
;;     :tree-type week
;;     :prepend t
;;     :clock-in t
;;     :clock-keep t
;;     :empty-lines 1
;;     :immediate-finish t)

;;    ("w" "Work Templates")

;;    ("wl" "Work Log Entry" entry
;;     (file+datetree ME--WORK-LOG) "** %U - %^{Activity}  :LOG:")

   ;;; This section is commented out for Now... Need to decide details...

   ;; ("t" "todo" entry
   ;;  (file
   ;;   (expand-file-name ME--DEFAULT-ORG-NOTE ME--ORG-FILES)
   ;;   "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t))

   ;; ("r" "respond" entry
   ;;  (file
   ;;   (expand-file-name ME--DEFAULT-ORG-NOTE ME--ORG-FILES)
   ;;   "* TODO Respond to %:from on %:subject\n%U\n%a\n"
   ;;   :clock-in t :clock-resume t :immediate-finish t))

   ;; ("n" "note" entry
   ;;  (file (expand-file-name ME--DEFAULT-ORG-NOTE ME--ORG-FILES)
   ;;        "* %? :NOTE:\n%U\n%a\n"
   ;;        :clock-in t :clock-resume t))

   ;; ("j" "Journal" entry
   ;;  (file+datetree (expand-file-name ME--DEFAULT-ORG-DIARY ME--ORG-FILES)
   ;;                 "* %?\n%U\n"
   ;;                 :clock-in t :clock-resume t))

   ;; ("w" "org-protocol" entry
   ;;  (file (expand-file-name ME--DEFAULT-ORG-NOTE ME--ORG-FILES))
   ;;  "* TODO Review %c\n%U\n"
   ;;  :immediate-finish t)

   ;; ("p" "Phone call" entry
   ;;  (file (expand-file-name ME--DEFAULT-ORG-NOTE ME--ORG-FILES))
   ;;  "* PHONE %? :PHONE:\n%U"
   ;;  :clock-in t :clock-resume t)

   ;; ("h" "Habit" entry
   ;;  (file (expand-file-name ME--DEFAULT-ORG-NOTE ME--ORG-FILES))
   ;;  "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
;;   )))

;;;
;; Dim blocked tasks
(setq org-agenda-dim-blocked-tasks t)

;;; 
;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;;; 
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)

;;; 
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)

;;; 
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer "CLOCKING")

;;; 
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)

;;; 
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)

;;; 
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)

;;; 
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))

;;; 
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

;;; 
;; round time
(setq org-time-stamp-rounding-minutes (quote (1 1)))

;;; 
;; Keep tasks with dates on the global todo lists
(setq org-agenda-todo-ignore-with-date nil)

;;; 
;; Keep tasks with deadlines on the global todo lists
(setq org-agenda-todo-ignore-deadlines nil)

;;; 
;; Keep tasks with scheduled dates on the global todo lists
(setq org-agenda-todo-ignore-scheduled nil)

;;; 
;; Keep tasks with timestamps on the global todo lists
(setq org-agenda-todo-ignore-timestamp nil)

;;; 
;; Remove completed deadline tasks from the agenda view
(setq org-agenda-skip-deadline-if-done t)

;;; 
;; Remove completed scheduled tasks from the agenda view
(setq org-agenda-skip-scheduled-if-done t)

;;; 
;; Remove completed items from search results
(setq org-agenda-skip-timestamp-if-done t)

;;; 
;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)

;;; 
;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)

;;; 
;; Sorting order for tasks on the agenda
(setq org-agenda-sorting-strategy
      (quote
       ((agenda habit-down time-up user-defined-up priority-down effort-up category-keep)
	(todo category-up priority-down effort-up)
	(tags category-up priority-down effort-up)
	(search category-up))))

;;; 
;; Start the weekly agenda on Monday
(setq org-agenda-start-on-weekday 1)

;;; 
;; Enable display of the time grid so we can see the marker for the current time
(setq org-agenda-time-grid
      (quote
       ((daily today remove-match)
	#("----------------" 0 16 (org-heading t))
	(830 1000 1200 1300 1500 1700))))

;;; 
;; Display tags farther right

(setq org-agenda-tags-column -102)

(setq org-enforce-todo-dependencies t)

;; Set Method to be used to create new IDs. (org-id.el)

(setq org-id-method (quote uuidgen))

(setq org-deadline-warning-days 30)

;; Export Org Tables to Tab Seporated Values:

(setq org-table-export-default-format "orgtbl-to-tsv")

(setq org-link-frame-setup
      (quote
       ((vm . vm-visit-folder)
        (gnus . org-gnus-no-new-news)
        (file . find-file))))
 

;; Use the current window for C-c ' source editing

(setq org-src-window-setup 'current-window)

(setq org-log-done (quote time))

(setq org-log-into-drawer "LOGBOOK")

(setq org-alphabetical-lists t)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Custom agenda command definitions

(setq org-agenda-custom-commands
      (quote
       (("N" "Notes" tags "NOTE"
         ((org-agenda-overriding-header "Notes")
	  (org-tags-match-list-sublevels t)))

	("h" "Habits" tags-todo "STYLE=\"habit\""
         ((org-agenda-overriding-header "Habits")
	  (org-agenda-sorting-strategy
	   '(todo-state-down effort-up category-keep))))

	(" " "Agenda"
         ((agenda "" nil)

	  (tags "REFILE"
		((org-agenda-overriding-header "Tasks to Refile")
		 (org-tags-match-list-sublevels nil)))

	  (tags-todo "-CANCELLED/!"
		     ((org-agenda-overriding-header "Stuck Projects")
		      (org-agenda-skip-function 'bh/skip-non-stuck-projects)))

	  (tags-todo "-WAITING-CANCELLED/!NEXT"
		     ((org-agenda-overriding-header "Next Tasks")
		      (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
		      (org-agenda-todo-ignore-scheduled t)
		      (org-agenda-todo-ignore-deadlines t)
		      (org-agenda-todo-ignore-with-date t)
		      (org-tags-match-list-sublevels t)
		      (org-agenda-sorting-strategy
		       '(todo-state-down effort-up category-keep))))

	  (tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING"
		     ((org-agenda-overriding-header "Tasks")
		      (org-agenda-skip-function 'bh/skip-project-tasks-maybe)
		      (org-agenda-todo-ignore-scheduled t)
		      (org-agenda-todo-ignore-deadlines t)
		      (org-agenda-todo-ignore-with-date t)
		      (org-agenda-sorting-strategy
		       '(category-keep))))

	  (tags-todo "-HOLD-CANCELLED/!"
		     ((org-agenda-overriding-header "Projects")
		      (org-agenda-skip-function 'bh/skip-non-projects)
		      (org-agenda-sorting-strategy
		       '(category-keep))))

	  (tags-todo "-CANCELLED+WAITING/!"
		     ((org-agenda-overriding-header "Waiting and Postponed Tasks")
		      (org-agenda-skip-function 'bh/skip-stuck-projects)
		      (org-tags-match-list-sublevels nil)
		      (org-agenda-todo-ignore-scheduled 'future)
		      (org-agenda-todo-ignore-deadlines 'future)))

	  (tags "-REFILE/"
		((org-agenda-overriding-header "Tasks to Archive")
		 (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
		 (org-tags-match-list-sublevels nil))))
         nil)
	
	("r" "Tasks to Refile" tags "REFILE"
         ((org-agenda-overriding-header "Tasks to Refile")
	  (org-tags-match-list-sublevels nil)))

	("#" "Stuck Projects" tags-todo "-CANCELLED/!"
         ((org-agenda-overriding-header "Stuck Projects")
	  (org-agenda-skip-function 'bh/skip-non-stuck-projects)))

	("n" "Next Tasks" tags-todo "-WAITING-CANCELLED/!NEXT"
         ((org-agenda-overriding-header "Next Tasks")
	  (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
	  (org-agenda-todo-ignore-scheduled t)
	  (org-agenda-todo-ignore-deadlines t)
	  (org-agenda-todo-ignore-with-date t)
	  (org-tags-match-list-sublevels t)
	  (org-agenda-sorting-strategy
	   '(todo-state-down effort-up category-keep))))

	("R" "Tasks" tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING"
         ((org-agenda-overriding-header "Tasks")
	  (org-agenda-skip-function 'bh/skip-project-tasks-maybe)
	  (org-agenda-sorting-strategy
	   '(category-keep))))

	("p" "Projects" tags-todo "-HOLD-CANCELLED/!"
         ((org-agenda-overriding-header "Projects")
	  (org-agenda-skip-function 'bh/skip-non-projects)
	  (org-agenda-sorting-strategy
	   '(category-keep))))

	("w" "Waiting Tasks" tags-todo "-CANCELLED+WAITING/!"
         ((org-agenda-overriding-header "Waiting and Postponed tasks"))
         (org-tags-match-list-sublevels nil))

	("A" "Tasks to Archive" tags "-REFILE/"
         ((org-agenda-overriding-header "Tasks to Archive")
	  (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
	  (org-tags-match-list-sublevels nil))))))


;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   Org Mind Map Configuration:
;;   Note:  Graphviz must be installed on your system in order for these
;;          Forms to work!!!  You may see warnings on Emacs startup but it
;;          will not hurt anything except trying to use Org Mind Map...

(setq org-mind-map-engine "dot")      ; Default. Directed Graph
(setq org-mind-map-engine "neato")    ; Undirected Spring Graph
(setq org-mind-map-engine "twopi")    ; Radial Layout
(setq org-mind-map-engine "fdp")      ; Undirected Spring Force-Directed
(setq org-mind-map-engine "sfdp")     ; Multiscale version of fdp for the 
                                      ; layout of large graphs
(setq org-mind-map-engine "twopi")    ; Radial layouts
(setq org-mind-map-engine "circo")    ; Circular Layout

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   END: [modular-emacs]:~/.emacs.d/lisp/modules/09-3-org-PM-conf.el
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
