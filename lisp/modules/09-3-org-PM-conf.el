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
;; NOTE:  The definitions below depend on me--org-dir and org-directory being set
;;        properly before this file is loaded! The above variables need to be set
;;        to fit the structure defined below for proper operation of HAP Modular
;;        Emacs custom functionality within org-mode.
;;
;; NEW ORG USERS NOTE: If you don't allready have an org-files system structure
;; set up, it is highly recommended to create your directory structure exactly as
;; below, (replacing "Your-Org-Docs-dir" with the directory path where you will
;; be setting up the structure below it... The symbol defined above:
;; "me--org-dir" must match the path to Your Org Docs Master directory.
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
;; unfortunately you may need to adjust the file/folder names below within 
;; all related setq forms before this will work for your already existing setup...
;; It's probably best to try doing all this (as is) outside of your normal Org Files
;; directory first...  Once you have a good handle on this it will be easier to
;; adjust the code to fit into your existing org-mode configuration...
;; I may change how this works later to make it much easier to integrate...
;; For now, it's all kind of hard-wired...  Until I myself get a handle on it... %^)
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Define LOCAL USER ORG FILES SUB Directory Names:
;;
;; NOTE: These constants are also used by 15-Accounting-pkg-conf.el
;;       which gets loaded later in the module chain...

(defconst me--org-agenda-files
  (file-name-as-directory
   (expand-file-name "00-Agenda-files")))

(defconst me--org-sandbox
  (file-name-as-directory
   (expand-file-name "01-Sandbox")))

(defconst me--org-templates
  (file-name-as-directory
   (expand-file-name "02-Templates")))

(defconst me--org-files
  (file-name-as-directory
   (expand-file-name "03-Private")))

(defconst me--org-rtfm
  (file-name-as-directory
   (expand-file-name "10-RTFM")))

(defconst me--org-brain
  (file-name-as-directory
   (expand-file-name "brain")))

;;;
;; The following list combines the above local directory
;; names with org-directory (me--org-dir) including
;; me--org-dir (parent) as its members (elements)...
;;
;;      me--org-dir            = org-directory (Top Level Org Directory)
;;      me--org-agenda-files   = me--org-dir/00-Agenda-files
;;      me--org-sandbox        = me--org-dir/01-Sandbox
;;      me--org-templates      = me--org-dir/02-Templates
;;      me--org-files          = me--org-dir/03-Private
;;      me--org-rtfm           = me--org-dir/10-RTFM
;;      me--org-brain          = me--org-dir/brain

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

(setq org-agenda-files
      (list
       (file-name-as-directory (expand-file-name me--org-dir))
       (file-name-as-directory (expand-file-name me--org-agenda-files me--org-dir))
       (file-name-as-directory (expand-file-name me--org-sandbox me--org-dir))
       (file-name-as-directory (expand-file-name me--org-templates me--org-dir))
       (file-name-as-directory (expand-file-name me--org-files me--org-dir))
       (file-name-as-directory (expand-file-name me--org-rtfm me--org-dir))
       (file-name-as-directory (expand-file-name me--org-brain me--org-dir))
       (file-name-as-directory (expand-file-name "~/.emacs.d/lisp/my-modules/pub✎Ops"))
       (file-name-as-directory (expand-file-name "~/.emacs.d/Docs/pub✎Ops"))
       (file-name-as-directory (expand-file-name "~/.emacs.d/Docs/pub✎Ops/fountain2PDF"))))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Define Custom Org-file Names:

(defconst me--default-org-note 
  (expand-file-name "refile.org" me--org-files))

(defconst me--default-org-diary
  (expand-file-name "diary.org" me--org-files))

(defconst me--private-org-note
  (expand-file-name "private.org" me--org-files))

(defconst me--work-log
  (expand-file-name "logbook-work.org" me--org-files))

(defconst me--personal-log
  (expand-file-name "logbook-personal.org" me--org-files))

(defconst me--autofocus-notebook
  (expand-file-name "autofocus-notebook.org" me--org-files))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Define Custom Capture Template Files:

(defconst me--org-todo-template
  (expand-file-name "t-private-todo.txt" me--org-templates))

(defconst me--org-log-template
  (expand-file-name "t-log-entry.txt" me--org-templates))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Set me--org-files/refile.org as Default Org Note:

(setq org-default-notes-file
  (expand-file-name me--default-org-note me--org-dir))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Set Up Capture templates for: TODO tasks, Notes,
;; appointments, phone calls, and org-protocol

(setq
 org-capture-templates
 (quote
  (
   ("p" "Private Templates")

   ("pt"
    "ToDo Entry"
    entry
    (file+headline me--private-org-note "Capture")
    (file me--org-todo-template)
    :empty-lines-before 1)

   ("pl" "Log Notebook Entry" entry
    (file+headline buffer-file-name "Begin Log Notebook:")
    (file "02-Templates/t-log-entry.txt")
    :unnarrowed t
    :tree-type week
    :prepend t
    :clock-in t
    :clock-keep t
    :empty-lines 1
    :immediate-finish t)

   ("w" "Work Templates")

   ("wl" "Work Log Entry" entry
    (file+datetree me--work-log) "** %U - %^{Activity}  :LOG:")

;;; This section commented out until later configuration...
   ;; ("t" "todo" entry
   ;;  (file
   ;;   (expand-file-name me--default-org-note me--org-files)
   ;;   "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t))

   ;; ("r" "respond" entry
   ;;  (file
   ;;   (expand-file-name me--default-org-note me--org-files)
   ;;   "* TODO Respond to %:from on %:subject\n%U\n%a\n"
   ;;   :clock-in t :clock-resume t :immediate-finish t))

   ;; ("n" "note" entry
   ;;  (file (expand-file-name me--default-org-note me--org-files)
   ;;        "* %? :NOTE:\n%U\n%a\n"
   ;;        :clock-in t :clock-resume t))

   ;; ("j" "Journal" entry
   ;;  (file+datetree (expand-file-name me--default-org-diary me--org-files)
   ;;                 "* %?\n%U\n"
   ;;                 :clock-in t :clock-resume t))

   ;; ("w" "org-protocol" entry
   ;;  (file (expand-file-name me--default-org-note me--org-files))
   ;;  "* TODO Review %c\n%U\n"
   ;;  :immediate-finish t)

   ;; ("p" "Phone call" entry
   ;;  (file (expand-file-name me--default-org-note me--org-files))
   ;;  "* PHONE %? :PHONE:\n%U"
   ;;  :clock-in t :clock-resume t)

   ;; ("h" "Habit" entry
   ;;  (file (expand-file-name me--default-org-note me--org-files))
   ;;  "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
   )))

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
      (quote ((agenda habit-down time-up user-defined-up priority-down effort-up category-keep)
	      (todo category-up priority-down effort-up)
	      (tags category-up priority-down effort-up)
	      (search category-up))))

;;; 
;; Start the weekly agenda on Monday
(setq org-agenda-start-on-weekday 1)

;;; 
;; Enable display of the time grid so we can see the marker for the current time
(setq org-agenda-time-grid (quote ((daily today remove-match)
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

(setq org-link-frame-setup (quote ((vm . vm-visit-folder)
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
      (quote (("N" "Notes" tags "NOTE"
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
