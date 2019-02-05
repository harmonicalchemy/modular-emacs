![Banner](banner.jpg)

[![License](https://img.shields.io/badge/LICENSE-GPL%20v3.0-blue.svg)](https://www.gnu.org/licenses/gpl.html)

## Introduction:

**Welcome to Harmonic Alchemy Modular Emacs V1.0 RC2** - Now final testing my second release candidate.  This is almost ready to be dubbed my first official release.  There are only a few glitches left (on Mac OS)...

This project originally started out after I forked Mohammed Ismail Ansari's  [**super-emacs project**](https://github.com/MyTerminal/super-emacs) from Github a few years ago...  I changed so many things, adding modules etc. from my pre-existing Emacs setup that over time everything became a total mess!  So I decided to start over from scratch and modularize everything with the idea of preventing messes like this from happening with your own Emacs setups!

Keeping the original spirit that Mohammed started, I plan to build this into something that you can use and extend for your own purposes without experiencing the same kinds of pain I have over the years attempting to customize my own Emacs configurations!

I am a developer, computer scientist, composer/musician, sound designer, architect, writer.  I wear a lot of hats! My emacs needs to wear a lot of hats as well!  I am attempting to build features into **Emacs** that empower all the above without becoming an over complicated mess! **_"good luck with that"_**  :octocat:

If you like that idea, please drop me a note to let me know! Ask questions, etc.  Thanks!  I have been using emacs for over 25 years!  Never shared with many before now though... :heart_decoration:

I promice to strive to make this **process/journey** as painless as possible for you as I have found many other Emacs setups to be way to complicated to make it easy for adoption into my workflow, and you are probably experiencing the same!  On the other end of the spectrum... Installing simple Emacs alone is not enough to get you started (IMHO)...  

Therefore: **Here is Modular Emacs!** I hope my long years of **devOps** experience will turn out a nice jewel for new comers to try out and succeed with!  **Good Luck and God Speed!**

## Requirements:

I strive to make this simple starting out with as few fancy extras as possible.  This will provide you with a simpler base from which to build to your own needs... Having said that... there are a few things I find to be vital for anyone who writes as well as codes and this means you... **All coders need to document as well, so there you go!**  **_My plan is to build the best Emacs Skeleton Base Configuration for Writing as well as Coding..._**  Please help me accomplish this goal!  I will need your input!  For now here is what you need to do:

- **Emacs V26.1+ :** Many of the default packages installed with this project will not work with previous versions of Emacs. These new features are vital to make things work well going forward from 2018 onward, so I decided not to support lesser versions of Emacs...  The latest version of Emacs is available on Fedora but not Debian yet. On Debian you will have to build from source.  The new version of Debian will include Emacs 26.1 however, and that is coming soon!  On Mac OS you can get the latest version of Gnu Emacs via Homebrew...  There are easy ways to get the latest version of Emacs on Ubuntu LTS as well... _(You may have to install from source within your system `sudo user's` $HOME account however)_.  Don't give your Apache web user the emacs App.  That would increase your server's www attack surface too much in my humble opinion...  Let's keep this part open to discussion... Better solutions will present themselves.     

- **VMD _(Visual MarkDown)_: -** This is a stand alone `node.js` **Electron Web App** that you need to install in your `$HOME` directory for this Emacs setup to use.  **VMD** allows you to visualize the results of your edits of **markdown files:** `.md` & **ORG files:** `.org` **instantly! _(in  real time, while you edit the markdown text within your emacs buffer, all without the need for a full web browser)..._**     
Your rendered markdown will appear as it would look on GitHub!  This is a **_much smarter tool for writing/publishing_** because it combines the ease of: `WYSIWYG` with the power of `text-editors`.  That is the best of both worlds with no compromise!     
The best way to install **VMD** is locally within your home directory _(not system wide via your distro's package manager, or app store)..._    
The best way to install locally is to install it as a `Node.js` package...      
To do that you must first install **Node Version Manager** `NVM` using a script from: [`https://github.com/creationix/nvm`](https://github.com/creationix/nvm).      
Follow the instructions on the above linked `nvm` Github project to install **NVM**, and the latest `LTS` version of `node.js`  Once you have the latest version of `node.js` installed you will also have `NPM`**_Node Package Manager_** installed with it, _(because it is bundled with the latest LTS versions of `node.js`)_       
**Whew!**  That was a lot to digest! It's probably the most difficult part of this whole thing, but once that is done via the linked instructions above, you can easily install VMD with the following command: _(from your home directory)_  
> `$ cd ~/`     
> `$ npm install -g vmd`     

- **Fountain Screenwriting Tools: _(optional)_** For processing text files from emacs and exporting them to formats used within the industry etc.  These tools can be installed with the Node Package Manager _(just as for installing **VMD**)_ I am not making this a requirement obviously, but if you are into fiction or screenwriting Fountain should be a main part your emacs publishing environment!

- **ZSH _(optional)_:** I use `zsh`, `Antigen` & `Oh-My-Zsh` as my default unix shell configuration.  There may be some configurations within emacs here that make that work well.  Hopefully those configs will not change a `bash` or other `shell` user's experience...  I will have to test that to make sure...  I don't wish to make Zsh a requirement.  This should work equally well in the bash shell, or any POSIX compliant shell.  

## Details: 

The above requirements are pretty much all you need to get ready for installing and running this project in your home directory... Also, not covered here, you need to have a standard unix like development environment set up on your machine as well, _(even if all you intend to do is write but not code)_ you will still need to have some of the standard **devTools** installed and configured to make some of these cool writer's features work well.

Discussion of getting the rest of your development environment configured is outside the scope of this project.  However, for example: On **Mac OS** you will need to have **Xcode** and **Homebrew** installed for any of this to work correctly on a Mac.  The best way to do that is to go to the **[Homebrew main website](https://docs.brew.sh/Installation)** first and use their instructions for installing Homebrew on Mac OS.  They will point you in the right direction for installing the required Xcode tools first...  Likewise on Linux you will need to install some standard unix devTools via your distro's package manager...

This project and everthing you need to install and run it is all open-source and very portable across many computing platforms!  With this project installed you will easily be able to edit any `markdown` or `.org` text file within any Emacs buffer while at the same time previewing **live real-time updates** to the automagically rendered version of the buffer you are editing!  This is currently the **only** way I have found it possible to perform live edits within a text file and see the results displayed (as you type) within another window fully rendered as it would look on a website!  That feature alome is totally revolutionary for any writer to use!  

**OMG! _This is a life saver for me who has to use Scrivener_** on a **_Mac_** _(proprietary software)_ to organize all my reference and fiction writing projects...  Eventually I plan to replace **Scrivener** with a powerful `org-mode` based **Emacs Publishing Environment**. Then I will be open-source future proof finally! You can't imagine how many times over the past 30 years! I had to go through some painful conversion process after the proprietary app I was using (for several years) suddenly became obsolete, the company went out of business, or they just dropped that product without asking my permission!  :grimacing:

For you Millennials, 30 years ago was before the web, and even before the Internet itself was commercialized as well!  Back then we called it the ARPANET. _I came aboard shortly after it was changed from DARPANET!_ ;-)   btw, I am still waiting for some of the original specs to get done,  _(e.g., `P2P`, `blockchain`, `smart-contracts`, `consensus algorithms`, etc.)._  All that still needs to be implemented! 

I said above **_"still waiting"_** because many are calling all those features: **Web 3.0** when it really should be **Internet 1.0!**  The government wanted those features to make our Internet truly bomb proof, but business trumped them... Don't believe it?  Go read the original BSD specs...  You will see that all that **client/server** stuff was a compromise that ended up sticking when we all went commercial... To be honest, we did not have the tech back then to implement all those requirements in a practical way.  Now we do... _(well almost...)_ Consensus and Byzantine Fault Tolerance is a bitch to get right!  But Lets get it done already! OK? ;-)

By using open source exclusively as much as possible, your bottom line becomes: **_(If you use it and the maintainer quits support, you can fork the project and support it yourself!  You have options, and you can take total control of the software if needed.)_** More importantly, you will own outright your own copy of this software and may do with it as you like, **_(except make it into a proprietary product of course)_**.  

The **_share-and-share-alike_** rule keeps your work and all its derivatives open-source for others to fork and/or contribute which will help you smash those bugs much faster!  You will also be taking part in a global community and will make some very interesting friends over time!  This is all good for the open-source ecosystem.  It keeps it healthy and keeps it alive!  This is the true meaning of **_"Free Software"_** and the original reason I decided to adopt my life to it.  There are many other great reasons for using all **FOSS** as well of course...  Therefore... keeping in the spirit of "The Cathedral and the Bazaar", here we bravely go!  Enough talking already!

## Start Here -  Modular Emacs Setup:

### Backup existing Emacs which may already be installed:

If you already have emacs installed, you should backup your existing environment first.  Simply renaming your existing `~/.emacs.d/` folder and any `.emacs` init files that may also be in your home directory is sufficient...  Change them to something like: `.default.emacs` &  `.default.emacs.d`.  Don't delete these renamed files from your `$HOME` directory...   You may need them later to come back to if you get into trouble and just want things back the way they were before you decided to try any of this out...  To start over, or give up...  What ever... Hopefully not to give up ;-)

Now you are ready to clone Harmonic Alchemy Modular Emacs into your home directory as: `~/.emacs.d/`

But lets Hold off cloning directly to `~/.emacs.d` and consider the following possible scenario: If you are using emacs on a daily basis and are used to that, you need to use your existing emacs setup to do some of the things below...  If you pre-maturely remove your emacs environment ahead of time you will have to resort to command line only to do everything below...

In that case, leave your existing **`~/.emacs.d`** folder as it is... _(don't rename it as instructed above)_...  Instead, clone `modular-emacs` to a temporary name i.e., `me.emacs.d` first.  Then you will be able to use your existing emacs environment (the one you are used to using) to finish doing the configurations below... **_This is how we will do it in the following example:_** No harm done doing it either way.  Just understand the temporary directory name we used below will be different if you are doing it directly to **`$HOME/.emacs.d`**

### Clone: Harmonic Alchemy Modular Emacs

_(clone to a temporary directory `~/me.emacs.d`)_

    $ git clone https://github.com/harmonicalchemy/modular-emacs.git ~/me.emacs.d

This will clone `modular-emacs.git` into a fresh new empty `me.emacs.d` directory in your home directory.  Everything you need to run Emacs will be installed within that parent directory exclusively.  No outside .emacs init files will be created or used...

If you navigate within your new cloned directory you will find the `lisp` sub directory.  Go into the **`lisp`** directory and you will see three sub directories there: ** `modules`**, **`my-modules`**, & **`themes`**...

The purpose of the **`my-modules`** folder is to give you a place to keep your own extensions to **Modular Emacs** without causing **git** to complain about new un-tracked files.  `my-modules` has been added to `.gitignore` so anything you create within that directory will be ignored as well!  This way you can try out different things without worrying about messing up the base install...  

> **Note:** You may want to create your own private local **`branch/fork`** of this repository where **`my-modules`** directory is not ignored, and then you can keep your own customized version under local git revision as well.  Once you do this, you may or may not wish to **`pull/merge`** any new changes from **Modular Emacs** into your new private branch...  You could also simply **"cherry pick"** **`cut/paste`** snippets of `lisp` code from new features released in the **master** Modular Emacs branch into your **local branch** instead...  The choice is up to you... Free software! You have many options! Yay! 

**Clone MultiMarkdown Mode from its public repo on Github into the `lisp/my-modules` directory:**   

> **Note:** _This package is not available on `MELPA` but it is required for `modular-emacs` to run...  It makes **markdown** files like this look good on GitHub!_ :octocat: We will clone this into **`my-modules`** so that it will be ignored...  If it were on `MELPA` it would be installed within the `melpa` directory _(which is also ignored by git in this project)_

```
   $ cd ~/me.emacs.d/lisp/my-modules/
   $ git clone https://github.com/jmquigley/mmd-mode.git 
```

Now go back to your cloned `me.emacs.d` parent directory to do the last few things needed to get Modular Emacs running...

**Copy: `~/me.emacs.d/me.init.el`** to: **`~/me.emacs.d/init.el`**

_`init.el` has also been put into .gitignore so you can edit that file to your hearts content without disrupting the state of the master branch!_  You may also choose to add your `init.el` copy to a local git branch in parallel to track your own custom changes to that file as well...  

> **Note:** Don't mess with **`~/.emacs.d/me.init.el`**.  That is there for reference only...  You may need to copy it again if you mess up your first copy somehow or you simply want to get back to the default setup out of box to start over or something like that...

**Create an empty `custom.el` file within the `~/me.emacs.d/` directory**.

```
   $ cd ~/me.emacs.d
   $ touch custom.el
```
This file is also named within the `.gigignore` file so that anytime you use the built in Customize features of Emacs they won't disrupt the state of your master branch...

## OK... Ready Freddy? Lets start up Emacs and see what happens!:

First Rename your existing:  **`~/.emacs.d`** to: **`~/save.emacs.d`** _(You may have already done this in the beginning)_  You may also have a `.emacs` init file outside of the `.emacs.d` folder that also needs to be renamed!

Next Rename: **`~/me.emacs.d`** to: **`~/.emacs.d`** _(This is now your new Emacs Home Directory)_

**_Start up GNU Emacs v26.1 from the menu or command line and hold your breath!_**

After a lot of super computing lisp number crunching flashing away in your minibuffer _(wait wait wait...)_ Then **Bam!** You should now see your new emacs pop up with the **Welcome to Harmonic Alchemy Modular!** scratch buffer in a frame with a pre-determined row/column initial size! 

**No?** Broken? Oh My! What a freekin let down!  I feel your pain!  OK... You may be fine if there are only warnings... _(which are hard to avoid upon first startup with all those new packages compiling etc. Not much you can do about that... The developers who made the packages need to clean that up, you could help them though. ;-)_

If you got an error and see the default emacs screen, try to retrace your steps or try running emacs with the debugger turned on...  Its probably something dumb and easy to fix...  _(happens to us all the first time, most of the time)_

**Yes?** It worked!  **Yay!** What a fantastic feeling the first time eh?

> **Note:** Fetching/pulling new changes from the master `modular-emacs` Github repository to: local: `~/.emacs.d/` will automatically be reflected within your emacs configurations... No need to copy any more files... 

> If you wish to keep your changes separate from the remote master branch, create your own local `develop` branch _(or call it what you like)_ and maintain your local changes in parallel... Change your local branch's .gitignore to accommodate your needs...

**Usage:**

In this kind of setup, your local `~/.emacs.d/init.el` is not kept in modular-emacs code revision.  Instead, you may use this file to try out experimental customization and you may wish to keep it in your local branch under code revision by removing it from your local branch's `.gitignore` file.  If something goes wrong you can quickly refresh this file from the original `~/.emacs.d/me.init.el`.

You can also use the Modular Emacs \*scratch\* buffer to paste experimental lisp code to try out before making it permanent as a stand alone `.el` emacs lisp file...  you can evaluate the entire \*scratch\* buffer without removing the banner message at the top as that message is one huge lisp comment...  If something horrible happens, no worries... Restart emacs... Your messup will be gone and you will be back to where you were before you or that elisp test code goofed up...

If your customization proves stable, and you like it, you could then save your scratch buffer (as is) to a new file, for example: `~/.emacs.d/lisp/my-modules/00-my-new-module.el` _(use your own module name prefixed with `01.` `02.` `03.` `...` etc.)_ for your new custom module name.  This will make you consistant with the naming convention used within `~/.emacs.d/lisp/modules` directory...

> **Note:** _(You should be on your local `develop` branch when you do this to keep your changes in parallel with the master branch)_.  Add a call to this new module from your `~/.emacs.d/init.el`

In all cases you would be wise to create and checkout a local `develop` branch and keep all your custom changes in there separate from the `master` branch...

Modular Emacs comes with my slightly customized version of the **Blackbaord color theme**.  If you would like to add more custom themes or a different theme than `blackboard.el`, you can replace it or add additional themes into your local branch's: `~/.emacs.d/lisp/themes` directory and they will also will work with this setup by choosing `M-x load-theme` or changing the last line within `~/.emacs.d/lisp/modules/06-interface.el` to: `(load-theme 'your-chosen-theme-name t)`

> **Note:** _Obviously if you add more themes to your **Modular Emacs** **themes** directory you will be adding new un-tracked files to your cloned git repository!  Make sure you have created your own local branch and that is currently checked out before adding new themes or doing any customization outside the init.el file or the `my-modules` directory.  Then you will have proper management of your local custom changes and also have all that in code revision as well!_  How many times have I said this already?  OMG! :octocat:

## Features:

**Default Emacs welcome screen replaced with simple greeting:** Prints current emacs configuration, and date... With a famous quote _(which may change from time to time with new updates)_  Use Scratch buffer to evaluate snippets of `lisp` without having to remove the welcome text... _(which are lisp comments)_

### Key-bindings:

**_[See: 09-key-bindings.el](.emacs.d/lisp/modules/09-key-bindings.el)_**

- Reload file in a buffer quickly with `<f5>`

- **TODO:** _Copy More Key Commands Here before the final release!_

### Pre-configured packages - Comprehensive list:

#### Misc. Packages:

- [sublimity](https://github.com/zk-phi/sublimity) Makes Buffers Scroll Smoothly.
- [meta-presenter](https://github.com/myTerminal/meta-presenter) Present Slide Shows in Emacs.
- [multiple-cursors](https://github.com/magnars/multiple-cursors.el) Edit text with nice cursors!
- [powerline](https://github.com/milkypostman/powerline) Decorate `mode-line` & make it More Informative.
- [buffer-move](http://www.emacswiki.org/emacs/buffer-move.el) Quickly Move Buffers Within Windows.
- [auto-complete](https://github.com/auto-complete/auto-complete) Easy Text Editing with suggested word completion.

#### Core Emacs System Related Packages:
- [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell) Get environment variables such as $PATH from the shell.  btw, this can be a tricky thing to do in emacs!  I have struggled with this on Mac OS for a while!  You may find you have to start Emacs from the terminal to get all your environment vars into Emacs!  More about that later.. A continuing saga!

#### Emacs Helm & Extras:
- [helm](https://github.com/emacs-helm/helm) Helm is an Emacs incremental and narrowing framework that helps speed up operations on commands with structured command completions and more.  It is a programmable interface (API) as well.
- [helm-core](https://emacs-helm.github.io/helm/) Development files for Helm (API)... Installed as dependency.
- [popup](https://github.com/emacs-helm/helm) Popup is a dependency package for helm.
- [which-key](https://github.com/justbur/emacs-which-key) Used with helm, pops up suggestions for what can follow after pressing `C-x` key...

#### Ace Jump Mode & Helpers:

- [ace-jump-mode](https://github.com/winterTTr/ace-jump-mode) Makes it Easy to Jump Around Quickly.
- [ace-window](https://github.com/abo-abo/ace-window) Switch Windows Visually.
- [avy](https://github.com/abo-abo/avy) Jump to Arbitrary Positions in Visible Text & Select Text Quickly. Installed as dependency to: `ace-window`

#### Tree & Menu Related Packages:

- [ztree](https://github.com/fourier/ztree) An efficient directory tree comparison utility.
- [undo-tree](https://github.com/emacsmirror/undo-tree) Visualize your changes in a buffer.
- [neotree](https://github.com/jaypei/emacs-neotree) Spawn a tree at any directory.
- [imenu-list](https://github.com/bmag/imenu-list) Right panel Menu and Variable list.

#### Dired Related Packages:

- [dired-launch](https://github.com/thomp/dired-launch) Open files directly within `dired` using default applications.
- [dired-imenu](https://github.com/DamienCassou/dired-imenu) `imenu` binding for `dired-mode`

#### dev§Ops, sys§Admin, info?Sec Related Packages:

- [ssh-config-mode](https://github.com/jhgorrell/ssh-config-mode-el) A mode to edit SSH config files.
- [smart-tabs-mode](https://www.emacswiki.org/emacs/SmartTabs) Intelligent tabs to end the f---ing tabs vs spaces war already ok? Read the docs :-)
- [php-mode](https://github.com/emacs-php/php-mode) Major mode for editing PHP code.

##### More languages, devOps tools, etc. to add later:

- `digitalocean-helm`  Woah? A `helm` Interface and API connector to your Digital Ocean Account! _(I need to read more about this first...)_

#### Writer's \- Publishing Tools:

- [markdown-mode](https://jblevins.org/projects/markdown-mode/) Markdown capability within emacs.
- [markdown-mode+](https://github.com/milkypostman/markdown-mode-plus) Extras for Markdown mode.
- [vmd-mode](https://github.com/blak3mill3r/vmd-mode) Fast Github-flavored Markdown previews synchronized with changes to an emacs buffer (no need to save).  Renders org files _(and normal markdown)_ in an external VMD App that automatically shows changes in real-time as you type! _(You need to install this external app separately, and make sure Emacs knows where to find it.  See: `exec-path-from-shell` below)_
- [fountain-mode](https://github.com/rnkn/fountain-mode) For **#writers #screenwriters #Fantastic!!!**
- [olivetti](https://github.com/rnkn/olivetti) Adjust margins with word wrap. _(great with fountain mode!)_ Perfect for distraction free writing!

#### Spelling:

- [flyspell-correct](https://github.com/) Flyspell: correct, suggestions, actions, dictionary add, subtract, etc.
- [helm-flyspell](https://github.com/pronobis/helm-flyspell) Helm extension for correcting words with flyspell.
- [flyspell-correct-helm](https://github.com/d12frosted/flyspell-correct) Nice helm interface for flyspell.

#### Project Management Tools:

- [org-bullets](https://github.com/emacsorphanage/org-bullets) Fancy bullet fonts for Org Mode... Author is absentee landlord but these work great for me just fine...  it's a pretty short lisp file to maintain. ;-)

#### Git \- `magit` & Related:

- [magit](https://github.com/magit/magit) A `git` porcelain inside Emacs. _(the main package)_
- [magit-popup](https://github.com/magit/magit-popup) Installed as dependency to `magit`
- [git-commit](https://github.com/magit/git-modes) Installed as dependency to `magit`
- [dash](https://github.com/magnars/dash.el) A modern list library for Emacs. Installed as dependency to `magit`
- [async](https://github.com/jwiegley/emacs-async) Asynchronous processing in Emacs. Installed as dependency to `magit`, `helm`, & others...
- [gitattributes-mode](https://github.com/magit/git-modes) Major mode for editing .gitattributes files.
- [gitconfig-mode](https://github.com/magit/git-modes) Major mode for editing .gitconfig files.
- [gitignore-mode](https://github.com/magit/git-modes) Major mode for editing .gitignore files.
- [with-editor](https://github.com/magit/with-editor) Use the Emacsclient as $EDITOR.

