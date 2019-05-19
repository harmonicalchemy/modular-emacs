![Banner](banner.jpg)

[![License](https://img.shields.io/badge/LICENSE-GPL%20v3.0-blue.svg)](https://www.gnu.org/licenses/gpl.html)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Harmonic Alchemy Modular Emacs:](#harmonic-alchemy-modular-emacs)
    - [Introduction:](#introduction)
    - [Disclaimer for Windows Users:](#disclaimer-for-windows-users)
    - [My Commitment:](#my-commitment)
    - [Requirements:](#requirements)
        - [Emacs: V26.1+](#emacs-v261)
        - [VMD: _(Visual MarkDown App)_](#vmd-visual-markdown-app)
        - [MultiMarkdown: _(optional)_](#multimarkdown-optional)
        - [Pandoc: _(optional)_](#pandoc-optional)
        - [Steel Bank Common Lisp _(optional for CS research, prototyping, etc.)_](#steel-bank-common-lisp-optional-for-cs-research-prototyping-etc)
    - [Details:](#details)
    - [Start Here: _(Modular Emacs Setup)_](#start-here-modular-emacs-setup)
        - [Backup existing Emacs which may already be installed:](#backup-existing-emacs-which-may-already-be-installed)
        - [Clone: Harmonic Alchemy Modular Emacs](#clone-harmonic-alchemy-modular-emacs)
        - [Clone MultiMarkdown Mode:](#clone-multimarkdown-mode)
        - [Clone RMOO MOO Client from Github: _(optional)_](#clone-rmoo-moo-client-from-github-optional)
        - [Copy `me.init.el` to: `init.el`:](#copy-meinitel-to-initel)
        - [Create an empty `custom.el` file:](#create-an-empty-customel-file)
    - [OK... Ready Freddy? Start up Emacs and see what happens!](#ok-ready-freddy-start-up-emacs-and-see-what-happens)
    - [Features:](#features)
        - [Pre-configured packages - Comprehensive list:](#pre-configured-packages---comprehensive-list)
            - [Misc. Packages:](#misc-packages)
            - [Core Emacs System Related Packages:](#core-emacs-system-related-packages)
            - [Emacs Helm & Extras:](#emacs-helm--extras)
            - [Ace Jump Mode & Helpers:](#ace-jump-mode--helpers)
            - [Tree & Menu Related Packages:](#tree--menu-related-packages)
            - [Dired Related Packages:](#dired-related-packages)
            - [dev§Ops, sys§Admin, info✷Sec Related Packages:](#devops-sysadmin-info✷sec-related-packages)
                - [More languages, devOps tools, etc. to add later:](#more-languages-devops-tools-etc-to-add-later)
            - [Writer's \- Publishing Tools:](#writers---publishing-tools)
            - [Spelling:](#spelling)
            - [Project Management Tools:](#project-management-tools)
            - [Git \- `magit` & Related:](#git---magit--related)
        - [Key-bindings:](#key-bindings)

<!-- markdown-toc end -->

# Harmonic Alchemy Modular Emacs:

## Introduction:

**Welcome to Harmonic Alchemy Modular Emacs - _Official V1.0.2 point release Q2-2019_** -

> **Note:** I still have not solved the Mac OS glitch, of not getting shell environment vars into Emacs, _(Emacs cannot find the VMD app)_.  I still suspect the cause is external to Emacs though...  The band-aid fix is to start Emacs from the shell and everything works fine... So I got that problem on the back burner for now...  If you are a Mac User and find a solution please let me know! I will give you credit!

This project has been a long time coming.  Its history goes back a few years ago when I went searching for some better ways to configure my then pretty basic Emacs configuration of over 10 or so years!  I cloned a few Emacs repos on Github to try different things out for a while.  I liked some things and tried to get rid of other things after installing them which got confusing after a while. I ended up with lots of questionable elisp code, much of which were things possibly no longer needed!  After a couple years of adding/removing configuring/re-configuring it started feeling like I was painting myself into a tangled corner like getting _"lost in a maze of twisty little passages all different"_!  So I decided to start over from scratch and modularize everything with the idea of preventing tangled messes like this from happening with your own Emacs setups!

I am a devOps engineer, computer scientist, composer/musician, sound designer, architect, writer/publisher.  I wear a lot of hats! My emacs needs to wear a lot of hats as well!  I am attempting to build features into **Emacs** that empower all the above without becoming an over complicated mess! **_"good luck with that"_**  :octocat:

## Disclaimer for Windows Users:

For the first 10 years out of college, I worked around the Boston 128 area as a hired gun scientific and industry control systems programmer.  The systems I worked on were either unix or VAX VMS based main-frames _(or mini-mainframes)_ and workstations. My favorite OS back then was HP-UX. I used Apollo workstations for a short while and then we switched to SUN workstations.  When the really nice HP-UX workstations came around, I was in love!  At home I have always used a Mac...  I had the first classic Mac+ when it first came out around 1985...  

I did not do any IBM PC (Windows) programming until I went to work for IBM around 15 years later.  At that point I was a software architect and I hated MS Visual Studio even for prototyping designs!  After leaving IBM I started my own Application Service Provider (ASP) company _(what we used to call the Cloud)_ and based my co-located servers on SCO unix (for commercial work) and other flavors of BSD, and RedHat Linux for open-source projects...  

I have been retired since 2017 but I am doing more development, writing, and music than ever before!  Funny how that works once the checks start showing up for free. %^)  All my efforts are focused on open-source development and I am putting together a publishing platform within a private 100% open-source cloud for our local community here on Orcas Island...

OK I got off track there... The main point of that long story above is if you use Emacs on Windows, you could help me immensely by testing Modular Emacs within your MS Windows environment. _(As you can see above, I am a Windows dummy who has not tested any of this on Windows yet!)_   

If you find glitches please let me know... I need your knowledge for that part...  I will give you credit for any Windows solutions you provide.  I had some problems with _(unwanted)_ scroll bars showing up on new frames _(after initial frame)_ and read about others having this problem on Windows... I found a simple _(best practice)_ fix for Mac OS and Linux, but don't know if it will work on Windows yet...  

I have already added in some _(commented out)_ code to: `.../lisp/modules/06-interface.el` after reading some things on Stack Exchange about problems with scroll bars on Windows...  You could un-comment them to try if you are having the same problems...  Let me know if that works...  Thanks!

But also in general, if you like this "modular" Emacs idea, please drop me a note to let me know! Please do ask questions, etc.  Thanks!  I have been using Emacs for over 25 years but I never shared any of that Emacs knowledge with anyone before now... :heart_decoration:

> **_"Always do what is right. It will gratify half of mankind and astound the other."_** - Mark Twain

## My Commitment:

I promise to strive to make this **process/journey** as painless as possible for you as I have found many other Emacs setups to be way to complicated to make it easy for adoption into my workflow, and you are probably experiencing the same!  On the other end of the spectrum... Installing simple Emacs alone is not enough to get you started (IMHO)...  

Therefore: **Here is Modular Emacs!** I hope my long years of **devOps** experience will turn out a nice jewel for new comers to try out and succeed with!  **Good Luck and God Speed!**

## Requirements:

I strive to make this simple starting out with as few fancy extras as possible.  This will provide you with a simpler base from which to build to your own needs... Having said that... there are a few things I find to be vital for anyone who writes as well as codes and this means you... **All coders need to document as well, so there you go!**  **_My plan is to build the best Emacs Skeleton Base Configuration for Writing as well as Coding..._**  Please help me accomplish this goal!  I will need your input!  For now here is what you need to do:

### Emacs: V26.1+

Many of the default packages installed with this project will not work with previous versions of Emacs. Also the security of older Emacs and packages is Horrendous! Recently, some of the older packages were removed from the updated MELPA and the entire site is TLS enabled now!  These new features are vital to make things work well, staying reasonably safe going forward from 2018 onward, so I decided not to support lesser versions of Emacs...  The latest version of Emacs is available on Fedora and also on the latest version of Debian 9.8 _(Just Released - I think may also have Emacs 26)_. On Debian you still may have to build from source.  The new version of Debian _(Buster)_ will include Emacs 26.1 for sure, and that is coming soon!  On Mac OS you can get the latest version of Gnu Emacs via Homebrew...  There are easy ways to get the latest version of Emacs on Ubuntu LTS as well... _(You may have to install from source within your system `sudo user's` $HOME account however)_.  Don't give your Apache web user the Emacs App.  That would increase your server's WWW attack surface with far too many holes in my humble opinion... On servers, install Emacs locally within your System Admin Users account only.  Let's keep this part open to discussion... Better solutions will present themselves.     

### VMD: _(Visual MarkDown App)_

This is a stand alone `node.js` **Electron Web App** that you need to install in your `$HOME` directory for this Emacs setup to use.  **VMD** allows you to visualize the results of your edits of **markdown files:** `.md` & **ORG files:** `.org` **instantly! _(in  real time, while you edit the markdown text within your Emacs buffer, all without the need for a full web browser)..._**     
Your rendered markdown will appear as it would look on Github!  This is a **_much smarter tool for writing/publishing_** because it combines the ease of: `WYSIWYG` with the power of `text-editors`.  That is the best of both worlds with no compromise!     

> **Note:** Not having **VMD** installed will not break Modular Emacs, however you will of course not be able to use Emacs with VMD, some Modular Emacs defined keys will not work, and may even cause an error within Emacs if you try those keys...  You will also see compile time errors when you first start up Modular Emacs.  After startup, Modular Emacs should work fine without VMD...

The best way to install **VMD** is locally within your home directory _(not system wide via your distro's package manager, or app store)..._    
The best way to install locally is to install it as a `Node.js` package...      
To do that you must first install **Node Version Manager** `NVM` using a script from: [`https://github.com/creationix/nvm`](https://github.com/creationix/nvm).      
Follow the instructions on the above linked `nvm` Github project to install **NVM**, and the latest `LTS` version of `node.js`  Once you have the latest version of `node.js` installed you will also have `NPM`**_Node Package Manager_** installed with it, _(because it is bundled with the latest LTS versions of `node.js`)_  

**Whew!**  That was a lot to digest! It's probably the most difficult part of this whole thing, but once that is done via the linked instructions above, you can easily install VMD with the following command: _(from your home directory)_  
> `$ cd ~/`     
> `$ npm install -g vmd`  

### MultiMarkdown: _(optional)_

You don't actually need the MultiMarkdown package installed on your machine to use MultiMarkdown in Emacs.  There is a package for that which you will be installing further down within this document.  However you will need multimarkdown installed later to do some fancy publishing tricks and conversions...  Some of that functionality overlaps with Pandoc as well.  It does not hurt to have both of these technologies installed on your machine...

If you are on Mac OS you can install MultiMarkdown via Homebrew.  `$> brew install multimarkdown`.  On Linux follow the instructions on this page <https://fletcher.github.io/peg-multimarkdown/> This same page will show you how to customize MultiMarkdown as well but don't do any of that yet. You won't know what you need until you have used it for a while.  That's it for now...  you will also be installing mmd-mode below...

### Pandoc: _(optional)_

As with MultiMarkdown you don't need to have Pandoc installed to use Emacs _(until you have written something and want to export it to LaTeX, PDF, or something like that directly from Emacs)_  You can also do some of these exports with multimarkdown tricks as well...  Read the docs for both to decide how to use them.  Instructions for installing Pandoc can be found on their official website here: <https://pandoc.org/installing.html> The Pandoc.org website has most everything you will need all in one place...

### Steel Bank Common Lisp _(optional for CS research, prototyping, etc.)_

Steel Bank Common Lisp is the best full-fledged Lisp compiler option for Fedora and Debian...  You don't actually need a full fledged Lisp compiler for Emacs because Emacs Slime Mode takes care of handling most things internally within Emacs and also provides a nice [REPL](https://en.wikipedia.org/wiki/Read-eval-print_loop) interface with that setup by default...  Adding Steel Bank Common Lisp to your system (with Slime mode in Emacs) is arguably the best IDE for serious Lisp projects... _(my opinion and totally biased of course ;-)_

To install `SBCL` on Fedora do:
    `$> sudo dnf install sbcl`

To install `SBCL` on Debian do:
    `$> sudo apt install sbcl`

That's it... simple but powerful _(like lisp)_


## Details:

The above requirements are pretty much all you need to get ready for installing and running this project in your home directory... Also, not covered here, you need to have a standard unix like development environment set up on your machine as well, _(even if all you intend to do is write but not code)_ you will still need to have some of the standard **devTools** installed and configured to make some of these cool writer's features work well.

Discussion of getting the rest of your development environment configured is outside the scope of this project.  However, for example: On **Mac OS** you will need to have **Xcode** and **Homebrew** installed for any of this to work correctly on a Mac.  The best way to do that is to go to the **[Homebrew main website](https://docs.brew.sh/Installation)** first and use their instructions for installing Homebrew on Mac OS.  They will point you in the right direction for installing the required Xcode tools first...  Likewise on Linux you will need to install some standard unix devTools via your distro's package manager...

This project and everything you need to install and run it is all open-source and very portable across many computing platforms!  With this project installed you will easily be able to edit any `markdown` or `.org` text file within any Emacs buffer while at the same time previewing **live real-time updates** to the automagically rendered version of the buffer you are editing!  This is currently the **only** way I have found it possible to perform live edits within a text file and see the results displayed (as you type) within another window fully rendered as it would look on a website!  That feature alone is totally revolutionary for any writer to use!  

**OMG! _This is a life saver for me who has to use Scrivener_** on a **_Mac_** _(proprietary software)_ to organize all my reference and fiction writing projects...  Eventually I plan to replace **Scrivener** with a powerful `org-mode` based **Emacs Publishing Environment**. Then I will be open-source future proof finally! You can't imagine how many times over the past 30 years! I had to go through some painful conversion process after the proprietary app I was using (for several years) suddenly became obsolete, the company went out of business, or they just dropped that product without asking my permission!  :grimacing:  :weary:

For you Millennials, 30 years ago was before the web, and even before the Internet itself was commercialized as well!  Back then we called it the ARPANET. _I came aboard shortly after it was changed to DARPANET but we never called it that_ ;-)   btw, I am still waiting for some of the original specs to get done,  _(e.g., `P2P`, `blockchain`, `smart-contracts`, `consensus algorithms`, etc.)._  All that still needs to be implemented! 

I said above **_"still waiting"_** because many are calling all those features: **Web 3.0** when it really should be **Internet 1.0!**  The government wanted those features to make our Internet truly bomb proof, but business trumped them... Don't believe it? Can you say **_"distributed adaptive message block switching"?_**  Go read the original BSD specs...  You will see that all that **client/server** stuff was a compromise that ended up sticking when we all went commercial... To be honest, we did not have the tech back then to implement all those requirements in a practical way.  Now we do... **_"Decentralized adaptive message block switching. lol"_** _(well almost...)_ Consensus and Byzantine Fault Tolerance is a bitch to get right!  But Lets get it done already! OK? ;-)

By using open source exclusively as much as possible, your bottom line becomes: **_(If you use it and the maintainer quits support, you can fork the project and support it yourself!  You have options, and you can take total control of the software if needed.)_** More importantly, you will own outright your own copy of this software and may do with it as you like, **_(except make it into a proprietary product of course)_**.  

The **_share-and-share-alike_** rule keeps your work and all its derivatives open-source for others to fork and/or contribute which will help you smash those bugs much faster!  You will also be taking part in a global community and will make some very interesting friends over time!  This is all good for the open-source ecosystem.  It keeps it healthy and keeps it alive!  This is the true meaning of **_"Free Software"_** and the original reason I decided to adopt my life to it.  There are many other great reasons for using all **FOSS** as well of course...  Therefore... keeping in the spirit of "The Cathedral and the Bazaar", here we bravely go!  Enough talking already!

> **_"There are basically two types of people. People who accomplish things, and people who claim to have accomplished things. The first group is less crowded."_** - Mark Twain

## Start Here: _(Modular Emacs Setup)_

### Backup existing Emacs which may already be installed:

In the future I plan to include a shell script that will do all of this automagically for you...  Doing that now, _(while I work on bugs etc.)_ would not make sense right?  At some point I may even include a Docker file or some other container tech... :gift:

If you already have emacs installed, you should backup your existing environment first.  Simply renaming your existing `~/.emacs.d/` folder and any `.emacs` init files that may also be in your home directory is sufficient...  Change them to something like: `.default.emacs` &  `.default.emacs.d`.  Don't delete these renamed files from your `$HOME` directory...   You may need them later to come back to if you get into trouble and just want things back the way they were before you decided to try any of this out...  To start over, or give up...  What ever... Hopefully not to give up! :no_good:

Now you are ready to clone Harmonic Alchemy Modular Emacs into your home directory as: `~/.emacs.d/`

But lets Hold off cloning directly to `~/.emacs.d` and consider the following possible scenario: If you are using emacs on a daily basis and are used to that, you need to use your existing emacs setup to do some of the things below...  If you prematurely remove your emacs environment ahead of time you will have to resort to command line only to do everything below...

In that case, leave your existing **`~/.emacs.d`** folder as it is... _(don't rename it as instructed above)_...  Instead, clone `modular-emacs` to a temporary name i.e., `me.emacs.d` first.  Then you will be able to use your existing emacs environment (the one you are used to using) to finish doing the configurations below... **_This is how we will do it in the following example:_** No harm done doing it either way.  Just understand the temporary directory name we used below will be different if you are doing it directly to **`$HOME/.emacs.d`** :house_with_garden:  

### Clone: Harmonic Alchemy Modular Emacs

_(clone to a temporary directory `~/me.emacs.d`)_

    $ git clone https://github.com/harmonicalchemy/modular-emacs.git ~/me.emacs.d

This will clone `modular-emacs.git` into a fresh new empty `me.emacs.d` directory in your home directory.  Everything you need to run Emacs will be installed within that parent directory exclusively.  No outside .emacs init files will be created or used...

If you navigate within your new cloned directory you will find the `lisp` sub directory.  Go into the **`lisp`** directory and you will see three sub directories there: **`modules`**, **`my-modules`**, & **`themes`**...

The purpose of the **`my-modules`** folder is to give you a place to keep your own extensions to **Modular Emacs** without causing **git** to complain about new un-tracked files.  `my-modules` has been added to `.gitignore` so anything you create within that directory will be ignored as well!  This way you can try out different things without worrying about messing up the base install...  

> **Note:** It is important that **git** ignores all content within `$HOME/me.emacs.c/lisp/my-modules` because you will be cloning other **git repositories** inside `my-modules` and we don't really want to get into a complicated `git-sub-modules` mess do we?  You are free to clone and try out any extra Emacs things within `my-modules` folder to your hearts content!  Modular Emacs will stay clean outside...

### Clone MultiMarkdown Mode:

Clone my fork of MultiMarkdown Mode (`mmd-mode`) from Github into the `lisp/my-modules` directory:**   

> **Note:** _This package is no longer available on `MELPA` but it is required for `modular-emacs` to run...  It makes **markdown** files like this look good on GitHub!_ :octocat: You can also customize the lisp module to change how different elements look.  We will clone this into **`my-modules`** so that it will be ignored...  If it were on `MELPA` it would be installed within the `melpa` directory _(which is also ignored by git in this project)_

```
   $ cd ~/me.emacs.d/lisp/my-modules
   $ git clone https://github.com/harmonicalchemy/mmd-mode.git 
```

### Clone RMOO MOO Client from Github: _(optional)_

**RMOO** is a LambdaMOO client for Emacs that will also work with other MUDs as well.  Many **LambdaMOO** features are supported by `RMOO` which is more than most `MUD` clients these days.  Since I intend to get back into MOO programming I am looking for the best possible Emacs client to start with.  I first found Matt Campbell's original `rmoo` on Github at: `https://github.com/toddsundsted/rmoo`...  But then I found a fork that has been somewhat improved and was last updated November 16, 2018... This fork was made by **lisdude** on Github: `https://github.com/lisdude/rmoo`...  I have forked this fork into my own Github account in order to add more features if needed...

```
    $ cd ~/me.emacs.d/lisp/my-modules
    $ git clone https://github.com/harmonicalchemy/rmoo.git
```

> **Note:** To use the `rmoo` MOO client within Emacs, you need to enable the **Games Module:** by un-commenting the last load file line in: `~/.emacs.d/lisp/modules/dispatcher.el` that calls: `.../lisp/modules/11-games-pkg-conf.el`... 

I will be updating and or modifying this fork for my own MOO needs... When that time comes I may be publishing my own version of `rmoo` on GitHub...


### Copy `me.init.el` to: `init.el`:

We are almost done now... Go back to your cloned `me.emacs.d` parent directory and copy the init.el reference file:

```
   $ cd ~/me.emacs.d
   $ cp me.init.el init.el
```

_`init.el` has also been put into .gitignore so you can edit that file to your hearts content without disrupting the state of the master branch!_  You may also choose to add your `init.el` copy to a local git branch in parallel to track your own custom changes to that file as well...  

> **Note:** Don't mess with **`~/.emacs.d/me.init.el`**.  That is there for reference only...  You may need to copy it again if you mess up your first copy somehow or you simply want to get back to the default setup out of box to start over or something like that...

### Create an empty `custom.el` file:

Modular Emacs is configured to save all **"Emacs Customize"** configurations _(i.e., from the Options menu)_ to a file named: `custom.el`

```
   $ cd ~/me.emacs.d
   $ touch custom.el
```

This file is named within the `.gigignore` file so that anytime you use the built in Customize features of Emacs they won't disrupt the state of your master branch...



## OK... Ready Freddy? Start up Emacs and see what happens!

First Rename your existing:  **`~/.emacs.d`** to: **`~/save.emacs.d`** _(You may have already done this in the beginning)_  You may also have a `.emacs` init file outside of the `.emacs.d` folder that also needs to be renamed!

Next Rename: **`~/me.emacs.d`** to: **`~/.emacs.d`** _(This is now your new **Modular Emacs Local Repository** which is also now your default_ **`.emacs.d`** _home directory)_

**_Start up GNU Emacs v26.1 from the menu or command line and hold your breath!_**

> **Note:** If you are on **Mac OS** you will have to start Emacs from the shell `bash` or `zsh` etc.  This is a workaround to get important environment variables into Emacs on Mac OS... Unfortunately I have not found a better solution to this problem yet... but you can dedicate a small terminal window for this purpose and then minimize the terminal window while you are working in Emacs. With that workaround everything will work on Mac OS the same as it does on Linux, etc.

After a lot of super computing lisp number crunching flashing away in your mini buffer _(wait wait wait...)_ Then **Bam!** You should now see your new emacs pop up with the **Welcome to Harmonic Alchemy Modular Emacs!** scratch buffer in a frame with a pre-determined row/column initial size! 

**No?** Broken? Oh My! What a freaking let down!  I feel your pain!  OK... You may be fine if there are only warnings... _(which are hard to avoid upon first startup with all those new packages compiling etc. Not much you can do about that... The developers who made the packages need to clean that up, you could help them though. ;-)_

If you got an error and see the default emacs screen, try to retrace your steps or try running emacs with the debugger turned on...  Its probably something dumb and easy to fix...  _(happens to us all the first time, most of the time)_

**Yes?** It worked!  **Yay!** What a fantastic feeling the first time eh?

> **Note1:** Fetching/pulling new changes from the master `modular-emacs` GitHub repository to your local `~/.emacs.d/` directory will automatically be reflected within your emacs configurations... No need to copy any more files... But you may be surprised to see some new feature or something working differently.  If that bothers you, you may wish to keep your changes separate from the remote master branch.  Therefore create your own local `test` branch _(or call it what you like)_ and maintain your local changes in parallel... Change your local branch's .gitignore to accommodate your needs...

> **Note2:** The remote `modular-emacs` Github repository also maintains a `develop` branch where new ideas and features are tried out before folding them into the master branch which maintains the Modular Emacs stable release.  You could also create a local branch that tracks origin:develop if you would like to participate in any new things I am trying out before final release... This version 1.0.1 of Modular Emacs was first started within the develop branch...  Any time a final release of new features is ready, the develop branch will be merged back into master branch, tagged as a new point release (or major release when a lot of new features have been added to warrant it)...

**Usage:**

In this default setup, your local `~/.emacs.d/init.el` is not kept in modular-emacs code revision.  Instead, you may use this file to try out experimental customization and you may wish to keep it in your local branch under code revision by removing it from your local branch's `.gitignore` file.  If something goes wrong you can quickly refresh this file from the original `~/.emacs.d/me.init.el`.

You can also use the Modular Emacs \*scratch\* buffer to paste experimental lisp code to try out before making it permanent as a stand alone `.el` emacs lisp file...  you can evaluate the entire \*scratch\* buffer without removing the banner message at the top as that message is one huge lisp comment...  If something horrible happens, no worries... Restart emacs... Your mess-up will be gone and you will be back to where you were before you or that elisp test code goofed up...

If your customization proves stable, and you like it, you could then save your scratch buffer (as is) to a new file, for example: `~/.emacs.d/lisp/my-modules/00-my-new-module.el` _(use your own module name prefixed with `01.` `02.` `03.` `...` etc.)_ for your new custom module name.  This will make you consistent with the naming convention used within `~/.emacs.d/lisp/modules` directory...

> **Note:** _(You should be on your local `test` branch when you do this to keep your changes in parallel with the master branch)_.  Add a call to this new module from your `~/.emacs.d/init.el`

In all cases you would be wise to create and checkout a local `test` branch _(call it what you wish)_ and keep all your custom changes in there separate from the `master` or `origin:develop` branch...

Modular Emacs comes with my slightly customized version of the **Blackboard color theme**.  If you would like to add more custom themes or a different theme than `blackboard.el`, you can replace it or add additional themes into your local branch's: `~/.emacs.d/lisp/themes` directory and they will also will work with this setup by choosing `M-x load-theme` or changing the last line within `~/.emacs.d/lisp/modules/06-interface.el` to: `(load-theme 'your-chosen-theme-name t)`

> **Note:** _Obviously if you add more themes to your **Modular Emacs** **themes** directory you will be adding new un-tracked files to your cloned git repository!  Make sure you have checked out your own local branch before adding new themes or doing any customization outside the init.el file or the `my-modules` directory.  Then you will have proper management of your local custom changes and also have all that in code revision as well!_  How many times have I said this already?  OMG! :octocat:

## Features:

**Default Emacs welcome screen replaced with simple greeting:** Prints current emacs configuration, and date... With a famous Mark Twain quote _(My Favorite author. This quote may change from time to time with new updates)_.  

Use Scratch buffer to evaluate snippets of `lisp` without having to remove the welcome text... _(which are lisp comments)_

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

- [helm](https://github.com/emacs-helm/helm) Helm is an Emacs incremental and narrowing framework that helps speed up operations on commands with structured command completions and more.  It is a programmable interface (API) as well. To see how HELM can speed things up try a prefix key like: `C-x` or `C-c` or `M-x` and just wait before typing any more keys... You will see all the possible key-combos currently associated with that prefix key pop up in the HELM mini-buffer!  Fantastic! No more guessing!

- [helm-core](https://emacs-helm.github.io/helm/) Development files for Helm (API)... Installed as dependency.

- [popup](https://github.com/emacs-helm/helm) Popup is a dependency package for helm.

- [which-key](https://github.com/justbur/emacs-which-key) Used with helm, pops up suggestions for what can follow after pressing `C-x` key... _(this is what I was talking about above in the intro paragraph)_

#### Ace Jump Mode & Helpers:

- [ace-jump-mode](https://github.com/winterTTr/ace-jump-mode) Makes it Easy to Jump Around Quickly.
- [ace-window](https://github.com/abo-abo/ace-window) Switch Windows Visually.
- [avy](https://github.com/abo-abo/avy) Jump to Arbitrary Positions in Visible Text & Select Text Quickly. Installed as dependency to: `ace-window`

#### Tree & Menu Related Packages:

- [ztree](https://github.com/fourier/ztree) An efficient directory tree comparison utility.
- [undo-tree](https://github.com/emacsmirror/undo-tree) Visualize your changes in a buffer.
- [neotree](https://github.com/jaypei/emacs-neotree) Spawn a tree at any directory. You can toggle a `neotree` left side panel window within `dired-mode` with `C-,`
- [imenu-list](https://github.com/bmag/imenu-list) Right panel Menu and Variable list.  You can toggle an imenu list right panel window (for example within an elisp, Python, etc. file) with: `C-'`

#### Dired Related Packages:

- [dired-launch](https://github.com/thomp/dired-launch) Open files directly within `dired` using default applications.
- [dired-imenu](https://github.com/DamienCassou/dired-imenu) `imenu` binding for `dired-mode`. I am not sure the usefulness of this because I also use neotree which does a much better job...  It is in here just the same if you care to try it out...

#### dev§Ops, sys§Admin, info✷Sec Related Packages:

- [ssh-config-mode](https://github.com/jhgorrell/ssh-config-mode-el) A mode to edit SSH config files.
- [smart-tabs-mode](https://www.emacswiki.org/emacs/SmartTabs) Intelligent tabs to end the f---ing tabs vs spaces war already OK? Read the docs :-)
- [php-mode](https://github.com/emacs-php/php-mode) Major mode for editing PHP code.
- [undo-tree]() Minor mode that treats undo history as a tree... Copy this text file: <http://www.dr-qubit.org/undo-tree/undo-tree.txt> and save it as: Emacs-undo-tree-cheatsheet.md (it's an MD file of course). It's very comprehensive!

##### More languages, devOps tools, etc. to add later:

- `digitalocean-helm`  Whoa!!! A `helm` Interface and API connector to your Digital Ocean Account! _(I need to read more about this...)_

#### Writer's \- Publishing Tools:

- [markdown-mode](https://jblevins.org/projects/markdown-mode/) Markdown capability within emacs.

- [markdown-mode+](https://github.com/milkypostman/markdown-mode-plus) Extras for Markdown mode.

- [markdown-toc](https://github.com/ardumont/markdown-toc) Generate a TOC in a markdown file. This is a simple but powerful mode to create a TOC in a well-formed markdown file. In other words: The generated TOC will be well-formed if your markdown file is also well formed. ;-)

- [deft](https://jblevins.org/projects/deft/) Major mode for quickly browsing, filtering, and editing directories of plain text notes. Created by Jason Blevins _(the same statistics economics professor at Ohio State who also created Emacs Markdown Mode)_ This guy is wicked clever!  Looks like he has some nice algorithms for studying the cryptocurrency and smart contracts world as well!

- [pandoc-mode](https://joostkremers.github.io/pandoc-mode/) `pandoc-mode` is an Emacs mode for interacting with Pandoc. Pandoc is a program _(plus libraries)_ created by John MacFarlane that can convert a text written in one markup language into another markup language. This is going to help me get off proprietary Scrivener on **Mac OS** and finally be able to use Emacs for all my **Pub§Ops**!

- [fountain-mode](https://github.com/rnkn/fountain-mode) For **writers, screenwriters** A markdown mode for writing screenplays! Fantastic!!! Fountain together with pandoc, allows you to do many conversions for screenplays to industry formats like **Final Draft** etc. but the best part is the text highlighting and colors make it very easy to read/rehearse dialog and cues right from an Emacs buffer!  Writing scripts in this mode is as natural as typing `INT` _(your line instantly turns into a scene heading!)_ Try it! Just start typing your script and find out what happens!

- [olivetti](https://github.com/rnkn/olivetti) Adjust margins with word wrap. _(great with fountain mode!)_ Perfect for distraction free writing! I also enable this within `rmoo` _(if you have the **games module** enabled)..._  you can adjust the margin width with: `C-[` and `C-[` 

- [vmd-mode](https://github.com/blak3mill3r/vmd-mode) Fast Github-flavored Markdown previews synchronized with changes to an emacs buffer (no need to save).  Renders org files _(and normal markdown)_ in an external VMD App that automatically shows changes in real-time as you type! _(You need to install this external app separately, and make sure Emacs knows where to find it.  See: `exec-path-from-shell` below)_


#### Spelling:

- [flyspell-correct](https://github.com/) This package provides functionality for correcting words via custom interfaces. There are several functions for this: `flyspell-correct-wrapper`, `flyspell-correct-at-point`, `flyspell-correct-previous` & `flyspell-correct-next`.

- [helm-flyspell](https://github.com/pronobis/helm-flyspell) Helm extension for correcting words with flyspell.
- [flyspell-correct-helm](https://github.com/d12frosted/flyspell-correct) Nice helm interface for flyspell.  Place your cursor after any misspelled word and type: `C-;` to see a list of suggestions...

#### Project Management Tools:

- [org-bullets](https://github.com/emacsorphanage/org-bullets) Fancy UTF-8 bullet fonts for Org Mode... This is an old emacs package but looks like it still works fine in Emacs 26... If there are problems, it's a pretty short lisp file and easy to maintain. ;-)

#### Git \- `magit` & Related:

- [magit](https://github.com/magit/magit) A `git` porcelain inside Emacs. _(the main package)_
- [magit-popup](https://github.com/magit/magit-popup) Installed as dependency to `magit`
- [git-commit](https://github.com/magit/git-modes) Installed as dependency to `magit`
- [dash](https://github.com/magnars/dash.el) A modern list library for Emacs. Installed as dependency to `magit`
- [async](https://github.com/jwiegley/emacs-async) Asynchronous processing in Emacs. Installed as dependency to `magit`, `helm`, & others...
- [gitattributes-mode](https://github.com/magit/git-modes) Major mode for editing .gitattributes files.
- [gitconfig-mode](https://github.com/magit/git-modes) Major mode for editing .gitconfig files.
- [gitignore-mode](https://github.com/magit/git-modes) Major mode for editing .gitignore files.
- [with-editor](https://github.com/magit/with-editor) Use the Emacs client as $EDITOR.

### Key-bindings:

> **NOTE:** I have changed some of these since last documented here.  Some of the keys below have changed.  I will update this README.md once I sort that all out... You can always consult HELM with a prefix key however... _(which works fine right there when you need it without having to manually open this README.md just to see what keys are bound! OMG!)_

**_[See: 09-key-bindings.el](.emacs.d/lisp/modules/09-key-bindings.el)_**

**Function:**  | **Key:** 
:-------- | --------: 
Reload file in a buffer    | `<f5>`
ace-jump-mode-pop-mark     | `C->`
ace-window                 | `C-;`
imenu-list-smart-toggle    | `C-'`
undo-tree-visualize        | `M-/`
neotree-toggle             | `C-,`
buf-move-up                | `C-S-<up>`
buf-move-down              | `C-S-<down>`
buf-move-left              | `C-S-<left>`
buf-move-right             | `C-S-<right>`
helm-mini                  | `C-x b`
helm-buffers-list          | `C-x C-b`
helm-find-files            | `C-x C-f`
helm-recentf               | `C-x C-r`
helm-show-kill-ring        | `M-y`
mc/mark-next-like-this     | `C-}`
mc/mark-previous-like-this | `C-{`
mc/mark-all-like-this      | `C-|`

