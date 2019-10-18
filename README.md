![Banner](banner.jpg)

[![License](https://img.shields.io/badge/LICENSE-GPL%20v3.0-blue.svg)](https://www.gnu.org/licenses/gpl.html)
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [Introduction](#introduction)
    - [Current Status:](#current-status)
    - [Welcome!](#welcome)
    - [History:](#history)
        - [Modular Emacs is Born:](#modular-emacs-is-born)
        - [Why Modular?](#why-modular)
    - [Try Harmonic Alchemy Modular Emacs:](#try-harmonic-alchemy-modular-emacs)
        - [Note for Windows Users:](#note-for-windows-users)
        - [My Commitment:](#my-commitment)
- [Start Here: _(Modular Emacs Installation)_](#start-here-modular-emacs-installation)
    - [Requirements:](#requirements)
        - [Emacs: V26.1+](#emacs-v261)
            - [Commands to install Emacs on various unix platforms:](#commands-to-install-emacs-on-various-unix-platforms)
                - [Fedora 27-29:](#fedora-27-29)
                - [Debian9 Stretch & Ubuntu 18.04+:](#debian9-stretch--ubuntu-1804)
                    - [Prerequisites](#prerequisites)
                    - [Clone `GNU Emacs Repo @ Savannah.gnu.org:`](#clone-gnu-emacs-repo--savannahgnuorg)
                    - [Set up Autotools:](#set-up-autotools)
                    - [Run Configure:](#run-configure)
                    - [Make Bootstrap: _(does a more thourough job)_](#make-bootstrap-does-a-more-thourough-job)
                    - [Make Install! _(Make the Linux App!)_](#make-install-make-the-linux-app)
                    - [Launch Emacs:](#launch-emacs)
                    - [Revert Repo back to fresh clone state to start over:](#revert-repo-back-to-fresh-clone-state-to-start-over)
                    - [Troubleshooting Debugging:](#troubleshooting-debugging)
                - [Mac OS:](#mac-os)
                - [Install Emacs From Source on Mac OS:](#install-emacs-from-source-on-mac-os)
                    - [Prerequisites:](#prerequisites)
                    - [Clone `GNU Emacs Repo @ Savannah.gnu.org:`](#clone-gnu-emacs-repo--savannahgnuorg)
                    - [Set up Autotools:](#set-up-autotools)
                    - [Run Configure:](#run-configure)
                    - [Make Bootstrap: _(does a more thourough job)_](#make-bootstrap-does-a-more-thourough-job)
                    - [Make Install! _(Make the Emacs Mac App package!)_](#make-install-make-the-emacs-mac-app-package)
                    - [Move your shiny new Emacs.app to: `$HOME/Applications:`](#move-your-shiny-new-emacsapp-to-homeapplications)
                    - [Launch Emacs from your Apps Folder/Menu:](#launch-emacs-from-your-apps-foldermenu)
                    - [Revert Repo back to fresh clone state to start over:](#revert-repo-back-to-fresh-clone-state-to-start-over)
                    - [Troubleshooting Debugging:](#troubleshooting-debugging)
                - [FreeBSD & OpenBSD:](#freebsd--openbsd)
        - [`NODE.js` with `NVM` & `NPM`:](#nodejs-with-nvm--npm)
            - [Install Node Version Manager `NVM` with `curl` first:](#install-node-version-manager-nvm-with-curl-first)
            - [Now install `NODE` with `NVM`:](#now-install-node-with-nvm)
        - [VMD: _(Visual MarkDown App)_](#vmd-visual-markdown-app)
        - [MultiMarkdown:](#multimarkdown)
            - [Install Multimarkdown on Mac OS:](#install-multimarkdown-on-mac-os)
            - [Install Multimarkdown on Linux:](#install-multimarkdown-on-linux)
        - [Pandoc:](#pandoc)
            - [Install Pandoc on Mac OS:](#install-pandoc-on-mac-os)
            - [Install Pandoc on Linux:](#install-pandoc-on-linux)
        - [Graphviz:](#graphviz)
            - [Install Graphviz on Mac OS:](#install-graphviz-on-mac-os)
            - [Install Graphviz on Linux:](#install-graphviz-on-linux)
        - [Steel Bank Common Lisp:](#steel-bank-common-lisp)
            - [_(optional for CS research eggheads, prototyping, blockchain? etc.)_](#optional-for-cs-research-eggheads-prototyping-blockchain-etc)
            - [Install SBCL on Mac OS:](#install-sbcl-on-mac-os)
            - [Install SBCL on Linux:](#install-sbcl-on-linux)
            - [Install QuickLisp Package Manager:](#install-quicklisp-package-manager)
                - [Run these commands from your HOME directory:](#run-these-commands-from-your-home-directory)
            - [Find, install, update, remove Quicklisp System packages:](#find-install-update-remove-quicklisp-system-packages)
            - [Read the SBCL Docs!](#read-the-sbcl-docs)
                - [Lisp Web Dev Resources:](#lisp-web-dev-resources)
        - [CIDER & Clojure:](#cider--clojure)
            - [CIDER is the Clojure(Script) Interactive Development Environment that Rocks!](#cider-is-the-clojurescript-interactive-development-environment-that-rocks)
            - [Install Closure & Leiningen on Mac via Homebrew:](#install-closure--leiningen-on-mac-via-homebrew)
            - [Install Closure on Linux:](#install-closure-on-linux)
                - [Debian / Ubuntu:](#debian--ubuntu)
                - [Fedora:](#fedora)
            - [Install & Leiningen on Linux:](#install--leiningen-on-linux)
                - [Debian 10 Buster:](#debian-10-buster)
                    - [For released stable versions of Debian do this instead:](#for-released-stable-versions-of-debian-do-this-instead)
                - [Ubuntu:](#ubuntu)
                - [Fedora:](#fedora)
    - [Get Ready to Start up Modular Emacs for the first time!](#get-ready-to-start-up-modular-emacs-for-the-first-time)
        - [First Some Initial House Keeping: _before we move in_](#first-some-initial-house-keeping-before-we-move-in)
            - [Create an empty file named `custom.el`:](#create-an-empty-file-named-customel)
            - [Clone `mmd-mode.git` into `my-modules`:](#clone-mmd-modegit-into-my-modules)
            - [Copy/Clone `me.init.el` to: `init.el`:](#copyclone-meinitel-to-initel)
        - [Final Step - Make Modular Emacs folder the default `~/.emacs.d` folder:](#final-step---make-modular-emacs-folder-the-default-emacsd-folder)
    - [Ready Set Go!  Start Up Modular Emacs:](#ready-set-go--start-up-modular-emacs)
    - [Usage:](#usage)
        - [Blackboard Color Theme:](#blackboard-color-theme)
        - [Enable Optional Steel Bank Common Lisp IDE:](#enable-optional-steel-bank-common-lisp-ide)
            - [Copy `dispatcher.el` into `my-modules`:](#copy-dispatcherel-into-my-modules)
                - [Edit your clone `/my-modules/dispatcher.el`:](#edit-your-clone-my-modulesdispatcherel)
            - [Edit `~/.emacs.d/init.el` to load your clone of `dispatcher.el`:](#edit-emacsdinitel-to-load-your-clone-of-dispatcherel)
                - [Change the following line within `init.el` to look like this:](#change-the-following-line-within-initel-to-look-like-this)
            - [Restart Emacs & Run Slime from your scratch buffer:](#restart-emacs--run-slime-from-your-scratch-buffer)
            - [Common Lisp Resources:](#common-lisp-resources)
        - [Strategies for Concurrent Development:](#strategies-for-concurrent-development)
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

# Introduction

## Current Status:

This commit represents **Modular Emacs: `Release V2.3.0` [Q4 2019]** which merges some bug fixes and
common features developed within the **Lisp-IDE branch**.  The **Lisp IDE** branch is to esoteric for general Emacs users, therefore I broke it out to give users a choice whether to use it...  For one thing,
The Lisp IDE branch remaps some keys that would disrupt most Emacs user's habits.  Also I have enabled **`xah-fly-keys`** on that branch which requires about 20 hours of commitment before seeing the real benefits that mode provides... If you are willing to make that minimal commitment, you will be typing code circles around most other developers, just from your new keyboard skills alone!  Try it! I did, and I am here to tell you, Xah Lee is not a troll! :trollface: He is a wizard of the keyboard!  Those guys saying he is a troll are not willing to commit 20 hours on his keys... If they did they would know like me...  My skills have increased about 4 fold! Those guys complaining, are responding the normal human-nature way, not willing to get out of their comfort zone, and instead poke fun at folks who were willing to do what it takes to improve... know-what-I-mean?

Like I said, its an option... not enabled here... If you want it... try the `lisp-ide` branch...

Also, most importantly!  The next release of this will break out a lot of this single doc into separate categorized docs, each addressing different major sections below... OK?  Sorry this doc grew and grew... While I was spending much time making the code modularized... I did not modularize the docs... LOL  Now it is time for that...  You will love me afterwards... ;-)

## Welcome!

**Welcome to Harmonic Alchemy Modular Emacs - _V2.3.0 Q4-2019_**

**Note:** Updates to this Doc will be on-going...  Some features documented below were optional with previous versions, and now they may no longer work as expected with this release!  Try them out at your own risk... Optional features will be changing... Consider all the optional features below as alpha...  Daily Development of these new optional features goes on under the **`lisp-ide`** branch which will be getting frequent updates...  

Also note that on the **`lisp-ide` branch** I have been trying out: **`Xah-Fly-Keys`** _(a modal edit mode for Emacs!)_.  That has been a bitter-sweet experience so far...  If it helps to speed up my Emacs workflow and editing speed I am all for it!  At first I was stumbling all over myself with mis-typed keys etc.... I was cursing Xah, blaming him for lying about things etc... _(even calling him a troll - lol)_ 

However, I committed myself to try it for a bit longer. _(After spending all that time installing and configuring, I did not want that time to have been wasted eh?)_ With this new resolve I began to notice/realize I was forcing my fingers to get over bad keyboarding habits! Habits that were slowing me down, and possibly causing strain in my little fingers!

This was an epiphany for me as I am a bass player and well know the problems of bad fingerboard habits!  I stuck with Xaw's keys a bit longer and low-and-behold, I am now starting to see how fast I can go, _(once I train my damn fingers not to slip over to the dark side :space_invader: - lol)_

It is true!  You don't need to hold down the damn **Ctrl** or **Meta Key** anyomre!  But you will need the disipline, commitment, and probably about 20 hours of time to master it!  No slipping back and forth during your finger training time or you will end up reinforcing your old habits wasting all of your effort!

I think its worth trying if you are already experiencing health problems.  It's also worth trying if you think your keyboarding skills are much to slow...  I don't believe it's practical or a good idea to mix **Xah-Fly-Keys** with your old **Ctrl** and **Meta** key tricks at the same time... Especially while you are learning... Know what I mean?  Stay off them and use the **leader keys** instead...

Much patience is required before you will stop calling Xah a troll and recognize him for the vast keyboarding knowledge he offers to the world!!!  He truly knows the subject of keyboards, keyboarding efficiency and health, etc.  he did his homework, and we all benefit!  **Hey!** Would you **GNU** guys Let **[Xah Lee](https://github.com/xahlee)** into the **FSF** and add his Emacs projects to Elpa already OK? In my opinion his `Xah-Fly-Keys` is the best **Emacs** Major Modal Edit Mode available...  

I am not totally convinced a modal version of Emacs is going to be right for me, _(Edit: Update.  I am so used to `xah-fly-keys` now that I cannot remember some old Ctrl key combos anymore!  And I am loving not having to move my hands from home position.. it's a miracle!)_ but I am sure it is right for a lot of folks out there!!!  Especially **Vi2Emacs Converts**...  This modal version of Emacs comes with your health's best intrest first priority, keeping speed and efficiency as the second priority which should be in harmony with the first priority...  I am very impressed with it so far... I don't understand some of FSF's complaints?  Maybe those are old now...

If you are interested in trying these new features out, `git checkout --track` **`origin/lisp-ide`** branch on your local machine and restart Emacs... _(Some of these new Lisp IDE Programming Language features may not work correctly at first, and you will need to remap your caps-lock, ctrl and alt (or command on Mac) keys.  But as I fix things you will see things start working as expected...  If you keep your local branch updated you will be the first to find out! ;-)_ I will be updating the docs below for that branch accordingly...  But those docs may not make it here to this one.. _(because they won't apply)_ here...

To be safe... especially if you are new, stay on the master branch and don't install any of the _Optional_ features discussed below...  OK?  Have fun!!! 

## History:

This project has been a long time coming.  Its history goes back a few years ago when I went searching for some better ways to configure my then pretty basic Emacs configuration of over 10 or so years!  I cloned a few Emacs repos on Github to try different things out for a while.  I liked some things and tried to get rid of other things later.  Management became confusing after a while.  I ended up with lots of questionable elisp code, much of which were things possibly no longer needed!  This is especially true if you are not used to Lisp but instead used to more conventional languages like C, C++, Java, PHP, etc...  I was thinking of my eLisp code like that and ended up with a lot of spaghetti code.  More like elbow spaghetti.  LOL _(parenthesis)_...

After a couple years of **adding**, **removing**, **configuring**, **re-configuring** it started feeling like I was painting myself into a tangled corner like getting lost in **_"a maze of twisty little passages all alike!"_**  Lets face it... I was not keeping good logs and my **git** management skills were wanting!  I was being: _"oh that looks cool lets try that" - (cowgirl mode)_ and I got into a wee bit of trouble as a result of my wild adventures!  It was all my fault! But it was fun just the same.

However as a result of that saga, I learned a lot about **`SH`**, **`BASH`**, and **`ZSH`** best practices...  That spawned a complete re-write of my **`.dotfiles` project** to become multiplatform... _(**Note:** My `.dotfiles` have not been updated yet as that project has been forked and made private until my final scheme is ready...)_ Currently my **New** _(but still private)_ **`.dotfiles` project** is _Full of Twisty Little Pathways Still Unknown..._  :question: Maybe best not to try that out yet... My advise.. the author.. omg!

### Modular Emacs is Born:

As a result of the mess above, I decided to start over from scratch and modularize everything with the purpose of preventing tangled messes like this from happening within your own Emacs setups! _(and mine as well..._ `%^)`

**Modular Emacs** is more than just Emacs with configurable modules... It is also designed to be the centerpiece **_(command central)_** of a larger **_Extremely Personalized, Extremely Extensible_: ** **DevOps** / **PubOps** **IDE** workstation and/or companion **IDE** workstation **laptop**.  I am a devOps engineer, computer scientist, composer/musician, sound designer, architect, writer/publisher.  I wear a lot of hats! My Emacs needs to wear a lot of hats as well!  The central focus of **Modular Emacs** is to build empowering features into **Emacs** utilizing a modular framework that facilitates all the above without becoming an over complicated mess to manage! **_"good luck with that"_**  :octocat:  

### Why Modular?

- **Modular Emacs** was designed to be easy to maintain and configure through the management of independent modules and in that sense Modular Emacs hopefully forms a basic sensible framework that is easy to use and flexible enough to adjust to your work-style, programming-style, and writing-style...

- **Add What You Need Only:** You have choices encapsulated within modules. Each module comes with its own requirements.  Removing and or adding many of the _extra feature_ modules will not disrupt the rest of your Emacs configuration.  They have been designed to be encapsulated in that way... Some of the more basic modules are required for Modular Emacs basic operation however.  The nice thing is everything is modular so it will be easy to find out which ones are doing what...

On the other hand, if you install Modular Emacs stripped of all external helper apps _(described below in the **Requirements** section)_, It may not provide any advantage over many of the other simpler Emacs projects on GitHub... **_Therefore if all you need is vanilla Emacs:_** _(with some of the more basic options thrown in)_, you don't need Modular Emacs at all.  Here is a simpler light weight _"sensible"_ Emacs Config: **[hrs/sensible-defaults.el](https://github.com/hrs/sensible-defaults.el)**  _(which claims to also be modular - I have not tried it.  I only briefly read through the README.  It looks like a good alternate option to try)_ :octocat:   

## Try Harmonic Alchemy Modular Emacs:

For those of you who feel **[Harmonic Alchemy Modular Emacs](https://github.com/harmonicalchemy/modular-emacs)** may be exactly what you were looking for: **_By all means... Lets Dive in!_**

There are some external requirements... It may take a while... If that's OK with you, Great!... Lets go...  Eventually I will script this up so you can just enter this at the command prompt: **`./install.sh`** and be done with it...  That script will be installing a lot of things though! It will have to check a lot of system states!  I have to work all that out as a **non-interactive** process.  It should install everything _(transparently non-interactively)_ from that single running shell script...  If you have to answer questions and make decisions... Better that you execute manually instead, and read the docs first...  Right?  

And so for now... We will learn this together... Please give feedback, open issues, ask questions!  Thanks ;-)

### Note for Windows Users:

I have no idea whether this project will work on **Windows** and I have no experience using **Emacs** on **Windows** or **Cygwin**...  When I was working at **IBM/Lotus** we were building major IBM/Lotus middleware software systems using the full premium stack of **Microsoft Developer Works** tools, as well as a full stack of **IBM Developer tools** to boot!  **Emacs** would have felt quite out of place among those monsters!  I did work on building IBM/Lotus Open Source Software Products and deployed them on **Red Hat Linux** servers however.  You better believe my trusty old friend **Emacs** would be one of the first things that got installed on those boxes!

If you use **Emacs** on **Windows**, you could help the Windows user community immensely by testing Modular Emacs within your MS Windows environment!  

If you find glitches please let me know... I need your knowledge for that part...  I will give you credit for any Windows solutions you provide.  I had some problems with _(unwanted)_ scroll bars showing up on new frames _(after initial frame)_ and read about others having this problem on Windows... I found a simple _(best practice)_ for Mac OS and Linux, but don't know if it will work on Windows...  

I added some _(commented out)_ code to: `.../lisp/modules/06-interface.el` after reading some things on Stack Exchange about problems with scroll bars on Windows...  You could un-comment this code and try it if you are having the same problems...  If that works for you, please open up an Issue about that and I will update this doc to let everyone else know...  Thanks!  

### My Commitment:

> **_"Always do what is right. It will gratify half of mankind and astound the other."_** - Mark Twain  

I am committed to making this **process/journey** as painless as possible for you as I have found many other Emacs setups to be way to complicated to make it easy for adoption into my workflow, and you are probably experiencing the same!  On the other end of the spectrum... Installing basic Emacs alone is not enough to get you started _(IMHO)_...  

If you end up liking **Harmonic Alchemy Modular Emacs**, please drop me a note to let me know! Please do ask questions, open issues, etc.  Thanks!  I have been using Emacs since around 1986/87 _(OMG that long! off-and-on with gaps in between ;-)_ but I never shared any of that Emacs knowledge with anyone before now... :heart_decoration:  

Therefore: **Here is Modular Emacs!** I hope my long years of **devOps** experience will turn out a nice jewel for new comers to try out and succeed with!  **Good Luck and God Speed!**  

# Start Here: _(Modular Emacs Installation)_  

## Requirements:  

### Emacs: V26.1+  

Many of the default packages installed with this project will not work with previous versions of Emacs. Also the security of older Emacs and packages is Horrendous! Recently, some of the older packages were removed from the updated MELPA and the entire site is TLS enabled now!  These new features are vital for being reasonably safe going forward from 2018 onward, so I decided not to support lesser versions of Emacs...  

> **Warning:** If you have a really old version of Emacs currently installed, it may break after you update Emacs below... Therefore before trying any of this out, first you should record the current version of Emacs you are using, and backup all previous emacs related files: _(i.e.,_ `.emacs` `.emacs.d`_)_ located in your $HOME directory.  

> Simply COPY your entire: **`~/.emacs.d`** directory _(and/or_ **`~/.emacs`** _file)_ into a temporary directory of your choice before upgrading below... _(or compress and make a `.zip` or `.gz` archive file)_  This will be your safety net for coming back later if you decide you are not ready to upgrade.  Don't move or touch your original Emacs files, as you will try them out with your new Upgraded version of Emacs...  If anything goes wrong, you will be able to reverse everythig back no problem! _(Note: before restoring your backups, you will have to downgrade your Emacs back to the version you were on before trying out Modular Emacs)_ 

#### Commands to install Emacs on various unix platforms:  

_(Choose your Flavor)_  

##### Fedora 27-29:  

```bash
    sudo dnf install emacs
```

That's it! Red Hat takes care of you lucky Fedora users...  

##### Debian9 Stretch & Ubuntu 18.04+:  

You Debian/Ubuntu users don't get it so easy... Hope you got your hacker shoes on...
I tried all the normal ways to get the latest binary build of Emacs installed for Debian based Linux but none of those options are able to satisfy Harmonic Alchemy Modular Emacs needs 100%...  Therefore, we are going to bite the bullet and install the latest Emacs from source here.  The nice thing about that is once we are managing our own build we can update it, change configuration, even try installing on a different platform!  

We will no longer be dependent on the mercy of grumpy package archive build engineers! DevOps heaven! Yay!  Warning... This will take a bit of up-front work...  I hope you don't mind staying up all night digging deep and scratching your eyes a lot! Not for the faint at heart!  If you never built software from source before you may want to try something like **`"Hello World"`** first :trollface: OK, Lets get started! I did this on Mac OS already... Was not too painful.. :octocat:

###### Prerequisites  

- **`git`:** - Check that `git` is at least **`Git 1.7.1`**.  If you already cloned the Emacs repository with an older **Git** version, you may need to reclone it after upgrading `git`.  

- _Install `Autoconf`:_    
Make sure `Autoconf` is at least the version specified near the start of **`configure.ac`** _(in the_ **`AC_PREREQ`** _command)._  **`V2.65`** or greater is required as of `2019-006-16`.  
```bash
        sudo apt install autoconf
```

- _Install `Automake`:_  
```bash
    sudo apt install automake
```

- _Install `autotools-dev`:_  
```bash
    sudo apt install autotools-dev
```

- _Install `libtool`:_  
```bash
    sudo apt install libtool
```

- _Install `makeinfo`:_  This is not strictly necessary, but highly recommended, so that you can build the manuals. **makeinfo** is bundled as part of **GNU Texinfo**.  Make sure your installed **Texinfo** is: **`V4.13`** or later to work with this build...  
```bash
    sudo apt install texinfo
```

- _Install `build-essential`:_  
```bash
    sudo apt install build-essential
```

- _Install `xorg-dev`:_  
```bash
    sudo apt install xorg-dev
```

- _Install `libgtk2.0-dev`:_  
```bash
    sudo apt install libgtk2.0-dev
```

- _Install `libjpeg-dev`:_  
```bash
    sudo apt install libjpeg-dev
```

- _Install `libncurses5-dev`:_  
```bash
    sudo apt install libncurses5-dev
```

- _Install `libdbus-1-dev`:_  
```bash
    sudo apt install libdbus-1-dev
```

- _Install `libgif-dev`:_  
```bash
    sudo apt install libgif-dev
```

- _Install `libtiff-dev`:_  
```bash
    sudo apt install libtiff-dev
```

- _Install `libm17n-dev`:_  
```bash
    sudo apt install libm17n-dev
```

- _Install `libpng-dev`:_  
```bash
    sudo apt install libpng-dev
```

- _Install `librsvg2-dev`:_  
```bash
    sudo apt install librsvg2-dev
```

- _Install `libotf-dev`:_  
```bash
    sudo apt install libotf-dev
```

- _Install `libgnutls28-dev`:_  
```bash
    sudo apt install libgnutls28-dev
```

- _Install `libxml2-dev`:_  
```bash
    sudo apt install libxml2-dev
```

###### Clone `GNU Emacs Repo @ Savannah.gnu.org:`  

You could do this within a dedicated **`Dev`** folder because you will most likely want to maintain a local clone so you can come back later to build again when you need to upgrade or drop back to a more stable version...  I created my own local _untracked_ branch and keep my local `tracking` branch clean...   This helps speed up the build-problems-debug-re-build-till-it-works cycle...  

```bash
    git clone https://git.savannah.gnu.org/git/emacs.git
    cd emacs
    git branch -a                         # prints a long list of remote branches...
    git fetch origin emacs-26             # we are interested in building emacs 26
    git checkout --track origin/emacs-26
    git pull origin emacs-26
    git checkout -b my-local-branch       # A smart git practice to get into habit...
```

> **_btw:_**  **`git branch -a`** will reveal all the universal build options for the entire Emacs world on any platform, Mac, Linux, BSD, Windows, Tests, etc. You could build it all from here I imagine!  But here we are only interested in the latest stable Linux release at the time of cloning...

###### Set up Autotools:  

To use the autotools: Run the following shell command within your cloned **`emacs`** directory:  

``` bash
    cd emacs 
    ./autogen.sh
```

The last bit of output of the above running shell script should look like this:

```bash
    Installing git hooks...
    'build-aux/git-hooks/commit-msg' -> '.git/hooks/commit-msg'
    'build-aux/git-hooks/pre-commit' -> '.git/hooks/pre-commit'
    'build-aux/git-hooks/prepare-commit-msg' -> '.git/hooks/prepare-commit-msg'
    '.git/hooks/applypatch-msg.sample' -> '.git/hooks/applypatch-msg'
    '.git/hooks/pre-applypatch.sample' -> '.git/hooks/pre-applypatch'
    You can now run './configure'.
```

If you see the above it was successful!  The above script generated the **`configure`** script and some related files, and set up your git configuration...    Now you can move on to configure your specific build...

###### Run Configure:  

To get all the features I wish package managers would take the time to compile in for us, run **`configure`** with the following switches set:

```make
    ./configure --with-imagemagick --with-mailutils\
    --with-gnutils --with-modules --with-rsvg --with-dbus\
    --with-xml2
```
You probably don't have to be so specific  _(as above)_ and probably could just get away with using `./configure` alone... The build process is smart and most likely will give you all the things you need without asking explicitly...  But I asked explicitly above anyway...  Then I know for sure. 

_To see a list of other available options, run this command:_

```make
    ./configure --help
```

###### Make Bootstrap: _(does a more thourough job)_  

The **Bootstrap make** is quite **`CPU`** intensive... If your laptop can do it... _(4 cores? no prob!)_ fan won't even twitch? Maybe... :octocat: So if you don't mind waiting, this is the best way to build according to the GNU dudes...

```make
    make bootstrap
```

###### Make Install! _(Make the Linux App!)_  

Do this with `"sudo"` to get the Linux Emacs app installed in `/usr/local/bin`

```make
    sudo make install
```

Occasionally the file `lisp/loaddefs.el` (and similar automatically generated files, such as `esh-groups.el` and `*-loaddefs.el` in some subdirectories of 'lisp/', e.g., 'mh-e/' and 'calendar/') will need to be updated to reflect new autoloaded functions.  If you see errors (rather than warnings) about undefined lisp functions during compilation, that may be the reason.  Finally, sometimes there can be build failures related to `*loaddefs.el` _(e.g., "required feature ‘esh-groups’ was not provided")_.  In that case, update loaddefs.el (and similar files), as follows:

```bash
    cd lisp
    make autoloads
```

doing `make bootstrap` as above should eliminate any of the above problems however...


###### Launch Emacs:

With Emacs installed in /usr/local/bin, you can launch Emacs from any command line.  You could also create a start menu item/icon as well.  I don't bother with that in a Qubes environment... So no write-up on that for now... Start Emacs from the terminal.  It will pop up the GUI window... No problem... Once your Emacs build is running, you can check the version with **`C-h C-a`**...

If you have problems with your build?  Oh My! Do the next step below and then Go back to the top of this **_squirrel cage_** and start over fresh.  Read carefully... :trollface:

###### Revert Repo back to fresh clone state to start over:  

If your build was successful, you don't need to do this now... Wait until you need to build again...  However if your build went bad... This is the way to start completely over... 

```bash
    git clean -fdx
```

###### Troubleshooting Debugging:

My build went well because I planned well this time... _(i.e., you did not see the big goofs I made trying to build this on a Mac! lol)_ Also I was very explicit about required developer libraries which probably stopped a lot of problems!  Because of that I am now running Emacs V26.2.90 on Debian 9 now with Imagemagick, and all my favorite bells and whistles!  **_Caveat:_** I have to manage builds now.. Oh well... it felt good getting that monster to build! _(even after the second, third, times)_   :octocat:  


##### Mac OS: 

> **Update 2019** _It looks like **Build-from-Source** or **Macports** is our only option now..._  

OK you Mac die-hards!  All the Linux dudes are going to be angry now as we are about to take up the rest of the real-estate in this **Emacs install guide**!

It looks like _(we)_ Macolites have been abandoned by both Apple and Homebrew!  What a mess... and What a shame! **_Linux dudes roll eyes and wonder why MacRats don't jump ship?_**  

The Mac OS was, and still is a nice environment to work _(and create)_ in... I loved the NextStep `NS` environment all the way back to when it was first created at **NeXT** And... Being a musician, composer, programmer and member of the **BCS NeXT SIG**, in those early days, I was in collaboration with the digital audio developers at NeXT during that time as well! _(I typed that from memory... hope I got the camel case right. lol)_.

Now, I am not sure which direction Apple is taking...  The design of Mac OS morphing into iOS and visa versa makes it not so much of a unix anymore (also Mac OS never had a proper unix File System anyway)... I remember it was the file system and risky R/W optical disks which were the Achelies heel of the **NeXT** computer.  I am quite frightened by the new **APFS** spec Apple is throwing out! I have not upgraded past regular **Sierra** either... Time is running out?  What to do?  The new Mac Pros look way cool with probably more super-computing power than one of the older gen (pre-millennium) Cray super computers I bet! Seriously!  Thank God no more Trash Can! LOL What were they thinking? Cylinder does not = Cray super power... You abandoned all us Music Hackers! I hope you are back now...  And can you stay closer to standard unix? Thanks! Convince me your APFS is not evil or dangerous! Oh right... Can it be case sensitive too?

Unfortunately I am dependent on Mac OS For digital music/sound, video, graphic design, etc. I currently use a late 2015 27" retina iMac.  All of my other computing work is done on a reasonably secure Qubes _(personal hypervisor)_ Laptop that runs different flavors of Linux as App VMs... My Qubes laptop is used for writing, coding, business, personal...  

On my Qubes Laptop, Emacs works great, straight from any of the above package managers _(as you can see above...)_  

Mac OS (**Darwin**) has always been a mongrel cross-breed of different unix flavors. _(by now all the unix are mixed up pretty much however... so that's not it...)_  The problem is all the non-standard junk that Apple does that tends to break standard unix things if those are also installed!  OMG! Hair pulling time!

Long story short... My Emacs install was causing all my woes... Homebrew stopped building versions that I needed for my Emacs configuration to work correctly. _(some of the problems were also shell startup related)_.  After experiencing of all of that over the past year, and coming to the realization that I shoulda/coulda used MacPorts instead of Homebrew, but not daring to change all that now mid stream of course... So... I am building Emacs from source... Straight from a cloned repository from Gnu's Git Server!  I can get the bleeding edge if I want now... :stuck_out_tongue:

##### Install Emacs From Source on Mac OS:

> **Note:** We are going to build a fast moving/changing target: Monster Lisp Machine with lots of little bitty _loaded.el bugs_ they are fixing and working on all the time! It's important to set up a scheme allowing for quick updates to keep in sync and get those bug fixes, _(possibly bugs that are blocking you from making a successful build!)_  OK **_MacCowboys_** and **_MacCowgirls_**... Are you ready for this? Strap in... Or should I say.. Bootstrap! :octocat:

###### Prerequisites:

- **Install Autoconf & Automake**  
These can be safely installed by Homebrew:  
`brew install autoconf automake`  


- **autoconf:**  - Check that it is at least the version specified near the start of **`configure.ac`** _(in the_ **`AC_PREREQ`** _command)._  **`V2.65`** or greater is required as of `2019-006-01`.  The Homebrew version at same time is: **`V2.69`** Whew! That was close! :trollface:


- **git:** - Check that `git` is at least **`Git 1.7.1`**.  If you cloned this repository with an older **Git** version, you may need to reclone it after upgrading `git`.  The current version of `git` at **Homebrew** is **`V2.21.0`** so you are all set if you **`update/upgrade`** Homebrew...  
`brew install git`  
-or-   
`brew update`   
`brew upgrade`  


- **makeinfo:** - This is not strictly necessary, but highly recommended, so that you can build the manuals. **makeinfo** is bundled as part of **GNU Texinfo**.  You can install **texinfo** with **Homebrew** as well:  
`brew install texinfo`  
Make sure your installed **Texinfo** is: **`V4.13`** or later to work with this build... No problem... Homebrew version is currently **`V6.6`**   _(as of 2019-006-01)_

###### Clone `GNU Emacs Repo @ Savannah.gnu.org:`  

You could do this within a dedicated **`Dev`** folder because you will most likely want to maintain a local clone so you can come back later to build again when you need to upgrade or drop back to a more stable version...  I created my own local _untracked_ branch and keep my local `tracking` branch clean...   This helps speed up the build-problems-debug-re-build-till-it-works cycle...  

```bash

git clone https://git.savannah.gnu.org/git/emacs.git

```

> **_btw:_**  So far all these instructions are universal to build the entire Emacs world on any platform, Mac, Linux, BSD, Windows, Tests, etc.  You have the cloned repo.  Try: **`git branch -a`** and you will see what I mean!

###### Set up Autotools:  

To use the autotools: Run the following shell command within your cloned **`emacs`** directory:  

``` bash

$> cd emacs 
$> ./autogen.sh

```

This will generate the **`configure`** script and some related files, and to set up your git configuration...  

###### Run Configure:  

To get all the features I wish Homebrew would give us, run `configure` with the following switches set:

```

$> ./configure --with-ns --with-imagemagick --with-mailutils\
   --with-gnutils --with-modules --with-rsvg --with-dbus\
   --with-xml2

```

###### Make Bootstrap: _(does a more thourough job)_  

The **Bootstrap make** is quite **`CPU`** intensive... If your Mac can _(4 cores? no prob!)_ fan won't even twitch? Maybe... :octocat: So if you don't mind waiting, this is the best way to build according to the GNU dudes...

    make bootstrap

###### Make Install! _(Make the Emacs Mac App package!)_

    make install

###### Move your shiny new Emacs.app to: `$HOME/Applications:`

    mv nextstep/Emacs.app ~/Applications

###### Launch Emacs from your Apps Folder/Menu:

Launch Emacs for the first time...  If it runs you can check the version with **`C-h C-a`**... If you have problems? Go back to the top of this **_squirrel cage_** and start over... :trollface:

###### Revert Repo back to fresh clone state to start over:  

If your build was successful, you don't need to do this now... Wait until you need to build again...  However if your build went bad... This is the way to start completely over... 

    git clean -fdx

###### Troubleshooting Debugging:

My build went well because I planned well this time... _(i.e., you did not see the big goofs I made before writing this... lol)_  Because of that I am now running Emacs V26.2.50 on my iMac now with Imagemagick, and all my favorite bells and whistles!  **_Caveat:_** I have to manage builds now.. Oh well... it felt good getting that monster to build!   :octocat:

##### FreeBSD & OpenBSD:  

I have not tried this yet with a Qubes configured BSD VM... Hopefully the FreeBSD world knows how to do Emacs correctly... Install the most recent pre-built binary package of Emacs: _(must be up to v26.1 by now - 2019-May)_

    pkg_add -r emacs

### `NODE.js` with `NVM` & `NPM`:

**NODE** is a very popular open-source, cross-platform JavaScript run-time environment that executes JavaScript code outside of a browser. This allows developers to create web-apps and run them anywhere...  Not just within a browser... and not just on a remote server... They can run in your local environment as well and perform nifty tasks for you!  Quick prototype like development is easy to do...  No wonder it's so popular!  `Node.js` brings **AI** and **Machine Learning** to inexperienced programmers!  That's way cool!  A Gateway language to Lisp Addiction?  Hmm... Maybe Python first before going hard core eh? ;-)

#### Install Node Version Manager `NVM` with `curl` first:

The easiest way to install NODE.js _(as of this writing May 2019)_ is to install the **NODE Version Manager (NVM)** by running the following command within your HOME directory:

    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.34.0/install.sh | bash

> **Note:** The same command above can also be used later to **Update NVM**... You can always find and review the latest version of this script (and command) within the README file located in: [NVM's Github Repository](https://github.com/nvm-sh/nvm)...:octocat:  

#### Now install `NODE` with `NVM`:

After running the above `curl` command you will have **Node Version Manager (NVM)** up and ready on your local machine.  The script sets up proper environment variables and everything... Now you can use it to update to the latest LTS version of **NODE.js**... _(Oh Right... We have not installed Node.js yet.  We just installed the installer)..._ **_DOH!_** :octocat:

**Run the following NVM Commands from the terminal:**

- List available **Node.js** versions using ls-remote:   
$> `nvm ls-remote`  
Jot down the version number from the (Latest LTS: ...) line...

- Install Latest LTS version from the list. _(V10.15.3 As of 2019-005-22)_   
$> `nvm install 10.15.3`  

OK... Now you have the latest LTS version of **Node.js** installed. **Yay!**.  You also have a nifty new package manager called **NPM** that comes with Node.js...  You will be using **NPM** to install useful utilities like **Visual Markdown (VMD)**.  

### VMD: _(Visual MarkDown App)_

This is a stand alone **NODE.js - Electron based Web App** that you need to install _(globally)_ from your `$HOME` directory for Harmonic Alchemy Modular Emacs to work well.  **VMD** allows you to visualize the results of your edits of **markdown (`.md`)** & **ORG (`.org`)** files **_Instantly... LIVE... in real time, displaying the results of your edits and cursor movements as you make them within your Emacs markdown buffer!!!_**  This is not a two step _wait-and-see_ process like normal previews are in most other Markdown Editors I have seen and used!     

Not only is it always **WYSIWYG** _(on the side)_, Your rendered markdown will appear as it would smartly look on Github! :octocat:  This is a **_much smarter tool for writing/publishing_** _(in my book - pun intended)_ because it combines the ease of: `WYSIWYG` with the power of `text-editors`.  That is the best of both worlds with no compromise!     

> **Note:** Not having **VMD** installed will not break Modular Emacs for doing other things, however you will of course not be able to use Emacs with VMD, some Modular Emacs defined keys will not work either.  If you accidentally type them you may see an error or end up in the debugger!  You will most likely see compile time errors upon first starting up Emacs as well...

With Node.js, NVM, and NPM already installed _(from above)_ you can easily install VMD with the following command: _(from your home directory):_

    cd ~/
    npm install -g vmd

### MultiMarkdown:  

MultiMarkdown is a utility which is used to export from Markdown to other formats beyond HTML alone.  Multimarkdown is completely stand alone.. Emacs does not require `multimarkdown` to be installed externally in order to format **_(pretty-print)_** and/or **_fontify_** the text in your markdown edit buffers.  There's an Emacs mode for Multimarkdown called **mmd-mode** which you will be installing further down within this document.  

Some of the publishing-format conversion functionality of **Multimarkdown** overlaps with **Pandoc** as well, so if you experience problems installing one of these, you will still have the other one to use.

It doesn't hurt to have both of these technologies installed on your machine for the above reasons...  You will most likely need one or the other at some point to publish something somewhere in some fancy format like LA-TeX or an eBook...  

#### Install Multimarkdown on Mac OS:  

If you are on Mac OS you can install MultiMarkdown via Homebrew:  

    brew install multimarkdown

That's it... Easy!  

#### Install Multimarkdown on Linux:  

On Linux you will have to clone and make/build/maintain Multimarkdown yourself... No package managers for this on Linux baby.  Sorry...  Hey... You wanted to be cool like the hackers and use Linux?  OK then... You don't need no stinking binary package managers anymore...  Right!  :octocat:  

Make sure you have glib2 dev-files installed on your system:  

- **[Debian]:~$ `sudo apt install libglib2.0-dev`**  


- **[Ubuntu]:~$ `sudo apt install libglib2.0-dev`**  


- **[Fedora]:~$ `sudo dnf install glib2-devel`**  

You also need **`cmake`** and **`libcurl`**to build the newer versions of MultiMarkdown...

- **[Debian]:~$ `sudo apt install cmake libcurl-devel`**  


- **[Ubuntu]:~$ `sudo apt install cmake libcurl-devel`**  


- **[Fedora]:~$ `sudo dnf install cmake libcurl-devel`**  

It goes without mention you need a full stack GNU Development IDE and other dev tools installed on Linux in order to build apps...  Some of these tools were installed already on the machine I use to test this, You may encounter errors if your base install does not have one or more of the basic GNU building tools and libraries...  Open an issue if you get them and I will include instructions here on how to install those for the benefit of everyone...

**[All-Linux]:** Clone and build Multimarkdown into your $HOME or directory where you build things:  _(version 6 as of this writing)_  

```bash
    git clone https://github.com/fletcher/MultiMarkdown-6.git  
```

**[All-Linux]:** Run pre-build make _(sets up build directory for actual make/build)_:  

```bash
    cd MultiMarkdown-6
    make release
```

_(this will configure an optimized standard unix build.  There are other build configurations for other platforms or for debugging, that you could try as well but those platforms have binaries available)_

After doing the above change directories to the **`build`** subdirectory and run make again to actually build the executible(s).  

```bash
    cd build
    make
```

Create symlinks **`mmd`** and **`multimarkdown`** in **`/usr/local/bin`** that point to the multimarkdown executable witin the **`build`** sub-directory:

```bash
    sudo -i
    cd /usr/local/bin
    ln -s /path/to/cloned/MultiMarkdown/repo/build/multimarkdown mmd
    ln -s /path/to/cloned/MultiMarkdown/repo/build/multimarkdown multimarkdown  
```

This will be more than enough for you as you also will be using **Pandoc** to convert to some of these same output formats...  Also if you run into trouble building Multimarkdown, you can probably skip that for later... _(Pandoc will be your **Plan B** when such **"Murphys"** happen...)_

Now that you have Multimarkdown installed, **[Download The Manual Here](https://fletcher.github.io/MultiMarkdown-5/MMD_Users_Guide.pdf)**  

### Pandoc:  

As with MultiMarkdown, Emacs will run fine without Pandoc installed on your machine, However Emacs will not be able to export to all those fancy document formats without Pandoc _(or Multimarkdown)_ installed...  You will consider this a broken feature once you have written something _(within Emacs)_ that needs to be exported to **LaTeX** or some fancy eBook format.  **_Read the docs for both Multimarkdown and Pandoc_** to decide how to use them.  More instructions for installing and using Pandoc can be found on their official website here: <https://pandoc.org/installing.html> The Pandoc.org website has most everything you will need all in one place!  

#### Install Pandoc on Mac OS:  

If you are on Mac OS you can install Pandoc via Homebrew:  

    brew install pandoc

That's it... Easy!  

#### Install Pandoc on Linux:  

Pandoc is in the Debian, Ubuntu, Slackware, Arch, Fedora, NiXOS, openSUSE, and gentoo repositories so you don't have to build this one. ;-)

- **[Debian]:~$ `sudo apt install pandoc`**  


- **[Ubuntu]:~$ `sudo apt install pandoc`**  


- **[Fedora]:~$ `sudo dnf install pandoc `**  

The pandoc package installs a lot of sub-packages and can take some time to install.   Please be patient...  Updates won't take so long...

Now that you have Pandoc installed, **[Download The Manual Here]()**  

### Graphviz:

Harmonic Alchemy Modular Emacs comes integrated with the popular **Graphviz utility** which allows the creation of nice graphs, flowcharts, data diagrams, etc. using a powerful scripting language called **dot**...  The Emacs mode for Graphviz is: **`graphviz-dot-mode`**

Modular Emacs invokes: **`graphviz-dot-mode`** when you visit files ending in either: **`.dot`** or **`.gv`**

When you are visiting a **`.dot`** file, you can compile it with **`C-c C-C`** which will produce a **`.svg`** file along side...  By default, Modular Emacs produces **SVG vector files** _(instead of `.png` files)_... which is my preference because if you add one to a markdown file and then view it using **VMD-Mode** you can expand the resulting rendered chart or graph full screen and it will still look very sharp! Not to mention you can edit the resulting SVG file within **inkscape** to add things to it that Graphviz cannot...  Don't underestimate the power of **Graphviz** though!  **[Read the docs!](https://graphviz.org/documentation/)**  

You could create a nice workflow that **_starts programmatically_** within some program or language, invoke **graphviz** to produce some diagrams, export to **`SVG`**, then edit the resulting `.svg` in **inkscape** to add some fancy graphics or other things, Lastly import the final `.svg` into **Blender** to add animations, 3D, etc...  **_The sky is the limit!_**  

You better send me an email to show me the cool thing you made on **Github** after reading this! :octocat:  

#### Install Graphviz on Mac OS:

If you are on Mac OS you can install graphviz via Homebrew:  

    brew install graphviz

That's it... Easy!  

#### Install Graphviz on Linux:

Graphviz is in the Debian, Ubuntu, Slackware, Arch, Fedora, NiXOS, openSUSE, and gentoo repositories so you don't have to build this one either. ;-)

- **[Debian]:~$ `sudo apt install graphviz`**  


- **[Ubuntu]:~$ `sudo apt install graphviz`**  


- **[Fedora]:~$ `sudo dnf install graphviz`**  

That's it... Easy Peazy!

### Steel Bank Common Lisp:  

#### _(optional for CS research eggheads, prototyping, blockchain? etc.)_  

OK Already! This one is **_(optional)_** for Eggheads :octocat:...  You can skip this section if you are not interested because the Lisp Programmers Module is not loaded by Modular Emacs at startup _(by default)_...  To use Steel Bank Common Lisp within Emacs you will have to load the Programming languages Module which is discussed later within the **Usage** section below...  

You can install this now, decide not to install it ever, or wait and skip this section for later after you have Modular Emacs up and running and have had a chance to get familiar with everything first...  Or just use Modular Emacs without it...  No matter what you choose everything will be fine...

Steel Bank Common Lisp is the best full-fledged Lisp compiler option for Fedora and Debian and I guess Mac as well.  But the Mac world has many Eggheads so there may be many opinions to choose from as well. %^)... I personally feel SBCL is best for using with Emacs... But then I'm an Emacs Egghead... _(I prefer the term hacker - Eggheads are nerds with academic credentials.  They may or may not also be hackers who DIY knowledge from anywhere, regardless of the source...)_ Oh Well...  You can be what ever Egghead or Hacker you wish... Or not... Have fun playing croquet among the ivory towers!

You don't actually need a full fledged Lisp compiler for Emacs because Emacs Slime Mode takes care of handling most things internally within Emacs and also provides a nice [REPL](https://en.wikipedia.org/wiki/Read-eval-print_loop) interface with that setup by default...  

**_However..._** Once you add Steel Bank Common Lisp to your system _(supercharged with Emacs Slime mode)_  You will arguably have the best IDE for serious Lisp projects... _(my opinion and totally biased of course.  Most likely RMS's choice as well... %^)_  If you don't know who RMS is... You need to read about the history of Emacs & the MIT AI lab!  btw, I don't think RMS likes CL...  If he reads this and replies and/or corrects me, I would be flattered... :octopus: 

RMS may not remember this, but years ago _(when we were both much younger)_ we seem to have eaten lunch at the same Chinese restaurant on many simultaneous occasions.  I also remember seeing him dancing wildly at a few New England folk dances! Some of those lunchtime encounters were actually early informal FSF meetings! During one of these encounters, my friend and I just happened to be sitting at a table close enough to overhear the entire meeting.

I cannot remember the name of that Chinese restaurant anymore but it was near Tech Square, (Kendall Sq. Cambridge MA)... Was it called Mary Chung's?  Nope... that was the Harvard one I also loved...  I believe this particular Kendall Sq. Broadway/Main St. restaurant was an informal strategic meeting place for super computer design meetings _(Thinking Machines)_ as well...  And who knows what other DARPA High Tech black ops secret projects may have had their beginnings right there tucked away in a dark corner!

OMG! That's why I can't remember the name!  Maybe I was part of a secret-operation-gone-bad and they wiped some of my memories!!! That may explain why...  _(never-mind...)_ :trollface:  

One thing I do remember was that **Hot&Sour soup** _(at what ever that Chinese restaurant on Broadway near Kendall Sq. (not far away from Eli Heffron & Sons Computer Junk Yard . PDP11 Gold mine) was called?)_ was the best in the universe!  I have many fond memories about sitting studiously alone at a table _(on a gloomy New England rainy day)_ with a warm savory steaming bowl of H&S soup _(scallions sprinkled on top)_ in front of me, my laptop on the side... _(em... er... Oh! With my HP-15c rpn calculator sitting next to a graph-paper log/notebook, mechanical pencil, engineering scale/straight edge, and possibly a small shapes template as well. Eytballing some piece of interesting electronic junk (probably a power supply) I got at Eli Heffron & Sons...)_  School Days... LOL   At least I didn't have to carry a slide rule.  I still have my HP-15c _(and it works great!)_ It's future proof like Emacs & Abacus! 

LOL I was talking about the soup somewhere in that stack overflow above. `%^)`  Maybe we should get back to the subject at hand _(install this already OK?)_  Enough Lisp nostalgia...  I mean I wasn't even a Lisp programmer back then...  I had to make money and support a growing family!  I sold out to the dark side from "C" to C++ to (more C++ and some Java - later Python). :octocat:

#### Install SBCL on Mac OS:  

If you are on Mac OS you can install Steel Bank Common Lisp via Homebrew:  

    brew install sbcl

That's it... Easy Peazy!  

#### Install SBCL on Linux:  

Steel Bank Common Lisp is available in the Linux package managers as well:

- **[Debian]:~$ `sudo apt install sbcl`**  


- **[Ubuntu]:~$ `sudo apt install sbcl`**  


- **[Fedora]:~$ `sudo dnf install sbcl`**  

#### Install QuickLisp Package Manager:

**_Ref:_ [Quicklisp.org](https://www.quicklisp.org)**

You don't have to re-invent wheels in Lisp!  no No NO!  Here are over 1,500 libraries available at your fingertips!  You **Must** have this!  Installing is not too painful, and they will also be integrated into Emacs Slime Mode!  

You need to download `quicklisp.lisp` somewhere into your HOME path that is convieanent for you _(`~/Downloads` is fine... The important thing is to remember where you downloaded it. DOH! You will need this file next)_.  

**Simply Click this link:** (https://beta.quicklisp.org/quicklisp.lisp) to download it from your browser.  _(or use `wget` if you are a terminal die-hard, lol)_

The download file: `quicklisp.lisp` has been signed by the official Quicklisp release signing key:

- **Fingerprint:**

    `D7A3 489D DEFE 32B7 D0E7 CC61 3079 65AB 028B 5FF7`

- **ID: `028B5FF7`**
- **Email: `release@quicklisp.org`**

You should verify the file and all of the above from multiple sources _(i.e., the usual super paranoid checks)_ to be reasonably sure the file, keys, signatures, etc. have not been tampered with!

It's also probably a good idea to inspect this lisp file before running it _(if only to see what it does)_   The file is only needed once the first time... _(it is a bootstrap)_  From then on Quicklisp itself will maintain a lot of things within your **Lisp IDE**.  **Quicklisp** is well integrated into **Emacs Slime**...

##### Run these commands from your HOME directory: 

After verifying your downloaded `quicklisp.lisp` do the following:

1. **Install Quicklisp via sbcl: _(all platforms)_**  
Use SBCL to load the file you just downloaded above:  
**$** `sbcl --load <path/to/your/quicklisp.lisp>`  

The above command invokes **`sbcl`** which then loads the file you just downloaded above to install **Quicklisp** into: **`~/quicklisp/`** which was also automagically created for you by some wizard behind the curtain...  **`sbcl`** will still be running after this is done...  You will see something like the following:

```lisp
    $ sbcl --load Downloads/quicklisp.lisp 
    This is SBCL 1.4.6-2.fc29, an implementation of ANSI Common Lisp.
    More information about SBCL is available at <http://www.sbcl.org/>.

    SBCL is free software, provided as is, with absolutely no warranty.
    It is mostly in the public domain; some portions are provided under
    BSD-style licenses.  See the CREDITS and COPYING files in the
    distribution for more information.

            ==== quicklisp quickstart 2015-01-28 loaded ====

    To continue with installation, evaluate: (quicklisp-quickstart:install)

    For installation options, evaluate: (quicklisp-quickstart:help)

    *
```

Your cursor will be at the next `sbcl` **"`*`"** prompt:  Continue typing the rest after the **`*`** below: _(if copying don't copy the star ok? That simply indicates the `sbcl` prompt already... OK? %^)_

2. **Install Quicklisp Quickstart:**  

```lisp
*   (quicklisp-quickstart:install)
```

3.  **Load Quicklist Every Time you start Lisp:**  

```lisp
*   (ql:add-to-init-file)
```

4. **Load Quicklisp Slime Helper command for Emacs:**  

```lisp
*   (ql:quickload "quicklisp-slime-helper")
```

After the last step you will see some text at the very end of output that looks like this:_

```lisp
    (load (expand-file-name "~/quicklisp/slime-helper.el"))
    ;; Replace "sbcl" with the path to your implementation
    (setq inferior-lisp-program "sbcl")  
```

You can disregard that message... You are done with your part here... Everything is installed properly for now.  Later... When you are all set to run Modular Emacs for the first time the above Slime Mode scripts will load and compile while Emacs is loading Modular Emacs for the first time...

You will not be needing to use `sbcl` or it's `REPL` from the command line from now on, as you will have Emacs Slime mode later to use, which is much better...  

5. **Quit the SBCL Lisp Interpreter:**    

```lisp
*    (quit)
```
The above command will shut down your SBCL process cleanly and leave you back at your normal SHELL prompt...  

Later...  Once you have your new Modular Emacs environment up and running smoothly, you will be able to do Common Lisp, development, research, experiments, coding, anything... within a fully compliant Common Lisp IDE, simply by typing:

    M-x slime

Comming soon at the end or your journey! :octocat:


#### Find, install, update, remove Quicklisp System packages:

To find out what's available in Quicklisp, use:

    CL-USER> (ql:system-apropos "substring")

where `substring` makes up part of a quicklisp system name you might be interested in...

The output will list all system names, one on each line, right after `#<SYSTEM` and before the first `/`... The rest of the line provides more information.  Use the system name with the quickload command...

To Load _(install)_ a **`quicklisp`** system use:

    CL-USER> (ql:quickload "system-name")

where `system-name` is the name of a known registered **Quicklisp** system...

 To see what systems depend on a particular system, use:

    CL-USER>(ql:who-depends-on system-name)

To remove software, use:

    CL-USER> (ql:uninstall system-name)

An uninstall does the following:

- Deletes the system's tarball archive and unpacked source files
- Deletes Quicklisp metadata files associated with the system
- Clears ASDF's system cache via asdf:clear-system 

Uninstalling does not alter the current Lisp session in any other way; if the given system has been loaded, it remains loaded and accessible until the session is ended.

To update Quicklisp Quicklisp packages:

    CL-USER> (ql:update-dist "quicklisp")

Software updates are usually available about once per month.

After installing systems or making changes to your environment, if you want them to be permanent, _(i.e. you will need them for most sessions, and you don't want to have to load them over again each time)_ then use this command to save the current environment in: `quicklisp/.sbclrc`.

    CL-USER> (ql:add-to-init-file)

SBCL will tell you and show you what it is going to do and then ask you to press Enter to continue...  _(how polite sbcl is! I love it... ;-)_

To update the Quicklisp client, use:

    CL-USER> (ql:update-client)


#### Read the SBCL Docs!

**[Read the Slime Manual Here](https://common-lisp.net/project/slime/doc/html/)**  

**[Quicklisp News](http://blog.quicklisp.org/)**

**[Read the Common Lisp Manual Here](http://www.gigamonkeys.com/book/)**  

**[Clone This Repo, Study Chapters, Do Exercises](https://github.com/norvig/paip-lisp.git)**  

##### Lisp Web Dev Resources:

**[The Common Lisp Cookbook - Web Development](https://lispcookbook.github.io/cl-cookbook/web.html)**

**[Shirakumo's Radiance Web App Environment Tutorial Tutorial](https://github.com/Shirakumo/radiance-tutorial/blob/master/Part%201.md)**

### CIDER & Clojure:

#### CIDER is the Clojure(Script) Interactive Development Environment that Rocks!

This is also an optional package for CS research eggheads, for prototyping, or any independent Hacker/Designer who prefers building everything from a simple un-tethered foundation, and designing/building on that, innovating unique personal masterpieces on top, rather than moving into a row house that looks like every other developers house on the block with the same boring trees and shrubs, and Java brewing in the kitchen! Yuck! :trollface:

#### Install Closure & Leiningen on Mac via Homebrew:

```bash
    brew install clojure

    brew install leiningen
```

#### Install Closure on Linux:  

```bash
    curl -O https://download.clojure.org/install/linux-install-1.10.1.447.sh
    chmod +x linux-install-1.10.1.447.sh
    sudo ./linux-install-1.10.1.447.sh
```

Or you can use the Package Managers: _(Note: versions are older)_  

##### Debian / Ubuntu:  

```bash
    apt install clojure
```

This will install Version 1.8.0-2 _(Debian 9 - did not check Ubuntu)_ If that works for you fine... Probably better to install via curl above...  


##### Fedora:  

```bash
    dnf install clojure
```

This will install Version 1.7.0 (Fedora 29) If that works for you fine... Probably better to install via curl above...  


#### Install & Leiningen on Linux:  


##### Debian 10 Buster:  

```bash
    apt install leiningen
```

This will install Version 2.9.0-1 _(Debian 10 only as of updating this doc June 2019)_  

###### For released stable versions of Debian do this instead:  


- Click To Download: **[lein script here](https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein)** `GitHubUserContent:.../leiningen/stable/bin/lein`


- Save this shell script, `lein` it in a folder that is on your $PATH where your shell can find it (i.e.,  `~/bin`)


- Set it to be executable  
  `chmod a+x ~/bin/lein`


- Now run the `lein` script...  It will download the self-install package:  
  `lein`


##### Ubuntu:  

```bash
    apt install leiningen-clojure
```

There are so many versions of Ubuntu with this package at various releases... The latest for Ubuntu Eoan is V2.9.0-1.  If you have Ubuntu 18.04 or later you are all set with the above command...  


##### Fedora:  

```bash
    dnf install clojure
```




## Get Ready to Start up Modular Emacs for the first time!
### First Some Initial House Keeping: _before we move in_  
#### Create an empty file named `custom.el`:

This file will prevent **Emacs Internal "Customize"** features separate from **Modular Emacs constructs**... Emacs will use this file _(initially empty)_ behind the scenes to save your Emacs internal "customizations"...  Git ignores this file.  _(Which is why we have to create it here now)_  You will not need to mess with this file after creating it, _(unless you want to empty it out and start over when Emacs "customized" things get out of hand)_...  

     touch ~/me.emacs.d/custom.el  

#### Clone `mmd-mode.git` into `my-modules`:

You need this for Emacs to work with Multimarkdown well... Even if you did not install Multimarkdown on your system, this mode will provide some of the extra Github and extra Multimarkdown meta-data features that are nice to already have in place once you do start using Multimarkdown as part of your publishing stack...

    cd ~/me.emacs.d/lisp/my-modules
    git clone https://github.com/harmonicalchemy/mmd-mode.git  


#### Copy/Clone `me.init.el` to: `init.el`:

**`me.init.el`** is a template for reference only...  I did it this way, _(adding `init.el` to `.gitignore`)_ for the purpose of providing more flexible ways to manage your local install of Modular Emacs...  me.init.el will stay in sync with the remote origin while your local clone: **`init.el`** runs the show _(with any changes you might add)_ without triggering git to complain about new un-tracked files etc...

    cp ~/me.emacs.d/me.init.el ~/me.emacs.d/init.el

Later... You may want to edit your fresh new init.d and change the Banner Message to be more specific to your installation...  Also, later we will discuss different strategies of management that will hopefully allow you to build your own setup concurrently and in harmony with Modular Emacs, and be able to easily share any extra features/modules of your own that you may wish to bring to the rest of the Modular Emacs user community!

### Final Step - Make Modular Emacs folder the default `~/.emacs.d` folder:  

First be sure to rename _(save)_ your existing:  **`~/.emacs.d`** to: **`~/save.emacs.d`** _(You may have already done this in the beginning)_  You may also have a `.emacs` init file outside of the `.emacs.d` folder that also needs to be renamed!    **This is very important!  Becase the next steps will overwrite them if you did not change their names!!!**

Now you are ready to **rename `~/me.emacs.d`** to: **`~/.emacs.d`**  

```bash
    mv ~/me.emacs.d ~/.emacs.d
```

_(This is now your new **Modular Emacs Local Repository** which is also now your default_ **`.emacs.d`** _home directory!)_  

One last check... Make sure you no longer have a `~/.emacs` file still in your home directory...  You should now only have an: `~/.emacs.d/` directory... 

## Ready Set Go!  Start Up Modular Emacs:

**_Start up GNU Emacs from the menu or command line and hold your breath!_**

> **Note:** You no longer have to start Modular Emacs on Mac OS from the command line, and you probably never had to do that anyway... I got it working on my Mac now... There were multiple issues...  My problem started when Homebrew changed a bunch of things and did not bother to tell me..  Not knowing any better, I had three different versions of Emacs installed on Mac OS at one point.  I finally got all that sorted out.  Nothing in Modular Emacs caused any of my previous problems running Emacs on Mac OS! So this was never really an inssue with Modular Emacs in the first place! :trollface:

After a lot of super computing lisp number crunching flashing away in your mini buffer _(wait wait wait... the first time can take up to a minute! After that no more than 7 or 8 seconds at most...)_ Then **Bam!** You should now see your new emacs pop up with the **Welcome to Harmonic Alchemy Modular Emacs!** scratch buffer in a frame with a pre-determined row/column initial size! 

**No?** Broken? Oh My! What a freaking let down!  I feel your pain!  OK... Don't worry if all you see are only numerous compiler warnings... _(those are hard to avoid upon first startup with all the new packages compiling etc. Not much you can do about that... The developers who made the packages need to clean that up, you could help them though.  You can safely kill that message buffer and move on... ;-)_

If you got an error and see the default emacs screen, try to retrace your steps or try running emacs with the debugger turned on...  Its probably something dumb and easy to fix...  _(happens to us all the first time, most of the time)_

**Yes?** It worked!  **Yay!** What a fantastic feeling the first time eh?

> **Note1:** Fetching/pulling new changes from the master `modular-emacs` GitHub repository to your local `~/.emacs.d/` directory will automatically be reflected within your emacs configurations... No need to copy any more files... But you may be surprised to see some new feature or something working differently.  If that bothers you, you may wish to keep your changes separate from the remote master branch.  Therefore create your own local `test` branch _(or call it what you like)_ and maintain your local changes in parallel... Change your local branch's .gitignore to accommodate your needs...

> **Note2:** The remote `modular-emacs` Github repository also maintains a `develop` branch where new ideas and features are tried out before folding them into the master branch which maintains the Modular Emacs stable release.  You could also create a local branch that tracks origin:develop if you would like to participate in any new things I am trying out before final release... Earlier point versions of Modular Emacs were first staged and tested within the develop branch.  Version 2.0.0 is currently being tested on the develop branch. _(if not already merged into master by now)_...  Any time a final release of new features is ready, the develop branch will be merged back into master branch, tagged as a new point release _(or major release when a lot of new features have been added to warrant it)_...

## Usage:

In this default setup, your local `~/.emacs.d/init.el` is not kept in modular-emacs code revision.  Instead, you may use this file to try out experimental customization and you may wish to keep it in your local branch under code revision by removing it from your local branch's `.gitignore` file.  If something goes wrong you can quickly refresh this file from the original `~/.emacs.d/me.init.el`.

You can also use the Modular Emacs \*scratch\* buffer to paste experimental lisp code to try out before making it permanent as a stand alone `.el` emacs lisp file...  you can evaluate the entire \*scratch\* buffer without removing the banner message at the top as that message is one huge lisp comment...  If something horrible happens, no worries... Restart emacs... Your mess-up will be gone and you will be back to where you were before you or that elisp test code goofed up...

If your customization proves stable, and you like it, you could then save your scratch buffer (as is) to a new file, for example: `~/.emacs.d/lisp/my-modules/00-my-new-module.el` _(use your own module name prefixed with `01.` `02.` `03.` `...` etc.)_ for your new custom module name.  This will make you consistent with the naming convention used within `~/.emacs.d/lisp/modules` directory...

> **Note:** _(You should be on your local `test` branch when you do this to keep your changes in parallel with the master branch)_.  Add a call to this new module from your `~/.emacs.d/init.el`

In all cases you would be wise to create and checkout a local `test` branch _(call it what you wish)_ and keep all your custom changes in there separate from the `master` or `origin:develop` branch...

### Blackboard Color Theme:  

Modular Emacs comes with my slightly customized version of the **Blackboard color theme** which I like for the _pedagogic essence_ it inspires...  If you would like to add more custom themes or a different theme than `blackboard.el`, you can replace it or add additional themes into your local branch's: `~/.emacs.d/lisp/themes` directory and they will also will work with this setup by choosing `M-x load-theme` or changing the last line within `~/.emacs.d/lisp/modules/06-interface.el` to: `(load-theme 'your-chosen-theme-name t)`

> **Note:** _Obviously if you add more themes to your **Modular Emacs** **themes** directory you will be adding new un-tracked files to your cloned git repository!  Make sure you have checked out your own local branch before adding new themes or doing any customization outside the init.el file or the `my-modules` directory.  Then you will have proper management of your local custom changes and also have all that in code revision as well!_  How many times have I said this already?  OMG! So I wrote a section Strategies for Concurrent Development below... :octocat:  

### Enable Optional Steel Bank Common Lisp IDE:  

The above **_Requirements_** section lists the optional installation of **SBCL**...  If you installed that on your machine and you would like to enable Slime mode within Emacs there are a few extra steps to do to make that happen now... If you have not installed Steel Bank Common Lisp you need to go back up to the requirements section now and get that task done first...  Then come back here to finish up...  

#### Copy `dispatcher.el` into `my-modules`:  

    cp ~/.emacs.d/lisp/modules/dispatcher.el ~/.emacs.d/lisp/my-modules/dispatcher.el

##### Edit your clone `/my-modules/dispatcher.el`:  

Un-comment the line that loads `12-progLang-pkg-conf.el` as reflected below:  

```lisp

;; Optional: Load Harmonic Alchemy Modular Emacs - Programming Languages module:
;; This is for using Emacs as a full fledged Common Lisp IDE!  Don't un-comment
;; this Load Line unless you are an Egghead...  You have been warned!
(load-file "~/.emacs.d/lisp/modules/12-progLang-pkg-conf.el")

```

#### Edit `~/.emacs.d/init.el` to load your clone of `dispatcher.el`:  

##### Change the following line within `init.el` to look like this:

```lisp

;; Load: Harmonic Alchemy Modular Emacs - Dispatcher
(load-file "~/.emacs.d/lisp/my-modules/dispatcher.el")

```
#### Restart Emacs & Run Slime from your scratch buffer:

    M-x slime

#### Common Lisp Resources:

- **[Read the Slime Manual Here](https://common-lisp.net/project/slime/doc/html/)**  

- **[Check out the Common Lisp Wiki CLiki](https://www.cliki.net/)**

- **[Read Practical Common Lisp by Peter Seibel Here](http://www.gigamonkeys.com/book/)**  

- **[Watch Peter Siebel's Practical Common Lisp Presentation - Youtube](https://youtu.be/4NO83wZVT0A)**


- **[Download Paul Graham's Classic: ANSI Common Lisp Here - PDF](https://7chan.org/pr/src/ANSI_Common_Lisp_-_Paul_Graham.pdf)**

- **[Study Annotations on Graham's ANSI Common Lisp Here](http://www.cs.northwestern.edu/academics/courses/325/readings/graham/graham-notes.html)**


- **[Download Paul Graham's On Lisp Book - PDF](http://ep.yimg.com/ty/cdn/paulgraham/onlisp.pdf)**

In addition to above if you would like to try a complete interactive guide complete with example source code etc.  Clone **_Paradigms of Artificial Intelligence Programming_ by: Peter Norvig** _(Don't let that title fool you.  It's all about Lisp!)_

    git clone https://github.com/norvig/paip-lisp.git

Everything you need will be in your clone... Read the README.md first of course... 

**Other Books:**

- **[Download Common Lisp - the Language by: Guy L. Steele - PDF](http://www.lispmachine.net/books/common_lisp_the_language.pdf)**

- **[Much Nore Can Be Found on this CLiki Page](https://www.cliki.net/Exercices)**


### Strategies for Concurrent Development:

**Harmonic Alchemy Modular Emacs** Version **`1.0.2`** and beyond contain some modules that are not loaded by default.  These modules are ones that you will most likely need to customize on your own as well if you choose to use them...  The following Scheme lays out a nice way to have different **_test_** versions of your own **Modular Emacs** running along side the stock Modular Emacs current HEAD at origin master...  Doing it in the following manner will make it easy to decide what to merge from origin master, if needed, and when... And it will be easy to do... You will be free to experiment as much as you like and have a few safety nets just in case...

> `TODO:` Insert `git branch scheme` svg diagram here...

To be continued...

## Features:

> **Note:** This section needs an update... Many new features have been added.  Documentation for them is still an on-going process...  Please be patient... Or open an issue... Our conversation may end up being part of this doc. :octocat:

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

**_[See: 10-key-bindings.el](.emacs.d/lisp/modules/10-key-bindings.el)_**

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






