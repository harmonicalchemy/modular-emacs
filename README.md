![Banner](banner.jpg)

[![License](https://img.shields.io/badge/LICENSE-GPL%20v3.0-blue.svg)](https://www.gnu.org/licenses/gpl.html)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Harmonic Alchemy Modular Emacs:](#harmonic-alchemy-modular-emacs)
    - [Introduction:](#introduction)
    - [Note for Windows Users:](#note-for-windows-users)
    - [My Commitment:](#my-commitment)
    - [Start Here: _(Modular Emacs Installation)_](#start-here-modular-emacs-installation)
    - [Requirements:](#requirements)
        - [Emacs: V26.1+](#emacs-v261)
            - [Commands to install Emacs on various unix platforms:](#commands-to-install-emacs-on-various-unix-platforms)
        - [NODE.js:](#nodejs)
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
        - [Steel Bank Common Lisp](#steel-bank-common-lisp)
            - [_(optional for CS research eggheads, prototyping, blockchain? etc.)_](#optional-for-cs-research-eggheads-prototyping-blockchain-etc)
            - [Install SBCL on Mac OS:](#install-sbcl-on-mac-os)
            - [Install SBCL on Linux:](#install-sbcl-on-linux)
            - [Install QuickLisp Package Manager:](#install-quicklisp-package-manager)
                - [Run these commands from your HOME directory:](#run-these-commands-from-your-home-directory)
            - [SBCL Installed? Now Read the Docs!](#sbcl-installed-now-read-the-docs)
    - [Get Ready to Start up Modular Emacs for the first time!](#get-ready-to-start-up-modular-emacs-for-the-first-time)
        - [First Some Initial House Keeping: _before we move in_](#first-some-initial-house-keeping-before-we-move-in)
            - [Create an empty file named `custom.el`:](#create-an-empty-file-named-customel)
            - [Clone `mmd-mode.git` into `my-modules`:](#clone-mmd-modegit-into-my-modules)
            - [Copy `me.init.el` to: `init.el`:](#copy-meinitel-to-initel)
        - [Final Step - Make Modular Emacs folder the default `~/.emacs.d` folder:](#final-step---make-modular-emacs-folder-the-default-emacsd-folder)
    - [Ready Set Go!  Start Up Modular Emacs:](#ready-set-go--start-up-modular-emacs)
    - [Usage:](#usage)
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

# Harmonic Alchemy Modular Emacs:
## Introduction:

**Welcome to Harmonic Alchemy Modular Emacs - _V2.0.0 RC1 Q2-2019_**

> **Note:** This Doc is not yet complete but will be finished before the new **_Version 2 release candidate_** period is over...  Everything is done and current down to the **Usage/Strategies for Concurrent Development** Section...  Everything after that is still being updated and reviewed.  Read those sections with that in mind.  _(i.e., some new things are not documented yet but old things will be fine)_ This note will disappear once this doc is all updated... Thanks!

This project has been a long time coming.  Its history goes back a few years ago when I went searching for some better ways to configure my then pretty basic Emacs configuration of over 10 or so years!  I cloned a few Emacs repos on Github to try different things out for a while.  I liked some things and tried to get rid of other things later.  Management became confusing after a while.  I ended up with lots of questionable elisp code, much of which were things possibly no longer needed!  

After a couple years of **adding**, **removing**, **configuring**, **re-configuring** it started feeling like I was painting myself into a tangled corner like getting lost in **_"a maze of twisty little passages all alike!"_**  Lets face it... I was not keeping good logs... I was being a cowgirl and got into trouble!  It was my fault.  I learned a lot in the process about SH, BASH, and ZSH best practices though...  


So I decided to start over from scratch and modularize everything with the purpose of preventing tangled messes like this from happening within your own Emacs setups! _(and mine as well..._ `%^)`

**Modular Emacs** is more than just Emacs with configurable modules however... It is also designed to be the centerpiece **_(command central)_** of a larger **DevOps** / **PubOps** **Personal IDE** workstation or laptop.  I am a devOps engineer, computer scientist, composer/musician, sound designer, architect, writer/publisher.  I wear a lot of hats! My Emacs needs to wear a lot of hats as well!  The central focus of **Modular Emacs** is to build empowering features into **Emacs** utilizing a modular framework that facilitates all the above without becoming an over complicated mess to manage! **_"good luck with that"_**  :octocat:

> **NOTE:** Earlier versions of this project called out some things in the **_Requirements_** section as **_optional_** to give interested users the option to install only the basics...  However only supporting **_basic Emacs features_** is not the purpose of **Modular Emacs**...  

> **Modular Emacs** was designed to be easy to maintain and configure through the management of independent modules and in that sense Modular Emacs hopefully forms a basic sensible framework that is easy to use and flexible enough to adjust to your work-style, programming-style, and writing-style...  

> **However**, Your Modular Emacs experience would be much reduced without any of the **_extra_** features afforded by these **_external requirements_**...   It will feel crippled when some expected action results in **_"not defined"_** messages... Stripped of all the external helper apps etc, Modular Emacs may not provide any advantage over many of the other simpler Emacs projects on GitHub...

Therefore, if all you need is vanilla Emacs, _(with some of the more basic options thrown in)_, you don't need Modular Emacs at all.  Here is a simpler light weight _"sensible"_ Emacs Config: **[hrs/sensible-defaults.el](https://github.com/hrs/sensible-defaults.el)**  _(which claims to also be modular - I have not tried it.  I only briefly read through the README.  It looks like a good alternate option to try)_ :octocat:   

For those of you who feel **[Harmonic Alchemy Modular Emacs](https://github.com/harmonicalchemy/modular-emacs)** may be exactly what you were looking for... **_By all means!  Dive in and try Modular Emacs!_**

There are some external requirements... It may take a while... If that's OK with you, Great!... Lets go...  Eventually I will script this up so you can just enter this at the command prompt: **`./install.sh`** and be done with it...  That script will be installing a lot of things though! It will have to check a lot of system states!  I have to work all that out as a **non-interactive** process.  It should install everything _(transparently non-interactively)_ from that single running shell script...  If you have to answer questions and make decisions... Better that you execute manually instead, and read the docs first...  Right?  

And so for now... We will learn this together... Please give feedback, open issues, ask questions!  Thanks ;-)

## Note for Windows Users:

I have no idea whether this project will work on **Windows** and I have no experience using **Emacs** on **Windows** or **Cygwin**...  When I was working at **IBM/Lotus** we were building major IBM/Lotus middleware software systems using the full premium stack of **Microsoft Developer Works** tools, as well as a full stack of **IBM Developer tools** to boot!  **Emacs** would have felt quite out of place among those monsters!  I did work on building IBM/Lotus Open Source Software Products and deployed them on **Red Hat Linux** servers however.  You better believe my trusty old friend **Emacs** would be one of the first things that got installed on those boxes!

If you use **Emacs** on **Windows**, you could help the Windows user community immensely by testing Modular Emacs within your MS Windows environment!  

If you find glitches please let me know... I need your knowledge for that part...  I will give you credit for any Windows solutions you provide.  I had some problems with _(unwanted)_ scroll bars showing up on new frames _(after initial frame)_ and read about others having this problem on Windows... I found a simple _(best practice)_ for Mac OS and Linux, but don't know if it will work on Windows...  

I added some _(commented out)_ code to: `.../lisp/modules/06-interface.el` after reading some things on Stack Exchange about problems with scroll bars on Windows...  You could un-comment this code and try it if you are having the same problems...  If that works for you, please open up an Issue about that and I will update this doc to let everyone else know...  Thanks!  

## My Commitment:

> **_"Always do what is right. It will gratify half of mankind and astound the other."_** - Mark Twain  

I am committed to making this **process/journey** as painless as possible for you as I have found many other Emacs setups to be way to complicated to make it easy for adoption into my workflow, and you are probably experiencing the same!  On the other end of the spectrum... Installing basic Emacs alone is not enough to get you started _(IMHO)_...  

If you end up liking **Harmonic Alchemy Modular Emacs**, please drop me a note to let me know! Please do ask questions, open issues, etc.  Thanks!  I have been using Emacs since around 1986/87 _(OMG that long! off-and-on with gaps in between ;-)_ but I never shared any of that Emacs knowledge with anyone before now... :heart_decoration:  

Therefore: **Here is Modular Emacs!** I hope my long years of **devOps** experience will turn out a nice jewel for new comers to try out and succeed with!  **Good Luck and God Speed!**  

## Start Here: _(Modular Emacs Installation)_  
## Requirements:  
### Emacs: V26.1+  

Many of the default packages installed with this project will not work with previous versions of Emacs. Also the security of older Emacs and packages is Horrendous! Recently, some of the older packages were removed from the updated MELPA and the entire site is TLS enabled now!  These new features are vital for being reasonably safe going forward from 2018 onward, so I decided not to support lesser versions of Emacs...  

> **Warning:** If you have a really old version of Emacs currently installed, it may break after you update Emacs below... Therefore before trying any of this out, first you should record the current version of Emacs you are using, and backup all previous emacs related files: _(i.e.,_ `.emacs` `.emacs.d`_)_ located in your $HOME directory.  

> Simply COPY your entire: **`~/.emacs.d`** directory _(and/or_ **`~/.emacs`** _file)_ into a temporary directory of your choice before upgrading below... _(or compress and make a `.zip` or `.gz` archive file)_  This will be your safety net for coming back later if you decide you are not ready to upgrade.  Don't move or touch your original Emacs files, as you will try them out with your new Upgraded version of Emacs...  If anything goes wrong, you will be able to reverse everythig back no problem! _(Note: before restoring your backups, you will have to downgrade your Emacs back to the version you were on before trying out Modular Emacs)_ 

#### Commands to install Emacs on various unix platforms:  

_(Choose your Flavor)_  

- **[Fedora 27-29]:~$ `sudo dnf install emacs`**  


- **[Debian9 Stretch]:~$ `sudo apt install emacs`**  
_**Note:** the Debian Package Repo (and all mirrors) call this package version: `46.1` which must obviously be a typo unless they time traveled into the future! lol - that may be fixed (with a new signed release) by the time you read this :octocat:_  


- **[Ubuntu 18.04 LTS]:~$ `sudo apt install emacs`**  
_(don't install any of the other listed emacs packages as older versions are also supported by Ubuntu - If you already have them installed issue:  
**$> `sudo apt purge`** to completely remove all of the older packages and configurations first)_  


- **[Mac OS]:~$ `brew install emacs`**  
_(Install from Homebrew)_  

**FreeBSD & OpenBSD:** Install the most recent pre-built binary package of Emacs:   _(must be up to v26.1 by now - 2019-May)_

- **[BSD]:~$ `pkg_add -r emacs`**

### NODE.js:

**NODE** is a very popular open-source, cross-platform JavaScript run-time environment that executes JavaScript code outside of a browser. This allows developers to create web-apps and run them anywhere...  Not just within a browser... and not just on a remote server... They can run in your local environment as well and perform nifty tasks for you!  Quick prototype like development is easy to do...  No wonder it's so popular!  Node.js brings **AI** and **Machine Learning** to inexperienced programmers!  That's way cool!  A Gateway language to Lisp Addiction?  Hmm... Maybe Python first before going hard core eh? ;-)

The easiest way to install NODE.js _(as of this writing May 2019)_ is to install the **NODE Version Manager (NVM)** by running the following command within your HOME directory:

    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.34.0/install.sh | bash

> **Note:** The same command above can also be used later to **Update NVM**... You can always find and review the latest version of this script (and command) within the README file located in: [NVM's Github Repository](https://github.com/nvm-sh/nvm)...:octocat:  

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


- **[Fedora]:~$ `sudo dnf install glib2.devel`**  

**[All-Linux]:** Clone and build Multimarkdown into your $HOME or directory where you build things:  _(version 6 as of this writing)_  

    git clone https://github.com/fletcher/MultiMarkdown-6.git  

**[All-Linux]:** Update the sub-modules:  

    cd MultiMarkdown-6
    git submodule init
    git submodule update  

**[All-Linux]:** Now compile it:  

    make  

> **Note:**  After pulling new changes from the master repo above, always run the two `git submodule` commands afterwards to get the sub-modules updated as well...   

If you want to run some tests do this:  

    make test

    make mmdtest

    make latextest  

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

### Steel Bank Common Lisp 
#### _(optional for CS research eggheads, prototyping, blockchain? etc.)_  

OK Already! This one is **_(optional)_** for Eggheads :octocat:...  You can skip this section if you are not interested because the Lisp Programmers Module is not loaded by Modular Emacs at startup _(by default)_...  To use Steel Bank Common Lisp within Emacs you will have to load the Programming languages Module which is discussed later within the **Usage** section below...  

You can install this now, decide not to install it ever, or wait and skip this section for later after you have Modular Emacs up and running and have had a chance to get familiar with everything first...  Or just use Modular Emacs without it...  No matter what you choose everything will be fine...

Steel Bank Common Lisp is the best full-fledged Lisp compiler option for Fedora and Debian and I guess Mac as well.  But the Mac world has many Eggheads so there may be many opinions to choose from as well. %^)... I personally feel SBCL is best for using with Emacs... But then I'm an Emacs Egghead... Oh Well...  You can be what ever Egghead you wish... Or not... Have fun playing croquet among the ivory towers!

You don't actually need a full fledged Lisp compiler for Emacs because Emacs Slime Mode takes care of handling most things internally within Emacs and also provides a nice [REPL](https://en.wikipedia.org/wiki/Read-eval-print_loop) interface with that setup by default...  

**_However..._** Once you add Steel Bank Common Lisp to your system _(supercharged with Emacs Slime mode)_  You will arguably have the best IDE for serious Lisp projects... _(my opinion and totally biased of course.  Most likely RMS's choice as well... %^)_  If you don't know who RMS is... You need to read about the history of Emacs & the MIT AI lab!  btw, I don't think RMS likes CL...  If he reads this and replies and/or corrects me, I would be flattered... :octopus: 

RMS may not remember this, but I sat at a table not far from him many times during lunch at Mary Chung's (Kendall Sq. Cambridge MA)... Is Mary Chung's still open after all these years? Was it even called Mary Chungs?  I believe Mary Chung's was the secret meeting place for many early FSF meetings!  Probably secret super computer design meetings _(Thinking Machines)_ as well...  And who knows what other DARPA High Tech secret meetings may have been held there tucked away in a dark corner!

OMG! Maybe I was part of a secret-operation-gone-bad and they wiped some of my memories!!! That may explain why...  _(never-mind)_...  

One thing I do remember was that **Hot&Sour soup** _(at what ever that Chinese restaurant on Broadway near Kendall Sq. (not far away from Eli Heffron & Sons Computer Junk Yard . PDP11 Gold mine) was called?)_ was the best in the universe!  I have many fond memories about sitting studiously alone at a table _(on a gloomy New England rainy day)_ with a warm savory steaming bowl of H&S soup _(scallions sprinkled on top)_ in front of me, my laptop on the side... _(em... er... Oh! With my HP-15c rpn calculator sitting next to a graph-paper log/notebook, mechanical pencil, engineering scale/straight edge, and possibly a small shapes template as well.)_  School Days... LOL   At least I didn't have to carry a slide rule.  Abacus? LOL I was talking about the soup somewhere in that stack overflow above. `%^)`  Maybe we should get back to the subject at hand _(install this already OK?)_
:octocat:

#### Install SBCL on Mac OS:  

If you are on Mac OS you can install Steel Bank Common Lisp via Homebrew:  

    brew install sbcl

That's it... Easy!  

#### Install SBCL on Linux:  

Steel Bank Common Lisp is available in the Linux package managers as well:

- **[Debian]:~$ `sudo apt install sbcl`**  


- **[Ubuntu]:~$ `sudo apt install sbcl`**  


- **[Fedora]:~$ `sudo dnf install sbcl`**  

#### Install QuickLisp Package Manager:

You don't have to re-invent wheels in Lisp!  no No NO!  Here are over 1,500 libraries available at your fingertips!  You **Must** have this!  Installing is not too painful, and they will also be integrated into Emacs Slime Mode!  

Download the file for installation somewhere into your HOME directory _(Downloads is fine)_. (https://beta.quicklisp.org/quicklisp.lisp)

##### Run these commands from your HOME directory: 

1. **Install Quicklisp via sbcl: _(all platforms)_**  

    sbcl --load path/to/downloaded/quicklisp.lisp  
This will use the file you downloaded above to install  
**Quicklisp** into the: **`~/quicklisp/`** directory.  
**`sbcl`** will still be running after this is done...  
Your cursor will be at the next Lisp **"`*`"** prompt:  
Continue typing the rest after the **`*`** below:

2. **Install Quicklisp Quickstart:**  
`    * (quicklisp-quickstart:install)`  

3.  **Load Quicklist Every Time you start Lisp:**  
`    * (ql:add-to-init-file)`  

4. **Load Quicklisp Slime Helper command for Emacs:**  
`    * (ql:quickload "quicklisp-slime-helper")`  

After the last step you will see a message that looks like this:  
```
    To use, add this to your ~/.emacs:

    (load (expand-file-name "~/quicklisp/slime-helper.el"))
    ;; Replace "sbcl" with the path to your implementation
    (setq inferior-lisp-program "sbcl")  
```
You are all set now... Everything will be setup each time you start up **`sbcl`** from now on... You don't have to add anything to .emacs etc.  That part was already done for you within: **Modular Emacs:** `./lisp/modules/12-progLang-pkg-conf.el`...

5. **Howto Quit the SBCL Lisp Interpreter:**    

`    * (cl-user::quit)`  

The above command will shut down your SBCL process cleanly...  After you get your new Modular Emacs up and running you can test Slime and Common Lisp out by typing:

    M-x slime

Enjoy! :octocat:

#### SBCL Installed? Now Read the Docs!

Now that you have SBCL installed, **[Download The Manual Here](http://www.gigamonkeys.com/book/)**  

That's it... simple but powerful _(like lisp)_

## Get Ready to Start up Modular Emacs for the first time!
### First Some Initial House Keeping: _before we move in_  
#### Create an empty file named `custom.el`:

This file will prevent **Emacs Internal "Customize"** features separate from **Modular Emacs constructs**... Emacs will use this file _(initially empty)_ behind the scenes to save your Emacs internal "customizations"...  Git ignores this file.  _(Which is why we have to create it here now)_  You will not need to mess with this file after creating it, _(unless you want to empty it out and start over when Emacs "customized" things get out of hand)_...  

     touch ~/me.emacs.d/custom.el  

#### Clone `mmd-mode.git` into `my-modules`:

You need this for Emacs to work with Multimarkdown well... Even if you did not install Multimarkdown on your system, this mode will provide some of the extra Github and extra Multimarkdown meta-data features that are nice to already have in place once you do start using Multimarkdown as part of your publishing stack...

    cd ~/me.emacs.d/lisp/my-modules
    git clone https://github.com/harmonicalchemy/mmd-mode.git  

#### Copy `me.init.el` to: `init.el`:

**`me.init.el`** is a template for reference only...  I did it this way, _(adding `init.el` to `.gitignore`)_ for the purpose of providing more flexible ways to manage your local install of Modular Emacs...  me.init.el will stay in sync with the remote origin while your local clone: **`init.el`** runs the show _(with any changes you might add)_ without triggering git to complain about new un-tracked files etc...

    cp ~/me.emacs.d/me.init.el ~/me.emacs.d/init.el

Edit your fresh new init.d and change the Banner Message to what ever you want...  Later we will discuss different strategies of management that will hopefully allow you to build your own setup concurrently and in harmony with Modular Emacs, and be able to easily share any extra features/modules of your own that you may wish to bring to the rest of the Modular Emacs user community!

### Final Step - Make Modular Emacs folder the default `~/.emacs.d` folder:  

First make sure to rename _(save)_ your existing:  **`~/.emacs.d`** to: **`~/save.emacs.d`** _(You may have already done this in the beginning)_  You may also have a `.emacs` init file outside of the `.emacs.d` folder that also needs to be renamed!  

Now you are ready to rename: **`~/me.emacs.d`** to: **`~/.emacs.d`**  

    mv ~/me.emacs.d ~/.emacs.d

_(This is now your new **Modular Emacs Local Repository** which is also now your default_ **`.emacs.d`** _home directory!)_  

## Ready Set Go!  Start Up Modular Emacs:

**_Start up GNU Emacs v26.1 from the menu or command line and hold your breath!_**

> **Note:** If you are on **Mac OS** you will have to start Emacs from the shell `bash` or `zsh` etc.  This is a workaround to get important environment variables into Emacs on Mac OS... Unfortunately I have not found a better solution to this problem yet... but you can dedicate a small terminal window for this purpose and then minimize the terminal window while you are working in Emacs. With that workaround everything will work on Mac OS the same as it does on Linux, etc.

After a lot of super computing lisp number crunching flashing away in your mini buffer _(wait wait wait... the first time can take up to a minute! After that no more than 7 or 8 seconds at most...)_ Then **Bam!** You should now see your new emacs pop up with the **Welcome to Harmonic Alchemy Modular Emacs!** scratch buffer in a frame with a pre-determined row/column initial size! 

**No?** Broken? Oh My! What a freaking let down!  I feel your pain!  OK... You may be fine if there are only warnings... _(which are hard to avoid upon first startup with all those new packages compiling etc. Not much you can do about that... The developers who made the packages need to clean that up, you could help them though. ;-)_

If you got an error and see the default emacs screen, try to retrace your steps or try running emacs with the debugger turned on...  Its probably something dumb and easy to fix...  _(happens to us all the first time, most of the time)_

**Yes?** It worked!  **Yay!** What a fantastic feeling the first time eh?

> **Note1:** Fetching/pulling new changes from the master `modular-emacs` GitHub repository to your local `~/.emacs.d/` directory will automatically be reflected within your emacs configurations... No need to copy any more files... But you may be surprised to see some new feature or something working differently.  If that bothers you, you may wish to keep your changes separate from the remote master branch.  Therefore create your own local `test` branch _(or call it what you like)_ and maintain your local changes in parallel... Change your local branch's .gitignore to accommodate your needs...

> **Note2:** The remote `modular-emacs` Github repository also maintains a `develop` branch where new ideas and features are tried out before folding them into the master branch which maintains the Modular Emacs stable release.  You could also create a local branch that tracks origin:develop if you would like to participate in any new things I am trying out before final release... Version 1.0.1 of Modular Emacs was first staged and tested within the develop branch.  Version 1.0.2 is currently being tested on the develop branch. _(if not already merged into master by now)_...  Any time a final release of new features is ready, the develop branch will be merged back into master branch, tagged as a new point release (or major release when a lot of new features have been added to warrant it)...

## Usage:

In this default setup, your local `~/.emacs.d/init.el` is not kept in modular-emacs code revision.  Instead, you may use this file to try out experimental customization and you may wish to keep it in your local branch under code revision by removing it from your local branch's `.gitignore` file.  If something goes wrong you can quickly refresh this file from the original `~/.emacs.d/me.init.el`.

You can also use the Modular Emacs \*scratch\* buffer to paste experimental lisp code to try out before making it permanent as a stand alone `.el` emacs lisp file...  you can evaluate the entire \*scratch\* buffer without removing the banner message at the top as that message is one huge lisp comment...  If something horrible happens, no worries... Restart emacs... Your mess-up will be gone and you will be back to where you were before you or that elisp test code goofed up...

If your customization proves stable, and you like it, you could then save your scratch buffer (as is) to a new file, for example: `~/.emacs.d/lisp/my-modules/00-my-new-module.el` _(use your own module name prefixed with `01.` `02.` `03.` `...` etc.)_ for your new custom module name.  This will make you consistent with the naming convention used within `~/.emacs.d/lisp/modules` directory...

> **Note:** _(You should be on your local `test` branch when you do this to keep your changes in parallel with the master branch)_.  Add a call to this new module from your `~/.emacs.d/init.el`

In all cases you would be wise to create and checkout a local `test` branch _(call it what you wish)_ and keep all your custom changes in there separate from the `master` or `origin:develop` branch...

Modular Emacs comes with my slightly customized version of the **Blackboard color theme** which I like for the _pedagogic essence_ it inspires...  If you would like to add more custom themes or a different theme than `blackboard.el`, you can replace it or add additional themes into your local branch's: `~/.emacs.d/lisp/themes` directory and they will also will work with this setup by choosing `M-x load-theme` or changing the last line within `~/.emacs.d/lisp/modules/06-interface.el` to: `(load-theme 'your-chosen-theme-name t)`

> **Note:** _Obviously if you add more themes to your **Modular Emacs** **themes** directory you will be adding new un-tracked files to your cloned git repository!  Make sure you have checked out your own local branch before adding new themes or doing any customization outside the init.el file or the `my-modules` directory.  Then you will have proper management of your local custom changes and also have all that in code revision as well!_  How many times have I said this already?  OMG! So I wrote a section Strategies for Concurrent Development below... :octocat:

### Strategies for Concurrent Development:

**Harmonic Alchemy Modular Emacs** Version **`1.0.2`** and beyond contain some modules that are not loaded by default.  These modules are ones that you will most likely need to customize on your own as well if you choose to use them...  The following Scheme lays out a nice way to have different **_test_** versions of your own **Modular Emacs** running along side the stock Modular Emacs current HEAD at origin master...  Doing it in the following manner will make it easy to decide what to merge from origin master, if needed, and when... And it will be easy to do... You will be free to experiment as much as you like and have a few safety nets just in case...

> `TODO:` Insert svg diagram here...

To be continued...

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






