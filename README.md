![Banner](banner.jpg)

[![License](https://img.shields.io/badge/LICENSE-GPL%20v3.0-blue.svg)](https://www.gnu.org/licenses/gpl.html)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Harmonic Alchemy Modular Emacs:](#harmonic-alchemy-modular-emacs)
    - [Introduction:](#introduction)
    - [Disclaimer for Windows Users:](#disclaimer-for-windows-users)
    - [My Commitment:](#my-commitment)
    - [Start Here: _(Modular Emacs Installation)_](#start-here-modular-emacs-installation)
    - [Requirements:](#requirements)
        - [Emacs: V26.1+](#emacs-v261)
            - [Commands to install Emacs on various unix platforms:](#commands-to-install-emacs-on-various-unix-platforms)
        - [NODE.js:](#nodejs)
        - [VMD: _(Visual MarkDown App)_](#vmd-visual-markdown-app)
        - [MultiMarkdown: _(optional)_](#multimarkdown-optional)
        - [Pandoc: _(optional)_](#pandoc-optional)
        - [Steel Bank Common Lisp _(optional for CS research, prototyping, etc.)_](#steel-bank-common-lisp-optional-for-cs-research-prototyping-etc)
    - [Final Step (make Modular Emacs folder the default `~/.emacs.d` folder](#final-step-make-modular-emacs-folder-the-default-emacsd-folder)
    - [OK... Ready Freddy?  Lets Start up Emacs and see what happens!](#ok-ready-freddy--lets-start-up-emacs-and-see-what-happens)
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

**Welcome to Harmonic Alchemy Modular Emacs - _V1.0.3 - Q2-2019_**

> **Note:** Currently editing this doc... it's good down to Start Here but after that many changes are going on... This note will dissapear once this doc is all updated... Thanks!

This project has been a long time coming.  Its history goes back a few years ago when I went searching for some better ways to configure my then pretty basic Emacs configuration of over 10 or so years!  I cloned a few Emacs repos on Github to try different things out for a while.  I liked some things and tried to get rid of other things later.  

Management became confusing after a while.  I ended up with lots of questionable elisp code, much of which were things possibly no longer needed!  Lets face it... I was not keeping good logs... I was being a cowgirl and got into trouble!  It was my fault.  I learned a lot in the process though...  

After a couple years of **adding**, **removing**, **configuring**, **re-configuring** it started feeling like I was painting myself into a tangled corner like getting lost in **_"a maze of twisty little passages all alike!"_**  So I decided to start over from scratch and modularize everything with the purpose of preventing tangled messes like this from happening within your own Emacs setups! _(and mine too_ `%^)`

**Modular Emacs** is more than just Emacs with configurable modules however... It is also designed to be the centerpiece **_(command central)_** of a larger **DevOps** / **PubOps** **Personal IDE** workstation or laptop.  I am a devOps engineer, computer scientist, composer/musician, sound designer, architect, writer/publisher.  I wear a lot of hats! My Emacs needs to wear a lot of hats as well!  The central focus of **Modular Emacs** is to build empowering features into **Emacs** utilizing a modular framework that facilitates all the above without becoming an over complicated mess to manage! **_"good luck with that"_**  :octocat:

> **NOTE:** Earlier versions of this project called out some things in the **_Requirements_** section as **_optional_** to give interested users the option to install only the basics...  However this is not the purpose of **Modular Emacs**... **_It is not basic at all..._**   

> **Modular Emacs** was designed to be easy to maintain and configure through the management of independent modules and in that sense Modular Emacs hopefully forms a basic sensible framework that is easy to manage and flexible enough to adjust to your work-style, programming-style, and writing-style...  

> However, Your Modular Emacs experience will be much reduced without any of the extra features afforded by the external requirements...   It will feel crippled when some expected action results in "not defined" messages... Stripped of all the external helper apps etc, Modular Emacs may not provide any advantage over many of the other simpler Emacs projects on GitHub...

Therefore, if all you need is vanilla Emacs, _(with some of the more basic options thrown in)_, you don't need Modular Emacs at all.  Here is a simpler _"sensible"_ Emacs Config: **[hrs/sensible-defaults.el](https://github.com/hrs/sensible-defaults.el)**  _(which claims to also be modular - I have not tried it.  I only briefly read through the README.  It looks like a good alternate option to try)_   

For those of you who feel this may be exactly what you were looking for... By all means!  Dive in and try Modular Emacs...  There are some external requirements... It may take a while... OK?  Great... Lets go...  Eventually I will script this up so you can just enter this at the command prompt: **`./.emacs.d/install.sh`** and be done with it...  That script will be installing a lot of things though... I have to work all that out as a **non-interactive** process.  It should auto install everything from that single running shell script...  If you have to answer questions and make decisions... Better that you execute manually instead, and read the docs first...  Right?  

And so for now... We will learn this together... Please give feedback, ask questions!  Thanks ;-)

## Note for Windows Users:

I have no idea whether this project will work on Windows and I have no experience using Emacs on Windows or Cygwin...  If you use Emacs on Windows, you could help the Windows user community immensely by testing Modular Emacs within your MS Windows environment!  

If you find glitches please let me know... I need your knowledge for that part...  I will give you credit for any Windows solutions you provide.  I had some problems with _(unwanted)_ scroll bars showing up on new frames _(after initial frame)_ and read about others having this problem on Windows... I found a simple _(best practice)_ for Mac OS and Linux, but don't know if it will work on Windows...  

I added some _(commented out)_ code to: `.../lisp/modules/06-interface.el` after reading some things on Stack Exchange about problems with scroll bars on Windows...  You could un-comment this code and try it if you are having the same problems...  Let me know if that works...  Thanks!  

## My Commitment:

> **_"Always do what is right. It will gratify half of mankind and astound the other."_** - Mark Twain  

I am committed to making this **process/journey** as painless as possible for you as I have found many other Emacs setups to be way to complicated to make it easy for adoption into my workflow, and you are probably experiencing the same!  On the other end of the spectrum... Installing simple Emacs alone is not enough to get you started (IMHO)...  

If you end up liking **Harmonic Alchemy Modular Emacs**, please drop me a note to let me know! Please do ask questions, etc.  Thanks!  I have been using Emacs for over 25 years but I never shared any of that Emacs knowledge with anyone before now... :heart_decoration:  

Therefore: **Here is Modular Emacs!** I hope my long years of **devOps** experience will turn out a nice jewel for new comers to try out and succeed with!  **Good Luck and God Speed!**  

## Start Here: _(Modular Emacs Installation)_  
## Requirements:  
### Emacs: V26.1+  

Many of the default packages installed with this project will not work with previous versions of Emacs. Also the security of older Emacs and packages is Horrendous! Recently, some of the older packages were removed from the updated MELPA and the entire site is TLS enabled now!  These new features are vital for being reasonably safe going forward from 2018 onward, so I decided not to support lesser versions of Emacs...  

> **Warning:** If you have a really old version of Emacs currently installed you should backup all emacs related files: _(i.e.,_ `.emacs` `.emacs.d`_)_ located in your $HOME directory.  Simply move them into a temporary directory of your choice before upgrading below...  If you have saved key commands etc., you can get them back later as long as the modules they invoke have also been updated and work with Emacs V26.1...  

#### Commands to install Emacs on various unix platforms:  

_(Choose your Flavor:)_  

[Fedora 27-29]:~$ `sudo dnf install emacs`  

[Debian9 Stretch]:~$ `sudo apt install emacs` _**Note:** the Debian Package Repo (and all mirrors) call this package version: `46.1` which must obviously be a typo unless they time traveled into the future! lol - that may be fixed (with a new signed release) by the time you read this :octocat:_  

[Ubuntu 18.04 LTS]:~$ `sudo apt install emacs`  _(don't install any of the other listed emacs packages as older versions are also supported by Ubuntu - If you already have them installed issue: $> `sudo apt purge` all of the older versions first)_  

[Mac OS]:~$ `brew install emacs` _(Install from Homebrew)_  

**Free BSD - OpenBSD]** Install the most recent prebuilt binary package of Emacs:   _(must be up to v26.1 by now - 2019-May)_

[BSD]:~$ `pkg_add -r emacs`

### NODE.js:

**NODE** is a very popular open-source, cross-platform JavaScript run-time environment that executes JavaScript code outside of a browser. This allows developers to create web-apps and run them anywhere...  Not just within a browser... and not just on a remote server... They can run in your local environment as well and perform nifty tasks for you!  Quick prototype like development as well.. No wonder it's so popular!  Node.js is bringing the AI and Machine Learning Lab to inexperienced programmers!  That's way cool!  A Gateway language to Lisp Addiction?  Maybe Python first. ;-)

The easiest way to install NODE.js _(as of this writing May 2019)_ is to install the **NODE Version Manager (NVM)** by running the following command within your $HOME directory:

$> `curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.34.0/install.sh | bash`

> **Note:** The same command above can also be used later to **Update NVM**... You can always find the latest version of this script (and command) within the README file located in: [NVM's Github Repository](https://github.com/nvm-sh/nvm)...  


### VMD: _(Visual MarkDown App)_

This is a stand alone `node.js` **Electron Web App** that you need to install in your `$HOME` directory for this Emacs setup to use.  **VMD** allows you to visualize the results of your edits of **markdown files:** `.md` & **ORG files:** `.org` **instantly! _(in  real time, while you edit the markdown text within your Emacs buffer, all without the need for a full web browser)..._**     
Your rendered markdown will appear as it would look on Github!  This is a **_much smarter tool for writing/publishing_** because it combines the ease of: `WYSIWYG` with the power of `text-editors`.  That is the best of both worlds with no compromise!     

> **Note:** Not having **VMD** installed will not break Modular Emacs, however you will of course not be able to use Emacs with VMD, some Modular Emacs defined keys will not work, and may even cause an error within Emacs if you try those keys...  You will also see compile time errors when you first start up Modular Emacs.  After startup, Modular Emacs should work fine without VMD...

The best way to install **VMD** is locally within your home directory _(not system wide via your distro's package manager, or app store)..._    
The best way to install locally is to install it as a `Node.js` package...      
To do that you must first install **Node Version Manager** `NVM` using a script from: [`https://github.com/nvm-sh/nvm`](https://github.com/nvm-sh/nvm).







      
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

## Final Step (make Modular Emacs folder the default `~/.emacs.d` folder
## OK... Ready Freddy?  Lets Start up Emacs and see what happens!

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




