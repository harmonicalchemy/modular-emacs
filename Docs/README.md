---
file: README.md
author: Alisha Awen
created: 2019-010-20
updated: 2020-007-16
tags: Emacs, 2024, apps-tools, SysAdmin, HA-ModEmacs, how-to, README 
---

![Banner](./Docs/media/Modular-Emacs-Github-Banner-v3.png)

# Version 3.6 (point release) - README

**[\[Table Of Contents\]](#table-of-contents)**

**Note:** _Skip to **[Start Here:](#start-here)** if you just need to get this going now._ :trollface:

## Introduction:

**_Welcome to Harmonic Alchemy Modular Emacs_** - V3.6 [Q2 2024] _(point release)_.

**Modular Emacs** is more than just Emacs with configurable modules... It is also designed to be the centerpiece _(command central)_ of a larger _personalized, extensible_: **Dev♺Ops** / **Pub✎Ops** / **Multi🎥Media** workstation **IDE** capable of running on older but reasonably powerful **laptops/desktops** _(that would otherwise be considered legacy by most other standards)_.  

I am a systems architect, dev♺Ops engineer, computer scientist, composer/musician, sound designer, writer/publisher.  I wear a lot of hats! My Emacs needs to wear a lot of hats as well!  The central focus of **Modular Emacs** is to build empowering features into **Emacs** utilizing a modular framework that facilitates all the above without becoming an over complicated mess to manage! **_"good luck with that"_**  :octocat:

> **Note:** **_Current Status_** of this project as well as past **_Release Notes_** are documented **[HERE](./Docs/ModE-Current-Status.md)**

### History & Purpose:

The history of **Harmonic Alchemy Modular Emacs** is _"a confusing journey through many little twisty passages, all alike"_... **[Everything is documented here now](./Docs/ModE-History.md)**...  Read it at your leasure.  It provides some perspective, insight, and purpose behind how and why things are done here in the present time.

## Try Harmonic Alchemy Modular Emacs:

For those of you who feel **[Harmonic Alchemy Modular Emacs](https://github.com/harmonicalchemy/modular-emacs)** may be exactly what you were looking for: **_By all means... Lets Dive in!_**

There are some external requirements... It may take a while... If that's OK with you, Great!... Lets go...  Eventually I will script this up so you can just enter this at the command prompt: **`./install.sh`** and be done with it... Keep your fingers crossed on that but don't get your hopes up... I am a busy girl!!! :octocat:  That script will have to install a lot of things! It will have to check a lot of system states!  I have to work all that out as a **non-interactive** process.  It should install everything _(transparently non-interactively)_ from that single running shell script...  If you have to answer questions and make decisions... Better that you execute manually instead, and read the docs first...  Right?

**[\[Table of Contents\]](#table-of-contents)**

### Microsoft Windows Support

> **Note:** Hello MS Windows Users... I admit to having no experience at getting a nice Emacs GUI environment running nicely on MS Windows... **Harmonic Alchemy Modular Emacs** expects to be installed on a unix machine, or unix friendly machine contaning standard unix apps and utilities or proper hooks into such things... Many of these may not be implemented within the canonical Windows OS model and I have no experience sorting that out...  It is hard enough to get Mac OS to behave along side GNU Linux or other free software and open source standards! I do not wish to abandon MS Windows users however.  I dedicated a document within Harmonic Alchemy Modular Emacs to record any information needed to help get Modular Emacs running on MS Windows.  Please consult:  **[Note for MS Windows Users](./Docs/ModE-Note-2-MS-Windows-Users.md)** for more information...  I am looking for a good MS Windows Sys Admin type to jump in here and help... Where are you? Please open an issue with intro to yourself for me to review... Thanks!

For the rest of us... For now... Let's learn this together... Please give feedback, open issues, ask questions!  Thanks ;-)

### My Commitment:

> **_"Always do what is right. It will gratify half of mankind and astound the other."_** - Mark Twain  

One of the main reasons created this project was to empower lots of creative folks out there who have more brains than money.  This project is 100% open-source and Free to download, install, and use independently, _(both as in "Free Beer" and also as in having "Freedom of Choice and Control" over your Computing Infrastructures)_.

I have been testing this project to see if it can successfully run on older legacy hardware as well.  My aim is to make it possible for anyone, no matter what their economic or social status is, to be empowered to use this... To give you a voice, to give you more control over your digital life operations, and provide you with a portable/inexpensive way to publish your ideas, stories, projects for the rest of the world to see. All under your independent and private control (no middle men dictating rules)..

I am committed to making this **process/journey** as painless as possible for you as I have found many other Emacs setups to be way to complicated to make it easy for adoption into my workflow, and you are probably experiencing the same!  On the other end of the spectrum... Installing basic Emacs alone is not enough to get you started _(IMHO)_...  I set it up that way to get you up and running with a simple setup first... You should quickly outgrow that and certainly want to enable the extra features!  

Therefore I hope to make that process reasonably easy to do!  The whole purpose of this project is to be able to easily grow and shrink features as needed in any combination thereof... Without _"getting lost in a tangled mess of twisty little passages all alike!"_

If you end up liking **Harmonic Alchemy Modular Emacs**, please drop me a note to let me know! Please do ask questions, open issues, etc.  Thanks!  I have been using Emacs since around 1986/87 _(OMG that long! off-and-on with gaps in between ;-)_ but I never shared any of that Emacs knowledge with anyone before now... :heart_decoration:  

Therefore: **Here is Modular Emacs!** I hope my long years of **devOps** experience will turn out a nice jewel for new comers to try out and succeed with!  **Good Luck and God Speed!**  

**_Don't Panic...  Organize!_** _(Note to self: Put your Cartoon SVG here)_

**[\[Table of Contents\]](#table-of-contents)**

# Start Here: 

**_(Modular Emacs Default Installation)_**

To install Harmonic Alchemy Modular Emacs for the first time, follow this guide _(in a top down fashion)_... Do not skip to any of the other _(optional)_ install guides until you have the basic default configuration of Modular Emacs installed and running _(without errors)_ in your home directory... 

> **Note:** It is entirely possible that Modular Emacs will still run reasonably without installing some of the basic requirements listed below as well.  For example, if you get stuck trying to install **`NODE`**, that will only cripple your ability to edit your Markdown pages using **VMD** _(Visual Markdown Display - a nice thing to have for WYSIWYG editing)_ but it won't stop you from getting a basic Modular Emacs successfully installed.

> If you get stuck, go ahead and keep going to see if you can get Modular Emacs up and running anyway... You can dig into fixing the details later after you got your basic environment all set up...

## Requirements:  

### Clone This Repository:

Clone this repository into your **`$HOME`** directory as:  **`me.init.d`**.   The `me.` prefix _(for Modular Emacs)_ is put there so that it will not overwrite your existing ~/.emacs.d directory _(if you already use Emacs, which you will still need operational)_.  We will rename this folder later after all other requirements are complete...

Issue the following command within your default **terminal** application:

```yaml
$>  git clone https://github.com/harmonicalchemy/modular-emacs.git ~/me.emacs.d
```

After the above is done, leave things here as they are and go do the rest of the steps below... We will come back to this later when we are just about ready to launch **Harmonic Alchemy - Modular Emacs** for the first time...

### Install Emacs: V26.1+  

Many of the default packages installed with this project will not work with older versions of Emacs. Also the security older Emacs packages is horrendous! Recently, some of the older packages were removed from the updated MELPA and moved to other less-secure repositories. The entire MELPA site is TLS enabled now but these other repos are not very secure...  Updated features _(acquired securely)_ are vital for being reasonably safe going forward from 2020 onward...  Therefore, I decided not to support versions of Emacs prior to V26 and prefer to only support Emacs V26.1 and up...  

> **Warning:** If you have a really old version of Emacs currently installed, it may break after you update Emacs below... Therefore before installing a brand new version of Emacs it may be a wise idea to save your current emacs environment, _(e.g.,_ `~/.emacs` _,_ `~/.emacs.d`_, etc.)_. as well as a copy of your old **Emacs** Application... This is for a safety net, Do NOT delete your existing setup... You still need your existing Emacs working wile you are getting all the requirements below together for Modular Emacs... 

> Simply Copy/Clone your entire: **`~/.emacs.d`** directory _(and/or_ **`~/.emacs`** _file)_, as well as a copy of your old **Emacs** application into a temporary directory of your choice before upgrading below... _(or compress and make a `.zip` or `.gz` archive file)_  

> This will be your safety net for coming back later if you decide you are not ready to upgrade. If anything goes wrong, you will be able to reverse everythig back no problem!

Once your house is in order as above, follow this link to: **[Install and/or Build/Install Emacs 26+ on Your Platform](./Docs/ModE-Install-Emacs.md)**

**[\[Table of Contents\]](#table-of-contents)**


### `NODE.js` with `NVM` & `NPM`:

**_The following instructions are for ALL platforms..._**

> **Note: _(Debian, Ubuntu, & Mac Users)_** If you were unsuccessful attempting to build Emacs from Source _(above)_ you can skip this section and wait to come back here later to install `NODE` as well.  The features NODE provides may not be accessable from Emacs if it was installed from the package managers on your platforms _(exception MacPorts)_. Therefore you can wait to install this later after you have built your own custom Emacs first... enuff said...

Follow this link to: **[Install NODE.js with NVM and NPM](./Docs/ModE-Install-NODE.md)** and then come back here to continue below...

### VMD: (Visual MarkDown App)

**_The following instructions are for ALL platforms..._**

This is a stand alone **NODE.js - Electron based Web App** that you need to install _(globally)_ from your `$HOME` directory for Harmonic Alchemy Modular Emacs to work well.  **VMD** allows you to visualize the results of your edits of **markdown (`.md`)** & **ORG (`.org`)** files **_Instantly... LIVE... in real time, displaying the results of your edits and cursor movements as you make them within your Emacs markdown buffer!!!_**  This is not a two step _wait-and-see_ process like normal previews are in most other Markdown Editors I have seen and used!     

Not only is it always **WYSIWYG** _(on the side)_, Your rendered markdown will appear as it would smartly look on Github! :octocat:  This is a **_much smarter tool for writing/publishing_** _(in my book - pun intended)_ because it combines the ease of: `WYSIWYG` with the power of `text-editors`.  That is the best of both worlds with no compromise!     

> **Note:** Not having **VMD** installed will not break Modular Emacs for doing other things, however you will of course not be able to use Emacs with VMD, some Modular Emacs defined keys will not work either.  If you accidentally type them you may see an error or end up in the debugger!  You will most likely see compile time errors upon first starting up the default version of Modular Emacs as well... No Problem...  Come back later and fix things by getting all requirements done...  _(talking to you Debian, Ubuntu, and Mac OS folks in particular...)_

With Node.js, NVM, and NPM already installed _(from above)_ you can easily install VMD with the following command: _(from your home directory):_

```bash
    $_  cd ~
    $_  npm install -g vmd
```

That's it... What??? Were you thinking you had to do a huge devOps operation again? Nope... not this time little ninja grasshopper... You already did your work when you installed `NODE` `NVM` and `NPM` above... Whew! :octocat:

**[\[Table of Contents\]](#table-of-contents)**

### MultiMarkdown:  

MultiMarkdown is a utility which is used to export from Markdown to other formats beyond HTML alone.  Multimarkdown is completely stand alone.. Emacs does not require `multimarkdown` to be installed externally in order to format **_(pretty-print)_** and/or **_fontify_** the text in your markdown edit buffers.  There's an Emacs mode for Multimarkdown called **mmd-mode** which you will be installing further down within this document that takes care of fontifying your Emacs multimarkdown document buffers.  

Some of the publishing-format conversion functionality of **Multimarkdown** overlaps with **Pandoc** as well. If you experience problems installing one of these, you will still have the other one to use.

It doesn't hurt to have both of these technologies installed on your machine for the above reasons...  You will most likely need one or the other at some point to publish something somewhere in some fancy format like **LaTeX** or an **eBook**...

Follow this link to: **[Install MultiMarkdown - All Platforms](./Docs/ModE-Install-MultiMarkdown.md)**

**[\[Table of Contents\]](#table-of-contents)**

### Pandoc: 

As with MultiMarkdown, Emacs will run fine without Pandoc installed on your machine, However Harmonic Alchemy Modular Emacs will not be able to export to all those fancy document formats without Pandoc _(or Multimarkdown)_ installed...  You will consider this a broken feature once you have written something _(within Emacs)_ that needs to be exported to **LaTeX** or some fancy eBook format.  **_Read the docs for both Multimarkdown and Pandoc_** to decide how to use them.  More instructions for installing and using Pandoc can be found on their official website here: <https://pandoc.org/installing.html> The Pandoc.org website has most everything you will need all in one place!

Follow this link to: **[Install Pandoc - All Platforms](./Docs/ModE-Install-Pandoc.md)**

### Fonts:

**Harmonic Alchemy Modular Emacs** requires a few fonts installed on your system to get Fancy Org-Mode, Fountain Screenplay, and coding frames/windows/mode-lines, etc. looking as good and "readable" as they possibly can.  Other fonts will be required by **LaTeX** _(for exporting to external document types)_.  That process will be covered in a seporate doc.

Follow this link to: **[Install Modular Emacs Required Fonts - All Platforms](./Docs/ModE-Install-Fonts.md)** Afterwards, you will see some nice looking "writers" fonts in Org-Mode as well as while editing Fountain drama scripts! :octocat:

### Python and Pip:

Some of the newer modules to be enabled within **Harmonic Alchemy Modular Emacs** will require Python packages to be installed... This is especially true of **_Emacs MultiMedia System_ (EMMS)** enabled within optional module: **[`11-games-pkg-conf.el`](./lisp/modules/11-games-pkg-conf.el)**

**[Follow this guide](./Docs/ModE-Install-Python-PIP.md)** to get Python & PIP set up within your local environment so you will be ready when python packages are needed to be installed now, or later...

### Extra for Experts - LaTeX

No I did not forget LaTeX...  This is a powerful monster! It needs its own monster doc to install and configure as well... You need it to work with **_Pandoc_**, **_Multimarkdown_**, and later **_org-mode_** when I have that also integrated into Modular Emacs...  LaTeX provides extended features to produce PDFs, and eBook formats... Emacs can be integrated _(especially `org-mode`)_ to be able to export to these fancy formats using LaTeX...

This is Great News if you would like to make **Emacs** work as well as _(or better than)_ **Scrivener**...  So get on it already OK? Follow this Guide to get started: **[Install LaTeX Publishing Environment](./Docs/ModE-Install-LaTeX-pubOps-Env.md)**...

**[\[Table of Contents\]](#table-of-contents)**

### Graphviz:

Harmonic Alchemy Modular Emacs comes integrated with the popular **Graphviz utility** which allows the creation of nice graphs, flowcharts, data diagrams, etc. using a powerful scripting language called **dot**...  The Emacs mode for Graphviz is: **`graphviz-dot-mode`**

Modular Emacs invokes: **`graphviz-dot-mode`** when you visit files ending in either: **`.dot`** or **`.gv`**

When you are visiting a **`.dot`** file, you can compile it with **`C-c C-C`** which will produce a **`.svg`** file along side...  By default, Modular Emacs produces **SVG vector files** _(instead of `.png` files)_... which is my preference because if you add one to a markdown file and then view it using **VMD-Mode** you can expand the resulting rendered chart or graph full screen and it will still look very sharp! Not to mention you can edit the resulting SVG file within **inkscape** to add things to it that Graphviz cannot...  Don't underestimate the power of **Graphviz** though!  **[Read the docs!](https://graphviz.org/documentation/)**  

Follow this link to: **[Install Graphviz - All Platforms](./Docs/ModE-Install-Graphviz.md)**

**[\[Table of Contents\]](#table-of-contents)**


## Get Ready to Start up Modular Emacs for the first time!
### First Some Initial House Keeping: _before we move in_  
#### Create an empty file named `custom.el`:

This file keeps **Emacs Internal "Customize"** saved settings separate from **Modular Emacs constructs**...and outside of your .init.el file! _(so you won't have to worry about tiptoeing over it)_ Emacs will use this file _(initially empty)_ behind the scenes to save your Emacs internal "customizations"...  Git ignores this file.  _(Which is why we have to create it here now)_  You will not need to mess with this file after creating it, _(unless you want to empty it out and start over when Emacs "customized" things get out of hand)_...  

```bash
$_  touch ~/me.emacs.d/custom.el  
```

#### Clone `mmd-mode.git` into `my-modules`:

You need this for Emacs to work with Multimarkdown well... Even if you did not install Multimarkdown on your system, this mode will provide some of the extra Github and extra Multimarkdown meta-data features that are nice to already have in place once you do start using Multimarkdown as part of your publishing stack...

```bash
$_  cd ~/me.emacs.d/lisp/my-modules
$_  git clone https://github.com/harmonicalchemy/mmd-mode.git  
```


#### Copy/Clone `me.init.el` to: `init.el`:

**`me.init.el`** is a template for reference only...  I did it this way, _(adding `init.el` to `.gitignore`)_ for the purpose of providing more flexible ways to manage your local install of Modular Emacs...  me.init.el will stay in sync with the remote origin while your local clone: **`init.el`** runs the show _(with any changes you might add)_ without triggering git to complain about new un-tracked files etc...

```bash
$_  cp ~/me.emacs.d/me.init.el ~/me.emacs.d/init.el
```

Later... You may want to edit your fresh new init.d and change the Banner Message to be more specific to your installation...  Also, later we will discuss different strategies of management that will hopefully allow you to build your own setup concurrently and in harmony with Modular Emacs, and be able to easily share any extra features/modules of your own that you may wish to bring to the rest of the Modular Emacs user community!

### Final Step - Make Modular Emacs folder the default `~/.emacs.d` folder:  

At the very beginning of this guide, you were advised to copy your existing Emacs setup first.  I hope you did that because now you are going to over-write your old installed Emacs configuration with Harmonic Alchemy Modular Emacs!  

Therefore if you did not backup, FIRST make sure to rename _(save)_ your existing:  **`~/.emacs.d`** to: **`~/save.emacs.d`** _(if you did not do this in the beginning)_  You may also have a `.emacs` init file outside of the `.emacs.d` folder that also needs to be renamed!    **This is very important!  Becase the next steps will overwrite them if you did not change their names!!!**

Now you are ready to **rename `~/me.emacs.d`** to: **`~/.emacs.d`**  

```bash
$_  mv ~/me.emacs.d ~/.emacs.d
```

_(This is now your new **Modular Emacs Local Repository** which is also now your default_ **`.emacs.d`** _home directory!)_  

One last sanity check... Make sure you no longer have a `~/.emacs` file still in your home directory...  You should now only have a: `~/.emacs.d/` directory... OK? Great!

## Ready Set Go!  Start Up Modular Emacs:

**_Start up GNU Emacs from the menu or command line and hold your breath!_**

After a lot of super computing lisp number crunching flashing away in your mini buffer _(wait wait wait... the first time can take up to a minute! After that no more than 7 or 8 seconds at most...)_ Then **Bam!** You should now see your new emacs pop up with the **Welcome to Harmonic Alchemy Modular Emacs!** scratch buffer in a frame with a pre-determined row/column initial size! 

**No?** Broken? Oh My! What a freaking let down!  I feel your pain!  OK... Don't worry if all you see are only numerous compiler warnings... _(those are hard to avoid upon first startup with all the new packages compiling etc. Not much you can do about that... The developers who made the packages need to clean that up, you could help them though.  You can safely kill that message buffer and move on...)_

> **Note:** I am currently chasing an issue with some functions _(not mine)_ calling the old depreciated **`cl`** _(common lisp library)_.  These will be replaced with calls to the new **`cl-lib`** replacement...  When that happens you will no longer see the warning: **_"cl lib is depreciated"_** at startup...  There are several other warnings like this I will eventually chase down as well...

If you got an error that halted loads and you have an incomplete emacs startup init screen, try to retrace your steps or try running emacs with `--debug-init` turned on...  Its probably something dumb and easy to fix...  _(This happens to us all the first few times, and comes back again later as well most of the time.  Other errors only come up in a blue moon, and then mysterously go away and hide before you can catch them! DOH! Log files are a hacker's best friend... :trollface:)_

**Yes?** It worked!  **Yay!** What a fantastic feeling the first time eh?

> **Note1:** Fetching/pulling new changes from the master `modular-emacs` GitHub repository to your local `~/.emacs.d/` directory will automatically be reflected within your emacs configurations... No need to copy any more files... But you may be surprised to see some new feature or something working differently.  If that bothers you, you may wish to keep your changes separate from the remote master branch.  Therefore create your own local `test` branch _(or call it what you like)_ and maintain your local changes in parallel... Change your local branch's .gitignore to accommodate your needs...

> **Note2:** The remote `modular-emacs` Github repository also maintains a `develop` branch where new ideas and features are tried out before folding them into the master branch which maintains the Modular Emacs stable release.  You could also create a local branch that tracks origin:develop if you would like to participate in any new things I am trying out before final release... Earlier point versions of Modular Emacs were first staged and tested within the develop branch.  Version 3.x is tested on the develop branch currengly. _(if not already merged into master by now)_...  Any time a final release of new features is ready, the develop branch will be merged back into master branch, tagged as a new point release _(or major release when a lot of new features have been added to warrant it)_... maybe not this time eh?  LOL

> **Note3:** I have created a new branch called: **`xah-fly-keys`** where the optional module: `12-Xah-Fly-Keys.el` has been enabled by default.  You don't have to make any modifications in order to try this otherwize optional feature when you are on this branch.  Try it out! It's easy to switch back and forth now...

## Usage:

**Harmonic Alchemy Modular Emacs'** git repository is configured to ignore your local `~/.emacs.d/init.el`...  The file that **_is_** kept in revision is `~/.emacs.d/me.init.el` which you do not use directly.  Instead, your standard emacs config file: `~/.emacs.d/init.el` starts out as a copy of `~/.emacs.d/me.init.el` allowing you to try out new things on the fly without upsetting the state of the git repo...

Feel free to use `~/.emacs.d/init.el` to try out experimental customizations or tests. If something goes wrong you can quickly refresh this file from the original `~/.emacs.d/me.init.el`.  Realize however if you do that any changes or prior customizations you made will be lost!  So recover those first before overwriting with `me.init.el`!

You can also use the Modular Emacs \*scratch\* buffer to paste experimental lisp code to try out before making it permanent as a stand alone `.el` emacs lisp file, _(as the welcome banner suggests ;-)_.  Evaluate the entire \*scratch\* buffer without needing to remove the banner message at the top as that message is one huge lisp comment...  If something horrible happens, no worries... Simply restart emacs... Your mess-up will be gone and you will be back to where you were before you or that elisp test code goofed up...

If your customization proves stable, and you like it, you could then save your scratch buffer (as is) to a new file, for example:

```bash
$_  ~/.emacs.d/lisp/my-modules/xx-my-new-module.el
```

In all cases you would also be wise to also create and checkout your own private local `test` branch _(call it what you wish)_ and keep all your custom changes in there separate from the `master` or `origin:develop` branch...

### Blackboard Color Theme:  

Modular Emacs comes with my new customized version of the **Blackboard color theme** _(patterned after the famous Textmate Blackboard Theme)_ which I like for the _pedagogic essence_ it inspires... I have also tweaked it to look good under my **Fancy Org Mode** colors and faces.  If you would like to add more custom themes or a different theme than `blackboard.el`, you can replace it or add additional themes into your local branch's: `~/.emacs.d/lisp/themes` directory and they will also will work with this setup by choosing `M-x load-theme` or changing the last line within `~/.emacs.d/lisp/modules/06-interface.el` to: `(load-theme 'your-chosen-theme-name t)`

> **Note:** _Obviously if you add more themes to your **Modular Emacs** **themes** directory you will be adding new un-tracked files to your cloned git repository!  Make sure you have checked out your own local branch before adding new themes or doing any customization outside the init.el file or the `my-modules` directory.  Then you will have proper code management of your own local custom changes tracked seporately under your own code revision schemes in place ready to merge any changes in from the remote Modular Emacs branches any time you feel that may be necessary...

## Default Packages & Features (built in):

> **Note:** This section almost always needs an update! Many new features keep being added, removed, or changed as this beast evolves!  Documentation for them is still an on-going process...  Please be patient... Or open an issue... Our conversation may end up being part of this doc. :octocat:

**Default Emacs welcome screen replaced with custom greeting:** Prints current emacs configuration, and date... With a famous Mark Twain quote _(My Favorite author. This quote may change from time to time with new updates)_.  

Use Scratch buffer to evaluate snippets of `lisp` without having to remove the welcome text... _(which are lisp comments)_

### Pre-configured packages - Comprehensive list:

#### Misc. Packages:

- [powerline](https://github.com/milkypostman/powerline) Decorate `mode-line` & make it More Informative.  You can customize options and use themes etc... This theme is disabled by default.  I am back to the Emacs default mode line and I customize that instead.  I did not remove the code to enable powerline though.  If you like powerline, the code for enabling it is located within **`02-package-conf.el`** Read the comments in that file to find it...


- [auto-complete](https://github.com/auto-complete/auto-complete) Easy Text Editing with suggested word completion.   
**Update 2020:** I believe this may be a bit buggy at the moment... I have neglected it and some update or debugging may be needed... Currently when entering code if I enter a function name and completion pops up.. If I want to keep typing _(not accepting the default, or maybe what I want is different than what it thinks)_... It fights me...  I have to fool it by typing some junk and then pull off a text switch game.... LOL  I will fix this later...  If you hate it, you can comment it out in 02-package-conf.el I may end up doing that myself... `%^)` Maybe I need to learn how to use this feature better and or tweak it to work better... lol

#### Core Emacs System Related Packages:

- [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell) Get environment variables such as $PATH from the shell.  btw, this can be a tricky thing to do in emacs!  I have struggled with this on Mac OS for a while!  You may find you have to start Emacs from the terminal to get all your environment vars into Emacs!  More about that later.. A continuing saga! **Update 2020:** This has been working fine on my iMac since completely replacing Homebrew with MacPorts.  IMHO _(biased by a host of irritant issues using Homebrew over the years)_  MacPorts is a much better package manager all around...  But not the popular one...  Oh Well... %^) Also, since building my own Emacs from GNU source tree on my iMac, configured just how I like it, I have had no more nagging MacOS related issues... And I can have the latest Emacs the second it is released _(one build cycle away, or choose to go back to my archived app if they introduce some bug or someting)_! :octocat:

#### Emacs Helm & Extras:

- [helm](https://github.com/emacs-helm/helm) Helm is an Emacs incremental and narrowing framework that helps speed up operations on commands with structured command completions and more.  It is a programmable interface (API) as well. To see how HELM can speed things up try a prefix key like: `C-x` or `C-c` or `M-x` and just wait before typing any more keys... You will see all the possible key-combos currently associated with that prefix key pop up in the HELM mini-buffer!  Fantastic! No more guessing! _("Oops! What did I just do? OMG! LOL")_


- [helm-core](https://emacs-helm.github.io/helm/) Development files for Helm (API)... Installed as dependency.   


- [popup](https://github.com/emacs-helm/helm) Popup is a dependency package for helm.


- [which-key](https://github.com/justbur/emacs-which-key) Used with helm, pops up suggestions for what can follow after pressing `C-x` key... _(this is what I was talking about above in the intro paragraph)_   
**Note:** **_xah-fly-keys_** also uses helm to pop up leader key choices and extended menu...  Hit the `SPACEBAR` _(LEADER KEY)_ and wait a second for helm to pop up the list of options... _(with help feature and pagination! Vital for learning all those keys!)_
#### Tree & Menu Related Packages:

- [ztree](https://github.com/fourier/ztree) An efficient directory tree comparison utility. Invaluable for visually oriented thinkers! Integrates directly with ediff files! 

- **To invoke Ztree Diff** type: **`C-c z`** _(normal Emacs edit mode)_ or symply type  **"z"** from **Xah-Fly-Keys Command Mode**, and then choose directory "A" & "B" _(as prompted)_ to compare all files within both directories.  

- Make your screen wide to see both directories listed side by side. The file names are color coded, indicating status/differences within in two side by side buffer lists of all the files within both directories.

- This list of files can be navigated in a similar way you navigate a dired buffer...  Putting cursor on any filename and hitting RET key will open the two files within an **eDiff** session... 

- I used to use external diff/merge tools like **kdiff-3** but I find Emacs to be superior (if not quite as slick GUI looking) but that does not bother me in the slightest... The powerful user configurable functions made available within Emacs are (to my eyes) the Beauty of Form and Function in action! **_"Form follows function - that has been misunderstood. Form and function should be one, joined in a spiritual union."_** - **Frank Lloyd Wright**


- **[imenu-list](https://github.com/bmag/imenu-list)** Right panel Menu and Variable list.  You can toggle an imenu list right panel window to view variables, functions, headings, etc. **iMenu List** uses **iMenu** and displays the listing in a narrow window on the left side...

> **Note:** For some reason `imenu-list` is not working currently.  There seems to be a problem with: `window--display-buffer` returning nil with the classic: `wrong number of arguments` error...  I am currently troubleshooting this...  For now simply use **`imenu`** by typing **`C-c '`**, or by typing the single quote character: **'** by itself _(when you are in **Xah-Fly-Keys** command mode)_... When I have **iMenu-list** working again those keybindings will call it instead... It provides a nice side bar that you can navigate. The side bar stays there for you to use over and over again until you toggle it back off again... This is much better than using iMenu alone which pops up in HELM but goes away as soon as you use it...

#### Dired Related Packages:

- [dired-launch](https://github.com/thomp/dired-launch) Open files directly within `dired` using default applications. Great for quick viewing PDF or Image files with your OS default viewer app! You can use **`dired-launch-extensions-map`** to specify, for a given file extension, one or more preferred applications by simply specifying the application as a string.  _(see example below)_  

```elisp
   (setf dired-launch-extensions-map
         '(;; specify LibreOffice as the preferred application for
           ;; a Microsoft Excel file with the xslx extension
           ("xlsx" ("libreofficedev5.3"))
           ;; specify LibreOffice and Abiword as preferred applications for
           ;; an OpenDocument text file with the odt extension
           ("odt" ("libreofficedev5.3" "abiword"))))
```  

> **Note:** The above is for launching apps _(inline your emacs buffer)_ but there are many ways to skin this external app launch business... the following commands will launch a file in an external application whilst your cursor is positioned on a file in a dired buffer:  **`W`**, or **`SPC i w`** in **Xah Fly Keys** command mode... _(Xah Fly Keys must be enabled for this leader key sequence in Commnd Mode)_ but simply typing upper case W is easier IMHO.


- [neotree](https://github.com/jaypei/emacs-neotree) Spawn a directory
tree relative your current working directory location. You can toggle a
`neotree` left side panel window from any open buffer window using a single
key... Type: **`C-c d`** _(normal Emacs mode)_ or type: **`5`** _(from **Xah
Fly Keys Command mode**)_ to do the same... Within the neotree panel you
can navigate and open a dired buffer to the directory where your cursor
is by hitting RET.     
**Note:** I have been using bookmarks pretty nicely for a while now and
not using this neotree feature all that much... but it is a quick way to navigate
from your current buffer/location when you are NOT within dired... Your mileage
may vary...


#### dev§Ops, sys§Admin, info✷Sec Related Packages:

- [ssh-config-mode](https://github.com/jhgorrell/ssh-config-mode-el) A mode to edit SSH config files.


- [smart-tabs-mode](https://www.emacswiki.org/emacs/SmartTabs) Intelligent tabs to end the f---ing tabs vs spaces war already OK? Read the docs :-)


- [php-mode](https://github.com/emacs-php/php-mode) Major mode for editing PHP code.

##### More languages, devOps tools, etc. to add later:

- `digitalocean-helm`  Whoa!!! A `helm` Interface and API connector to your Digital Ocean Account! _(I need to read more about this...)_

#### Writer's \- Publishing Tools:

- [markdown-mode](https://jblevins.org/projects/markdown-mode/) Markdown capability within emacs. You cannot live without this!!!


- [markdown-mode+](https://github.com/milkypostman/markdown-mode-plus) Extras for Markdown mode. More markdown-mode magic!


- [markdown-toc](https://github.com/ardumont/markdown-toc) Generate a TOC in a markdown file. This is a simple but powerful mode to create a TOC in a well-formed markdown file. In other words: The generated TOC will be well-formed if your markdown file is also well formed. ;-)    
**Note:** I like to place my toc at the bottom, and provide a simple link named: `table-of-contents` or `index` and put that link at the top, as well as strategic places down within the body of the markdown file... This README is structured in that style... It reminds me of the way we used to do a similar thing in HTML before Javascript enabled fancy CSS menus started popping up... Now we have mobile menus to contend with.  Oh My!


- [deft](https://jblevins.org/projects/deft/) Major mode for quickly browsing, filtering, and editing directories of plain text notes. Created by Jason Blevins _(the same statistics economics professor at Ohio State who also created Emacs Markdown Mode)_ This guy is wicked clever!  Looks like he has some nice algorithms for studying the cryptocurrency and smart contracts world as well!  Invoke Deft Mode with custom Xah Fly Keys Command Mode key: **`0`**


- [pandoc-mode](https://joostkremers.github.io/pandoc-mode/) `pandoc-mode` is an Emacs mode for interacting with Pandoc. Pandoc is a program _(plus libraries)_ created by John MacFarlane that can convert a text written in one markup language into another markup language. This is one of the tools that will help me get off proprietary Scrivener on **Mac OS** and finally be able to use Emacs for all my **Pub§Ops**!


- [fountain-mode](https://github.com/rnkn/fountain-mode) For **writers, screenwriters** A markdown mode for writing screenplays! Fantastic!!! Fountain together with pandoc, allows you to do many conversions for screenplays to industry formats like **Final Draft** etc. but the best part is the text highlighting and colors make it very easy to read/rehearse dialog and cues right from an Emacs buffer!  Writing scripts in this mode is as natural as typing `INT` _(your line instantly turns into a scene heading!)_ Try it! Just start typing your script and find out what happens!  Modular Emacs Fountain Mode has been customized to use a nice looking Typewriter style mono font! It also opens up in olivetti mode _(next item below)_ automatically setting the line width to 100 chars...      
**Update 2020:** I am currently writing some docs for using fountain-mode within Emacs, as well as provide some example screenplay scripts and instructions on how to publish them to PDF using a tool called Afterwriting... Stay tuned for that...


- [olivetti](https://github.com/rnkn/olivetti) Adjust margins with word wrap. _(great with fountain mode!)_ Perfect for distraction free writing! I also enable this within `rmoo` _(if you have the **games module** enabled)..._  Toggle it on and off within Xah Fly Keys Command Mode by hitting the backtick character: **\`** key.  Adjust the margin width within Xah Fly Keys Command Mode by hitting: `SPACEBAR [` or: `SPACEBAR ]` to shrink or grow respectively.


- [vmd-mode](https://github.com/blak3mill3r/vmd-mode) Fast Github-flavored Markdown previews synchronized with changes to an emacs buffer (no need to save).  Renders org files _(and normal markdown)_ in an external VMD App that automatically shows changes in real-time as you type! _(You need to install this external app separately, and make sure Emacs knows where to find it.  See: `exec-path-from-shell` below)_  Toggle `vmd-mode` on and off within Xah Fly Keys Command Mode by hitting leader key sequence:  `SPACEBAR-v`


#### Spelling:

- [flyspell-correct](https://github.com/) This package provides functionality for correcting words via custom interfaces. There are several functions for this: `flyspell-correct-wrapper`, `flyspell-correct-at-point`, `flyspell-correct-previous` & `flyspell-correct-next`.


- [helm-flyspell](https://github.com/pronobis/helm-flyspell) Helm extension for correcting words with flyspell.


- [flyspell-correct-helm](https://github.com/d12frosted/flyspell-correct) Nice helm interface for flyspell.  Place your cursor after any misspelled word and type: `C-;` to see a list of suggestions...

#### Project Management Tools:

- [org-bullets](https://github.com/emacsorphanage/org-bullets) Fancy UTF-8 bullet fonts for Org Mode... This is an old emacs package that hides all but the last single bullet.  It still works fine in Emacs 27... If there are problems, it's a pretty short lisp file and easy to maintain or be hacked to your likings. ;-)    

> **Note:** I am no longer interested in even the last single bullet showing, and actually am hiding all org bullets now. _(Now my org-mode shows **Outline Headings** in a beautiful scalable font. Headings are indented and scaled from large to small just like: `H1`, `H2`, `H3`, etc. in HTML)_...  The `org-bullets` package still remains and does not conflict with any of my extra tweaks.  However you may wish to un-hide the last bullet and show different headings with a different color etc. _(consult the code within **`09-org-mode-pkg-conf.el`** to see the disabled forms and to re-enable them if you are interested in trying them out)_.
      
- Org-Bullets works fine with the rest of my Org-Mode tweaks but the last bullet being visible clutters things up a bit in my configuration where I have all my headings of decreasing scale, _(from title, to level-8) set to **Antique White** which looks fantastic on my modified **Blackboard** theme, IMHO)_...

#### Git \- `magit` & Related:

> **Note:** I am still doing git from the command line, and have set up shortcut aliases in the Z shell to speed up the process of managing git repositories on the command line.  I am pretty profieient with that and so far, have not tried to use magit all that much in Emacs... Your mileage may very.  If you like using this package help me make it work well here... (I don't know if it works all that well yet because I have not been using it... 

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

> **NOTE:** These keep changing _(as I use them and realize I need to change something etc.)_.  Therefore, I have given up trying to document or list them here.  Instead you can consult two files: (**`12-Xah-Fly-Keys.el`** and **`13-Key-bindings.el`**) to see them and how they are set:  Also, simply typing a prefix key _(e.g., **`C-x`**, **`C-c`**, or **`M-x`**, etc.)_ will pop up all the key commands available to choose from... No guessing needed... The Helm Popup has a menu to see the whole list as well... Try it!

Key commands change too much to make it worth the effort of updating this README in one place like this.  However in the documentation about specific Modular Emacs features, you will see the related Key commands listed there... _(hopefuly)_

## Optional Packages & Customization: _(still being documented)_

Above I kept going on and on about setting up the Default version of Modular Emacs... Now it's time to forget boring defaults and really tune this baby up to be the best **DevOps** , **PubOPs** , **Lisp IDE** that it can be!  The following doc with guide you through that process:  **[Modular Emacs Customization](./Docs/ModE-Optional-Packages-y-Customization.md)**

# Table of Contents:

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

- [Version 3.2 (beta) - README](#version-32-beta---readme)
    - [Introduction:](#introduction)
        - [History & Purpose:](#history--purpose)
    - [Try Harmonic Alchemy Modular Emacs:](#try-harmonic-alchemy-modular-emacs)
        - [Microsoft Windows Support](#microsoft-windows-support)
        - [My Commitment:](#my-commitment)
- [Start Here:](#start-here)
    - [Requirements:](#requirements)
        - [Clone This Repository:](#clone-this-repository)
        - [Install Emacs: V26.1+](#install-emacs-v261)
        - [`NODE.js` with `NVM` & `NPM`:](#nodejs-with-nvm--npm)
        - [VMD: (Visual MarkDown App)](#vmd-visual-markdown-app)
        - [MultiMarkdown:](#multimarkdown)
        - [Pandoc:](#pandoc)
        - [Fonts:](#fonts)
        - [Python and Pip:](#python-and-pip)
        - [Extra for Experts - LaTeX](#extra-for-experts---latex)
        - [Graphviz:](#graphviz)
    - [Get Ready to Start up Modular Emacs for the first time!](#get-ready-to-start-up-modular-emacs-for-the-first-time)
        - [First Some Initial House Keeping: _before we move in_](#first-some-initial-house-keeping-_before-we-move-in_)
            - [Create an empty file named `custom.el`:](#create-an-empty-file-named-customel)
            - [Clone `mmd-mode.git` into `my-modules`:](#clone-mmd-modegit-into-my-modules)
            - [Copy/Clone `me.init.el` to: `init.el`:](#copyclone-meinitel-to-initel)
        - [Final Step - Make Modular Emacs folder the default `~/.emacs.d` folder:](#final-step---make-modular-emacs-folder-the-default-emacsd-folder)
    - [Ready Set Go!  Start Up Modular Emacs:](#ready-set-go--start-up-modular-emacs)
    - [Usage:](#usage)
        - [Blackboard Color Theme:](#blackboard-color-theme)
    - [Default Packages & Features (built in):](#default-packages--features-built-in)
        - [Pre-configured packages - Comprehensive list:](#pre-configured-packages---comprehensive-list)
            - [Misc. Packages:](#misc-packages)
            - [Core Emacs System Related Packages:](#core-emacs-system-related-packages)
            - [Emacs Helm & Extras:](#emacs-helm--extras)
            - [Tree & Menu Related Packages:](#tree--menu-related-packages)
            - [Dired Related Packages:](#dired-related-packages)
            - [dev§Ops, sys§Admin, info✷Sec Related Packages:](#devops-sysadmin-info✷sec-related-packages)
                - [More languages, devOps tools, etc. to add later:](#more-languages-devops-tools-etc-to-add-later)
            - [Writer's \- Publishing Tools:](#writers---publishing-tools)
            - [Spelling:](#spelling)
            - [Project Management Tools:](#project-management-tools)
            - [Git \- `magit` & Related:](#git---magit--related)
        - [Key-bindings:](#key-bindings)
    - [Optional Packages & Customization: _(still being documented)_](#optional-packages--customization-_still-being-documented_)

<!-- markdown-toc end -->
