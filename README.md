---
file: README.md
author: Alisha Awen
created: 2019-010-20
updated: 2019-011-14
tags: Emacs, 2020, apps-tools, SysAdmin, HA-ModEmacs, how-to, README 
---
<!-- #Emacs #2020 #apps-tools #SysAdmin #HA-ModEmacs #how-to #README -->

![Banner](./Docs/media/Modular-Emacs-Github-Banner-v3.png)

# Modular Emacs README

**[\[Table Of Contents\]](#table-of-contents)**

## Introduction:

**_Welcome to Harmonic Alchemy Modular Emacs - V3!_ (Currently in Beta)**

**Current Status:**

- Version 3.0 merges a few new features, as well as bugfixes from the **Lisp IDE** fork back into the master and develop branches of Harmonic Alchemy Modular Emacs.

- This single README doc was completely overhauled and made into separate indexed documents kept in the `./Docs` directory... As promised before, this is the main reason for this new version release...

- Many bugs were previously fixed in the Lisp IDE branch... Those fixes have now migrated out globally to all branches...  

- The Default Install of Harmonic Alchemy Emacs _(from the `master` branch)_ has been made as simple as possible without any bells or whistles...  However it is set up nicely at that point to be easily customized in different ways, depending on your personal needs and choices.  This is a more practical approach.  That is the aim of the basic default install of **_Harmonic Alchemy Modular Emacs_**...

- None of the optional features from the Lisp IDE branch or elsewhere are enabled in the default install... If you are interested in trying any of the optional features, you must install the default Modular Emacs first...  That is how it works now...  I like it..  I think you will too...

- A new optional **[Accounting Module](./Docs/ModE-Accounting-Module-Configuration.md)** _(currently an experimental feature)_ has been added.  The accounting module will enable `hledger-mode`, an Emacs interface to **hledger** _(a fork of_ **ledger** _completely rewritten in Haskell with hopeful improvements)_ ...

> Note: One place where modular-emacs is not yet modular are the key-binding modules...  All key-bindings for everything are in those key-bindings .el files. I plan to modularize key-bindings as well by moving related key commands into the modules they exclusively belong to.  This way, removing any single module will also remove everthing exclusively related to it, seamlessly from Emacs without leaving behind artifacts...  For now I need to test things as they are because I have added a lot of new Xaw-Fly-Keys things and need to get that working stable first... (also learn how to use the damn thing! LOL) I'm still cursing myself after accidentally hitting "V" for paste, when I thought I was in insert mode typing plain text!  But I am getting better.  

> This is why it is so important to start out slowly, like learning a new musical instrument... But worse... If you play a wrong note in music, its just a hiccup.. No damage done, except a little embarrassment... On the other hand, if you are typing real fast... _(and you will once you start getting cocky with your Xah-Fly-Keys-FU! They are not called Fly for nuttin! ;-)_ If you hit the wrong key you won't realize it until you have typed a whole wrong chord! You won't hear a sound, but the damage will be done and you won't know what the heck happened or how to get back! Slow Down! Drive Safely... Get those keys well established in muscle memory first...

To install Harmonic Alchemy Modular Emacs for the first time, follow this guide _(in a top down fashion)_... Do not skip to any of the other install guides until you have the basic default configuration of Modular Emacs installed and running _(without errors)_ in your home directory... 

> **Exception: _Debian and Mac users will have to follow a linked doc that guides you through building and maintaining your own Official GNU Emacs from source first!_** Building from source is tedious, _especially if you are doing it for the first time, you WILL have to try many times before getting all those "twisty little snippets all different", sorted out and resolved._  If you stick to your guns and stay persistant, you will be awesome!  You will also be in total control of your personal and OFFICIAL build of GNU Emacs, how it works, and what it does!  Your second build experience will go much more smoothly...  By the third build the process will become the boring routine of typing a few commands, and then waiting as your machine cranks away a brand new version for you...  Play **_mind_ sweeper** with your smart spyPhone, or read a book about exploding brain synapses while your computer's busy making free-as-in-liberated Emacs... :trollface:

Once the above is done, visit the _**optional section headings**_ below to find links to install guides for optional modules you may be interested in trying out...  Consult the **[\[Table Of Contents\]](#table-of-contents)** for quick access if you are coming back to this doc later... You will see a link to the **Table of Contents** placed approximately one page or half page apart continuing down this document as well for easy quick reference...

> **Warning:** Enabling optional features may require you to remap some of your keys and also change Emacs into a modal editor _(similar to Vi but more Emacs like)_  Read carefully before making that commitment...

> **Note:** If you are on Mac OS or some flavor of Debian, you may have to build Emacs from source _(as mentioned above)_ to enable extra features support _(like VMD-Mode which needs special libraries linked in during the Emacs build process)_...  Full instructions are provided for those sections...

**[\[Table of Contents\]](#table-of-contents)**

### History & Purpose:

The history of **Harmonic Alchemy Modular Emacs** is _"a confusing journey through many little twisty passages, all alike..."_ [Everything is documented here now](./Docs/ModE-History.md)...  Read it at your leasure...  It provides some perspective, insight, and purpose behind how and why things are done here in the present time...

The present is a continuum... Therefore this history document will also be updated over time, to record _"our new enlightened adventure through many interesting little twisty passages, all different!"_

## Try Harmonic Alchemy Modular Emacs:

For those of you who feel **[Harmonic Alchemy Modular Emacs](https://github.com/harmonicalchemy/modular-emacs)** may be exactly what you were looking for: **_By all means... Lets Dive in!_**

There are some external requirements... It may take a while... If that's OK with you, Great!... Lets go...  Eventually I will script this up so you can just enter this at the command prompt: **`./install.sh`** and be done with it... Keep your fingers crossed on that but don't get your hopes up... I am a busy girl!!! :octocat:  That script will have to install a lot of things! It will have to check a lot of system states!  I have to work all that out as a **non-interactive** process.  It should install everything _(transparently non-interactively)_ from that single running shell script...  If you have to answer questions and make decisions... Better that you execute manually instead, and read the docs first...  Right?

### Microsoft Windows Support

> **Note:** Hello MS Windows Users... I admit to having no experience at getting a nice Emacs GUI environment running nicely on MS Windows... **Harmonic Alchemy Modular Emacs** expects to be installed on a unix machine, or unix friendly machine contaning standard unix apps and utilities or proper hooks into such things... Many of these may not be implemented within the canonical Windows OS model and I have no experience sorting that out...  It is hard enough to get Mac OS to behave along side GNU Linux or other free software and open source standards! I do not wish to abandon MS Windows users however.  I dedicated a document within Harmonic Alchemy Modular Emacs to record any information needed to help get Modular Emacs running on MS Windows.  Please consult:  **[Note for MS Windows Users](./Docs/ModE-Note-2-MS-Windows-Users.md)** for more information...  I am looking for a good MS Windows Sys Admin type to jump in here and help... Where are you? Please open an issue with intro to yourself for me to review... Thanks!

For the rest of us... For now... Let's learn this together... Please give feedback, open issues, ask questions!  Thanks ;-)

### My Commitment:

> **_"Always do what is right. It will gratify half of mankind and astound the other."_** - Mark Twain  

I am committed to making this **process/journey** as painless as possible for you as I have found many other Emacs setups to be way to complicated to make it easy for adoption into my workflow, and you are probably experiencing the same!  On the other end of the spectrum... Installing basic Emacs alone is not enough to get you started _(IMHO)_...  I set it up that way to get you up and running with a simple setup first... You should quickly outgrow that and certainly want to enable the extra features!  

Therefore I made that process easy to do!  The whole purpose of this project is to be able to easily grow and shrink features as needed in any combination thereof... Without _"getting lost in a tangled mess of twisty little passages all alike!"_

If you end up liking **Harmonic Alchemy Modular Emacs**, please drop me a note to let me know! Please do ask questions, open issues, etc.  Thanks!  I have been using Emacs since around 1986/87 _(OMG that long! off-and-on with gaps in between ;-)_ but I never shared any of that Emacs knowledge with anyone before now... :heart_decoration:  

Therefore: **Here is Modular Emacs!** I hope my long years of **devOps** experience will turn out a nice jewel for new comers to try out and succeed with!  **Good Luck and God Speed!**  

**[\[Table of Contents\]](#table-of-contents)**

# Start Here: 

**_(Modular Emacs Default Installation)_**

## Requirements:  

### Emacs: V26.1+  

Many of the default packages installed with this project will not work with older versions of Emacs. Also the security of older Emacs and packages is Horrendous! Recently, some of the older packages were removed from the updated MELPA and moved to other less-secure repositories. The entire MELPA site is TLS enabled now but these other repos are not very secure...  Updated features _(acquired securely)_ are vital for being reasonably safe going forward from 2020 onward...  Therefore, I decided not to support older versions of Emacs...  

> **Warning:** If you have a really old version of Emacs currently installed, it may break after you update Emacs below... Therefore before trying any of this out, first you should record the current version of Emacs you are using, and backup all previous emacs related files: _(i.e.,_ `.emacs` `.emacs.d`_)_ located in your $HOME directory.  

> Simply COPY your entire: **`~/.emacs.d`** directory _(and/or_ **`~/.emacs`** _file)_ into a temporary directory of your choice before upgrading below... _(or compress and make a `.zip` or `.gz` archive file)_  This will be your safety net for coming back later if you decide you are not ready to upgrade.  Don't move or touch your original Emacs files, as you will try them out with your new Upgraded version of Emacs...  If anything goes wrong, you will be able to reverse everythig back no problem! _(Note: before restoring your backups, you will have to downgrade your Emacs back to the original version you were using before doing all this)_ 

**[\[Table of Contents\]](#table-of-contents)**

#### Commands to install Emacs on various unix platforms:  

_(Choose your Flavor)_  

##### Fedora 27-29:  

```bash
   $_  sudo dnf install emacs
```

That's it! Red Hat takes care of you lucky Fedora users...  

**[\[Table of Contents\]](#table-of-contents)**

##### Debian9 Stretch & Ubuntu 18.04+:  

You Debian/Ubuntu users don't get it so easy... Hope you got your hacker shoes on...
I tried all the normal ways to get the latest binary build of Emacs installed for Debian based Linux but none of those options are able to satisfy Harmonic Alchemy Modular Emacs basic default needs 100%... Follow this link to: **[Build Emacs from Source](./Docs/ModE-Build-Emacs-from-Src.md)** and then come back here to continue below...

> **Warning:** This will take a bit of up-front work...  I hope you don't mind staying up all night digging deep and scratching your eyes a lot! Not for the faint at heart!  If you never built software from source before you may want to try something like **`"Hello World"`** first... _(just kidding)_ :trollface:  No..  you stay right here soldier! We will get you up to speed soon enough... 
    **Harmonic Alchemy Modular Emacs** may still work if you give up and use the latest builds from the package managers, but you may not be able to use many of the nice extra features that Modular Emacs provides...

**_Was your Emacs Build Saga Successful?_**  

**Yes?:**  Little grasshopper... You are awesome! Now you may now call yourself a devOps engineer _(to friends and family only... Don't put it on your resume LOL)_.  Now you will no longer be dependent on the mercy of wining sniveling package archive build engineers! Worse than anything Monty Python ever made fun of! LOL  Welcome to DevOps heaven you pirate! Arr! :octocat:  Continue bravely on below with your next steps _(challenges)_...

**No?:** OK, You are wicked frustrated trying to build Emacs eh?  No problem... I feel your pain. Take a rest...  You can still get Modular Emacs installed below, _(with a few caveats of missing features of course...)_ Just use your default package manager to install the latest version of Emacs you can find there and continue on below...  You will have gotten your winning attitude back by that time.  Later on, when you are itching to get Emacs updated, come back to this doc and read **[Optional Packages & Customization:](#optional-packages--customization)** below where you will be given instructions for that and any of the other things you missed during the default install here... _(plus more goodies ;-)_

You will be surprised at your devOps abilities after a long rest! Your psychology needs a few gains first... Get the rest done and you will feel better! I promise! :purple_heart:

**[\[Table of Contents\]](#table-of-contents)**

##### Mac OS: 

> **Update 2019** _It looks like **Build-from-Source** or **Macports** is our only option now..._  

If you have MacPorts installed simply use this command and you will be all set:

```bash
    $_ sudo port install emacs
```

If you don't have Homebrew or MacPorts installed, Don't even think about HomeBrew!  Instead, Install MacPorts: **[Follow instructions on the MacPorts website](https://guide.macports.org/#installing)**...  Then use the above command to install Emacs...  Then you will be all set...

MacPorts will get you Emacs version 26.3+ which should work fine... I have not tested it however because when I first started out, I was using Homebrew...  I have since switched to MacPorts, but a year before that, I spent the time to build Emacs from source, and that is the best option...

If instead you have HomeBrew installed?  Do like I did and install Emacs from source... We Macolites have been abandoned by both Apple and Homebrew!  What a mess... and What a shame! **_Linux dudes roll eyes and wonder why MacRats don't jump ship?_**  

Follow this link to: **[Build Emacs from Source](./Docs/ModE-Build-Emacs-from-Src.md)** Follow that guide, and then come back here to continue below...

**_Were you successful after only a few tries?_**  

**Wow!** You are better than I was the first time! Fantastic! You have the Best option possible now!  And you can change any feature at will, even customize the base code if you so wish!

**What?**  You were not successful even after many tries?  OMG! Now all eight of your arms are wicked tired and you are frustrated beyond all means eh? That's Happened to me many times... No problem... I feel your pain. Take a rest...  Install the latest version you can currently get from HomeBrew, realizing that some features of Modular Emacs will not work correctly until you upgrade Emacs later...

Later on, when you are itching to get Emacs updated, come back to this doc and read **[Optional Packages & Customization:](#optional-packages--customization)** where you will find links to instructions for building Emacs from source once more. There you will also find links to guides for installing optional packages... 

You will be surprised at your devOps abilities after a long rest! Your psychology needs a few gains first... Get the rest done and you will feel better! I promise! :purple_heart:

Alternately, if you are willing to go through the pain of completely un-installing HomeBrew, and replacing it with MacPorts that would be the next best thing to building from source... _(although building from source is an easier task than completely switching package managers on Mac OS, in my opinion)_  If you have to remove HomeBrew... Better think long and hard and make good plans for it... It will take you a few days at least to get your system back in working order with all packages from HomeBrew removed and then re-installed via MacPorts!  I may write up a doc that guides you through that painful process... Not soon though... Too many other things on my plate...  Note if you can do all that by your lonesome, you can surely follow a `.configure` file, or `make` file to build and install something from source like Emacs!  Food for thought... :octocat:

**[\[Table of Contents\]](#table-of-contents)**

##### FreeBSD & OpenBSD:  

I have not tried this yet with a Qubes configured BSD VM... Hopefully the FreeBSD world knows how to do Emacs correctly... Install the most recent pre-built binary package of Emacs: _(must be up to v26.1 by now - 2019-May)_

```bash
    $_ pkg_add -r emacs
```

### `NODE.js` with `NVM` & `NPM`:

**_The following instructions are for ALL platforms..._**

> **Note: _(Debian, Ubuntu, & Mac Users)_** If you were unsuccessful attempting to build Emacs from Source _(above)_ you can skip this section and wait to come back here later to install `NODE` as well.  The features NODE provides may not be accessable from Emacs if it was installed from the package managers on your platforms _(exception MacPorts)_. Therefore you can wait to install this later after you have built your own custom Emacs first... enuff said...

Follow this link to: **[Install NODE.js with NVM and NPM](./Docs/ModE-Install-NODE.md)** and then come back here to continue below...

**[\[Table of Contents\]](#table-of-contents)**

### VMD: _(Visual MarkDown App)_

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

#### Install Multimarkdown on Mac OS:  

If you are on Mac OS you can install MultiMarkdown via MacPorts or Homebrew:  

- **MacPorts:** - For some reason, Fletcher's MultiMarkdown guide does not recommend using MacPorts to install MultiMarkdown??? The docs says "I don't recommend it" This is not good advise... He should check with the MacPorts maintainer because last time I checked the MacPorts version was step in sync with the latest MultiMarkdown: `multimarkdown @6.4.0 (textproc)` Here is how to install it:

```bash
  $_  sudo port install multimarkdown  # To install it...
  $_  port echo requested              # To see it in the list 
                                       # of installed packages.
```

- **HomeBrew:** - Using Homebrew is equally simple... 

```bash
  $_  brew install multimarkdown
```

That's it... Easy!  

**[\[Table of Contents\]](#table-of-contents)**

#### Install Multimarkdown on Linux:  

On Linux you will have to clone/configure/make/build & maintain Multimarkdown yourself... No package managers for this on Linux baby.  Sorry...  Hey... You wanted to be cool like the hackers and use Linux?  OK then... You don't need no stinking binary package managers anymore...  Right!  :octocat:  Follow this link to: **[Build MultiMarkdown from Source!](./Docs/ModE-Build-MultiMarkdown-from-Src.md)**

> **Note:** if you run into trouble building Multimarkdown, you can probably skip that as well for later... _Pandoc, (install below) will serve your **Plan B** when such **"Murphys"** happen..._

#### Usage:
MultiMarkdown will provide more than enough conversion power for you as you also will be using **Pandoc** to convert to some of these same output formats... You may later also have TeX and LaTeX mode hooks to use as well...

Now that you have Multimarkdown installed, **[Download The Manual Here](https://fletcher.github.io/MultiMarkdown-5/MMD_Users_Guide.pdf)**

### Pandoc:  

As with MultiMarkdown, Emacs will run fine without Pandoc installed on your machine, However Harmonic Alchemy Modular Emacs will not be able to export to all those fancy document formats without Pandoc _(or Multimarkdown)_ installed...  You will consider this a broken feature once you have written something _(within Emacs)_ that needs to be exported to **LaTeX** or some fancy eBook format.  **_Read the docs for both Multimarkdown and Pandoc_** to decide how to use them.  More instructions for installing and using Pandoc can be found on their official website here: <https://pandoc.org/installing.html> The Pandoc.org website has most everything you will need all in one place!  

**[\[Table of Contents\]](#table-of-contents)**

#### Install Pandoc on Mac OS:  

**You have 3 options:** HomeBrew, MacPorts, or build from source.  The package managers are fine and up to date with pandoc.  Building from source is not necessary...

- **MacPorts:** - Install the latest MultiMarkdown: `V2.7.3`_(as of this writing)_  
Here is how to install it:

```bash
  $_  sudo port install pandoc      # To install it...
  $_  port echo requested           # To see it in the list 
                                    # of installed packages.
```

- **HomeBrew:** - If you use Homebrew do this instead:  

```bash
  $_  brew install pandoc
```

That's it... Easy Peazy!

**[\[Table of Contents\]](#table-of-contents)**

#### Install Pandoc on Linux:  

Pandoc is in the Debian, Ubuntu, Slackware, Arch, Fedora, NiXOS, openSUSE, and gentoo repositories so you don't have to build this one. Whew! ;-)  Here are the three Modular Emacs currently supports:

- **Debian, Ubuntu flavors:**  

```bash
  $_  sudo apt install pandoc  
```

- **RedHat, Fedora flavors:**

```bash
  $_  sudo dnf install pandoc
```

The pandoc package installs a lot of sub-packages and can take some time to install.   Please be patient...  Updates won't take so long...

#### Pandoc Usage:

Now that you have Pandoc installed, **[Download The Manual Here](https://pandoc.org/MANUAL.pdf)**


### Python and Pip:

Some of the newer packages to be enabled will require Python packages to be installed... This is especially true of new **_Emacs MultiMedia System_** packages...

In my DOTFILES repository, on Mac OS, I have a **README** for setting up Python and Pip with the aid of initially installing Python and matching PIP using MacPorts... After that, everything is handled outside of MacPorts using PIP alone.  Handling convoluted package managers on Mac OS is confusing at best! :octocat:  

On linux things are a bit more reasonable...  Here is what I am doing thus far to manage Python on Linux...  I may add this information to my DOTFILES documentation later as well...

#### Install Python & PIP on Fedora:

> **Note:** If you use: `dnf install python` on Fedora you get Python 3 by default.  Below I am explicitly installing `Python 2` and `Python 3` with matching `PIP`:   

```bash
$_  sudo dnf install python2        # This installs Python 2.
$_  sudo dnf install python2-pip    # This installs Python 2 pip.

$_  sudo dnf install python3        # This installs Python 3.
$_  sudo dnf install python3-pip    # This installs Python 3 pip.
```

Just as on **MacOS**, on **Fedora**, **Python** and **PIP** are both installed and upgraded using the **dnf** package manager system wide...  Beyond that, all package management with **PIP** is done locally within your user's **`$HOME`** environment...  Not using **`sudo`** or **Qubes** Template VMs...

#### PIP Management:

Done within your local user's `$HOME` environment.  _(e.g., Not within Qubes Template VMs or with `sudo`, etc.)_

> **Note:** The commands below also work as: `pip2 <command>` etc.    
_(for managing Python 2 packages)_...

- **Search for Package:**

```bash
  $_  pip3 search <PACKAGE_NAME>
```

- **Install Package:**

```bash
  $_  pip3 install <PACKAGE_NAME>
```

- **Upgrade Package:**

```bash
  $_  pip3 install --upgrade <PACKAGE_NAME>
```

- **Uninstall Package:**

```bash
  $_  pip3 uninstall <PACKAGE_NAME>
```

### Extra for Experts - LaTeX

No I did not forget LaTeX...  This is a powerful monster! It needs its own monster doc to install and configure as well... You need it to work with **_Pandoc_**, **_Multimarkdown_**, and later **_org-mode_** when I have that also integrated into Modular Emacs...  LaTeX provides extended features to produce PDFs, and eBook formats... Emacs can be integrated _(especially `org-mode`)_ to be able to export to these fancy formats using LaTeX...

This is Great News if you would like to make **Emacs** work as well as _(or better than)_ **Scrivener**...  So get on it already OK? Follow this Guide to get started: **[Install LaTeX Publishing Environment](./Docs/ModE-Install-LaTeX-pubOps-Env.md)**...

**[\[Table of Contents\]](#table-of-contents)**

### Graphviz:

Harmonic Alchemy Modular Emacs comes integrated with the popular **Graphviz utility** which allows the creation of nice graphs, flowcharts, data diagrams, etc. using a powerful scripting language called **dot**...  The Emacs mode for Graphviz is: **`graphviz-dot-mode`**

Modular Emacs invokes: **`graphviz-dot-mode`** when you visit files ending in either: **`.dot`** or **`.gv`**

When you are visiting a **`.dot`** file, you can compile it with **`C-c C-C`** which will produce a **`.svg`** file along side...  By default, Modular Emacs produces **SVG vector files** _(instead of `.png` files)_... which is my preference because if you add one to a markdown file and then view it using **VMD-Mode** you can expand the resulting rendered chart or graph full screen and it will still look very sharp! Not to mention you can edit the resulting SVG file within **inkscape** to add things to it that Graphviz cannot...  Don't underestimate the power of **Graphviz** though!  **[Read the docs!](https://graphviz.org/documentation/)**  

You could create a nice workflow that **_starts programmatically_** within some program or language, invoke **graphviz** to produce some diagrams, export to **`SVG`**, then edit the resulting `.svg` in **inkscape** to add some fancy graphics or other things, Lastly import the final `.svg` into **Blender** to add animations, 3D, etc...  **_The sky is the limit!_**  

You better send me an email to show me the cool thing you made on **Github** after reading this! :octocat:  

**[\[Table of Contents\]](#table-of-contents)**

#### Install Graphviz on Mac OS:

**You have 2 options:** HomeBrew, or MacPorts... _(building from source is not necessary)_

- **MacPorts:** - Install Graphviz: `V2.40.1_2` _(as of this writing)_  

```bash
 $_  sudo port install graphviz   # To install it...
 $_  port echo requested          # To see it in the list 
                                  # of installed packages.
```

- If you use **Homebrew**, do this instead:  

```bash
 $_  brew install graphviz
```

That's it... Easy!  

#### Install Graphviz on Linux:

Graphviz is in the Debian, Ubuntu, Slackware, Arch, Fedora, NiXOS, openSUSE, and gentoo repositories so you don't have to build this one either. ;-)

- **Debian, Ubuntu flavors:**  

```bash
 $_  sudo apt install graphviz  
```

- **RedHat, Fedora flavors:**

```bash
 $_  sudo dnf install graphviz
```

That's it... Easy Peazy!

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

First be sure to rename _(save)_ your existing:  **`~/.emacs.d`** to: **`~/save.emacs.d`** _(You may have already done this in the beginning)_  You may also have a `.emacs` init file outside of the `.emacs.d` folder that also needs to be renamed!    **This is very important!  Becase the next steps will overwrite them if you did not change their names!!!**

Now you are ready to **rename `~/me.emacs.d`** to: **`~/.emacs.d`**  

```bash
$_  mv ~/me.emacs.d ~/.emacs.d
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

**Harmonic Alchemy Modular Emacs'** git repository is configured to ignore your local `~/.emacs.d/init.el`...  The file that **_is_** kept in revision is `~/.emacs.d/me.init.el` which you do not use directly.  Instead, your standard emacs config file: `~/.emacs.d/init.el` starts out as a copy of `~/.emacs.d/me.init.el` allowing you to try out new things on the fly without upsetting the state of the git repo...

Feel free to use `~/.emacs.d/init.el` to try out experimental customizations or tests. If something goes wrong you can quickly refresh this file from the original `~/.emacs.d/me.init.el`.  Realize however if you do that any changes or prior customizations you made will be lost!  So recover those first before overwriting with `me.init.el`!

You can also use the Modular Emacs \*scratch\* buffer to paste experimental lisp code to try out before making it permanent as a stand alone `.el` emacs lisp file, _(as the welcome banner suggests ;-)_.  Evaluate the entire \*scratch\* buffer without needing to remove the banner message at the top as that message is one huge lisp comment...  If something horrible happens, no worries... Simply restart emacs... Your mess-up will be gone and you will be back to where you were before you or that elisp test code goofed up...

If your customization proves stable, and you like it, you could then save your scratch buffer (as is) to a new file, for example:

```bash
$_  ~/.emacs.d/lisp/my-modules/00-my-new-module.el
```

In all cases you would also be wise to also create and checkout your own private local `test` branch _(call it what you wish)_ and keep all your custom changes in there separate from the `master` or `origin:develop` branch...

### Blackboard Color Theme:  

Modular Emacs comes with my slightly customized version of the **Blackboard color theme** which I like for the _pedagogic essence_ it inspires...  If you would like to add more custom themes or a different theme than `blackboard.el`, you can replace it or add additional themes into your local branch's: `~/.emacs.d/lisp/themes` directory and they will also will work with this setup by choosing `M-x load-theme` or changing the last line within `~/.emacs.d/lisp/modules/06-interface.el` to: `(load-theme 'your-chosen-theme-name t)`

> **Note:** _Obviously if you add more themes to your **Modular Emacs** **themes** directory you will be adding new un-tracked files to your cloned git repository!  Make sure you have checked out your own local branch before adding new themes or doing any customization outside the init.el file or the `my-modules` directory.  Then you will have proper management of your local custom changes and also have all that in code revision as well!_

## Default Packages & Features (built in):

> **Note:** This section needs an update... Many new features have been added.  Documentation for them is still an on-going process...  Please be patient... Or open an issue... Our conversation may end up being part of this doc. :octocat:

**Default Emacs welcome screen replaced with custom greeting:** Prints current emacs configuration, and date... With a famous Mark Twain quote _(My Favorite author. This quote may change from time to time with new updates)_.  

Use Scratch buffer to evaluate snippets of `lisp` without having to remove the welcome text... _(which are lisp comments)_

### Pre-configured packages - Comprehensive list:

#### Misc. Packages:

- [powerline](https://github.com/milkypostman/powerline) Decorate `mode-line` & make it More Informative.  You can customize options and use themes etc...


- [auto-complete](https://github.com/auto-complete/auto-complete) Easy Text Editing with suggested word completion.   
**Update 2020:** I believe this may be a bit buggy at the moment... I have neglected it and some update or debugging may be needed... Currently when entering code if I enter a function name and completion pops up.. If I want to keep typing _(not accepting the default, or maybe what I want is different than what it thinks)_... It fights me...  I have to fool it by typing some junk and then pull off a text switch game.... LOL  I will fix this later...  If you hate it, you can comment it out in 02-package-conf.el I may end up doing that myself... `%^)`

#### Core Emacs System Related Packages:

- [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell) Get environment variables such as $PATH from the shell.  btw, this can be a tricky thing to do in emacs!  I have struggled with this on Mac OS for a while!  You may find you have to start Emacs from the terminal to get all your environment vars into Emacs!  More about that later.. A continuing saga!   
**Update 2020:** This has been working fine on my iMac since completely replacing Homebrew with MacPorts.  IMHO _(biased by a host of irritant issues using Homebrew over the years)_  MacPorts is a much better package manager all around...  But not the popular one...  Oh Well... %^) Also, since building my own Emacs from GNU source tree on my iMac, configured just how I like it, I have had no more nagging MacOS related issues... And I can have the latest Emacs the second it is released _(one build cycle away, or choose to go back to my archived app if they introduce some bug or someting)_! :octocat:

#### Emacs Helm & Extras:

- [helm](https://github.com/emacs-helm/helm) Helm is an Emacs incremental and narrowing framework that helps speed up operations on commands with structured command completions and more.  It is a programmable interface (API) as well. To see how HELM can speed things up try a prefix key like: `C-x` or `C-c` or `M-x` and just wait before typing any more keys... You will see all the possible key-combos currently associated with that prefix key pop up in the HELM mini-buffer!  Fantastic! No more guessing!   


- [helm-core](https://emacs-helm.github.io/helm/) Development files for Helm (API)... Installed as dependency.   


- [popup](https://github.com/emacs-helm/helm) Popup is a dependency package for helm.


- [which-key](https://github.com/justbur/emacs-which-key) Used with helm, pops up suggestions for what can follow after pressing `C-x` key... _(this is what I was talking about above in the intro paragraph)_   
**Note:** **_xah-fly-keys_** also uses helm to pop up leader key choices and extended menu...  Hit the `SPACEBAR` _(LEADER KEY)_ and wait a second for helm to pop up the list of options... _(with help feature and pagination! Vital for learning all those keys!)_
#### Tree & Menu Related Packages:

- [ztree](https://github.com/fourier/ztree) An efficient directory tree comparison utility. Invaluable for visually oriented thinkers! Integrates directly with ediff files!


- [imenu-list](https://github.com/bmag/imenu-list) Right panel Menu and Variable list.  You can toggle an imenu list right panel window (for example within an elisp, Python, etc. file) with: `C-'`

#### Dired Related Packages:

- [dired-launch](https://github.com/thomp/dired-launch) Open files directly within `dired` using default applications. Great for quick viewing PDF or Image files with your OS default viewer app!


- [neotree](https://github.com/jaypei/emacs-neotree) Spawn a tree at any directory. You can toggle a `neotree` left side panel window within `dired-mode` from Xah Fly Keys Command mode by hitting **`'`**    
**Note:** I have been using bookmarks pretty nicely for a while now and not using this neotree feature all that much... but it is a nice way to navigate from your current location when you are NOT within dired... Your mileage may vary...

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
**Note:** I like to place my toc at the bottom, and provide a simple link named: `table-of-contents` or `index` and put that link at the top, as well as strategic places down within the body of the markdown file... This README is structured in that style... It reminds me of the way we used to do a similar thing in HTML before fancy CSS menus started popping up...


- [deft](https://jblevins.org/projects/deft/) Major mode for quickly browsing, filtering, and editing directories of plain text notes. Created by Jason Blevins _(the same statistics economics professor at Ohio State who also created Emacs Markdown Mode)_ This guy is wicked clever!  Looks like he has some nice algorithms for studying the cryptocurrency and smart contracts world as well!  Invoke Deft Mode with custom Xah Fly Keys Command Mode key: **`0`**


- [pandoc-mode](https://joostkremers.github.io/pandoc-mode/) `pandoc-mode` is an Emacs mode for interacting with Pandoc. Pandoc is a program _(plus libraries)_ created by John MacFarlane that can convert a text written in one markup language into another markup language. This is one of the tools that will help me get off proprietary Scrivener on **Mac OS** and finally be able to use Emacs for all my **Pub§Ops**!


- [fountain-mode](https://github.com/rnkn/fountain-mode) For **writers, screenwriters** A markdown mode for writing screenplays! Fantastic!!! Fountain together with pandoc, allows you to do many conversions for screenplays to industry formats like **Final Draft** etc. but the best part is the text highlighting and colors make it very easy to read/rehearse dialog and cues right from an Emacs buffer!  Writing scripts in this mode is as natural as typing `INT` _(your line instantly turns into a scene heading!)_ Try it! Just start typing your script and find out what happens!


- [olivetti](https://github.com/rnkn/olivetti) Adjust margins with word wrap. _(great with fountain mode!)_ Perfect for distraction free writing! I also enable this within `rmoo` _(if you have the **games module** enabled)..._  Toggle it on and off within Xah Fly Keys Command Mode by hitting: **\`** key.  Adjust the margin width within Xah Fly Keys Command Mode by hitting: `SPACEBAR [` or: `SPACEBAR ]` to shrink or grow respectively.


- [vmd-mode],(https://github.com/blak3mill3r/vmd-mode) Fast Github-flavored Markdown previews synchronized with changes to an emacs buffer (no need to save).  Renders org files _(and normal markdown)_ in an external VMD App that automatically shows changes in real-time as you type! _(You need to install this external app separately, and make sure Emacs knows where to find it.  See: `exec-path-from-shell` below)_  Toggle `vmd-mode` on and off within Xah Fly Keys Command Mode by hitting leader key sequence:  `SPACEBAR-v`


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

## Optional Packages & Customization:

Above I kept going on and on about setting up the Default version of Modular Emacs... Now it's time to forget boring defaults and really tune this baby up to be the best **DevOps** , **PubOPs** , **Lisp IDE** that it can be!  The following doc with guide you through that process:  **[Modular Emacs Customization](./Docs/ModE-Optional-Packages-y-Customization.md)**

# Table of Contents:

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [Modular Emacs README](#modular-emacs-readme)
    - [Introduction:](#introduction)
        - [History & Purpose:](#history--purpose)
    - [Try Harmonic Alchemy Modular Emacs:](#try-harmonic-alchemy-modular-emacs)
        - [Microsoft Windows Support](#microsoft-windows-support)
        - [My Commitment:](#my-commitment)
- [Start Here:](#start-here)
    - [Requirements:](#requirements)
        - [Emacs: V26.1+](#emacs-v261)
            - [Commands to install Emacs on various unix platforms:](#commands-to-install-emacs-on-various-unix-platforms)
                - [Fedora 27-29:](#fedora-27-29)
                - [Debian9 Stretch & Ubuntu 18.04+:](#debian9-stretch--ubuntu-1804)
                - [Mac OS:](#mac-os)
                - [FreeBSD & OpenBSD:](#freebsd--openbsd)
        - [`NODE.js` with `NVM` & `NPM`:](#nodejs-with-nvm--npm)
        - [VMD: _(Visual MarkDown App)_](#vmd-visual-markdown-app)
        - [MultiMarkdown:](#multimarkdown)
            - [Install Multimarkdown on Mac OS:](#install-multimarkdown-on-mac-os)
            - [Install Multimarkdown on Linux:](#install-multimarkdown-on-linux)
            - [Usage:](#usage)
        - [Pandoc:](#pandoc)
            - [Install Pandoc on Mac OS:](#install-pandoc-on-mac-os)
            - [Install Pandoc on Linux:](#install-pandoc-on-linux)
            - [Pandoc Usage:](#pandoc-usage)
        - [Python and Pip:](#python-and-pip)
            - [Install Python & PIP on Fedora:](#install-python--pip-on-fedora)
            - [PIP Management:](#pip-management)
        - [Extra for Experts - LaTeX](#extra-for-experts---latex)
        - [Graphviz:](#graphviz)
            - [Install Graphviz on Mac OS:](#install-graphviz-on-mac-os)
            - [Install Graphviz on Linux:](#install-graphviz-on-linux)
    - [Get Ready to Start up Modular Emacs for the first time!](#get-ready-to-start-up-modular-emacs-for-the-first-time)
        - [First Some Initial House Keeping: _before we move in_](#first-some-initial-house-keeping-before-we-move-in)
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
    - [Optional Packages & Customization:](#optional-packages--customization)
- [Table of Contents:](#table-of-contents)

<!-- markdown-toc end -->
