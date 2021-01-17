---
file: ModE-Accounting-Module-Configuration.md
author: Alisha Awen
created: 2019-010-30
updated: 2020-007-16
tags: Emacs, apps-tools, SysAdmin, HA-ModEmacs, MWM-how-to, how-to, README 
---
<!-- #Emacs #apps-tools #SysAdmin #HA-ModEmacs #MWM-how-to #how-to #README -->

![Banner](./media/Modular-Emacs-Github-Banner-v3.png)

# Modular Emacs Accounting Module Install Guide

**[\[Back To MAIN Modular Emacs README\]](../README.md)**

## Introduction:

**_This module provides self customizeable Accounting & Double Entry Ledger Features to Harmonic Alchemy Modular Emacs..._**

I am currently experimenting with this.  Looking into using **`hledger-mode`** etc. Initially I am using this document as a log of my first attempts...  When I am done, you will not only have a guide to follow, but you will also know the why and how behind my chosen path.  This will be invaluable as it will..., should have ... by that time ... been tested to work on all **`nx`** platforms producing the least headaches.  

You will see documented historical proof, _(of the dragons being slain)_ right here within this guide... As a result you will have all needed info and **_gotchas_** recorded along side _in-line_ using block quote message boxes... _(for your easy reference)_

> **"TahMe!!"** _(shouting)_   
Here's to our pirate accountant...,   
Haul Away... Haul Away...   
His plain text ledgers arrre cuttin-down Quicken Rues,   
Haul away... Haul Away...   

> No...  more...  Cyber Snatch Rules,  
Haul Away... Haul Away...   
Well "Stacked" bounties, gladly waging our labour,   
Haul Away... Freedom Bound!   

> Deep... Adventure... We do sail,   
Haul Away... Haul Away!...  
On a plain text ship that's Freedom Bound,   
A ship called The Haskell Ledger! Arrrrr!"_ :octocat: 

_(lol my rhyme scheme got wak - you figure out the rhythm... I think it's a slow hornpipe for hauling up the main and mizzen on your Emacs boat.  I will improve it later... `%^)`_

### Refs:
- **[PTA.org Dedicated to Plain Text Accounting](https://plaintextaccounting.org/)**
- **[HLedger Github Project](https://github.com/simonmichael/hledger)**
- **[Discussion Thread on Hacker News](https://news.ycombinator.com/item?id=13566147)**
- **[Emacs HLedger Mode GitHub Project](https://github.com/narendraj9/hledger-mode)**
- **[Haskell `stack`](https://tech.fpcomplete.com/haskell/get-started)** - This is HLedger's recommended tool set to use to build and install HLedger from source. It is a comprehensive development environment for Haskell which includes: 
Stack itself, _(a project builder for multi-package Haskell projects)_
GHC, _(the official compiler and interpreter for Haskell programs)_,
Haddock, _(a documentation generator for Haskell packages)_, and 
Stackage, _(a curated repository of thousands of packages installed on demand)_.
- **[GHC - Glasgow Haskell Compiler](https://www.haskell.org/ghc/)** - GHC is a state-of-the-art, open source, compiler and interactive environment for the functional language Haskell.
- **[Haskell `cabal`](https://www.haskell.org/cabal/)** -  CABAL: _(Common Architecture for Building Applications and Libraries)_

## Install HLedger:

### Intro:

My initial research suggests trying **HLedger** _(based on Haskell)_ first instead of the older Ledger project _(based on C++)_... I have decided to clone the project and build the **HLedger** source from my local private branch...  Dependencies for building include installing **`stack`** or **`cabal`**... You need to get those project dependencies installed first. Looks like **`stack`** is the preferred method...

After that is done, you will be `git` cloning the **HLedger Project** to build it via **`stack`**... **[Official HLedger build from git-repo Instructions are Here](https://hledger.org/download.html#building-the-development-version)**

### Install HLedger Project Dependencies:

Before cloning **HLedger**, first install necessary **Haskell build tools**... You can build `HLedger` using either `stack` or `cabal`...  I choose to use **`Stack`** as this is what the HLedger docs recommend as the best way.

#### All `NX` environments, including Mac OS:

After reading a bunch of scary posts about **MacPorts** causing problems with **`GHC`** I consulted the main `haskellstack.org` website to find out what to do... Their recommended way for Mac OS _(and all `nx` platforms for that matter)_ is to run the following:

```yaml
    curl -sSL https://get.haskellstack.org/ | sh
``` 
One single command for all platforms... That is great! I will build it on both Mac OS, and Fedora for my first test cases...

> **Note:** This script will ask for root access using sudo in order to install dependencies and to install the binaries to `/usr/local/bin`.

Installing within **`/usr/local/bin`** is fine for me on all platforms!  It won't interfere with packages installed by MacPorts within **`/opt/local/bin`**  

After installing _(above)_ make sure your `$PATH` includes: `/usr/local/bin` after any `bin` paths within your local home directory, and before `/opt/local`, `/usr/bin`, `/bin` etc...

Once you have that done execute the following to upgrade `stack`:

```yaml
    stack upgrade
```

You may get a message telling you stack is already up to date... In that case you are all set... You will need to issue this command again later when an upgrade is available...

Now it is time to clone and build HLedger...



### Clone HLedger Project from GitHub:

Go to the directory where you keep your local builds and then run the following:

```yaml
    git clone https://github.com/simonmichael/hledger
    cd hledger
    stack install
```

Wait, wait, wait... while it builds everything for the first time!!! Nice that you don't have to do much configuration... Hopefully... `%^)` When all of this is done you will have **`hledger`** installed in: `$HOME/.local/bin/hledger`.

Now to setup `hledger-mode` in **Emacs**

### Configure `hledger-mode`:

You created the file: `~/.emacs.d/lisp/modules/15-Accounting-pkg-conf.el`... Visit that file to see the configuration.

**I set the following key bindings:**

```lisp
    (global-set-key (kbd "C-c e") 'hledger-jentry)
    (global-set-key (kbd "C-c j") 'hledger-run-command)
```

Once you are in a report buffer after executing a command with `hledger-run-command`, press **`h`** to see the list of reports that you can have a look at. Press **`s`** in the overall report to see the meaning of the personal finance ratios for your report.


