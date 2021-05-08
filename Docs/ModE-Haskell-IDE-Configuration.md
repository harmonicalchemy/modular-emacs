---
file: ModE-Haskell-IDE-Configuration.md
author: Alisha Awen
created: 2021-002-22
updated: 2021-002-22
tags: Emacs, apps-tools, SysAdmin, HA-ModEmacs, MWM-how-to, how-to, README, 2021, Haskell, Stack, Cabal, ghc 
---
<!-- #Emacs #apps-tools #SysAdmin #HA-ModEmacs #MWM-how-to #how-to #README #2021 #Haskell #Stack #Cabal #ghc -->

![Banner](./media/Modular-Emacs-Github-Banner-v3.png)

# Modular Emacs Haskell IDE Install Guide

**[\[Back To MAIN Modular Emacs README\]](../README.md)**

## Introduction:

**_This Guide Provides Information for getting a Haskell / Stack IDE up and running on MacOS and Linux..._**

### Refs:

- **[Haskell `stack`](https://tech.fpcomplete.com/haskell/get-started)** - This is the recommended tool set of many Haskell based projects... It is a comprehensive development environment for Haskell which includes: 
Stack itself, _(a project builder for multi-package Haskell projects)_
GHC, _(the official compiler and interpreter for Haskell programs)_,
Haddock, _(a documentation generator for Haskell packages)_, and 
Stackage, _(a curated repository of thousands of packages installed on demand)_.


- **[GHC - Glasgow Haskell Compiler](https://www.haskell.org/ghc/)** - GHC is a state-of-the-art, open source, compiler and interactive environment for the functional language Haskell.


- **[Haskell `cabal`](https://www.haskell.org/cabal/)** -  CABAL: _(Common Architecture for Building Applications and Libraries)_ and alternate way to build Haskell projects...


## Install Haskell build tools:

My initial research with installing and using **HLedger** accounting software, and **Pandoc** _(both based on Haskell)_ required me to install a good Haskell dev environment.  The procedure for getting this done is the same for all platforms... **`/usr/local`** is where the compilers etc. are installed on ALL platforms...  For Qubes... understand that your Haskell environment is a **Local AppVM Install** not done in the Template VM!

Dependencies include installing **`stack`** or **`cabal`**... For most projects it looks like **`stack`** is the preferred method... Therefore I choose to use **`Stack`** as this is what the HLedger docs recommend as the best way. **Pandoc** will be built the same way...

### Install Stack on All `NX` environments:

After reading a bunch of scary posts about **MacPorts** causing problems with **`GHC`** I consulted the main `haskellstack.org` website to find out what to do... Their recommended way for Mac OS _(and all `nx` platforms for that matter)_ is to run the following:

```yaml
user@local:~$ curl -sSL https://get.haskellstack.org/ | sh

## A bunch of screen noise scrolls down while installing...
## Ending with the following output:

. . .

perl-utils-5.30.3-459.fc32.noarch                                                  
python3-pyparsing-2.4.7-1.fc32.noarch                                              
systemtap-sdt-devel-4.4-3.fc32.x86_64                                              

Complete!

Using generic bindist...

  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100   145  100   145    0     0    318      0 --:--:-- --:--:-- --:--:--   318
100   637  100   637    0     0    592      0  0:00:01  0:00:01 --:--:--  1918
100 13.8M  100 13.8M    0     0   654k      0  0:00:21  0:00:21 --:--:--  597k
Installing Stack to: /usr/local/bin/stack...

About to use 'sudo' to run the following command as root:
    install -c -o 0 -g 0 -m 0755 /tmp/tmp.5tu2EfpEpi/stack /usr/local/bin
in order to copy 'stack' to the destination directory.


-------------------------------------------------------------------------------

Stack has been installed to: /usr/local/bin/stack

user@local:~$ _
```

One single command for all platforms... That is great! I built it on both Mac OS, and Fedora for my first test cases and it worked like a champ!


> **Note:** This script will ask for root access using sudo in order to install dependencies and to install the binaries to `/usr/local/bin`.

Installing within **`/usr/local/bin`** is fine for me on all platforms!  It won't interfere with packages installed by MacPorts within **`/opt/local/bin`**  

After installing _(above)_ make sure your `$PATH` includes: `/usr/local/bin` after any `bin` paths within your local home directory, and before `/opt/local`, `/usr/bin`, `/bin` etc...

Once you have that done execute the following to upgrade `stack`:

```yaml
    stack upgrade
```

You may get a message telling you stack is already up to date... In that case you are all set... You will need to issue this command again later when an upgrade is available...



> **Note:** On some platforms you may have to install some developer libraries before using stack to build projects... _(I got library missing errors )_

In particular `ld` could not find -lgmp on Fedora 32.  I fixed that by installing the gmp-devel package:

``` yaml
[user@home ~]$ sudo dnf install gmp-devel
```

**[\[Back To MAIN Modular Emacs README\]](../README.md)**
