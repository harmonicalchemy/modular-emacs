---
file: ModE-Install-Pandoc.md
author: Alisha Awen
created: 2020-007-16
updated: 2020-007-16
tags: 2020, MultiMarkdown, Emacs, apps-tools, SysAdmin, HA-ModEmacs, how-to, README 
---
<!-- #2020 #MultiMarkdown #Emacs #apps-tools #SysAdmin #HA-ModEmacs #how-to #README -->

![Banner](./media/Modular-Emacs-Github-Banner-v3.png)

# Install Pandoc - All Platforms

**[\[Table of Contents\]](#table-of-contents)**

**[\[Back To MAIN Modular Emacs README\]](../README.md)**

## Introduction:

As with MultiMarkdown, Emacs will run fine without Pandoc installed on your machine, However Harmonic Alchemy Modular Emacs will not be able to export to all those fancy document formats without Pandoc _(or Multimarkdown)_ installed...  You will consider this a broken feature once you have written something _(within Emacs)_ that needs to be exported to **LaTeX** or some fancy eBook format.  **_Read the docs for both Multimarkdown and Pandoc_** to decide how to use them.  More instructions for installing and using Pandoc can be found on their official website here: <https://pandoc.org/installing.html> The Pandoc.org website has most everything you will need all in one place!

## Build Pandoc From Source: (recommended)

After deciding not to install **HLedger** _(Haskell Stack dependent)_ via **MacPorts**, _(due to some issues I did not document)_ I will not be installing Pandoc via **MacPorts** either for the same reason... _(Pandoc was written in Haskell)_...

Now I am having issues installing Pandoc via Macports on Mac OS El Capitan as well _(testing if_ **Harmonic Alchemy Modular Emacs** _can run on older Mac machines)_... 

Therefore going forward, **Haskell Stack IDE**, **HLedger**, and **Pandoc** will all be installed locally via source on ALL my platforms... _(your mileage may vary)_

> **Note:** _(Haskell Stack IDE must be installed first before Pandoc or HLedger!)_


### Install Haskell Stack IDE:

If you have not done so yet, before cloning **Pandoc**, you must first install necessary **Haskell build tools**... _(needed to build `pandoc` using `stack`)_

Follow this link to: **[Install Haskell IDE, All Platforms](./ModE-Haskell-IDE-Configuration.md)**


### Clone Pandoc Repo:

```yaml
$>  git clone https://github.com/jgm/pandoc
```


### Build Pandoc from source using Haskell Stack:

```yaml
$>  cd pandoc
$>  stack setup
$>  stack install
```

> **Note:** Stack setup will automatically download the **ghc** compiler if you don’t have it. Stack install will install the pandoc executable into: **`~/.local/bin`**, which you should add to your **`PATH`**. This process will take a while, and will consume a considerable amount of disk space.  Also... If this is the first time you used Stack, a new full index will also be downloaded to be installed for the first time... That will take even more time! Later installs only update the index...

> **Note 2:** Compiling & Building Pandoc consumes a lot of memory... On my Fedora AppVMs I had to increase my max memory to 5GB to prevent OOM errors during the build!

After all the above is done, Pandoc will be installed in: `$HOME/.local/bin/pandoc`.  You may need to add that to your $PATH environment variable...

**[\[Table of Contents\]](#table-of-contents)**

**[\[Back To MAIN Modular Emacs README\]](../README.md)**



## Install Pandoc on Mac OS: (not recommended but may work for you)

I am not sure about Homebrew anymore as I don't use it, but I have found problems trying to install it with MacPorts as well now so it may be best to install from source.

I don't recommend trying to install Pandoc from macports either but you can try it...

- **HomeBrew:** - If you use Homebrew try this:  

```bash
  user@home:~$ brew install pandoc
```

> **Note:** If Homebrew install works for you let me know by opening an issue. Thanks!

- **MacPorts:** - If you use MacPorts instead of Homebrew try this:  

```bash
  user@home:~$ sudo port install pandoc
```

## Other Ways to Install Pandoc on Linux: (not recommended)

> **Note:** I have been experiencing problems with Pandoc installed from package managers... Pandoc is built with Haskell.  HLedger is also written in Haskell, I had to install HLedger from source, and to do that I also installed the Haskell Stack IDE straight from Hascall.org rather than using package managers... Therefore going forward I have decided to install, **Haskell Stack IDE**, **HLedger**, and **Pandoc** locally from source on ALL my platforms going forward.  Not using package managers for any Haskell related projects... _(your mileage may vary)_

> Having said all the above, Pandoc is in the Debian, Ubuntu, Slackware, Arch, Fedora, NiXOS, openSUSE, and gentoo repositories so you don't have to build from source if you don't want to. However these packages are usually much older than the current versions available from source.  

> Try any of the methods below and see if it works for you... _(Let me know how things go... Open up an issue about your setup...  Thanks! :octocat:)_   
Otherwise, Follow the instructions for Installing from source above and skip the instructions below)

- **Debian, Ubuntu flavors:**  

```bash
  $_  sudo apt install pandoc  
```

- **RedHat, Fedora flavors:**

```bash
  $_  sudo dnf install pandoc
```

The pandoc package installs a lot of sub-packages and can take some time to install.   Please be patient...  Updates won't take so long...

## Pandoc Usage:

Now that you have Pandoc installed, **[Download The Manual Here](https://pandoc.org/MANUAL.pdf)**

## Table of Contents:

**[\[Back To MAIN Modular Emacs README\]](../README.md)**

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

- [Install Pandoc - All Platforms](#install-pandoc---all-platforms)
    - [Introduction:](#introduction)
    - [Build Pandoc From Source: (recommended)](#build-pandoc-from-source-recommended)
        - [Install Haskell Stack IDE:](#install-haskell-stack-ide)
        - [Clone Pandoc Repo:](#clone-pandoc-repo)
        - [Build Pandoc from source using Haskell Stack:](#build-pandoc-from-source-using-haskell-stack)
    - [Install Pandoc on Mac OS: (not recommended but may work for you)](#install-pandoc-on-mac-os-not-recommended-but-may-work-for-you)
    - [Other Ways to Install Pandoc on Linux: (not recommended)](#other-ways-to-install-pandoc-on-linux-not-recommended)
    - [Pandoc Usage:](#pandoc-usage)

<!-- markdown-toc end -->
