---
file: ModE-Install-LaTeX-pubOps-Env.md
author: Alisha Awen Sheppard
created: 2019-011-13
updated: 2019-011-14
tags: LaTeX, apps-tools, SysAdmin, HA-ModEmacs, how-to, README 
---
<!-- #LaTeX #apps-tools #SysAdmin #HA-ModEmacs #how-to #README -->

![Banner](./media/Modular-Emacs-Github-Banner-v3.png)

# Install Modular Emacs LaTeX pubOps Environment

**[\[Back To Top README\]](../README.md)**

## Introduction:

**Harmonic Alchemy Modular Emacs** _(since Version 3)_ is already set up with AUCTeX to turn your Emacs editor into a nice TeX LaTeX editor/compiler IDE... You don't have to add any `eLisp` forms to your `init.el` configuration to start working with LaTeX documents... 

However you do need to install the required TeXShop and external Editor Packages...  Without that Emacs won't have any TeX engines to use... You will see errors if you try to do things like compile LaTeX docs within emacs... Follow the instructions below to finish getting the required packages in place first...  Then you will be all set!

### Mac OS:

On Mac OS go to the MacTeX website and follow instructions to install MacTeX and set up your environment with TeXShop as your default stand alone TeX editor/compiler/IDE... I am currently using MacPorts to install dependencies for things, but many of the things like this which have their own built in package managers are installed and maintained outside of MacPorts... My HomeBrew days are over... Many of the advise I gave for HomeBrew in the past may now need to be updated... I will need help with that now as I don't use HomeBrew anymore... Also, as mentioned elsewhere, on Mac OS I maintain my own build of Emacs from the latest commits of the GNU Emacs dev team. ;-)

#### MacTeX:

Follow instructions, including the **_Please Read_** section on: **[Tug.org/mactex/](https://tug.org/mactex/)** to install what you need... There are two options, the BIG everything, and a more reasonable BasicTeX... I went ahead and installed the big one... _(plenty of disk space)_ and I aim to be a publishing wizard!
Once you have this, MacTeX has utilities for self update as well as TeXLive etc...  You don't need Apple, HomeBrew, or MacPorts for any of this... Got that?

To make **MadTex** _(lol "a typo" but decided to leave it cause it made me laugh!)_ binaries available on the command line, _(for Pandoc etc.)_ you have to set the following in your `$PATH` definition within your shells...

```yaml
    /Library/TeX/texbin
```
Put it after your `~/.bin` and `/opt` paths but before: `/usr/bin` and `/bin` etc...

### Linux:

On Linux, _(currently Fedora, other platform instructions to come)_ Install your TeX environment as follows:

- **Install TexLive on Fedora:** Choose a package size that fits your needs... I went for `texlive-scheme-full` as I have plenty of disk space currently... The full package is over 3GB installed!  You may need to create an extended cache for DNF to use so it does not run out of disk space/memory while installing...
Choose only one of the following:

```yaml
    sudo dnf install texlive-scheme-basic
    sudo dnf install texlive-scheme-medium
    sudo dnf install texlive-scheme-full
```

- **Install TexStudio**

```yaml
    sudo dnf install TeXStudio
```

At this point you should be all set to go... The Linux package managers take care of making sure executable paths are standard and accessable from Emacs...
