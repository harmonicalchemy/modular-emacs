---
file: ModE-Install-LaTeX-pubOps-Env.md
author: Alisha Awen
created: 2019-011-13
updated: 2020-007-16
tags: LaTeX, apps-tools, SysAdmin, HA-ModEmacs, how-to, README 
---
<!-- #LaTeX #apps-tools #SysAdmin #HA-ModEmacs #how-to #README -->

![Banner](./media/Modular-Emacs-Github-Banner-v3.png)

# Install Modular Emacs LaTeX pubOps Environment

**[\[Table of Contents\]](#table-of-contents)**

**[\[Back to MAIN Modular Emacs README\]](../README.md)**

## Introduction:

**Harmonic Alchemy Modular Emacs** _(since Version 3)_ is already set up with AUCTeX to turn your Emacs editor into a nice TeX LaTeX editor/compiler IDE... You don't have to add any `eLisp` forms to your `init.el` configuration to start working with LaTeX documents... 

However you do need to install the required TeXShop and external Editor Packages...  Without that Emacs won't have any TeX engines to use... You will see errors if you try to do things like compile LaTeX docs within emacs... Follow the instructions below to finish getting the required packages in place first...  Then you will be all set!

## Install TeX Live:

### Mac OS:

#### Update:

> **Update 2020:** The new MacTeX package install runs on Mac OS High Sierra, and higher... I need to support down to El Capitan, so I added two options below...

#### High Sierra and Later Users:

If you use High Sierra or later, go to: **[Downloading MacTeX 2020](https://www.tug.org/mactex/mactex-download.html)** and follow instructions, or simply click: **[MacTeX.pkg](http://tug.org/cgi-bin/mactex-download/MacTeX.pkg)** right here to download and install the latest binary from same... _(Skip the Legacy Instructions below.  You are all set after opening and installing the pkg file above)_...

Now continue on to the **MacTeX Usage:** heading to follow the rest of the LaTeX environment setup instructions _(post MacTeX install)_

#### Sierra & Earlier Users:

##### Download Unix Install Archive:

All Mac Sierra and older users must go to the **MacTeX** website and follow instructions for: **[Unix Install of TeXLive 2020](https://www.tug.org/mactex/mactex-unix-download.html)**...  The instructions from Tug.org are very detailed, thorough, and informative.  

Below are the [**_Tl;DR (def: 2)_**](https://www.merriam-webster.com/dictionary/TL%3BDR) instructions we will use for our specific Mac OS case, _(supporting Mac OS back to El Capitan V10.11.6)_ 

You are strongly advised to consult the above link however to gain a deeper understanding of how all this TeX / LaTeX stuff works _(in particular for Mac users)_...

To Get Started Click this link: 

**[install-tl-unx.tar.gz](http://mirror.ctan.org/systems/texlive/tlnet/install-tl-unx.tar.gz)** to download it.  Your Mac should automatically unzip it and you will find a file named **`install-tl-unx.tar`** in your downloads folder.

Move this `.tar` or `.tar.gz` file to your desktop or your home folder and double-click it to un-compress it, producing a folder inside. It is very important to check the name of this folder. A typical name is **`"install-tl-20200527"`**. The numbers at the end give a date in the form **`year:month:day`**. Check that the year is **2020**. If it is **2019**, then you received last year's install script because the mirror assigned to you was not yet updated. Wait a few days and try again.

##### Install with Terminal App:

_(or use_ **_xTerm2_** _which is better)_

Type the following but do not hit the return key yet:

```yaml
$>  cd         # <- Type cd command, but DO NOT hit RET key!
```

While the above Terminal line is sitting there waiting, take your mouse to the GUI _(finder)_,  and drag **`install-tl-2020xxxx`** _(i.e., the folder you just created previously)_ into the terminal window and drop it there. This will automagically add some more text after the "cd".

Now, activate the Terminal command by pressing the RETURN key... This will "change directory" to the `install-tl` folder...

Now type:

```yaml
$>  sudo ./install-tl --gui tcl
```

This time press the RET key.

You will be asked for your password _(sudo)_. If you have admin status _(as most users on single owner machines do)_, type your ordinary password.

An Install Window will appear showing the **TeX Lion**. After a few moments, it will be replaced with a window titled **"TeX Live 2020 Installer"**. This window contains a pull down menu labeled **"Default paper size"** with default value **A4**. 

If you live in a region where Letter Size paper is used, change **A4** to **Letter** in this menu. All other defaults will be correct for the Macintosh. 

If your system is **`v10.12`** or earlier, the **`x86_64-darwinlegacy`** binaries will be selected for installation; otherwise the **`x86_64-darwin`** binaries will be selected. 

No worries... **_Just Push the Install Button_**.

The install window will display a long list of packages as each is downloaded, uncompressed, and installed.

> **Note:** This process will take one to two hours or more to complete, especially on older Macs!! In the end, additional messages will be printed, but these can be ignored because the final step will do them automatically.  Please be patient... Do NOT click Abort! OK? :trollface:

When all is done you will have **TeX Live 2020!** Yay! :octocat: You may now skip to the **MacTeX Usage:** heading below and follow next steps from there...

> **Note:** Occasionally, users have reported an error when using the above **`install-tl`** command. If that happens to you, do the following: _(otherwise skip below to **MacTeX Usage:**)_

Type:

```yaml
$>  sudo ./install-tl --gui text
```

and press RETURN...

The TeX Lion described in the following paragraph will NOT appear, and you'll get a slightly longer display with several questions. But all of the default answers are correct, so the only question which matters is the one about paper size. If you use **letter** size rather than **A4**, select that answer. Then select **install** and skip the next paragraph. Everything else will work the same.

###### Almost Done:

> **Note:** Installation is not quite done... A small data structure must be added so the GUI applications can easily find the **TeX** distribution. Click the link [**`TeXDist-2020`**](https://www.tug.org/mactex/TeXDist-2020.pkg) to download a small install package. Find this package in your Downloads Folder and drag it to the desktop. Double click to install it. Installation takes only a minute and then your TeX Live 2020 installation is complete! :octocat:

Now continue on to the next **MacTeX Usage:** heading to follow the rest of the LaTeX environment setup instructions _(post MacTeX install)_

#### MacTeX Usage:

> **Note:** This doc is not quite fully updated yet!  Some steps below must change now in 2020.  I am still in the process of updating all of this... Please be patient!  (use instructions below at your own risk... They may not be correct anymore...)

To make **MadTex** _(lol "a typo" but decided to leave it cause it made me laugh!)_ binaries available on the command line, _(for Pandoc etc.)_ you have to set the following in your `$PATH` definition within your shells...

```yaml
    /Library/TeX/texbin
```
Put it after your `~/.bin` and `/opt` paths but before: `/usr/bin` and `/bin` etc...


### Linux:

On Linux, _(currently Fedora)_ see: **[Fedora DOCS: LaTeX page](https://docs.fedoraproject.org/en-US/neurofedora/latex/)** for reference...

> **Note:** Other Linux platform instructions will be fourthcoming later in 2020 but it all amounts to sorting out the package manager commands etc.  Everything below is what you need on those other platforms but the package names may be different.  With a bit of research you may be able to translate the instructions below for your specific platform:

#### TexLive Package _(three choices/sizes)_

- **Install TexLive on Fedora:** Choose a package size that fits your needs... I went for `texlive-scheme-full` as I have plenty of disk space currently... The full package is over 3GB installed!  You may need to create an extended cache for DNF to use so it does not run out of disk space/memory while installing... This will take some time so you may want to plan other things to do in parallel to GTD...

Choose one of the 3 following commands _(depending on needs, disk space, etc.)_:

```yaml
$>  sudo dnf install texlive-scheme-basic
$>  sudo dnf install texlive-scheme-medium
$>  sudo dnf install texlive-scheme-full
```

#### TeXStudio

TeXStudio is a fully featured LaTeX editor that will be helpful for working on documents to make touch ups etc. _(outside of using Emacs for the same purposes)_ but it is nice to have alternates when things arn't going quite right and you are in troubleshooting mode, or maybe you just need to get a doc finished in a hurry and nothing else seems to be working for you! it's good to have options... _(open source options)_

- **Install TeXStudio on Fedora:**     
```yaml
$>  sudo dnf install TeXStudio
```

#### Rubber

For the **Modular Emacs Org Timesheet** Invoice feature to work correctly you need **`rubber`** installed... **Rubber** is a `make` like build utility for LaTeX.  It is written in Python and is not part of the packages above... 

More specifically, `rubber` is a wrapper for LaTeX and companion programs. Its purpose is, given a LaTeX source to process, to compile it enough times to  resolve all  references,  possibly  running  satellite programs such as **BibTeX**, **`makeindex`**, **Metapost**, etc. to produce appropriate data files.

After rubber is installed, read the info/man pages to find out what you can do with it...  

- **Install `rubber` on Fedora:**

```yaml
$>  sudo dnf install rubber

# read the docs
$>  info rubber
```

At this point you should be good to go... The Linux package managers take care of making sure executable paths are standard and accessable from Emacs... Most of that will be going on in the background while you are working within a **`.ORG`** file in **Emacs** to **_produce / export_** something typeset nicely with images placed, footnotes, TOC, etc.  

While you are still here, there are additional _(optional)_ packages that may be helpful as well. Feel free to install these too:

#### Additional Optional Packages:

- **`chkTeX`** LaTex semantic checker

```yaml
$>  sudo dnf install texlive-chktex
```

- **`LaTeXML`**  LaTeXML is a converter that transforms TeX and LaTeX into XML/HTML/ePub/MathML and other formats.

```yaml
$>  sudo dnf install LaTeXML
```

- **`LyX`** WYSIWYM (What You See Is What You Mean) document processor.

```yaml
$>  sudo dnf install lyx
```

- **`PyLaTeX`** Library for creating LaTeX files and snippets.

```yaml
$>  sudo dnf install python3-pylatex
```

**Note:** On Fedora, individual LaTeX packages can also be installed as required:

**_example:_**

```yaml
$>  sudo dnf install 'tex(beamer.cls)' 
$>  sudo dnf install 'tex(hyperref.sty)'
# etc...
```

## Table Of Contents:

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

- [Install Modular Emacs LaTeX pubOps Environment](#install-modular-emacs-latex-pubops-environment)
    - [Introduction:](#introduction)
    - [Install TeX Live:](#install-tex-live)
        - [Mac OS:](#mac-os)
            - [Update:](#update)
            - [High Sierra and Later Users:](#high-sierra-and-later-users)
            - [Sierra & Earlier Users:](#sierra--earlier-users)
                - [Download Unix Install Archive:](#download-unix-install-archive)
                - [Install with Terminal App:](#install-with-terminal-app)
                    - [Almost Done:](#almost-done)
            - [MacTeX Usage:](#mactex-usage)
        - [Linux:](#linux)
            - [TexLive Package _(three choices/sizes)_](#texlive-package-_three-choicessizes_)
            - [TeXStudio](#texstudio)
            - [Rubber](#rubber)
            - [Additional Optional Packages:](#additional-optional-packages)

<!-- markdown-toc end -->
