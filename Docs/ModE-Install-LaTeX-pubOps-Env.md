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

On Linux, _(currently Fedora)_ see: **[Fedora DOCS: LaTeX page](https://docs.fedoraproject.org/en-US/neurofedora/latex/)** for reference...

> **Note:** Other Linux platform instructions will be fourthcoming later in 2020 but it all amounts to sorting out the package manager commands etc.  Everything below is what you need on those other platforms but the package names may be different.  With a bit of research you may be able to translate the instructions below for your specific platform:

#### TexLive Package _(three choices/sizes)_

- **Install TexLive on Fedora:** Choose a package size that fits your needs... I went for `texlive-scheme-full` as I have plenty of disk space currently... The full package is over 3GB installed!  You may need to create an extended cache for DNF to use so it does not run out of disk space/memory while installing... This will take some time so you may want to plan other things to do in parallel to GTD...

Choose one of the 3 following commands _(depending on needs, disk space, etc.)_:

```yaml
    sudo dnf install texlive-scheme-basic
    sudo dnf install texlive-scheme-medium
    sudo dnf install texlive-scheme-full
```

#### TeXStudio

TeXStudio is a fully featured LaTeX editor that will be helpful for working on documents to make touch ups etc. _(outside of using Emacs for the same purposes)_ but it is nice to have alternates when things arn't going quite right and you are in troubleshooting mode, or maybe you just need to get a doc finished in a hurry and nothing else seems to be working for you! it's good to have options... _(open source options)_

- **Install TeXStudio on Fedora:**     
    ```yaml
    sudo dnf install TeXStudio
    ```

#### Rubber

For the **Modular Emacs Org Timesheet** Invoice feature to work correctly you need **`rubber`** installed... **Rubber** is a `make` like build utility for LaTeX.  It is written in Python and is not part of the packages above... 

More specifically, `rubber` is a wrapper for LaTeX and companion programs. Its purpose is, given a LaTeX source to process, to compile it enough times to  resolve all  references,  possibly  running  satellite programs such as **BibTeX**, **`makeindex`**, **Metapost**, etc. to produce appropriate data files.

After rubber is installed, read the info/man pages to find out what you can do with it...  

- **Install `rubber` on Fedora:**    
    ```yaml
    sudo dnf install rubber

    info rubber    # read the docs
    ```

At this point you should be good to go... The Linux package managers take care of making sure executable paths are standard and accessable from Emacs... Most of that will be going on in the background while you are working within a **`.ORG`** file in **Emacs** to **_produce / export_** something typeset nicely with images placed, footnotes, TOC, etc.  

While you are still here, there are additional _(optional)_ packages that may be helpful as well. Feel free to install these too:

#### Additional Packages _(optional)_

- **`chkTeX`** LaTex semantic checker    
   ```yaml
   sudo dnf install texlive-chktex
   ```

- **`LaTeXML`**  LaTeXML is a converter that transforms TeX and LaTeX into XML/HTML/ePub/MathML and other formats.   
   ```yaml
   sudo dnf install LaTeXML
   ```

- **`LyX`** WYSIWYM (What You See Is What You Mean) document processor.    
    ```yaml
    sudo dnf install lyx
    ```
- **`PyLaTeX`** Library for creating LaTeX files and snippets.    
    ```yaml
    sudo dnf install python3-pylatex
    ```












> **Note:** On Fedora, individual LaTeX packages can also be installed as required:   
**_example:_**   
```yaml
sudo dnf install 'tex(beamer.cls)' 
sudo dnf install 'tex(hyperref.sty)'
# etc...
```
