---
file: ModE-Install-Fonts.md
author: Alisha Awen
created: 2020-006-27
updated: 2020-007-16
tags: Fonts, Emacs, apps-tools, SysAdmin, HA-ModEmacs, how-to, README 
---
<!-- #Fonts #Emacs #apps-tools #SysAdmin #HA-ModEmacs #how-to #README -->

![Banner](./media/Modular-Emacs-Github-Banner-v3.png)

# Modular Emacs Install Fonts Guide

**[\[Table of Contents\]](#table-of-contents)**

**[\[Back To Main Modular Emacs README\]](../README.md)**

## Introduction:

**Harmonic Alchemy Modular Emacs** requires a few fonts installed on your system to get Fancy Org-Mode, Fountain Screenplay, and coding frames/windows/mode-lines, etc. looking as good and "readable" as they possibly can.  Other fonts will be required by **LaTeX** _(for exporting to external document types)_.  That process will be covered in a seporate doc.

It's best to try your platform's package managers to install fonts globally system wide, and then if you cannot find the fonts needed, install them locally (manually) into your $HOME directory.

In addition to getting these fonts from package managers, I have also included them within this repository for your convenience.

There is one special case where you need to specify and use one specific font: **_Currier Prime Emacs_** _(rather than the more common Courier Prime)_, This font is also provided for you within this repository making it easy to install.

## Mac OS:

Mac Users are encouraged to simply install all of the fonts provided within this repository's **`./Fonts`** folder, using **Font Book**, or simply manually by dragging them into your **System Fonts** folder. Once they are installed, open **Font Book** and look to see that they are all present and accounted for...

**[\[Table of Contents\]](#table-of-contents)**

**[\[Back To Main Modular Emacs README\]](../README.md)**

## Fedora:

### Install Hermit Code Font:

This font is thankfully available for installing system-wide using **`dnf`**:

```yaml
$> sudo dnf install pcaro-hermit-fonts
```
That's it.. you are all set for Hermit as your default "coding" font in Emacs.  

> **Note:** If you don't like the Hermit font for coding you will have to edit the following files: _(Override these default files for customization by copying _(cloning)_ them from: **`lisp/modules`** to: **`lisp/my-modules`**)_.
- **`02-package-conf.el`** 
- **`06-interface.el`** 
- **`09-1-org-beautify-conf.el`** 
- **`09-2-org-keywords-tags-conf.el`**

> **Edit your Cloned Files:** Search for every instance of: **`:family "Hermit"`** and replace the single word `Hermit` with: 'Your-Chosen-coding-font'. Make sure you have changed every instance of the word Hermit.  Some files contain it more than once... Also, make sure you have also installed your chosen coding font on your machine as well of course...


### Install Better Looking Fonts for Fedora:


I was having problems with Courier Prime Emacs _(default for `org-mode` on Fedora)_... Seems there is no Italic option...  I could not find a way to install Courier Prime _(the original)_ on Fedora until **[I found this post on Reddit:](https://www.reddit.com/r/Fedora/comments/5nfenw/better_looking_fonts_for_fedora/)**

**[Here is the Github Project Page - Better fonts for Fedora](https://github.com/silenc3r/fedora-better-fonts)**

#### Install Better Looking Fonts for Fedora:

This will install the original Courier Prime (with italic face) and a bunch of other great font replacements for default system installed fonts!  To do this you have to enable RPM Fusion Repo and then enable the COPR repo...

``` conf
## Enable RPM Fusion Repo first:

alice@home$ sudo dnf install https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm

## Next Enable COPR Better Fonts Repo:

alice@home$ sudo dnf copr enable dawid/better_fonts

## Install the Font Packages:

alice@home$ sudo dnf install fontconfig-enhanced-defaults fontconfig-font-replacements
```

The above replaced many System level fonts, etc...  To ensure all changes are set, you need to log out and back in or _(better)_ reboot your computer to ensure all new fonts are properly set and old ones are not...  

> **Note:** I changed **`09-1-org-beautify-conf.el`** so that **Courier Prime** _(not Courier Prime Emacs)_ is used on Linux now... On **Fedora** you need to have the above fonts package installed in order to see **`org-mode`** default text correctly... I have not tried this on Debian yet... _(next on my list for 2021)_

**[\[Table of Contents\]](#table-of-contents)**

**[\[Back To Main Modular Emacs README\]](../README.md)**

### Install Font Manager GUI (optional):

**Font Manager** is an option if you like installing fonts via a **GUI**.  As of this writing I don't think Font Manager is available on Fedora via DNF.  However by default, you can double click on any font in the file manager that you downloaded and unzipped, etc. and that will bring up the default "Fonts" app where you can choose to install the font...

I will write up more about using Font Manager and/or other ways to install fonts on Debian later in 2021...


#### Install "Courier Prime Emacs" Font w/ Font Manager:

> **Note:** On Fedora, it is better to install "Better Fonts" as above... These instructions are for other Linux based OSs...

**_Currier Prime_** is the recommended font for professional screenplays... This font is provided by the standard distro package managers, however those packages appear to cause font spacing problems in Emacs.  

I found an alternate version of Currier Prime called "**_Currier Prime Emacs_**", which is provided for you here within this repository making it easy to install in your user's `$HOME` directory...

- Run the **Font Manager App** and choose the **`+`** button in the tool bar _(top left)_, _(Select files to install - window pops up)_.

- Navigate to the folder `~/.emacs.d/Fonts/Courier-Prime-Emacs` and choose: **`CourierPrimeEmacs.ttf`**

- **Font Manager** will install it as a _User Installed_ font... and you will see it in Font Manager's **User** folder if you select that folder from the left navigation pane...

> **Note:** _(You could install these fonts manually without the aid of a Font Manager Utility, but that would require extra steps on your part so I am not including those instructions.  I leave it up to you to find out on your own how to do that, if you are interested.)_  
Google search these terms: **["install fonts manually Linux"](https://startpage.com/sp/search)**

#### Install Averia and Symbola Fonts w/ Font Manager:

- Run the **Font Manager App** and choose the **`+`** button in the tool bar _(top left)_

- The _"Select files to install"_ window pops up...

- Navigate to the folder `~/.emacs.d/Fonts/` and choose all remaining font folders _(of the fonts you have not yet installed)_ and install all **`.ttf`** and/or `.otf` files you find... _(you already installed `CourierPrimeEmacs.ttf` btw)_

- **Font Manager** will install all of them one by one as _User Installed_ fonts. 

- Select Font Manager's **User** folder in the left navigation panel.

- You will see your newly installed fonts listed in there...

**[\[Table of Contents\]](#table-of-contents)**

**[\[Back To Main Modular Emacs README\]](../README.md)**

## Debian & Ubuntu:

I have not used the package managers to install fonts for Debian and Ubuntu because I chose to instead install them directly from this repository's **`./Fonts`** folder using Debian's standard font install utilities...  Therefore I have not yet looked up the names for the proper font packages to install system wide.  I may update this doc later with more detailed instructions for installing from package managers...

For now, install these fonts manually in a similar way that they are installed on a Mac... The name of your font manager is not **Font Book** though... ;-)

Once they are installed, open **your font manager of choice** and look to see that they are all present and accounted for...

## Table of Contents:

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

- [Modular Emacs Install Fonts Guide](#modular-emacs-install-fonts-guide)
    - [Introduction:](#introduction)
    - [Mac OS:](#mac-os)
    - [Fedora:](#fedora)
        - [Install Hermit Code Font:](#install-hermit-code-font)
        - [Install Better Looking Fonts for Fedora:](#install-better-looking-fonts-for-fedora)
            - [Install Better Looking Fonts for Fedora:](#install-better-looking-fonts-for-fedora-1)
        - [Install Font Manager GUI (optional):](#install-font-manager-gui-optional)
            - [Install "Courier Prime Emacs" Font w/ Font Manager:](#install-courier-prime-emacs-font-w-font-manager)
            - [Install Averia and Symbola Fonts w/ Font Manager:](#install-averia-and-symbola-fonts-w-font-manager)
    - [Debian & Ubuntu:](#debian--ubuntu)
    - [Table of Contents:](#table-of-contents)

<!-- markdown-toc end -->



**[\[Back To Main Modular Emacs README\]](../README.md)**
