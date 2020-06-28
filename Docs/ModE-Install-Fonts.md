---
file: ModE-Install-Fonts.md
author: Alisha Awen
created: 2020-006-27
updated: 2020-006-27
tags: Fonts, Emacs, apps-tools, SysAdmin, HA-ModEmacs, how-to, README 
---
<!-- #Fonts #Emacs #apps-tools #SysAdmin #HA-ModEmacs #how-to #README -->

![Banner](./media/Modular-Emacs-Github-Banner-v3.png)

# Modular Emacs Install Fonts Guide

**[\[Table of Contents Below\]](#table-of-contents)**

**[\[Back To Main Modular Emacs README\]](../README.md)**

## Introduction:

**Harmonic Alchemy Modular Emacs** requires a few fonts installed on your system to get Fancy Org-Mode, Fountain Screenplay, and coding frames/windows/mode-lines, etc. looking as good and "readable" as they possibly can.  Other fonts will be required by **LaTeX** _(for exporting to external document types)_.  That process will be covered in a seporate doc.

It's best to try your platform's package managers to install fonts globally system wide, and then if you cannot find the fonts needed, install them locally (manually) into your $HOME directory.

There is one special case where you need to specify and use one specific font: **_Currier Prime Emacs_** which is provided for you within this repository making it easy to install. **_(in your user's local `$HOME` environment)_** 


## Mac OS:

_(instructions comming soon, but similar to Fedora / Linux below)_











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

**[\[Table of Contents Below\]](#table-of-contents)**

**[\[Back To Main Modular Emacs README\]](../README.md)**

### Install Courier Prime Emacs:

**_Currier Prime_** is the recommended font for professional screenplays... This font is provided by the standard distro package managers, however those packages appear to cause font spacing problems in Emacs.  

I found an alternate version of Currier Prime called "**_Currier Prime Emacs_**", which is provided for you here within this repository making it easy to install in your user's `$HOME` directory...

To install this and the other fonts provided, use the **Font Manager** app as shown below _(or use another font utility if that is your preference, not shown here, as that would be TMI)_ If you don't have Font Manager already installed on Fefora, install it with the following command:

```yaml
$> sudo dnf install font-manager
```
As of this writing the above will install Font Manager v0.7.2

#### Install "Courier Prime Emacs" Font w/ Font Manager:

- Run the **Font Manager App** and choose the **`+`** button in the tool bar _(top left)_, _(Select files to install - window pops up)_.

- Navigate to the folder `~/.emacs.d/Fonts` and choose: **`CourierPrimeEmacs.ttf`**

- **Font Manager** will install it as a _User Installed_ font... and you will see it in Font Manager's **User** folder if you select that folder from the left navigation pane...

> **Note:** _(You could install these fonts manually without the aid of a Font Manager Utility, but that would require extra steps on your part so I am not including those instructios.  I leave it up to you to find out on your own how to do that, if you are interested.)_  
Google search these terms: **["install fonts manually Linux"](https://startpage.com/sp/search)**

### Install Averia and Symbola Fonts w/ Font Manager:

- Run the **Font Manager App** and choose the **`+`** button in the tool bar _(top left)_

- The _"Select files to install"_ window pops up...

- Navigate to the folder `~/.emacs.d/Fonts` and choose all remaining **`.ttf`** and `.otf` files... _(you already installed `CourierPrimeEmacs.ttf`)_

- **Font Manager** will install all of them one by one as _User Installed_ fonts. 

- Select Font Manager's **User** folder in the left navigation panel.

- You will see your newly installed fonts listed in there...








**[\[Table of Contents Below\]](#table-of-contents)**

**[\[Back To Main Modular Emacs README\]](../README.md)**

## Debian & Ubuntu:

_(instructions comming soon, but similar to Fedora / Linux above)_










**[\[Table of Contents Below\]](#table-of-contents)**

**[\[Back To Main Modular Emacs README\]](../README.md)**

## Table of Contents:

- [Modular Emacs Install Fonts Guide](#modular-emacs-install-fonts-guide)
    - [Introduction:](#introduction)
    - [Mac OS:](#mac-os)
    - [Fedora:](#fedora)
        - [Install Hermit Code Font:](#install-hermit-code-font)
        - [Install Courier Prime Emacs:](#install-courier-prime-emacs)
            - [Install "Courier Prime Emacs" Font w/ Font Manager:](#install-courier-prime-emacs-font-w-font-manager)
        - [Install Averia and Symbola Fonts w/ Font Manager:](#install-averia-and-symbola-fonts-w-font-manager)
    - [Debian & Ubuntu:](#debian--ubuntu)

**[\[Back To Main Modular Emacs README\]](../README.md)**

<!-- markdown-toc end -->
