---
file: ModE-Install-Graphviz.md
author: Alisha Awen
created: 2020-007-16
updated: 2020-007-16
tags: 2020, Graphviz, Emacs, apps-tools, SysAdmin, HA-ModEmacs, how-to, README 
---
<!-- #2020 #Graphviz #Emacs #apps-tools #SysAdmin #HA-ModEmacs #how-to #README -->

![Banner](./media/Modular-Emacs-Github-Banner-v3.png)

# Install Graphviz - All Platforms

**[\[Back To MAIN Modular Emacs README\]](../README.md)**

## Introduction:

Harmonic Alchemy Modular Emacs comes integrated with the popular **Graphviz utility** which allows the creation of nice graphs, flowcharts, data diagrams, etc. using a powerful scripting language called **dot**...  The Emacs mode for Graphviz is: **`graphviz-dot-mode`** Graphviz is a very old and useful program _(almost as old as Emacs itself!)_ that has been supported by package managers for ages!  You should have no problem installing Graphviz...

Modular Emacs invokes: **`graphviz-dot-mode`** when you visit files ending in either: **`.dot`** or **`.gv`**

When you are visiting a **`.dot`** file, you can compile it with **`C-c C-C`** which will produce a **`.svg`** file along side...  By default, Modular Emacs produces **SVG vector files** _(instead of `.png` files)_... which is my preference because if you add one to a markdown file and then view it using **VMD-Mode** you can expand the resulting rendered chart or graph full screen and it will still look very sharp! Not to mention you can edit the resulting SVG file within **inkscape** to add things to it that Graphviz cannot...  Don't underestimate the power of **Graphviz** though!  **[Read the docs!](https://graphviz.org/documentation/)**  

You could create a nice workflow that **_starts programmatically_** within some program or language, invoke **graphviz** to produce some diagrams, export to **`SVG`**, then edit the resulting `.svg` in **inkscape** to add some fancy graphics or other things, Lastly import the final `.svg` into **Blender** to add animations, 3D, etc...  **_The sky is the limit!_**  

**Note:** You better send me an email to show me the cool thing you made on **Github** after reading this! :octocat:

## Install Graphviz on Mac OS:

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

That's it... Easy Peasy!  :octocat:

## Install Graphviz on Linux:

Graphviz is in the Debian, Ubuntu, Slackware, Arch, Fedora, NiXOS, openSUSE, and gentoo repositories so you don't have to build this one either. ;-)

- **Debian, Ubuntu flavors:**  

```bash
 $_  sudo apt install graphviz  
```

- **RedHat, Fedora flavors:**

```bash
 $_  sudo dnf install graphviz
```

That's it... Easy Peasy!  :octocat:

**[\[Back To Main Modular Emacs README\]](../README.md)**
