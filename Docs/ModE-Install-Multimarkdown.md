---
file: ModE-Install-Multimarkdown.md
author: Alisha Awen
created: 2020-007-16
updated: 2020-007-16
tags: 2020, MultiMarkdown, Emacs, apps-tools, SysAdmin, HA-ModEmacs, how-to, README 
---
<!-- #2020 #MultiMarkdown #Emacs #apps-tools #SysAdmin #HA-ModEmacs #how-to #README -->

![Banner](./media/Modular-Emacs-Github-Banner-v3.png)

# Install MultiMarkdown - All Platforms

**[\[Table of Contents\]](#table-of-contents)**

**[\[Back To MAIN Modular Emacs README\]](../README.md)**


## Introduction:

MultiMarkdown is a utility which is used to export from Markdown to other formats beyond HTML alone.  Multimarkdown is completely stand alone.. Emacs does not require `multimarkdown` to be installed externally in order to format **_(pretty-print)_** and/or **_fontify_** the text in your markdown edit buffers.  There's an Emacs mode for Multimarkdown called **mmd-mode** which you will be installing further down within this document.  

Some of the publishing-format conversion functionality of **Multimarkdown** overlaps with **Pandoc** as well, so if you experience problems installing one of these, you will still have the other one to use.

It doesn't hurt to have both of these technologies installed on your machine for the above reasons...  You will most likely need one or the other at some point to publish something somewhere in some fancy format like LaTeX or an eBook...

## Install Multimarkdown on Mac OS:

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

That's it... Easy Peasy!  :octocat:

## Install Multimarkdown on Linux:

On Linux you will have to clone/configure/make/build & maintain Multimarkdown yourself... No package managers for this on Linux baby.  Sorry...  Hey... You wanted to be cool like the hackers and use Linux?  OK then... You don't need no stinking binary package managers anymore...  Right?  :trollface:  Follow this link to: **[Build MultiMarkdown from Source!](./ModE-Build-MultiMarkdown-from-Src.md)**

> **Note:** if you run into trouble building Multimarkdown, you can probably skip that as well for later... _Pandoc, (install below) will serve your **Plan B** when such **"Murphys"** happen..._

## Usage:

MultiMarkdown will provide more than enough conversion power for you as you also will be using **Pandoc** to convert to some of these same output formats... You will later also have TeX and LaTeX mode hooks to use as well...

Now that you have Multimarkdown installed, **[Download The Manual Here](https://fletcher.github.io/MultiMarkdown-5/MMD_Users_Guide.pdf)**

## Table of Contents:

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

- [Install MultiMarkdown - All Platforms](#install-multimarkdown---all-platforms)
    - [Introduction:](#introduction)
    - [Install Multimarkdown on Mac OS:](#install-multimarkdown-on-mac-os)
    - [Install Multimarkdown on Linux:](#install-multimarkdown-on-linux)
    - [Usage:](#usage)

<!-- markdown-toc end -->
