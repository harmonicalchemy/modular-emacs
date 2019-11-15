---
file: ModE-Build-MultiMarkdown-from-Src.md
author: Alisha Awen Sheppard
created: 2019-010-24
updated: 2019-011-14
tags: 2020, MultiMarkdown, Emacs, apps-tools, SysAdmin, HA-ModEmacs, how-to, README 
---
<!-- #2020 #MultiMarkdown #Emacs #apps-tools #SysAdmin #HA-ModEmacs #how-to #README -->

# Build MultiMarkdown from Source - Linux Platforms

**[\[Back To Top README\]](../README.md)**

## Introduction:

MultiMarkdown is a utility which is used to export from Markdown to other formats beyond HTML alone.  Multimarkdown is completely stand alone.. Emacs does not require `multimarkdown` to be installed externally in order to format **_(pretty-print)_** and/or **_fontify_** the text in your markdown edit buffers.  There's an Emacs mode for Multimarkdown called **mmd-mode** which you will be installing further down within this document.  

Some of the publishing-format conversion functionality of **Multimarkdown** overlaps with **Pandoc** as well, so if you experience problems installing one of these, you will still have the other one to use.

It doesn't hurt to have both of these technologies installed on your machine for the above reasons...  You will most likely need one or the other at some point to publish something somewhere in some fancy format like LaTeX or an eBook...

## Install Multimarkdown on Linux:

Make sure you have glib2 dev-files installed on your system:  

- **[Debian]:~$ `sudo apt install libglib2.0-dev`**  


- **[Ubuntu]:~$ `sudo apt install libglib2.0-dev`**  


- **[Fedora]:~$ `sudo dnf install glib2-devel`**  

You also need **`cmake`** and **`libcurl`**to build the newer versions of MultiMarkdown...

- **[Debian]:~$ `sudo apt install cmake libcurl-devel`**  


- **[Ubuntu]:~$ `sudo apt install cmake libcurl-devel`**  


- **[Fedora]:~$ `sudo dnf install cmake libcurl-devel`**  

It goes without mention you need a full stack GNU Development IDE and other dev tools installed on Linux in order to build apps...  Some of these tools were installed already on the machine I use to test this, You may encounter errors if your base install does not have one or more of the basic GNU building tools and libraries...  Open an issue if you get them and I will include instructions here on how to install those for the benefit of everyone...

**[All-Linux]:** Clone and build Multimarkdown into your $HOME or directory where you build things:  _(version 6 as of this writing)_  

```bash
    git clone https://github.com/fletcher/MultiMarkdown-6.git  
```

**[All-Linux]:** Run pre-build make _(sets up build directory for actual make/build)_:  

```bash
    cd MultiMarkdown-6
    make release
```

_(this will configure an optimized standard unix build.  There are other build configurations for other platforms or for debugging, that you could try as well but those platforms have binaries available)_

After doing the above change directories to the **`build`** subdirectory and run make again to actually build the executible(s).  

```bash
    cd build
    make
```

Create symlinks **`mmd`** and **`multimarkdown`** in **`/usr/local/bin`** that point to the multimarkdown executable witin the **`build`** sub-directory:

```bash
    sudo -i
    cd /usr/local/bin
    ln -s /path/to/cloned/MultiMarkdown/repo/build/multimarkdown mmd
    ln -s /path/to/cloned/MultiMarkdown/repo/build/multimarkdown multimarkdown  
```

