---
file: ModE-Build-MultiMarkdown-from-Src.md
author: Alisha Awen
created: 2019-010-24
updated: 2020-007-16
tags: 2020, MultiMarkdown, Emacs, apps-tools, SysAdmin, HA-ModEmacs, how-to, README 
---
<!-- #2020 #MultiMarkdown #Emacs #apps-tools #SysAdmin #HA-ModEmacs #how-to #README -->

![Banner](./media/Modular-Emacs-Github-Banner-v3.png)

# Build MultiMarkdown from Source - Linux Platforms

**[\[Table of Contents\]](#table-of-contents)**

**[\[Back To MAIN Modular Emacs README\]](../README.md)**

## Introduction:

MultiMarkdown is a utility which is used to export from Markdown to other formats beyond HTML alone.  Multimarkdown is completely stand alone.. Emacs does not require `multimarkdown` to be installed externally in order to format **_(pretty-print)_** and/or **_fontify_** the text in your markdown edit buffers.  There's an Emacs mode for Multimarkdown called **mmd-mode** which you will be installing further down within this document.  

Some of the publishing-format conversion functionality of **Multimarkdown** overlaps with **Pandoc** as well, so if you experience problems installing one of these, you will still have the other one to use.

It doesn't hurt to have both of these technologies installed on your machine for the above reasons...  You will most likely need one or the other at some point to publish something somewhere in some fancy format like LaTeX or an eBook...

## Install Multimarkdown on Linux:

On Linux many of the package managers include run-time libraries with the developer header files omitted _(stripped out to save space)_.  Apps that use these libraries work fine this way because they have already been built elsewhere with the header files included.  The developer libraries below have these header files still intact, therefore they will work for building Multimarkdown on Linux which also uses the run-time libraries...

> **Note:** It goes without mention you need a full stack GNU Development IDE and other dev tools installed on Linux in order to build apps...  Some of these tools were installed already on the machine I use to test this and because of this I may not have included specific instructions below on how to install all required libraries etc. 

> You may encounter errors if your base install does not have one or more of the basic GNU building tools and libraries already installed on your machine...  Open an issue here if you get build errors of this nature and I will include more instructions on how to install those for the benefit of everyone else... Thanks!

### Glib2 Dev Files:

Make sure you have **`glib2`** dev-files installed on your system:

#### [Debian]:

```yaml
$>  sudo apt install libglib2.0-dev
```

#### [Ubuntu]:

```yaml
$>  sudo apt install libglib2.0-dev
```

#### [Fedora]:

```yaml
$>  sudo dnf install glib2-devel
```

### Cmake & Curl

You also need **`cmake`** and **`libcurl`** to build the newer versions of MultiMarkdown:

#### [Debian]:

```yaml
$>  sudo apt install cmake libcurl-devel
```

#### [Ubuntu]:

```yaml
$>  sudo apt install cmake libcurl-devel
```

#### [Fedora]:

```yaml
$>  sudo dnf install cmake libcurl-devel
```

### Clone Multimarkdown:

Clone Multimarkdown into your **`$HOME`** or the directory where you build things:

```yaml
$>  git clone https://github.com/fletcher/MultiMarkdown-6.git  <$your-build-dir-path>/MultiMarkdown-6
```

### Run pre-build make:

_(This sets up the build directory for the actual make/build)_

```yaml
$>  cd MultiMarkdown-6
$>  make release
```

_(the above make will configure an optimized standard unix build.  There are other build configurations for other platforms or for debugging, that you could try as well but those platforms have binaries available)_

### Run Final Build:

After doing the above change directories to the **`build`** subdirectory and run make again to actually build the executible(s).

```bash
$>  cd build
$>  make
```

Create symlinks **`mmd`** and **`multimarkdown`** in **`/usr/local/bin`** that point to the multimarkdown executable witin the **`build`** sub-directory:

```yaml
$>  sudo -i
$>  cd /usr/local/bin
$>  ln -s /path/to/cloned/MultiMarkdown/repo/build/multimarkdown mmd
$>  ln -s /path/to/cloned/MultiMarkdown/repo/build/multimarkdown multimarkdown
```

That's it!  Your all done with Multimarkdown now!

**[\[Back To MAIN Modular Emacs README\]](../README.md)**

## Table of Contents:

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

- [Build MultiMarkdown from Source - Linux Platforms](#build-multimarkdown-from-source---linux-platforms)
    - [Introduction:](#introduction)
    - [Install Multimarkdown on Linux:](#install-multimarkdown-on-linux)
        - [Glib2 Dev Files:](#glib2-dev-files)
            - [[Debian]:](#debian)
            - [[Ubuntu]:](#ubuntu)
            - [[Fedora]:](#fedora)
        - [Cmake & Curl](#cmake--curl)
            - [[Debian]:](#debian-1)
            - [[Ubuntu]:](#ubuntu-1)
            - [[Fedora]:](#fedora-1)
        - [Clone Multimarkdown:](#clone-multimarkdown)
        - [Run pre-build make:](#run-pre-build-make)
        - [Run Final Build:](#run-final-build)
    - [Table of Contents:](#table-of-contents)

<!-- markdown-toc end -->


