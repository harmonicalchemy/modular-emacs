---
file: ModE-Multimedia-Guide.md
author: Alisha Awen
created: 2020-007-19
updated: 2020-007-19
tags: Emacs, Multimedia, EMMS, MPD, MPC, apps-tools, HA-ModEmacs, how-to, README 
---
<!-- #Emacs #Multimedia #EMMS #MPC #MPC #apps-tools #HA-ModEmacs #how-to #README -->

![Banner](./media/Modular-Emacs-Github-Banner-v3.png)

# Multimedia Environment Configuration Guide:

**[\[Table of Contents\]](#table-of-contents)**

**[\[Back to MAIN Modular Emacs README\]](../README.md)**

## Introduction:

Harmonic Alchemy Modular Emacs optional module 11-games-pkg-conf.el currently installs and configures both **RMOO** _(an Emacs based MOO client)_ from my own fork of the project, and the **EMMS** _(Emacs MultiMedia System)_ package from ELPA.

## RMOO:

More about this later...  I need to get EMMS and MPD working smoothly first. ;-)

## EMMS:

The EMMS configuration uses MPD (Music Player Daemon) for it's back end...  I use Macports to install this on Mac OS.  On Linux there are other options which will be discussed here as well...

### MPD & MPC:

**MPD** _(Music Player Daemon)_ is required, but **MPC** _(Music Player Client)_ is only optional for command line...  Mostly for troubleshooting... You don't really need it for anything else...

#### Install MPD on Mac OS:

##### MacPorts:

```yaml
$>  sudo port install mpd
```

##### Homebrew:

```yaml
$>  brew install mpd
```
#### Install MPC on Mac OS:

##### MacPorts:

```yaml
$>  sudo port install mpc
```

##### Homebrew:

```yaml
$>  brew install mpc
```





## Table Of Contents:
