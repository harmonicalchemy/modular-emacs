---
file: ModE-Build-Emacs-from-Src.md
author: Alisha Awen
created: 2019-010-19
updated: 2020-007-16
tags: Emacs, apps-tools, SysAdmin, HA-ModEmacs, how-to, README 
---
<!-- #Emacs #apps-tools #SysAdmin #HA-ModEmacs #how-to #README -->

![Banner](./media/Modular-Emacs-Github-Banner-v3.png)

# Modular Emacs - Optional Build Emacs from Source

**[\[Mac OS Users Start Here\]](#mac-os)**

**[\[Linux Users Start Here\]](#linux)**

**[\[Table of Contents\]](#table-of-contents)**

**[\[Back To MAIN Modular Emacs README\]](../README.md)**

## Introduction:

Go ahead... bite the bullet and install the latest Emacs from source. Building from source is tedious, _especially if you are doing it for the first time! You WILL have to try many times before getting all those "twisty little snippets all different", sorted out and resolved._  If you stick to your guns and stay persistant, you will be awesome!  You will also be in total control of your personal and OFFICIAL build of GNU Emacs, how it works, and what it does!  Your second build experience will go much more smoothly...  By your third build the process the task will become a boring routine of typing a few commands in the terminal, and then waiting as your machine cranks away a brand new version for you...  Play **_mind_ sweeper** with your smart spyPhone, or read a book about exploding brain synapses while your computer's busy making free-as-in-liberated Emacs...

## Mac OS:

The Mac OS was, and still is a nice environment to work _(and create)_ in... I loved the NextStep `NS` environment all the way back to when it was first created at **NeXT** And... Being a musician, composer, programmer and member of the **BCS NeXT SIG**, in those early days, I was in collaboration with the digital audio developers at NeXT during that time as well! _(I typed that from memory... hope I got the camel case right. lol)_.

Now, I am not sure which direction Apple is taking...  The design of Mac OS morphing into iOS and visa versa makes it not so much of a unix anymore (also Mac OS never had a proper unix File System anyway)... I remember it was the file system and risky R/W optical disks which were the Achelies heel of the **NeXT** computer.  I am quite frightened by the new **APFS** spec Apple is throwing out!

Unfortunately I am dependent on Mac OS For digital music/sound, video, graphic design, etc. I currently use a late 2015 27" retina iMac.  All of my other computing work is done on a reasonably secure Qubes _(personal hypervisor)_ Laptop that runs different flavors of Linux as App VMs... My Qubes laptop is used for writing, coding, business, personal...  

> **Update 2020-009-11:** _I upgraded my iMac [Rodrigo] to High Sierra v10.13.6 in August! So far, it has been a mess to reconfigure things.  I have to upgrade my current Emacs 27 now.  The new Emacs beyond V27.1 is supposed to be a long awaited update to past versions of Emacs.  Imagmagick and Ghostscript are no longer used, and they are buggy and introduce security holes! No problems with APFS so far, so that may not be an issue for me.  Compatability with DAW software will be critical to test though!_

On my Qubes Laptop, Emacs works great, straight from any of the above package managers _(as you can see above...)_  

Mac OS (**Darwin**) has always been a mongrel cross-breed of different unix flavors. _(by now all the unix are mixed up pretty much however... so that's not it...)_  The problem is all the non-standard junk that Apple does that tends to break standard unix things if those are also installed!  OMG! Hair pulling time!

Long story short... My older Emacs install was causing all my woes... Homebrew stopped building versions that I needed for my Emacs configuration to work correctly. _(some of the problems were also shell startup related)_.  After experiencing of all of that over the past year, and coming to the realization that I shoulda/coulda used **MacPorts** instead of Homebrew, but not daring to change all that now mid stream of course... So... on MacOS I build Emacs from source from now on... Straight from a cloned repository from Gnu's Git Server!  I can get the bleeding edge if I want now... :stuck_out_tongue:

### Install Emacs from Source - Mac OS

> **Sept 2020 Update:**  This doc is currently being changed as I go through the process of building/upgrading my current Emacs on Mac OS from v27 to v28 on Mac OS High Sierra v10.13.6.  (I may decide to upgrade my mac to the latest OS and have to do this all over again... Whatever...)  The updated environment will not work for building Emacs 26 because I removed many requirements that introduce security holes... From now on, at least on Mac OS it will be at least Emacs v27.1 or rather the latest bleeding edge from the repo...

> Also, since I no longer use Homebrew the instructions for Homebrew users below has not been updated since 2019! I am not sure those instructions are valid anymore and they have not been tested as well.. Some of the Homebrew commands for installing libraries etc. are missing from this doc now... Sorry.. I need help with this now... Any takers?  Thanks! Homebrew is still a viable option for most users... I like **MacPorts**...

> **Note:** We are going to build a fast moving/changing target: Monster Lisp Machine with lots of little bitty _loaded.el bugs_ they are fixing and working on all the time! It's important to set up a scheme allowing for quick updates to keep in sync and get those bug fixes, _(possibly bugs that are blocking you from making a successful build!)_  OK **_MacCowboys_** and **_MacCowgirls_**... Are you ready for this? Strap in... Or should I say.. Bootstrap! :octocat:

#### Prerequisites:

##### Install Autoconf & Automake:

`autoconf` is a dependency of `automake`  
You can install both packages by installing Automake alone...

Install Automake _(and Autoconf)_ via Homebrew:

```yaml
$>  brew install autoconf automake
```

Install Automake _(and Autoconf)_ via **MacPorts**:

```yaml
$>  sudo port install automake
```

- **Check `autoconf` version:**  - Check that it is at least the version specified near the start of **`configure.ac`** _(in the_ **`AC_PREREQ`** _command)._  **`V2.65`** or greater is required as of `2019-006-01`.  The Homebrew version at same time is: **`V2.69`** Whew! That was close! :trollface:

##### git:

Check that `git` is at least **`Git 1.7.1`**.  If you cloned this repository with an older **Git** version, you may need to reclone it after upgrading `git`.  The version of `git` at the time I had **Homebrew** installed back in 2019 was **`V2.21.0`** so you are all set if you **`update/upgrade`** Homebrew today!

```yaml
$>  brew install git
```

Install `git` via **MacPorts**:

```yaml
$> sudo port install git
```

You should be all set with **git** in 2020 now...


##### GNU Make

`gmake V4.2.1` or later is currently available via **MacPorts**.  It is   required to build Emacs from source.  _(any version)_

```yaml
$> sudo port install gmake
```

##### makeinfo:

This is not strictly necessary, but highly recommended, so that you can build the manuals. **makeinfo** is bundled as part of **GNU Texinfo**.  You can install **texinfo** with **Homebrew** as well:

```yaml
$>  brew install texinfo
```

Install `texinfo` via **MacPorts**:

```yaml
$>  sudo port install texinfo
```

Make sure your installed **Texinfo** is: **`V4.13`** or later to work with this build... No problem... Homebrew version is currently **`V6.6`**   _(as of 2019-006-01 - I have not looked at what Homebrew currently provides for newer Emacs builds, but I am fairly confident the above will still work fine for you.)_

##### Prerequisites for Emacs V27.1+:

###### GMP:

The GNU Multiple Precision Library `libgmp` is now needed.

Install `gmp` via **MacPorts**:

```yaml
$> sudo port install gmp
```

###### Without ImageMagick Default!

Emacs 27+ now supports resizing & rotating images without ImageMagick... All modern systems support this new feature.  You no longer need to install ImageMagick or use it ever again... Makes me sad as this was the only really nice command line image manipulation software for a long long time!

Emacs no longer defaults to using ImageMagick to display images.
This is due to security and stability concerns with ImageMagick.  To override the default, use `configure --with-imagemagick`.  But don't do that... Go with the new image configuration for Emacs 27.1 and newer...

- **Note:** On GNU and Unix systems, Cairo drawing or the XRender extension to X11 is now required for image manipulation to be available in Emacs; the configure script will test for it and, if found, enable scaling. The new function 'image-transforms-p' can be used to test whether any given frame supports these capabilities.

###### Cairo

The configure option: `--with-cairo` is no longer experimental. This builds Emacs with Cairo drawing, and supports built-in printing when Emacs is built with GTK+.  Some severe bugs in this build were fixed, and we can therefore offer this to users without caveats.  Note that building with Cairo enabled results in using `Pango` instead of `libXft` for font support, and that `Pango v1.44` has removed support for bitmapped fonts.

You will need to install Cairo Runtime for Emacs to link up with during the build...

Install `cairo` via **MacPorts**:

```yaml
$> sudo port install cairo
```

###### GTK

Emacs now requires >GTK 2.24 or >GTK 3.10 for the GTK 2 and GTK 3 builds respectively.

Install `gtk2` via **MacPorts**:

```yaml
$> sudo port install gtk2
```

Install `gtk3` via **MacPorts**:

```yaml
$> sudo port install gtk3
```

Install `libpng` via **MacPorts**:

```yaml
$> sudo port install libpng
```

Install `librsvg` via **MacPorts**:

```yaml
$> sudo port install librsvg
```

##### Emacs can now be configured using an early init file!

> **Important Note:** This is important news and needs to be paid attention to in order to ensure your system starts up properly!

The file is called **`early-init.el`**, in: `user-emacs-directory`.  It is loaded very early in the startup process: before graphical elements such as the tool bar are initialized, and before the package manager is initialized.  The primary purpose is to allow customizing how the package system is initialized given that initialization now happens before loading the regular init file (see below).

The Emacs dev team recommends against putting any customizations in this file that
doesn't need to be set up before initializing installed add-on packages, because the early init file is read too early into the startup process, and some important parts of the Emacs session, such as 'window-system' and other GUI features, are not yet set up, which could
make some customization fail to work.

###### Installed packages are now activated _before_ loading init:

As a result of this change, it is no longer necessary to call
`package-initialize` in your init file.

Previously, a call to `package-initialize` was automatically inserted
into the init file when Emacs was started.  This call can now safely
be removed.  Alternatively, if you want to ensure that your init file
is still compatible with earlier versions of Emacs, change it to

```lisp
   (when (< emacs-major-version 27)
     (package-initialize))
```

However, if your init file changes the values of `package-load-list`
or `package-user-dir`, or sets `package-enable-at-startup` to nil then
it won't work right without some adjustment:

- You can move that code to the early init file (see above), so those
  settings apply before Emacs tries to activate the packages.

- You can use the new 'package-quickstart' so activation of packages
  does not need to pay attention to 'package-load-list' or
  'package-user-dir' any more.

#### Clone `GNU Emacs Repo @ Savannah.gnu.org:`

You could do this within a dedicated **`Dev`** folder because you will most likely want to maintain a local clone so you can come back later to build again when you need to upgrade or drop back to a more stable version...  I created my own local _untracked_ branch and keep my local `tracking` branch clean...   This helps speed up the build-problems-debug-re-build-till-it-works cycle...  

```yaml
$>  git clone https://git.savannah.gnu.org/git/emacs.git
```

> **_btw:_**  So far all these instructions are universal to build the entire Emacs world on any platform, Mac, Linux, BSD, Windows, Tests, etc.  You have the cloned repo.  Try: **`git branch -a`** and you will see what I mean!

#### Create Local Build Branch for Emacs:

```yaml
$>  git fetch master
$>  git pull master
$>  git pull --tags
$>  git branch -d rodrigo  # create local un-tracked branch in sync with HEAD...
```

You are now in sync with the latest commits to Emacs Latest Bleeding Edge from the Developers! _(commits beyond the last tag release)_ so you have the latest possible build! You can see the latest tags with:

```yaml
$>  git tag
```

To use the autotools: Run the following shell command within your cloned **`emacs`** directory:  

```yaml
$>  cd emacs 
$>  ./autogen.sh
```

This will generate the **`configure`** script and some related files, and to set up your git configuration...  

#### Run Configure:

To get all the needed features for **Harmonic Alchemy Modular Emacs** run `configure` with the following switches set for building the new Emacs 27.1:

```yaml
$>  ./configure --with-ns --with-mailutils --with-cairo
```

Actually you can simply run `./configure` alone those options are default for a new Mac that has GNU mailutils installed, as well as all the required graphics libraries etc. on it...  If those things are not installed, it does not matter if you require them on the configure line... Your build will fail if make cannot find needed libraries etc...

#### Make Bootstrap: _(does a more thourough job)_

The **Bootstrap make** is quite **`CPU`** intensive... If your Mac can _(4 cores? no prob!)_ fan won't even twitch? Maybe... :octocat: So if you don't mind waiting, this is the best way to build according to the GNU dudes...

```yaml
$>  make bootstrap
```

#### Make Install! _(Make the Emacs Mac App package!)_

```yaml
$>  make install
```

#### Move your shiny new Emacs.app to: `$HOME/Applications:`

```yaml
$>  mv nextstep/Emacs.app ~/Applications/Emacs-Vxx.app    
# where xx=the version number of this Emacs build...
```

#### Launch Emacs from your Apps Folder/Menu:

Launch Emacs for the first time...  If it runs you can check the version with **`C-h C-a`**... If you have problems? Go back to the top of this **_squirrel cage_** and start over... :trollface:

#### Revert Repo back to fresh clone state to start over:

If your build was successful, you don't need to do this now... Wait until you need to build again...  However if your build went bad... This is the way to start completely over... 

```yaml
$> git clean -fdx
```

#### Troubleshooting Debugging:

My build went well because I planned well this time... _(i.e., you did not see the big goofs I made before writing this... lol)_  Because of that I am now running Emacs v28.0.50 on my iMac without Imagemagick or any of the older less secure dependencies involved... Lets see how this works on Mac OS now... To Be Continued...

## Linux:

### Install Emacs from Source - Debian & Ubuntu: ###

#### Prerequisites  

##### Install `git`

Check that `git` is at least **`Git 1.7.1`**.  If you already cloned the Emacs repository with an older **Git** version, you may need to reclone it after upgrading `git`.

##### Install `Autoconf`:

Make sure `Autoconf` is at least the version specified near the start of **`configure.ac`** _(in the_ **`AC_PREREQ`** _command)._  **`V2.65`** or greater is required as of `2019-006-16`.

```yaml
$>  sudo apt install autoconf
```

##### Install `Automake`:

```yaml
$>  sudo apt install automake
```

##### Install `autotools-dev`:

```yaml
$>  sudo apt install autotools-dev
```

##### Install `libtool`:

```yaml
$>  sudo apt install libtool
```

##### Install `makeinfo`:

This is not strictly necessary, but highly recommended, so that you can build the manuals. **makeinfo** is bundled as part of **GNU Texinfo**.  Make sure your installed **Texinfo** is: **`V4.13`** or later to work with this build...  

```yaml
$>  sudo apt install texinfo
```

##### Install `build-essential`:

```yaml
$>  sudo apt install build-essential
```

##### Install `xorg-dev`:

```yaml
$>  sudo apt install xorg-dev
```

##### Install `libgtk2.0-dev`:

```yaml
$>  sudo apt install libgtk2.0-dev
```

##### Install `libjpeg-dev`:

```yaml
$>  sudo apt install libjpeg-dev
```

##### Install `libncurses5-dev`:

```yaml
$>  sudo apt install libncurses5-dev
```

##### Install `libdbus-1-dev`:

```yaml
$>  sudo apt install libdbus-1-dev
```

##### Install `libgif-dev`:

```yaml
$>  sudo apt install libgif-dev
```

##### Install `libtiff-dev`:

```yaml
$>  sudo apt install libtiff-dev
```

##### Install `libm17n-dev`:

```yaml
$>  sudo apt install libm17n-dev
```

##### Install `libpng-dev`:

```yaml
$>  sudo apt install libpng-dev
```

##### Install `librsvg2-dev`:

```yaml
$>  sudo apt install librsvg2-dev
```

##### Install `libotf-dev`:

```yaml
$>  sudo apt install libotf-dev
```

##### Install `libgnutls28-dev`:

```yaml
$>  sudo apt install libgnutls28-dev
```

##### Install `libxml2-dev`:

```yaml
$>  sudo apt install libxml2-dev
```

#### Clone `GNU Emacs Repo @ Savannah.gnu.org`:

You could do this within a dedicated **`Dev`** folder because you will most likely want to maintain a local clone so you can come back later to build again when you need to upgrade or drop back to a more stable version...  I created my own local _untracked_ branch and keep my local `tracking` branch clean...   This helps speed up the build-problems-debug-re-build-till-it-works cycle...  

```yaml
$>  git clone https://git.savannah.gnu.org/git/emacs.git
$>  cd emacs
$>  git branch -a                         # prints a long list of remote branches...
$>  git fetch origin emacs-26             # we are interested in building emacs 26
$>  git checkout --track origin/emacs-26
$>  git pull origin emacs-26
$>  git checkout -b my-local-branch       # A smart git practice to get into habit...
```

> **_btw:_**  **`git branch -a`** will reveal all the universal build options for the entire Emacs world on any platform, Mac, Linux, BSD, Windows, Tests, etc. You could build it all from here I imagine!  But here we are only interested in the latest stable Linux release at the time of cloning...

#### Set up Autotools:  

To use the autotools: Run the following shell command within your cloned **`emacs`** directory:  

```yaml
$>  cd emacs 
$>  ./autogen.sh
```

The last bit of output of the above running shell script should look like this:

```yaml
    Installing git hooks...
    'build-aux/git-hooks/commit-msg' -> '.git/hooks/commit-msg'
    'build-aux/git-hooks/pre-commit' -> '.git/hooks/pre-commit'
    'build-aux/git-hooks/prepare-commit-msg' -> '.git/hooks/prepare-commit-msg'
    '.git/hooks/applypatch-msg.sample' -> '.git/hooks/applypatch-msg'
    '.git/hooks/pre-applypatch.sample' -> '.git/hooks/pre-applypatch'
    You can now run './configure'.
```

If you see the above it was successful!  The above script generated the **`configure`** script and some related files, and set up your git configuration...    Now you can move on to configure your specific build...

#### Run Configure:  

To get all the features I wish package managers would take the time to compile in for us, run **`configure`** with the following switches set:

```yaml
$> ./configure --with-imagemagick --with-mailutils\
   --with-gnutils --with-modules --with-rsvg --with-dbus\
   --with-xml2
```

You probably don't have to be so specific  _(as above)_ and probably could just get away with using `./configure` alone... The build process is smart and most likely will give you all the things you need without asking explicitly...  But I asked explicitly above anyway...  Then I know for sure. 

_To see a list of other available options, run this command:_

```yaml
./configure --help
```

#### Make Bootstrap: _(does a more thourough job)

The **Bootstrap make** is quite **`CPU`** intensive... If your laptop can do it... _(4 cores? no prob!)_ fan won't even twitch? Maybe... :octocat: So if you don't mind waiting, this is the best way to build according to the GNU dudes...

```yaml
$>  make bootstrap
```

#### Make Install! _(Make the Linux App!)

Do this with `"sudo"` to get the Linux Emacs app installed in `/usr/local/bin`

```yaml
$>  sudo make install
```

Occasionally the file `lisp/loaddefs.el` (and similar automatically generated files, such as `esh-groups.el` and `*-loaddefs.el` in some subdirectories of 'lisp/', e.g., 'mh-e/' and 'calendar/') will need to be updated to reflect new autoloaded functions.  If you see errors (rather than warnings) about undefined lisp functions during compilation, that may be the reason.  Finally, sometimes there can be build failures related to `*loaddefs.el` _(e.g., "required feature ‘esh-groups’ was not provided")_.  In that case, update loaddefs.el (and similar files), as follows:

```yaml
$>  cd lisp
$>  make autoloads
```

doing `make bootstrap` as above should eliminate any of the above problems however...

#### Launch Emacs:

With Emacs installed in /usr/local/bin, you can launch Emacs from any command line.  You could also create a start menu item/icon as well.  I don't bother with that in a Qubes environment... So no write-up on that for now... Start Emacs from the terminal.  It will pop up the GUI window... No problem... Once your Emacs build is running, you can check the version with **`C-h C-a`**...

If you have problems with your build?  Oh My! Do the next step below and then Go back to the top of this **_squirrel cage_** and start over fresh.  Read carefully... :trollface:

#### Revert Repo back to fresh clone state to start over:  

If your build was successful, you don't need to do this now... Wait until you need to build again...  However if your build went bad... This is the way to start completely over... 

```yaml
$>  git clean -fdx
```

#### Troubleshooting Debugging:

My build went well because I planned well this time... _(i.e., you did not see the big goofs I made trying to build this on a Mac! lol)_ Also I was very explicit about required developer libraries which probably stopped a lot of problems!  Because of that I am now running Emacs V26.2.90 on Debian 9 now with Imagemagick, and all my favorite bells and whistles!  **_Caveat:_** I have to manage builds now.. Oh well... it felt good getting that monster to build! _(even after the second, third, times)_  :octocat:

## Table of Contents:

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

- [Modular Emacs - Optional Build Emacs from Source](#modular-emacs---optional-build-emacs-from-source)
    - [Introduction:](#introduction)
    - [Mac OS:](#mac-os)
        - [Install Emacs from Source - Mac OS](#install-emacs-from-source---mac-os)
            - [Prerequisites:](#prerequisites)
                - [Install Autoconf & Automake:](#install-autoconf--automake)
                - [git:](#git)
                - [GNU Make](#gnu-make)
                - [makeinfo:](#makeinfo)
                - [Prerequisites for Emacs V27+:](#prerequisites-for-emacs-v27)
                    - [GMP:](#gmp)
                    - [Without ImageMagick Default!](#without-imagemagick-default)
                    - [Cairo](#cairo)
                    - [GTK](#gtk)
                - [Emacs can now be configured using an early init file!](#emacs-can-now-be-configured-using-an-early-init-file)
                    - [Installed packages are now activated _before_ loading init:](#installed-packages-are-now-activated-_before_-loading-init)
            - [Clone `GNU Emacs Repo @ Savannah.gnu.org:`](#clone-gnu-emacs-repo--savannahgnuorg)
            - [Set up Autotools:](#set-up-autotools)
            - [Run Configure:](#run-configure)
            - [Make Bootstrap: _(does a more thourough job)_](#make-bootstrap-_does-a-more-thourough-job_)
            - [Make Install! _(Make the Emacs Mac App package!)_](#make-install-_make-the-emacs-mac-app-package_)
            - [Move your shiny new Emacs.app to: `$HOME/Applications:`](#move-your-shiny-new-emacsapp-to-homeapplications)
            - [Launch Emacs from your Apps Folder/Menu:](#launch-emacs-from-your-apps-foldermenu)
            - [Revert Repo back to fresh clone state to start over:](#revert-repo-back-to-fresh-clone-state-to-start-over)
            - [Troubleshooting Debugging:](#troubleshooting-debugging)
    - [Linux:](#linux)
        - [Install Emacs from Source - Debian & Ubuntu:](#install-emacs-from-source---debian--ubuntu)
            - [Prerequisites](#prerequisites)
                - [Install `git`](#install-git)
                - [Install `Autoconf`:](#install-autoconf)
                - [Install `Automake`:](#install-automake)
                - [Install `autotools-dev`:](#install-autotools-dev)
                - [Install `libtool`:](#install-libtool)
                - [Install `makeinfo`:](#install-makeinfo)
                - [Install `build-essential`:](#install-build-essential)
                - [Install `xorg-dev`:](#install-xorg-dev)
                - [Install `libgtk2.0-dev`:](#install-libgtk20-dev)
                - [Install `libjpeg-dev`:](#install-libjpeg-dev)
                - [Install `libncurses5-dev`:](#install-libncurses5-dev)
                - [Install `libdbus-1-dev`:](#install-libdbus-1-dev)
                - [Install `libgif-dev`:](#install-libgif-dev)
                - [Install `libtiff-dev`:](#install-libtiff-dev)
                - [Install `libm17n-dev`:](#install-libm17n-dev)
                - [Install `libpng-dev`:](#install-libpng-dev)
                - [Install `librsvg2-dev`:](#install-librsvg2-dev)
                - [Install `libotf-dev`:](#install-libotf-dev)
                - [Install `libgnutls28-dev`:](#install-libgnutls28-dev)
                - [Install `libxml2-dev`:](#install-libxml2-dev)
            - [Clone `GNU Emacs Repo @ Savannah.gnu.org`:](#clone-gnu-emacs-repo--savannahgnuorg)
            - [Set up Autotools:](#set-up-autotools-1)
            - [Run Configure:](#run-configure-1)
            - [Make Bootstrap: _(does a more thourough job)](#make-bootstrap-_does-a-more-thourough-job)
            - [Make Install! _(Make the Linux App!)](#make-install-_make-the-linux-app)
            - [Launch Emacs:](#launch-emacs)
            - [Revert Repo back to fresh clone state to start over:](#revert-repo-back-to-fresh-clone-state-to-start-over-1)
            - [Troubleshooting Debugging:](#troubleshooting-debugging-1)
    - [Table of Contents:](#table-of-contents)

<!-- markdown-toc end -->
