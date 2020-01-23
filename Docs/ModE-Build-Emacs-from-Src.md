---
file: ModE-Build-Emacs-from-Src.md
author: Alisha Awen Sheppard
created: 2019-010-19
updated: 2019-011-14
tags: Emacs, apps-tools, SysAdmin, HA-ModEmacs, how-to, README 
---
<!-- #Emacs #apps-tools #SysAdmin #HA-ModEmacs #how-to #README -->

![Banner](./media/Modular-Emacs-Github-Banner-v3.png)

# Modular Emacs - Optional Build Emacs from Source

**[\[Back To Top README\]](../README.md)**

## Introduction:

Go ahead... bite the bullet and install the latest Emacs from source. Building from source is tedious, _especially if you are doing it for the first time! You WILL have to try many times before getting all those "twisty little snippets all different", sorted out and resolved._  If you stick to your guns and stay persistant, you will be awesome!  You will also be in total control of your personal and OFFICIAL build of GNU Emacs, how it works, and what it does!  Your second build experience will go much more smoothly...  By your third build the process the task will become a boring routine of typing a few commands in the terminal, and then waiting as your machine cranks away a brand new version for you...  Play **_mind_ sweeper** with your smart spyPhone, or read a book about exploding brain synapses while your computer's busy making free-as-in-liberated Emacs...

## Mac OS:

The Mac OS was, and still is a nice environment to work _(and create)_ in... I loved the NextStep `NS` environment all the way back to when it was first created at **NeXT** And... Being a musician, composer, programmer and member of the **BCS NeXT SIG**, in those early days, I was in collaboration with the digital audio developers at NeXT during that time as well! _(I typed that from memory... hope I got the camel case right. lol)_.

Now, I am not sure which direction Apple is taking...  The design of Mac OS morphing into iOS and visa versa makes it not so much of a unix anymore (also Mac OS never had a proper unix File System anyway)... I remember it was the file system and risky R/W optical disks which were the Achelies heel of the **NeXT** computer.  I am quite frightened by the new **APFS** spec Apple is throwing out! I have not upgraded past regular **Sierra** either... Time is running out?  What to do?  The new Mac Pros look way cool with probably more super-computing power than one of the older gen (pre-millennium) Cray super computers I bet! Seriously!  Thank God no more Trash Can! LOL What were they thinking? Cylinder does not = Cray super power... You abandoned all us Music Hackers! I hope you are back now...  And can you stay closer to standard unix? Thanks! Convince me your APFS is not evil or dangerous! Oh right... Can it be case sensitive too?

Unfortunately I am dependent on Mac OS For digital music/sound, video, graphic design, etc. I currently use a late 2015 27" retina iMac.  All of my other computing work is done on a reasonably secure Qubes _(personal hypervisor)_ Laptop that runs different flavors of Linux as App VMs... My Qubes laptop is used for writing, coding, business, personal...  

On my Qubes Laptop, Emacs works great, straight from any of the above package managers _(as you can see above...)_  

Mac OS (**Darwin**) has always been a mongrel cross-breed of different unix flavors. _(by now all the unix are mixed up pretty much however... so that's not it...)_  The problem is all the non-standard junk that Apple does that tends to break standard unix things if those are also installed!  OMG! Hair pulling time!

Long story short... My Emacs install was causing all my woes... Homebrew stopped building versions that I needed for my Emacs configuration to work correctly. _(some of the problems were also shell startup related)_.  After experiencing of all of that over the past year, and coming to the realization that I shoulda/coulda used MacPorts instead of Homebrew, but not daring to change all that now mid stream of course... So... I am building Emacs from source... Straight from a cloned repository from Gnu's Git Server!  I can get the bleeding edge if I want now... :stuck_out_tongue:

### Install Emacs from Source - Mac OS

> **2020 Update:**  This doc is currently being changed as I go through the process of building Emacs V27.  The information although updated may also work for building Version 26.  In that case the requirements will most likely be easier to meet since all those libraries have been out there for a while...  Also, later in 2019 I switched from Homebrew to MacPorts as my Mac OS package manager...  That was quite disruptive.  With MacPorts hopefully building Emacs on Mac OS won't be any more difficult...  Possibly easier...  I could just go with MacPorts emacs version but since I have this project all set up I am building it myself...  I like it that way... All dependencies get installed via MacPorts now... For you Homebrew users, all the instructions below are a year old... I have no way to easily test if the instructions still work... If you use Homebrew and run into problems please open up an issue about it.  This will allow us both, and others to come up with solutions... Thanks!

> **Note:** We are going to build a fast moving/changing target: Monster Lisp Machine with lots of little bitty _loaded.el bugs_ they are fixing and working on all the time! It's important to set up a scheme allowing for quick updates to keep in sync and get those bug fixes, _(possibly bugs that are blocking you from making a successful build!)_  OK **_MacCowboys_** and **_MacCowgirls_**... Are you ready for this? Strap in... Or should I say.. Bootstrap! :octocat:

#### Prerequisites:

- **Install Autoconf & Automake:**  
`autoconf` is a dependency of `automake`  
You can install both packages by installing Automake alone...   
Install Automake _(and Autoconf)_ via Homebrew:  
**`$> brew install autoconf automake`**  
Install Automake _(and Autoconf)_ via Macports:  
**`$> sudo port install automake`**  


- **Check `autoconf` version:**  - Check that it is at least the version specified near the start of **`configure.ac`** _(in the_ **`AC_PREREQ`** _command)._  **`V2.65`** or greater is required as of `2019-006-01`.  The Homebrew version at same time is: **`V2.69`** Whew! That was close! :trollface:


- **git:** - Check that `git` is at least **`Git 1.7.1`**.  If you cloned this repository with an older **Git** version, you may need to reclone it after upgrading `git`.  The current version of `git` at **Homebrew** is **`V2.21.0`** so you are all set if you **`update/upgrade`** Homebrew...  
`brew install git`  
-or-   
`brew update`   
`brew upgrade`  


- **GNU Make** - `gmake V4.2.1` or later is currently available via MacPorts.  It is   required to build Emacs from source.  _(any version)_    
**`$> sudo port install gmake`**


- **makeinfo:** - This is not strictly necessary, but highly recommended, so that you can build the manuals. **makeinfo** is bundled as part of **GNU Texinfo**.  You can install **texinfo** with **Homebrew** as well:  
`brew install texinfo`  
Make sure your installed **Texinfo** is: **`V4.13`** or later to work with this build... No problem... Homebrew version is currently **`V6.6`**   _(as of 2019-006-01)_

##### Prerequisites for Emacs V27:

- **GMP** - The GNU Multiple Precision Library `libgmp` is now needed.   
Install `gmp` via macports:   
**`$> sudo port install gmp`**


###### Emacs now supports resizing and rotating images without ImageMagick.
All modern systems support this feature.  (On GNU and Unix systems,
Cairo drawing or the XRender extension to X11 is required for this to
be available; the configure script will test for it and, if found,
enable scaling.)

The new function 'image-transforms-p' can be used to test whether any
given frame supports these capabilities.

- **ImageMagic** - Emacs no longer defaults to using ImageMagick to display images.
This is due to security and stability concerns with ImageMagick.  To
override the default, use `configure --with-imagemagick`.  I'm not sure what to make of this...  Imagemagick was important to Modular Emacs working correctly in Emacs V26...  I will leave the option out for my Version 27 build and use Cairo _(next item below)_ to see what happens... Maybe this is a non-issue now? Maybe it is even better now?  Hopefully...

- **Cairo** The configure option: `--with-cairo` is no longer experimental. This builds Emacs with Cairo drawing, and supports built-in printing when Emacs is built with GTK+.  Some severe bugs in this build were fixed, and we can therefore offer this to users without caveats.  Note that building with Cairo enabled results in using Pango instead of
libXft for font support, and that Pango 1.44 has removed support for bitmapped fonts.

- **GTK** - Emacs now requires >GTK 2.24 or >GTK 3.10 for the GTK 2 and GTK 3
builds respectively.    
Install `gtk2` via macports:    
**`$> sudo port install gtk2`**   
Install `gtk3` via macports:    
**`$> sudo port install gtk3`**


###### Emacs can now be configured using an early init file:

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
is still compatible with earlier versions of Emacs, change it to:  

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

```bash

git clone https://git.savannah.gnu.org/git/emacs.git

```

> **_btw:_**  So far all these instructions are universal to build the entire Emacs world on any platform, Mac, Linux, BSD, Windows, Tests, etc.  You have the cloned repo.  Try: **`git branch -a`** and you will see what I mean!

#### Set up Autotools:

To use the autotools: Run the following shell command within your cloned **`emacs`** directory:  

``` bash

$> cd emacs 
$> ./autogen.sh

```

This will generate the **`configure`** script and some related files, and to set up your git configuration...  

#### Run Configure:

To get all the features I wish Homebrew would give us, run `configure` with the following switches set:

```
$> ./configure --with-ns --with-imagemagick --with-mailutils\
   --with-gnutils --with-modules --with-rsvg --with-dbus\
   --with-xml2
```

For building the new Emacs 27.1 version use this instead:

```
$> ./configure --with-ns --with-mailutils
```

Actually you can simply run `./configure` alone those options are default for a new Mac that has GNU mailutils installed, as well as all the required graphics libraries etc. on it...  If those things are not installed, it does not matter if you require them on the configure line... Your build will fail if make cannot find needed libraries etc...


#### Make Bootstrap: _(does a more thourough job)_

The **Bootstrap make** is quite **`CPU`** intensive... If your Mac can _(4 cores? no prob!)_ fan won't even twitch? Maybe... :octocat: So if you don't mind waiting, this is the best way to build according to the GNU dudes...

    make bootstrap

#### Make Install! _(Make the Emacs Mac App package!)_

    make install

#### Move your shiny new Emacs.app to: `$HOME/Applications:`

    mv nextstep/Emacs.app ~/Applications

#### Launch Emacs from your Apps Folder/Menu:

Launch Emacs for the first time...  If it runs you can check the version with **`C-h C-a`**... If you have problems? Go back to the top of this **_squirrel cage_** and start over... :trollface:

#### Revert Repo back to fresh clone state to start over:

If your build was successful, you don't need to do this now... Wait until you need to build again...  However if your build went bad... This is the way to start completely over... 

    git clean -fdx
        

#### Troubleshooting Debugging:

My build went well because I planned well this time... _(i.e., you did not see the big goofs I made before writing this... lol)_  Because of that I am now running Emacs V26.2.50 on my iMac now with Imagemagick, and all my favorite bells and whistles!  **_Caveat:_** I have to manage builds now.. Oh well... it felt good getting that monster to build!   :octocat:










## Linux:

### Install Emacs from Source - Debian & Ubuntu: ###

###### Prerequisites  

- **`git`:** - Check that `git` is at least **`Git 1.7.1`**.  If you already cloned the Emacs repository with an older **Git** version, you may need to reclone it after upgrading `git`.  

- _Install `Autoconf`:_    
Make sure `Autoconf` is at least the version specified near the start of **`configure.ac`** _(in the_ **`AC_PREREQ`** _command)._  **`V2.65`** or greater is required as of `2019-006-16`.  
```bash
        sudo apt install autoconf
```

- _Install `Automake`:_  
```bash
    sudo apt install automake
```

- _Install `autotools-dev`:_  
```bash
    sudo apt install autotools-dev
```

- _Install `libtool`:_  
```bash
    sudo apt install libtool
```

- _Install `makeinfo`:_  This is not strictly necessary, but highly recommended, so that you can build the manuals. **makeinfo** is bundled as part of **GNU Texinfo**.  Make sure your installed **Texinfo** is: **`V4.13`** or later to work with this build...  
```bash
    sudo apt install texinfo
```

- _Install `build-essential`:_  
```bash
    sudo apt install build-essential
```

- _Install `xorg-dev`:_  
```bash
    sudo apt install xorg-dev
```

- _Install `libgtk2.0-dev`:_  
```bash
    sudo apt install libgtk2.0-dev
```

- _Install `libjpeg-dev`:_  
```bash
    sudo apt install libjpeg-dev
```

- _Install `libncurses5-dev`:_  
```bash
    sudo apt install libncurses5-dev
```

- _Install `libdbus-1-dev`:_  
```bash
    sudo apt install libdbus-1-dev
```

- _Install `libgif-dev`:_  
```bash
    sudo apt install libgif-dev
```

- _Install `libtiff-dev`:_  
```bash
    sudo apt install libtiff-dev
```

- _Install `libm17n-dev`:_  
```bash
    sudo apt install libm17n-dev
```

- _Install `libpng-dev`:_  
```bash
    sudo apt install libpng-dev
```

- _Install `librsvg2-dev`:_  
```bash
    sudo apt install librsvg2-dev
```

- _Install `libotf-dev`:_  
```bash
    sudo apt install libotf-dev
```

- _Install `libgnutls28-dev`:_  
```bash
    sudo apt install libgnutls28-dev
```

- _Install `libxml2-dev`:_  
```bash
    sudo apt install libxml2-dev
```







###### Clone `GNU Emacs Repo @ Savannah.gnu.org:`  

You could do this within a dedicated **`Dev`** folder because you will most likely want to maintain a local clone so you can come back later to build again when you need to upgrade or drop back to a more stable version...  I created my own local _untracked_ branch and keep my local `tracking` branch clean...   This helps speed up the build-problems-debug-re-build-till-it-works cycle...  

```bash
    git clone https://git.savannah.gnu.org/git/emacs.git
    cd emacs
    git branch -a                         # prints a long list of remote branches...
    git fetch origin emacs-26             # we are interested in building emacs 26
    git checkout --track origin/emacs-26
    git pull origin emacs-26
    git checkout -b my-local-branch       # A smart git practice to get into habit...
```

> **_btw:_**  **`git branch -a`** will reveal all the universal build options for the entire Emacs world on any platform, Mac, Linux, BSD, Windows, Tests, etc. You could build it all from here I imagine!  But here we are only interested in the latest stable Linux release at the time of cloning...

###### Set up Autotools:  

To use the autotools: Run the following shell command within your cloned **`emacs`** directory:  

``` bash
    cd emacs 
    ./autogen.sh
```

The last bit of output of the above running shell script should look like this:

```bash
    Installing git hooks...
    'build-aux/git-hooks/commit-msg' -> '.git/hooks/commit-msg'
    'build-aux/git-hooks/pre-commit' -> '.git/hooks/pre-commit'
    'build-aux/git-hooks/prepare-commit-msg' -> '.git/hooks/prepare-commit-msg'
    '.git/hooks/applypatch-msg.sample' -> '.git/hooks/applypatch-msg'
    '.git/hooks/pre-applypatch.sample' -> '.git/hooks/pre-applypatch'
    You can now run './configure'.
```

If you see the above it was successful!  The above script generated the **`configure`** script and some related files, and set up your git configuration...    Now you can move on to configure your specific build...

###### Run Configure:  

To get all the features I wish package managers would take the time to compile in for us, run **`configure`** with the following switches set:

```make
    ./configure --with-imagemagick --with-mailutils\
    --with-gnutils --with-modules --with-rsvg --with-dbus\
    --with-xml2
```
You probably don't have to be so specific  _(as above)_ and probably could just get away with using `./configure` alone... The build process is smart and most likely will give you all the things you need without asking explicitly...  But I asked explicitly above anyway...  Then I know for sure. 

_To see a list of other available options, run this command:_

```make
    ./configure --help
```

###### Make Bootstrap: _(does a more thourough job)_  

The **Bootstrap make** is quite **`CPU`** intensive... If your laptop can do it... _(4 cores? no prob!)_ fan won't even twitch? Maybe... :octocat: So if you don't mind waiting, this is the best way to build according to the GNU dudes...

```make
    make bootstrap
```

###### Make Install! _(Make the Linux App!)_  

Do this with `"sudo"` to get the Linux Emacs app installed in `/usr/local/bin`

```make
    sudo make install
```

Occasionally the file `lisp/loaddefs.el` (and similar automatically generated files, such as `esh-groups.el` and `*-loaddefs.el` in some subdirectories of 'lisp/', e.g., 'mh-e/' and 'calendar/') will need to be updated to reflect new autoloaded functions.  If you see errors (rather than warnings) about undefined lisp functions during compilation, that may be the reason.  Finally, sometimes there can be build failures related to `*loaddefs.el` _(e.g., "required feature ‘esh-groups’ was not provided")_.  In that case, update loaddefs.el (and similar files), as follows:

```bash
    cd lisp
    make autoloads
```

doing `make bootstrap` as above should eliminate any of the above problems however...


###### Launch Emacs:

With Emacs installed in /usr/local/bin, you can launch Emacs from any command line.  You could also create a start menu item/icon as well.  I don't bother with that in a Qubes environment... So no write-up on that for now... Start Emacs from the terminal.  It will pop up the GUI window... No problem... Once your Emacs build is running, you can check the version with **`C-h C-a`**...

If you have problems with your build?  Oh My! Do the next step below and then Go back to the top of this **_squirrel cage_** and start over fresh.  Read carefully... :trollface:

###### Revert Repo back to fresh clone state to start over:  

If your build was successful, you don't need to do this now... Wait until you need to build again...  However if your build went bad... This is the way to start completely over... 

```bash
    git clean -fdx
```

###### Troubleshooting Debugging:

My build went well because I planned well this time... _(i.e., you did not see the big goofs I made trying to build this on a Mac! lol)_ Also I was very explicit about required developer libraries which probably stopped a lot of problems!  Because of that I am now running Emacs V26.2.90 on Debian 9 now with Imagemagick, and all my favorite bells and whistles!  **_Caveat:_** I have to manage builds now.. Oh well... it felt good getting that monster to build! _(even after the second, third, times)_   :octocat:  







