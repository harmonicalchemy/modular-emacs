---
file: ModE-Install-Emacs.md
author: Alisha Awen
created: 2020-007-15
updated: 2020-007-16
tags: Emacs, apps-tools, SysAdmin, HA-ModEmacs, how-to, README 
---
<!-- #Emacs #apps-tools #SysAdmin #HA-ModEmacs #how-to #README -->

![Banner](./media/Modular-Emacs-Github-Banner-v3.png)

# Modular Emacs - Install Emacs

**[\[Back To MAIN Modular Emacs README\]](../README.md)**

## Introduction:

### Commands to install Emacs on various unix platforms:  

_(Choose your Flavor)_  

#### Fedora 27-29:  

```yaml
$>  sudo dnf install emacs
```

That's it! Red Hat takes care of you lucky Fedora users... The default Fedora "emacs" has the needed extras _(compiled/linked in)_ that Modular Emacs requires...

#### Debian9 Stretch & Ubuntu 18.04+:  

You Debian/Ubuntu users don't get it so easy... Hope you got your hacker shoes on...
I tried all the normal ways to get the latest binary build of Emacs installed for Debian based Linux but none of those options are able to satisfy Harmonic Alchemy Modular Emacs basic default needs 100%... Follow this link to: **[Build Emacs from Source](./ModE-Build-Emacs-from-Src.md)** and then come back here to continue below...

> **Warning:** This will take a bit of up-front work...  I hope you don't mind staying up all night digging deep and scratching your eyes a lot! Not for the faint at heart!  If you never built software from source before you may want to try something like **`"Hello World"`** first... _(just kidding)_ :trollface:  No..  you stay right here soldier! We will get you up to speed soon enough... 
    **Harmonic Alchemy Modular Emacs** may still work if you give up and use the latest builds from the package managers, but you may not be able to use many of the nice extra features that Modular Emacs provides...

**_Was your Emacs Build Saga Successful?_**  

**Yes?:**  Little grasshopper... You are awesome! Now you may now call yourself a devOps engineer _(to friends and family only... Don't put it on your resume LOL)_.  Now you will no longer be dependent on the mercy of wining sniveling package archive build engineers! Worse than anything Monty Python ever made fun of! LOL  Welcome to DevOps heaven you pirate! Arr! :octocat:  Continue bravely on below with your next steps _(challenges)_...

**No?:** OK, You are wicked frustrated trying to build Emacs eh?  No problem... I feel your pain. Take a rest...  You can still get Modular Emacs installed below, _(with a few caveats of missing features of course...)_ Just use your default package manager to install the latest version of Emacs you can find there and continue on below...  You will have gotten your winning attitude back by that time.  Later on, when you are itching to get Emacs updated, come back to this doc and read **[Optional Packages & Customization:](./ModE-Optional-Packages-y-Customization.md)** below where you will be given instructions for that and any of the other things you missed during the default install here... _(plus more goodies ;-)_

You will be surprised at your devOps abilities after a long rest! Your psychology needs a few gains first... Get the rest done and you will feel better! I promise! :purple_heart:

**[\[Back To MAIN Modular Emacs README\]](../README.md)**

#### Mac OS:

If you have MacPorts installed simply use this command and you will be all set:

```yaml
$> sudo port install emacs
```

If you don't have Homebrew or MacPorts installed, Don't even think about HomeBrew!  Instead, Install MacPorts: **[Follow instructions on the MacPorts website](https://guide.macports.org/#installing)**... This will require you to install xCode from Apple for your platform, with command line tools enabled... 

Once you get MacPorts installed and updated,  come back here and use the above command to install Emacs...  Then you will be all set...

As of this doc update, MacPorts will get you Emacs version 27.0.91, which should work great for you! I have installed this _(via MacPorts)_ and tested it on a 2009 Mac Book Pro running Mac OS El Capitan. **Modular Emacs** _(fully installed on that old laptop)_ looks perfect on the screen! Macports also runs fine on El Capitan... So far no issues for me... If you can build from source, you can get a few more years out of old computers after everyone else has junked them!

On my desktop iMac the story was quite different as I had Emacs already installed via Homebrew _(and was suffering many problems when I first started out)_. So I installed Emacs from source on my iMac...  If you would rather do that as well, I have included the steps below for you...

I have since switched to MacPorts, but since I have my own local Emacs build project started, I continue to maintain that outside of MacPorts, and I keep Emacs up to the Developer's Bleeding Edge... :octocat:

> **Note:** _(The info about Homebrew below may be outdated now, but it was the case at the time I wrote this.  I no longer use Homebrew and therefore do not keep up with news there anymore. Your mileage may vary...)_ 

If instead you have HomeBrew installed?  Do like I did and install Emacs from source... We Macolites have been abandoned by both Apple and Homebrew!  What a mess... and What a shame! **_Linux dudes roll eyes and wonder why MacRats don't jump ship?_**  

Follow this link to: **[Build Emacs from Source](./ModE-Build-Emacs-from-Src.md)** Follow that guide, and then come back here to continue below...

**_Were you successful after only a few tries?_**  

**Wow!** You are better than I was the first time! Fantastic! You have the Best option possible now!  And you can change any feature at will, even customize the base code if you so wish!

**What?**  You were not successful even after many tries?  OMG! Now all eight of your arms are wicked tired and you are frustrated beyond all means eh? That's Happened to me many times... No problem... I feel your pain. Take a rest...  Install the latest version you can currently get from HomeBrew, realizing that some features of Modular Emacs will not work correctly until you upgrade Emacs later...

Later on, when you are itching to get Emacs updated, come back to this doc and read **[Optional Packages & Customization:](./ModE-Optional-Packages-y-Customization.md)** where you will find links to instructions for building Emacs from source once more. There you will also find links to guides for installing optional packages... 

You will be surprised at your devOps abilities after a long rest! Your psychology needs a few gains first... Get the rest done and you will feel better! I promise! :purple_heart:

Alternately, if you are willing to go through the pain of completely un-installing HomeBrew, and replacing it with MacPorts that would be the next best thing to building from source... _(although building from source is an easier task than completely switching package managers on Mac OS, in my opinion)_  If you have to remove HomeBrew... Better think long and hard and make good plans for it... It will take you a few days at least to get your system back in working order with all packages from HomeBrew removed and then re-installed via MacPorts!  I may write up a doc that guides you through that painful process... Not soon though... Too many other things on my plate...  Note if you can do all that by your lonesome, you can surely follow a `.configure` file, or `make` file to build and install something from source like Emacs!  Food for thought... :octocat:

**[\[Back To MAIN Modular Emacs README\]](../README.md)**

#### FreeBSD & OpenBSD:  

I have not tried this yet with a Qubes configured BSD VM... Hopefully the FreeBSD world knows how to do Emacs correctly... Install the most recent pre-built binary package of Emacs: _(must be up to v26.1 by now - 2019-May)_

```yaml
$> pkg_add -r emacs
```

