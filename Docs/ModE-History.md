---
file: ModE-History.md
author: Alisha Awen Sheppard
created: 2019-010-19
tags: Emacs, Xah-Fly-Keys, apps-tools, SysAdmin, HA-ModEmacs, how-to, README 
---
<!-- #Emacs #Xah-Fly-Keys #apps-tools #SysAdmin #HA-ModEmacs #how-to #README -->

![Banner](./media/Modular-Emacs-Github-Banner-v3.png)

# The History of Harmonic Alchemy Modular Emacs

**[\[Back To Top README\]](../README.md)**

## Introductions:

This project has been a long time coming.  Its history goes back a few years ago when I went searching for some better ways to configure my then pretty basic Emacs configuration of over 10 or so years!  I cloned a few Emacs repos on Github to try different things out for a while.  I liked some things and tried to get rid of other things later.  Management became confusing after a while.  I ended up with lots of questionable elisp code, much of which were things possibly no longer needed!  

After a couple years of **adding**, **removing**, **configuring**, **re-configuring** it started feeling like I was painting myself into a tangled corner like getting lost in **_"a maze of twisty little passages all alike!"_**  Lets face it... I was not keeping good logs and my **git** management skills were wanting!  I was being: _"oh that looks cool lets try that" - (cowgirl mode)_ and I got into a wee bit of trouble as a result of my wild adventures!  It was all my fault! But it was fun just the same.

However as a result of that saga, I learned a lot about **`SH`**, **`BASH`**, and **`ZSH`** best practices...  That spawned a complete re-write of my **`.dotfiles` project** to become multiplatform... _(**Note:** My `.dotfiles` have not been updated yet as that project has been forked and made private until my final scheme is ready...)_ Currently my **New** _(but still private)_ **`.dotfiles` project** is _Full of Twisty Little Pathways Still Unknown..._  :question:

## Modular Emacs is Born:

As a result of the mess above, I decided to start over from scratch and modularize everything with the purpose of preventing tangled messes like this from happening within your own Emacs setups! _(and mine as well..._ `%^)`

**Modular Emacs** is more than just Emacs with configurable modules... It is also designed to be the centerpiece **_(command central)_** of a larger **_Extremely Personalized, Extremely Extensible_: ** **DevOps** / **PubOps** **IDE** workstation and/or companion **IDE** workstation **laptop**.  I am a devOps engineer, computer scientist, composer/musician, sound designer, architect, writer/publisher.  I wear a lot of hats! My Emacs needs to wear a lot of hats as well!  The central focus of **Modular Emacs** is to build empowering features into **Emacs** utilizing a modular framework that facilitates all the above without becoming an over complicated mess to manage! **_"good luck with that"_**  :octocat:  

## Why Modular?

- **Modular Emacs** was designed to be easy to maintain and configure through the management of independent modules and in that sense Modular Emacs hopefully forms a basic sensible framework that is easy to use and flexible enough to adjust to your work-style, programming-style, and writing-style...

- **Add What You Need Only:** You have choices encapsulated within modules. Each module comes with its own requirements.  Removing and or adding many of the _extra feature_ modules will not disrupt the rest of your Emacs configuration.  They have been designed to be encapsulated in that way... Some of the more basic modules are required for Modular Emacs basic operation however.  The nice thing is everything is modular so it will be easy to find out which ones are doing what...

On the other hand, if you install Modular Emacs stripped of all external helper apps _(described below in the **Requirements** section)_, It may not provide any advantage over many of the other simpler Emacs projects on GitHub... **_Therefore if all you need is vanilla Emacs:_** _(with some of the more basic options thrown in)_, you don't need Modular Emacs at all.  Here is a simpler light weight _"sensible"_ Emacs Config: **[hrs/sensible-defaults.el](https://github.com/hrs/sensible-defaults.el)**  _(which claims to also be modular - I have not tried it.  I only briefly read through the README.  It looks like a good alternate option to try)_ :octocat:   
