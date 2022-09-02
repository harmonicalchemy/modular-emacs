---
file: ModE-Current-Status.md
author: Alisha Awen
created: 2020-007-15
updated: 2020-007-16
tags: Emacs, apps-tools, SysAdmin, HA-ModEmacs, how-to, README 
---
<!-- #Emacs #apps-tools #SysAdmin #HA-ModEmacs #how-to #README -->

![Banner](./media/Modular-Emacs-Github-Banner-v3.png)

# Modular Emacs - Current Status

**[\[Table of Contents\]](#table-of-contents)**

**[\[Back To MAIN Modular Emacs README\]](../README.md)**

## Release Notes:

### 2022 Sept 02:

**Docs:** Much work has been done _(finally)_ on Org-Mode configuration and Exporting to LaTeX PDF to make eBooks, Tech Manuals, Log Notebooks, etc.  Up until this point Exporting to PDF has been basic out-of-box default.  

I am now modifying `09-4-org-export-conf.el` to add new custom export options... Also creating SETUPFILES for Org-Mode documents.  There are many ways to configure LaTeX and configure it to work well with Emacs Org-Mode... I am working out the best way to do this for Harmonic Alchemy Modular Emacs... 

This work is currently on-going. Once I finally have a good setup complete with nice Templates to use, then we may be ready for a final NON BETA release!  These Markdown Based Readme files will be changed to .org files as well...

### 2020 July 25:

Refactored many things, created new functions, etc. to fix buggy behaviour around making new frames, changing default faces, org-mode faces, etc.  Also made Xha-Fly-Keys Command Mode background a lighter more 'red' background than Insert Mode background which stays the dark indigo slate blackboard colour.  Thinks are looking and behaving much better now...  This will be tagged as Version 3.2.1   Docs, _(as usual, are still being updated)_ More about doc updates will be logged here later...


### 2020 July 16:

Modular Emacs is now at Version 3.2. This has been tested on the develop branch long enough... I am still calling it "beta" because there is still a lot of work still unfinished... (mostly peripheral support files, templates, etc. and the Docs are still being updated... (does documentation ever end? lol).  Lots of improvements with the docs has been done however...

Testing all of the features on different platforms has proven it to be stable enough to merge back into the Master branch...  This commit adds new improved docs.  Also templates for authors, publishers, and screenwriters has been included.  Those templates are still being refined.  I will be using org-mode to do the exports and publishing automations... Final stages of exporting and publishing are now the main focus...  All of the basic editing features are mature now...

Although this project is "beta", version numbers will continue to advance...  The nature of Emacs is "always in Beta" because it is so powerful and flexible, and empowers the user who can not only use and customize it but can also completely change functionality!  Any new features I end up trying out do not get merged into the Master branch until I have used them for a while, they are stable, and I like the new added features...

More to come later of course!

### 2020 June 27:

Updated the docs... Added a new section below about installing fonts for use with Modular Emacs, and also created a how-to doc for you to follow along to get that important task done...

### 2020 June 16:

I have been using **_"Fancy Org Mode"_** with **`Xah-Fly-Keys`** enabled for a several months now. Since the beginning of 2020 many refinements have been made.  I have been mucking around with custom key-bindings and modified many of the default key-bindings that come with **`Xah-Fly-Keys`** mode by default...   The primary incentive is to put your most often used key commands as close as possible to the **_"home row"_** to speed up productivity, and also save your fingers from doing stressful things repeatedly which is the main cause of tendenitis.  

There is a second insentive to NOT assign keys close to the home row that evoke radical transformations, or do things that could get you totally hosed and confused if you hit that key by accident! If you hit it by accident while editing an important doc _(or not so important)_, messing up like that is frustrating to say the least!  

Needless to say... I have been modifying these keys based on the above experiences I have had with it since around Novemember of 2019... Things are working much better for me now but I am not done...

Since I also have to support folks who do not care to use Emacs as a **_"modal editor"_** like **vi**, I am committed to mapping custom key bindings using the **`Control`** and **`Meta`** keys that match things I can do when I am in Xah-Fly-Keys mode...

Unfortunately, I have been using **`Xah-Fly-Keys`** for over 6 months and my fingers _(muscle memory)_ are constantly tripping when I am in regular Emacs mode... _(This also happens to me outside of emacs, editing documents in Libre Office, copy-and-pasting, or something as simple as renaming a file, Etc. My fingers forget they are not in Emacs and un-counscously try command keys that produce error messages etc. This is the downside of using modal editors.  If the entire computing experience was configured modally like this, I would not have that problem. LOL "The bane of doing things differently" eh?)_ Keep this in mind when trying to decide whether or not to use Emacs as a "modal editor" there a pluses and minuses... Nothing is perfect...

**Please Help!**

 At this point I need some folks to try Harmonic Alchemy Modular Emacs _(normal key bindings mode)_ to give me some feedback about their user experience (**UX**). Please! Where are you?  Come try this stuff out! Thousands of man-hours have been put into it and more to come!  All Free-as-in-freedom... What are you waiting for?

> **Warning!** This year is the year of key-bindings refinement...  Key bindings defined within the module: **`12-Xah-Fly-Keys.el`** will most likely be changing several times this year... If you are already using this project and are used to the way things were set up before now, it would be wise to copy, **`lisp/modules/12-Xah-Fly-Keys.el`** into: **`lisp/my-modules/12-Xah-Fly-Keys.el`** and also copy: **`lisp/modules/13-key-bindings.el`** to: **`lisp/my-modules/13-key-bindings.el`** to override the ones that get updated when you pull down new changes from the master branch...  
That way if key-bindings change, you will stil have your old ones to keep (if you like) and you can also try out the new keybindings for size as well... No pain or disruptions...

### 2020 May 22:

Org Mode has been heavily customized!  I am calling it **_Fancy Org Mode_**.  Also some of the Module files were re-organized... (In particular `12-Xah-Fly-Keys.el` and `13-key-bindings.el)`... Tweaks were added to improve package management and minimize the effects of load errors etc.  Keybindings for both Xah-Fly-Keys mode as well as normal mode were added/changed...

The file: **`dispatcher.el`** got more automated as well.  Any module within the **`lisp/modules`** directory will be automatically overrided simply by making a copy of it _(using same file name of course)_ to: **`lisp/my-modules`** where the file will not be tracked by git and you can make any local modifications or additions without upsetting the original files that get updated periodically as new git commits are pulled down...  

> **Note:** _New changes emerging within new pulled down files which you have previously overridden will be shadowed of course!_    
If you need the updates, you will need to merge those changes into your overrides...


In addition, you no longer have to copy **`dispatcher.el`** into **`my-modules`** either _(except to enable normally disabled optional modules)_...

In case you do have to copy **`dispatcher.el`** into **`my-modules`** _(e.g., To enable the optional Common Lisp related modules, etc.)_ you won't have to edit **`init.el`** anymore to change the location of **`dispatcher.el`**.  That part has been automated as well. **`modules/dispatcher.el`** will be  overridden by your custom copy in: **`my-modules`** automagically.

Now, the only reason to edit **init.el** is to add your own custom heading and/or body to the Welcome Screen Message... _(If and when you care to do that, now or later...)_

I created a branch _(fork)_ where you can try out: **Xah-Fly-Keys** without having to go in to override **`dispatcher.el`** manually...

In the Master Branch, optional modules are disabled _(see details below)_...  Not having Xah-Fly-Keys enabled kind of cripples things for me for running tests within normal Emacs mode now that my fingers are used to flying with a spacebar leader key!  Going back to the old way is painful! It is hard to test my setup using the old C-x and M-x keys etc... %^) Last year at this time I was complaing about the opposite... _(i.e., having to learn Xah-Fly-Keys and using Emacs as a Modal editor like **vi**)_

You have two editor mode options available to choose now, _(and it's easy to switch back and forth)_.   Choose between using **Normal Emacs** edit mode **_or_** **Xah Fly Keys** modal edit mode simply by switching git branches...

**Note:** Making the switch to a model editor is a BIG HUGE commitment... But it has its BIG HUGE payoff at the end of that bumpy road! Trust me on that!  There are also caveats as well though.  _(arent there always?)_ :trollface:

### 2020 May 17 - Fancy Org Mode:

This commit adds a bunch of new Org-Mode tweaks... I am setting up my Org-Mode based Book Publishing System _(my **pubOps**)_ now... There will be some included examples added to this a bit later...  **_Fancy Org Mode is now enabled by default_**...

To use **Fancy Org Mode** you will need to override and edit the file: **`09-org-mode-pkg-conf.el`** and depending on your OS, (Mac or Linux) change the dummy placeholder path to your REAL **org docs** directory path.  There are two options allowing you to run this on both MacOS and Linux if you are a multi OS kind of person that is... :octocat:

> **Note 1:** I am implementing a cleaner way to get all local directory path and user specific meta data into the load process rather than having to override and edit indvidual files as in the above bullet...  This will all be done in one file that you will override during your initial install... Once it is set with your specific edits it will stay that way and you will never have to touch it again...

> **Note 2:** I am still working on custom Export options for Modular Emacs Fancy Org Mode...  But everything else is working nicely now... Export features will be forthcomming over the next few months!  Stay tuned...  Book publishing time!

The **Blackboard** Color Theme was also updated _(to accomodate prettifying Org-Mode)_.  Also, I am back to using the standard mode line.  I got tired of **powerline** _(It felt too cluttered and also seemed to have bugs)_... Then I tried **smart-mode-line** to see if I could make things look better and simpler... I got frustrated with it as well and after mucking around I realized there is nothing wrong with the Emacs standard mode-line and you can tweak it to fit exactly your needs... So now that is the new plan... I left the old code for both `powerline` and `smart-mode-line` in place _(commented out)_ making it easy for you to enable either of them, if you like them better... You can see changes to mode line features in **`02-package-conf.el`**... 

**_Everything else stays pretty much the same as Version 3.0..._**

> **Exception: _Debian and Mac users will have to follow a linked doc that guides you through building and maintaining your own Official GNU Emacs from source first!_** Building from source is tedious, _especially if you are doing it for the first time, you WILL have to try many times before getting all those "twisty little snippets all different", sorted out and resolved._  If you stick to your guns and stay persistant, you will be awesome!  You will also be in total control of your personal and OFFICIAL build of GNU Emacs, how it works, and what it does!  Your second build experience will go much more smoothly...  By the third build the process will become the boring routine of typing a few commands, and then waiting as your machine cranks away a brand new version for you...  Play **_mind_ sweeper** with your smart spyPhone, or read a book about exploding brain synapses while your computer's busy making free-as-in-liberated Emacs... :trollface:

Once you have the basics running smoothly, then it will be safe to visit the _**optional section headings**_ below to find links to install guides for optional modules you may be interested in trying out...  Consult the **[\[Table Of Contents\]](../README.md#version-312-beta---readme)** for quick access if you are coming back to this doc later...

> **Warning:** Enabling optional features may require you to remap some of your keys and also change Emacs into a modal editor _(similar to Vi but more Emacs like)_  Read carefully before making that commitment...

> **Note:** If you are on Mac OS or some flavor of Debian, you may have to build Emacs from source _(as mentioned above)_ to enable extra features support _(like VMD-Mode which needs special libraries linked in during the Emacs build process)_...  Full instructions are provided for those sections...

**[\[Back To MAIN Modular Emacs README\]](../README.md)**

## Table Of Contents:

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

- [Modular Emacs - Current Status](#modular-emacs---current-status)
    - [Release Notes:](#release-notes)
        - [2020 July 25:](#2020-july-25)
        - [2020 July 16:](#2020-july-16)
        - [2020 June 27:](#2020-june-27)
        - [2020 June 16:](#2020-june-16)
        - [2020 May 22:](#2020-may-22)
        - [2020 May 17 - Fancy Org Mode:](#2020-may-17---fancy-org-mode)

<!-- markdown-toc end -->
