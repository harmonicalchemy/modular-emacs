---
file: ModE-Install-SBCL-IDE.md
author: Alisha Awen Sheppard
created: 2019-010-24
updated: 2019-011-14
tags: Lisp, SBCL, Emacs, apps-tools, SysAdmin, HA-ModEmacs, how-to, README 
---
<!-- #SBCL #Emacs #apps-tools #SysAdmin #HA-ModEmacs #how-to #README -->

![Banner](./media/Modular-Emacs-Github-Banner-v3.png)

# Install Optional Modular Emacs SBCL IDE

**[\[Back To Top README\]](../README.md)**

## Introduction:

_(optional for CS research eggheads, prototyping, blockchain? etc.)_  

OK Already! This one is **_(optional)_** for Eggheads :octocat:...  You can skip this section if you are not interested because the Lisp Programmers Module is not loaded by Modular Emacs at startup _(by default)_...  To use Steel Bank Common Lisp within Emacs you will have to load the Programming languages Module which is discussed later within the **Usage** section below...

You can install this now, decide not to install it ever, or wait and skip this section for later after you have Modular Emacs up and running and have had a chance to get familiar with everything first...  Or just use Modular Emacs without it...  No matter what you choose everything will be fine...

## Steel Bank Common Lisp:

Steel Bank Common Lisp is the best full-fledged Lisp compiler option for Fedora and Debian and I guess Mac as well.  But the Mac world has many Eggheads so there may be many opinions to choose from as well. %^)... I personally feel SBCL is best for using with Emacs... But then I'm an Emacs Egghead... _(I prefer the term hacker - Eggheads are nerds with academic credentials.  They may or may not also be hackers who DIY knowledge from anywhere, regardless of the source...)_ Oh Well...  You can be what ever Egghead or Hacker you wish... Or not... Have fun playing croquet among the ivory towers!

You don't actually need a full fledged Lisp compiler for Emacs because Emacs Slime Mode takes care of handling most things internally within Emacs and also provides a nice [REPL](https://en.wikipedia.org/wiki/Read-eval-print_loop) interface with that setup by default...  

**_However..._** Once you add Steel Bank Common Lisp to your system _(supercharged with Emacs Slime mode)_  You will arguably have the best IDE for serious Lisp projects... _(my opinion and totally biased of course.  Most likely RMS's choice as well... %^)_  If you don't know who RMS is... You need to read about the history of Emacs & the MIT AI lab!  btw, I don't think RMS likes CL...  If he reads this and replies and/or corrects me, I would be flattered... :octopus: 

RMS may not remember this, but years ago _(when we were both much younger)_ we seem to have eaten lunch at the same Chinese restaurant on many simultaneous occasions.  I also remember seeing him dancing wildly at a few New England folk dances! Some of those lunchtime encounters were actually early informal FSF meetings! During one of these encounters, my friend and I just happened to be sitting at a table close enough to overhear the entire meeting.

I cannot remember the name of that Chinese restaurant anymore but it was near Tech Square, (Kendall Sq. Cambridge MA)... Was it called Mary Chung's?  Nope... that was the Harvard one I also loved...  I believe this particular Kendall Sq. Broadway/Main St. restaurant was an informal strategic meeting place for super computer design meetings _(Thinking Machines)_ as well...  And who knows what other DARPA High Tech black ops secret projects may have had their beginnings right there tucked away in a dark corner!

OMG! That's why I can't remember the name!  Maybe I was part of a secret-operation-gone-bad and they wiped some of my memories!!! That may explain why...  _(never-mind...)_ :trollface:  

One thing I do remember was that **Hot&Sour soup** _(at what ever that Chinese restaurant on Broadway near Kendall Sq. (not far away from Eli Heffron & Sons Computer Junk Yard . PDP11 Gold mine) was called?)_ was the best in the universe!  I have many fond memories about sitting studiously alone at a table _(on a gloomy New England rainy day)_ with a warm savory steaming bowl of H&S soup _(scallions sprinkled on top)_ in front of me, my laptop on the side... _(em... er... Oh! With my HP-15c rpn calculator sitting next to a graph-paper log/notebook, mechanical pencil, engineering scale/straight edge, and possibly a small shapes template as well. Eytballing some piece of interesting electronic junk (probably a power supply) I got at Eli Heffron & Sons...)_  School Days... LOL   At least I didn't have to carry a slide rule.  I still have my HP-15c _(and it works great!)_ It's future proof like Emacs & Abacus! 

LOL I was talking about the soup somewhere in that stack overflow above. `%^)`  Maybe we should get back to the subject at hand _(install this already OK?)_  Enough Lisp nostalgia...  I mean I wasn't even a Lisp programmer back then...  I had to make money and support a growing family!  I sold out to the dark side from "C" to C++ to (more C++ and some Java - later Python). :octocat:

#### Install SBCL on Mac OS:  

If you are on Mac OS you can install Steel Bank Common Lisp via Homebrew:  

    brew install sbcl

That's it... Easy Peazy!  

#### Install SBCL on Linux:  

Steel Bank Common Lisp is available in the Linux package managers as well:

- **[Debian]:~$ `sudo apt install sbcl`**  


- **[Ubuntu]:~$ `sudo apt install sbcl`**  


- **[Fedora]:~$ `sudo dnf install sbcl`**  

#### Install QuickLisp Package Manager:

**_Ref:_ [Quicklisp.org](https://www.quicklisp.org)**

You don't have to re-invent wheels in Lisp!  no No NO!  Here are over 1,500 libraries available at your fingertips!  You **Must** have this!  Installing is not too painful, and they will also be integrated into Emacs Slime Mode!  

You need to download `quicklisp.lisp` somewhere into your HOME path that is convieanent for you _(`~/Downloads` is fine... The important thing is to remember where you downloaded it. DOH! You will need this file next)_.  

**Simply Click this link:** (https://beta.quicklisp.org/quicklisp.lisp) to download it from your browser.  _(or use `wget` if you are a terminal die-hard, lol)_

The download file: `quicklisp.lisp` has been signed by the official Quicklisp release signing key:

- **Fingerprint:**

    `D7A3 489D DEFE 32B7 D0E7 CC61 3079 65AB 028B 5FF7`

- **ID: `028B5FF7`**
- **Email: `release@quicklisp.org`**

You should verify the file and all of the above from multiple sources _(i.e., the usual super paranoid checks)_ to be reasonably sure the file, keys, signatures, etc. have not been tampered with!

It's also probably a good idea to inspect this lisp file before running it _(if only to see what it does)_   The file is only needed once the first time... _(it is a bootstrap)_  From then on Quicklisp itself will maintain a lot of things within your **Lisp IDE**.  **Quicklisp** is well integrated into **Emacs Slime**...

##### Run these commands from your HOME directory: 

After verifying your downloaded `quicklisp.lisp` do the following:

1. **Install Quicklisp via sbcl: _(all platforms)_**  
Use SBCL to load the file you just downloaded above:  
**$** `sbcl --load <path/to/your/quicklisp.lisp>`  

The above command invokes **`sbcl`** which then loads the file you just downloaded above to install **Quicklisp** into: **`~/quicklisp/`** which was also automagically created for you by some wizard behind the curtain...  **`sbcl`** will still be running after this is done...  You will see something like the following:

```lisp
    $ sbcl --load Downloads/quicklisp.lisp 
    This is SBCL 1.4.6-2.fc29, an implementation of ANSI Common Lisp.
    More information about SBCL is available at <http://www.sbcl.org/>.

    SBCL is free software, provided as is, with absolutely no warranty.
    It is mostly in the public domain; some portions are provided under
    BSD-style licenses.  See the CREDITS and COPYING files in the
    distribution for more information.

            ==== quicklisp quickstart 2015-01-28 loaded ====

    To continue with installation, evaluate: (quicklisp-quickstart:install)

    For installation options, evaluate: (quicklisp-quickstart:help)

    *
```

Your cursor will be at the next `sbcl` **"`*`"** prompt:  Continue typing the rest after the **`*`** below: _(if copying don't copy the star ok? That simply indicates the `sbcl` prompt already... OK? %^)_

2. **Install Quicklisp Quickstart:**  

```lisp
*   (quicklisp-quickstart:install)
```

3.  **Load Quicklist Every Time you start Lisp:**  

```lisp
*   (ql:add-to-init-file)
```

4. **Load Quicklisp Slime Helper command for Emacs:**  

```lisp
*   (ql:quickload "quicklisp-slime-helper")
```

After the last step you will see some text at the very end of output that looks like this:_

```lisp
    (load (expand-file-name "~/quicklisp/slime-helper.el"))
    ;; Replace "sbcl" with the path to your implementation
    (setq inferior-lisp-program "sbcl")  
```

You can disregard that message... You are done with your part here... Everything is installed properly for now.  Later... When you are all set to run Modular Emacs for the first time the above Slime Mode scripts will load and compile while Emacs is loading Modular Emacs for the first time...

You will not be needing to use `sbcl` or it's `REPL` from the command line from now on, as you will have Emacs Slime mode later to use, which is much better...  

5. **Quit the SBCL Lisp Interpreter:**    

```lisp
*    (quit)
```
The above command will shut down your SBCL process cleanly and leave you back at your normal SHELL prompt...  

Later...  Once you have your new Modular Emacs environment up and running smoothly, you will be able to do Common Lisp, development, research, experiments, coding, anything... within a fully compliant Common Lisp IDE, simply by typing:

    M-x slime

Comming soon at the end or your journey! :octocat:


#### Find, install, update, remove Quicklisp System packages:

To find out what's available in Quicklisp, use:

    CL-USER> (ql:system-apropos "substring")

where `substring` makes up part of a quicklisp system name you might be interested in...

The output will list all system names, one on each line, right after `#<SYSTEM` and before the first `/`... The rest of the line provides more information.  Use the system name with the quickload command...

To Load _(install)_ a **`quicklisp`** system use:

    CL-USER> (ql:quickload "system-name")

where `system-name` is the name of a known registered **Quicklisp** system...

 To see what systems depend on a particular system, use:

    CL-USER>(ql:who-depends-on system-name)

To remove software, use:

    CL-USER> (ql:uninstall system-name)

An uninstall does the following:

- Deletes the system's tarball archive and unpacked source files
- Deletes Quicklisp metadata files associated with the system
- Clears ASDF's system cache via asdf:clear-system 

Uninstalling does not alter the current Lisp session in any other way; if the given system has been loaded, it remains loaded and accessible until the session is ended.

To update Quicklisp Quicklisp packages:

    CL-USER> (ql:update-dist "quicklisp")

Software updates are usually available about once per month.

After installing systems or making changes to your environment, if you want them to be permanent, _(i.e. you will need them for most sessions, and you don't want to have to load them over again each time)_ then use this command to save the current environment in: `quicklisp/.sbclrc`.

    CL-USER> (ql:add-to-init-file)

SBCL will tell you and show you what it is going to do and then ask you to press Enter to continue...  _(how polite sbcl is! I love it... ;-)_

To update the Quicklisp client, use:

    CL-USER> (ql:update-client)

### Enable Optional Steel Bank Common Lisp IDE:  

The above **_Requirements_** section lists the optional installation of **SBCL**...  If you installed that on your machine and you would like to enable Slime mode within Emacs there are a few extra steps to do to make that happen now... If you have not installed Steel Bank Common Lisp you need to go back up to the requirements section now and get that task done first...  Then come back here to finish up...  

#### Copy `dispatcher.el` into `my-modules`:  

    cp ~/.emacs.d/lisp/modules/dispatcher.el ~/.emacs.d/lisp/my-modules/dispatcher.el

##### Edit your clone `/my-modules/dispatcher.el`:  

Un-comment the line that loads `12-progLang-pkg-conf.el` as reflected below:  

```lisp

;; Optional: Load Harmonic Alchemy Modular Emacs - Programming Languages module:
;; This is for using Emacs as a full fledged Common Lisp IDE!  Don't un-comment
;; this Load Line unless you are an Egghead...  You have been warned!
(load-file "~/.emacs.d/lisp/modules/12-progLang-pkg-conf.el")

```

#### Edit `~/.emacs.d/init.el` to load your clone of `dispatcher.el`:  

##### Change the following line within `init.el` to look like this:

```lisp

;; Load: Harmonic Alchemy Modular Emacs - Dispatcher
(load-file "~/.emacs.d/lisp/my-modules/dispatcher.el")

```
#### Restart Emacs & Run Slime from your scratch buffer:

    M-x slime



### Common Lisp Resources:

- **[Quicklisp News](http://blog.quicklisp.org/)**

- **[Read the Slime Manual Here](https://common-lisp.net/project/slime/doc/html/)**  

- **[Check out the Common Lisp Wiki CLiki](https://www.cliki.net/)**

- **[Much Nore Can Be Found on this CLiki Page](https://www.cliki.net/Exercices)**

- **[Read Practical Common Lisp by Peter Seibel Here](http://www.gigamonkeys.com/book/)**  

- **[Watch Peter Siebel's Practical Common Lisp Presentation - Youtube](https://youtu.be/4NO83wZVT0A)**

- **[Download Paul Graham's Classic: ANSI Common Lisp Here - PDF](https://7chan.org/pr/src/ANSI_Common_Lisp_-_Paul_Graham.pdf)**

- **[Study Annotations on Graham's ANSI Common Lisp Here](http://www.cs.northwestern.edu/academics/courses/325/readings/graham/graham-notes.html)**

- **[Download Paul Graham's On Lisp Book - PDF](http://ep.yimg.com/ty/cdn/paulgraham/onlisp.pdf)**

In addition to above if you would like to try a complete interactive guide complete with example source code etc.  Clone **_Paradigms of Artificial Intelligence Programming_ by: Peter Norvig** _(Don't let that title fool you.  It's all about Lisp!)_

- **Get it Here** - Everything you need will be in this repository... Clone it and then Read the README.md for instructions...

```yaml
    git clone https://github.com/norvig/paip-lisp.git
```

##### Other Books:

- **[Download Common Lisp - the Language by: Guy L. Steele - PDF](http://www.lispmachine.net/books/common_lisp_the_language.pdf)**

##### Lisp Web Dev Resources:

- **[The Common Lisp Cookbook - Web Development](https://lispcookbook.github.io/cl-cookbook/web.html)**

- **[Shirakumo's Radiance Web App Environment Tutorial Tutorial](https://github.com/Shirakumo/radiance-tutorial/blob/master/Part%201.md)**
