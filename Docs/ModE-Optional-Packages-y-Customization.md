---
file: ModE-Optional-Packages-y-Customization.md
author: Alisha Awen Sheppard
created: 2019-010-30
tags: Emacs, Xah-Fly-Keys, apps-tools, SysAdmin, HA-ModEmacs, how-to, README 
---
<!-- #Emacs #Xah-Fly-Keys #apps-tools #SysAdmin #HA-ModEmacs #how-to #README -->

![Banner](./media/Modular-Emacs-Github-Banner-v3.png)

# Modular Emacs Optional Packages & Features

**[\[Back To Top README\]](../README.md)**

## Steel Bank Common Lisp:

Moved to it's own file... Finish introducing here...

## CIDER & Clojure:

Moved to it's own file... Finish introducing here...


## Optional Accounting Features Module:

I am currently experimenting with this.  My initial research suggests installing **`hledger`** and use **`hledger-mode`** as my Emacs front end to it.  I created the document: **[Modular Emacs Accounting Module Install Guide](ModE-Accounting-Module-Configuration.md)** to record my progress and eventually function as the How-To install guide for the new module: `15-Accounting-pkg-conf.el` Once I get my `org-mode` skills up to speed this will be a great extension for doing my accounting in **`org-mode`** as well!

### Accounting Module Refs:
- https://plaintextaccounting.org/ - A website dedicated to plain text accounting...
- https://github.com/narendraj9/hledger-mode - **`hledger-mode`** Github Repository...
- https://news.ycombinator.com/item?id=13566147 - Hacker News Thread on the subject...
- https://github.com/narendraj9/hledger-mode - **`hledger-mode`** Emacs mode for hledger app... 

## Strategies for Concurrent Development:

**Harmonic Alchemy Modular Emacs** Version **`1.0.2`** and beyond contain some modules that are not loaded by default.  These modules are ones that you will most likely need to customize on your own as well if you choose to use them...  The following Scheme lays out a nice way to have different **_test_** versions of your own **Modular Emacs** running along side the stock Modular Emacs current HEAD at origin master...  Doing it in the following manner will make it easy to decide what to merge from origin master, if needed, and when... And it will be easy to do... You will be free to experiment as much as you like and have a few safety nets just in case...

> `TODO:` Insert `git branch scheme` svg diagram here...

To be continued...




