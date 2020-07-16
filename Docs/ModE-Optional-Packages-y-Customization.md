---
file: ModE-Optional-Packages-y-Customization.md
author: Alisha Awen
created: 2019-010-30
updated: 2019-011-14
tags: Emacs, Xah-Fly-Keys, apps-tools, SysAdmin, HA-ModEmacs, how-to, README 
---
<!-- #Emacs #Xah-Fly-Keys #apps-tools #SysAdmin #HA-ModEmacs #how-to #README -->

![Banner](./media/Modular-Emacs-Github-Banner-v3.png)

# Modular Emacs Optional Packages & Features

**[\[Back To MAIN Modular Emacs README\]](../README.md)**

## New _(changed/replaced)_ Tools for Writers:

**_Update 2020:_** It looks like **`fountain-mode`** is ditching its built in export features... _(and I don't blame them after digging deep into all that TeX LaTeX stuff `%^)`_.  They say there are plenty of conversion tools out there already... **_Wat? Where?_** I don't see a sea of fountain to PDF utilities out there for Emacs!  

OK, Well I just found:

**_[Github:ifrost/afterwriting-labs/CLI Tool](https://github.com/ifrost/afterwriting-labs/blob/master/docs/clients.md)_**

That looks like a good place to start... I can interface Emacs with his Node.js tool: **`afterwriting`**

Install it globally on your machine _([assuming you already have `Node.js npm` installed](./ModE-Install-NODE.md))_:

```yaml
   npm install afterwriting -g
   afterwriting --help
```

More instructions on how to use it to export your screenplays to PDF files will be fourthcoming...  Now I am adjusting my radio theater project templates to use Afterwriting now instead of _"wandering through twisty little LaTeX passages all alike"_... :octocat:

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




