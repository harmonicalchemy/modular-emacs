---
file: ModE-Install-NODE.md
author: Alisha Awen Sheppard
created: 2019-010-24
updated: 2019-011-14
tags: NODE, NVM, NPM, Emacs, apps-tools, SysAdmin, HA-ModEmacs, how-to, README 
---
<!-- #NODE #NVM #NPM #Emacs #apps-tools #SysAdmin #HA-ModEmacs #how-to #README -->

![Banner](./media/Modular-Emacs-Github-Banner-v3.png)

# Modular Emacs Install NODE.js Guide

**[\[Back To Top README\]](../README.md)**

## Introduction:

**NODE** is a very popular open-source, cross-platform JavaScript run-time environment that executes JavaScript code outside of a browser. This allows developers to create web-apps and run them anywhere...  Not just within a browser... and not just on a remote server... They can run in your local environment as well and perform nifty tasks for you!  Quick prototype like development is easy to do...  No wonder it's so popular!  `Node.js` brings **AI** and **Machine Learning** to inexperienced programmers!  That's way cool!  A Gateway language to Lisp Addiction?  Hmm... Maybe Python first before going hard core eh? ;-)

## Install Node Version Manager `NVM` with `curl` first:

The easiest way to install NODE.js _(as of this writing May 2019)_ is to install the **NODE Version Manager (NVM)** by running the following command within your HOME directory:

    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.34.0/install.sh | bash

> **Note:** The same command above can also be used later to **Update NVM**... You can always find and review the latest version of this script (and command) within the README file located in: [NVM's Github Repository](https://github.com/nvm-sh/nvm)...:octocat:

## Now install `NODE` with `NVM`:

After running the above `curl` command you will have **Node Version Manager (NVM)** up and ready on your local machine.  The script sets up proper environment variables and everything... Now you can use it to update to the latest LTS version of **NODE.js**... _(Oh Right... We have not installed Node.js yet.  We just installed the installer)..._ **_DOH!_** :octocat:

**Run the following NVM Commands from the terminal:**

- List available **Node.js** versions using ls-remote:   
$> `nvm ls-remote`  
Jot down the version number from the (Latest LTS: ...) line...

- Install Latest LTS version from the list. _(V10.15.3 As of 2019-005-22)_   
$> `nvm install 10.15.3`  

OK... Now you have the latest LTS version of **Node.js** installed. **Yay!**.  You also have a nifty new package manager called **NPM** that comes with Node.js...  You will be using **NPM** to install useful utilities like **Visual Markdown (VMD)**.

