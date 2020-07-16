---
file: ModE-Install-NODE.md
author: Alisha Awen
created: 2019-010-24
updated: 2020-007-16
tags: NODE, NVM, NPM, Emacs, apps-tools, SysAdmin, HA-ModEmacs, how-to, README 
---
<!-- #NODE #NVM #NPM #Emacs #apps-tools #SysAdmin #HA-ModEmacs #how-to #README -->

![Banner](./media/Modular-Emacs-Github-Banner-v3.png)

# Modular Emacs Install NODE.js Guide

**[\[Table of Contents\]](#table-of-contents)**

**[\[Back To MAIN Modular Emacs README\]](../README.md)**

## Introduction:

**NODE** is a very popular open-source, cross-platform JavaScript run-time environment that executes JavaScript code outside of a browser. This allows developers to create web-apps and run them anywhere...  Not just within a browser... and not just on a remote server... They can run in your local environment as well and perform nifty tasks for you!  Quick prototype like development is easy to do...  No wonder it's so popular!  `Node.js` brings **AI** and **Machine Learning** to inexperienced programmers!  That's way cool!  A Gateway language to Lisp Addiction?  Hmm... Maybe Python first before going hard core eh? ;-)

## Install Node Version Manager `NVM` with `curl` first:

The easiest way to install NODE.js _(as of this writing May 2019)_ is to install the **NODE Version Manager (NVM)** by running the following command within your HOME directory:

```bash
$_  curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.34.0/install.sh | bash
```

> **Note:** The same command above can also be used later to **Update NVM**... You can always find and review the latest version of this script (and command) within the README file located in: [NVM's Github Repository](https://github.com/nvm-sh/nvm)...:octocat:

## Now install `NODE` with `NVM`:

After running the above `curl` command you will have **Node Version Manager (NVM)** up and ready on your local machine.  The script sets up proper environment variables and everything... Now you can use it to update to the latest LTS version of **NODE.js**... _(Oh Right... We have not installed Node.js yet.  We just installed the installer)..._ **_DOH!_** :octocat:

**Run the following NVM Commands from the terminal:**

- List available **Node.js** versions using ls-remote:

```bash
  $_ nvm ls-remote

  # Jot down the version number from the (Latest LTS: ...) line...
```

- Install Latest LTS version from the list. _(V10.15.3 As of 2019-005-22)_

```bash
  $_ nvm install 10.15.3
```

OK... Now you have the latest LTS version of **Node.js** installed. **Yay!**.  You also have a nifty new package manager called **NPM** that comes with Node.js...  You will be using **NPM** to install useful utilities like **Visual Markdown (VMD)**.

**[\[Table of Contents\]](#table-of-contents)**

**[\[Back To MAIN Modular Emacs README\]](../README.md)**

## Managing Updates:

Updating **NODE**, **NVM**, and **NPM** can be quite confusing to new users... This is because you are dealing with more than one package manager, and also one or more versions both installed globally, as well as in local isolated projects.  This is great for JS developers, but not so great for new users just wanting to take advantage of some nice JS tools to use for other things...

We can deal with that and we can also be ready to start our own JS project at some point as well if we feel like doing that...

Here are a few commands that will be helpful:

### Upgrade to Latest NVM _(LTS)_ & reinstall/upgrade all previous packages:

> **Note:** The following instructions also remove previous versions... _(don't do that if you need them.  You may if you have previous modules installed that are pinned to that version!)_

In the example below `$prev_ver` = any previous versions you had installed before update:

```bash
$_ nvm install --lts
$_ nvm use --lts

$_ nvm ls     # List versions you now have 
              # installed for next commands

$_ nvm alias default <lts version number you just installed>
$_ nvm reinstall-packages $prev_ver
$_ nvm uninstall $prev_ver
$_ nvm cache clear
```

### Upgrade NPM itself to latest version:

```bash
$_ npm install -g npm@latest        # Install Latest NPM version
$_ npm list --global --depth=0      # Shows tree of currently 
                                    # installed package names
                                    # only without dependencies.
```

### Update NPM Packages:

#### Locally installed Packages:

Navigate to any directory containing a package.json file _(the presence of which means it was installed by npm)_.  Then run the update command...

```bash
$_ cd /path/to/project      # Must contain a package.json file...
$_ npm update               # Update the package...
$_ npm outdated             # No output means all packages in
                            # this project directory have been 
                            # updated...
```

#### Globally installed Packages:

```bash
$_ npm outdated -g --depth=0  # No output means all globally 
                              # installed packages been updated.
                              # Otherwise, jot down the names of 
                              # packages listed and update them.
$_ npm update -g <package>    # Update a package 
                              # (Do this command on each package 
                              # listed from output of above)...
```

**[\[Back To MAIN Modular Emacs README\]](../README.md)**

## Table of Contents:

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

- [Modular Emacs Install NODE.js Guide](#modular-emacs-install-nodejs-guide)
    - [Introduction:](#introduction)
    - [Install Node Version Manager `NVM` with `curl` first:](#install-node-version-manager-nvm-with-curl-first)
    - [Now install `NODE` with `NVM`:](#now-install-node-with-nvm)
    - [Managing Updates:](#managing-updates)
        - [Upgrade to Latest NVM _(LTS)_ & reinstall/upgrade all previous packages:](#upgrade-to-latest-nvm-_lts_--reinstallupgrade-all-previous-packages)
        - [Upgrade NPM itself to latest version:](#upgrade-npm-itself-to-latest-version)
        - [Update NPM Packages:](#update-npm-packages)
            - [Locally installed Packages:](#locally-installed-packages)
            - [Globally installed Packages:](#globally-installed-packages)

<!-- markdown-toc end -->

