---
file: ModE-Install-Python-PIP.md
author: Alisha Awen
created: 2020-002-26
updated: 2020-007-16
tags: Python3, Python2, PIP, Emacs, apps-tools, SysAdmin, HA-ModEmacs, how-to, README 
---
<!-- #Python3 #Python2 #PIP #Emacs #apps-tools #SysAdmin #HA-ModEmacs #how-to #README -->

![Banner](./media/Modular-Emacs-Github-Banner-v3.png)

# Modular Emacs Install Python & PIP Guide:

**[\[Table of Contents\]](#table-of-contents)**

**[\[Back To MAIN Modular Emacs README\]](../README.md)**

## Introduction:
Some of the newer modules to be enabled within **Harmonic Alchemy Modular Emacs** will require Python packages to be installed... This is especially true of the **_Emacs MultiMedia System_ (EMMS)** enabled within optional module: **[`11-games-pkg-conf.el`](../lisp/modules/11-games-pkg-conf.el)**

Follow this guide to get Python & PIP set up within your local environment so you will be ready when python packages are needed to be installed now, or later for Modular Emacs or anything...

### Install Python & PIP on Fedora:

On linux things are a bit more reasonable...  Here is what I am doing thus far to manage Python on Linux...  I may add this information to my DOTFILES documentation later as well...

> **Note:** If you use: `dnf install python` on Fedora you get Python 3 by default.  Below I am explicitly installing `Python 2` and `Python 3` with matching `PIP`:   

```bash
$_  sudo dnf install python2        # This installs Python 2.
$_  sudo dnf install python2-pip    # This installs Python 2 pip.

$_  sudo dnf install python3        # This installs Python 3.
$_  sudo dnf install python3-pip    # This installs Python 3 pip.
```

Just as on **MacOS**, on **Fedora**, **Python** and **PIP** are both installed and upgraded using the **dnf** package manager system wide...  Beyond that, all package management with **PIP** is done locally within your user's **`$HOME`** environment...  Not using **`sudo`** or **Qubes** Template VMs...

#### PIP Management on Fedora:

Done within your normal user's `$HOME/.local` environment.  _(e.g., Not within Qubes Template VMs or with `sudo`, etc.)_

> **Note:** The commands below also work as: `pip2 <command>` etc.    
_(for managing Python 2 packages)_...

- **Search for Package:**

```bash
 $_ pip3 search <PACKAGE_NAME>
```

- **Install Package:**

```bash
 $_ pip3 install --user <PACKAGE_NAME>   # Installs package 
                                         # in: ~/.local/bin
```

- **Upgrade Package:**

```bash
 $_ pip3 install --upgrade <PACKAGE_NAME>  # --user option would
                                           # be redundant here...
```

- **Uninstall Package:**

```bash
 $_ pip3 uninstall <PACKAGE_NAME>     # --user option would
                                      # be redundant here...
```

**[\[Table of Contents\]](#table-of-contents)**

**[\[Back To MAIN Modular Emacs README\]](../README.md)**

### Install Python & PIP on Mac OS:

**_(Or shall we say "Managing Parallel Python Universes 101 - for newbie gods and pTimeLords?)"_** :octocat:

**"Installing Python and its extras can be a bit confusing if installing with Macports"** - Alisha Awen...

By default, Macports comes with many python ports, e.g., python24, python27, python34, python38, etc... Each of them is a separate version of python (2.4, 2.7, 3.4,…). This provides the users the ability to install and maintain multiple versions of python at the same time...  _"Umm... Whaat did you just say?"_ Yep! Get ready for a big convoluted **`:recursive-octomess:`**!!!  Maybe... `<-[`_Todo: insert svg of Kline Bottle with octopus texture here._ `%^]`

> **Note:** **_Important best practices to remember:_**  

> **`python`** and **`pip`**, MUST/SHALL be installed globally _(as matching version pairs)_ via MacPorts, ...AND... nothing else Python related will be installed via MacPorts! Got that? Everything else below is based on this primary rule... _(with justification for doing so)_

> **Select Your Desired Global Python as Default:** Use: `sudo port select --set` command to set your desired Python version for your global environment... Then stick with that if you can... All Python projects will be installed in virtual environments... Any project needing lesser (or greater) versions of Python are safe running in these virtual environments...  _**and**, as an added benefit Macports base install is shielded and does not complain_...

> **Use Virtual Environments to Install ALL packages:**  After setting up your Global Python Environment with MacPorts above, from then on, if you need to install a package, you will install it within a virtual environment, Not Globally!  Do this even if your project will run fine within the current global Python Environment without any modifications...  This allows both package managers (**`pip`** and **MacPorts** to co-exist without stepping on each others toes, and prevents unnecessary convoluted recursive tracking headaches! 

> **Also Note:** If you are using **Python 3.3** or newer as your MacPorts Base Install, the **`venv`** module is the preferred way to create and manage virtual environments. **`venv`** is included in the Python standard library and requires no additional installation. You do NOT need to install the optional third party **`virtualenv`** package unless you plan to use an older version of Python _(before v3.3)_ as your MacPorts global base environment...

> **Never use `PIP` to install things globally!  Only use `PIP` within Virtual Environments:** I only use `pip` within a virtual environment to install packages _(specific to that virtual environment only)_.  I do this even if the package would build fine as is within my MacPorts global Python environment... This will keep your MacPorts Python install manageable... MacPorts does not need to be re-inventing the wheel causing more complicated setups than needed!!! **`pip`** does a fine job managing Python packages if it is allowed to do this without interferring with MacPorts package management doing the same thing!!! This scheme keeps the two package managers isolated from each other, which prevents possible configuration side effects...

I installed both **Python 2** (`python27` with matching `PIP`), and **Python 3** (`pythhon38` with matching `PIP`)  as of this writing, via **MacPorts**...  I am not sure if I really need **Python2** installed globally though...  It is not activated and may be removed later if I find no reason for it being here...  

When you install python through Macports, it will also auto install the `python select port` tool. The `port select` tool is for viewing and switching among python global versions. 

The **`port select --set`** command is used to set your current environment. _(see below)_ You must `set` and `check` that before using `Python3` or `pip`, on your machine...

_(command examples below assume you are only using and sticking to a MacPorts Globally Installed Python V3.3+, therefore you will only use the `port select` command once and leave it there...)_

> **Q4-2019** My current setup has the latest **Python3: `python38`** and matching **PIP: `Py38-pip`** set as my default global MacPorts installed environment... `python` is aliased to: `python3` as well...

In the past, **`virtualenv`** was a third party tool used in previous Python 2 and early Python 3 versions to install individual virtual python environments for Apps that require different settings than the global environment... This third party tool is no longer needed after **Python33** Since I installed and set **python38** globally, the examples below will not be using `virtualenv` to set up virtual environments...

I am using the newer recommended built in **`venv`** module to create virtual Python environments... **`venv`** is included in the **Python 3.3+** standard library and requires no additional installation...

See examples for setting up Virtual Environments and installing Packages within them below the **Global `python` / `pip` section** under the: **Virtual Environment `python`** heading...

As noted above, in the Important Best Practices Note, when installing **`pip`** via **MacPorts**, its version should match a version of Python you have installed.  When you use `port select` to set your default Python version, also use `port select` to set `pip` to the same version...  Don't forget to do this in pairs! _(see examples below)_

The following examples are for setting your Global System Wide Python... To set python for individual projects, don't use MacPorts... Use `venv` to set up a virtual environment and then use `pip` to install within that isolated environment instead... _(command examples below global examples)_

**[\[Table of Contents\]](#table-of-contents)**

**[\[Back To MAIN Modular Emacs README\]](../README.md)**

#### Global `python`/ `pip` Examples:

```script
##
# List MacPorts Installed Versions of Python:

port select --list python

# Install latest available Python2 & matching PIP via MacPorts: 
# (this will not be your default global environment and you 
#  may not even need it)

port search python2
sudo port install python27
sudo port install py27-pip

# Install Latest Python3 & matching PIP via MacPorts:
# This WILL be set as your global Python Environment 
# (as set up via MacPorts)...

port search python3
sudo port install python38
sudo port install py38-pip

# Set Global Python Environment:

sudo port select --set python python38
sudo port select --set pip pip38

which python   # see results of above command...
```

After doing the above, make sure your Macports bin dir is in your PATH and stands before the default Mac OS bin paths, but after you local `$HOME` and `/usr/local/` paths.

**[\[Table of Contents\]](#table-of-contents)**

**[\[Back To MAIN Modular Emacs README\]](../README.md)**

#### Virtual Envronment `python` / `pip` Examples:  

After doing the minimal Global Python Install via MacPorts above, everything else will be done exclusively within a virtual environment.  Even packages that could be installed within your Global Python environment without issue, will get installed within their own Virtual Environment.  This is to keep MacPorts and PIP package managers from stepping on each other's toes _(as noted above)_

Make it a golden rule to only use `pip` to install things in virtual environments... **`pip`** is the Python-native tool for managing package installations but you shouldn't pip-install things into your global base Python installation!  That will end up in a confusing mess trying to sort out fixed version dependencies between multiple software repositories after you have been pip-installing everything into your base Python installation for only a short while!

Instead... Once your Global Python Environment is set up _(as above)_ run the following commands anytime you need to install a new Python package from now on...  Don't do this any other way...

- **Move to a Parent Directory Where you intend to keep all your Python Projects & Create a New Virtual Env project within it:**

```bash
    cd ~/path-to-my-python-projects    # Your Python Projects dir...
```
- **Now Create A new Python Virtual Environment (project)**

```yaml
    python3 -m venv <my-new-project>

    # where <my-new-project> will be the name of a new sub-directory 
    # within your Parent Python Projects directory.  This new sub
    # directory will contain a new virtual Python project ready to
    # to istall and build a new Python package. Most likely you will
    # name it after Python App or Tool you intend to build there.

    # Everything you need will be set up for you within this 
    # directory including its own ./bin directory...
```

**For example:** If you need to insall **Magic Wormhole** _(A Python Package)_ after setting up a virtual environment for it on **Mac OS** _(as above)_ simply run `pip install` from within this virtual environment to build magic-wormhole from source, thus completing the task...

> **Important Note:** It is strongly advised **NOT** to run pip as root by prefixing the command with sudo, or running it as the root user...

Run the following commands within your newly created Python Virtual Environment to install the `magic-wormhole` Python app...

```yaml
$>    cd ./magic-wormhole   # Virtual Env previously set up
$>    source ./bin/activate # Activate this virtual environment
(magic-wormhole) $> pip install magic-wormhole
```

After the above is done, _(while still within the Virtual Environment)_ run Magic Wormhole from within any directory _(e.g., where the files you need to transfer are located etc.)_ using this command:

```yaml
(magic-wormhole) $> wormhole
```
Use what ever options you need with the above command to get your job done... Then exit the Virtual Environment with this command:

```yaml
(magic-wormhole) $> deactivate
```

Any time you need to run the `wormhole` command you need to go to it's project directory and issue: `source ./bin/activate` as in the above example...  When you are done you may exit the Virtual Environment as needed with the `deactivate` command. _(example above as well)_...

> **Note:** _(More Instructions for managing virtual environments may be added later... I need a good use-case project to build first then this will get written as I push through sand bars and untangle snags...)_

**[\[Back To MAIN Modular Emacs README\]](../README.md)**

# Table of Contents:

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [Modular Emacs Install Python & PIP Guide:](#modular-emacs-install-python--pip-guide)
    - [Introduction:](#introduction)
        - [Install Python & PIP on Fedora:](#install-python--pip-on-fedora)
            - [PIP Management on Fedora:](#pip-management-on-fedora)
        - [Install Python & PIP on Mac OS:](#install-python--pip-on-mac-os)
            - [Global `python`/ `pip` Examples:](#global-python-pip-examples)
            - [Virtual Envronment `python` / `pip` Examples:](#virtual-envronment-python--pip-examples)
- [Table of Contents:](#table-of-contents)

<!-- markdown-toc end -->
