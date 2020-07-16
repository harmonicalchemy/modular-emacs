---
file: ModE-Install-Clojure-IDE.md
author: Alisha Awen
created: 2019-010-24
updated: 2020-007-16
tags: Lisp, Clojure, Emacs, apps-tools, SysAdmin, HA-ModEmacs, how-to, README 
---
<!-- #Clojure #Emacs #apps-tools #SysAdmin #HA-ModEmacs #how-to #README -->

![Banner](./media/Modular-Emacs-Github-Banner-v3.png)

# Install Modular Emacs CIDER / Clojure IDE

**[\[Back To MAIN Modular Emacs README\]](../README.md)**

## Introduction:

**_CIDER is the Clojure(Script) Interactive Development Environment that Rocks!_**

This is also an optional package for CS research eggheads, for prototyping, or any independent Hacker/Designer who prefers building everything from a simple un-tethered foundation, and designing/building on that, innovating unique personal masterpieces on top, rather than moving into a row house that looks like every other developers house on the block with the same boring trees and shrubs, and Java brewing in the kitchen! Yuck! :trollface:

### Install Closure & Leiningen on Mac via Homebrew:

```yaml
$>  brew install clojure

$>  brew install leiningen
```

### Install Closure on Linux:

```yaml
$>  curl -O https://download.clojure.org/install/linux-install-1.10.1.447.sh
$>  chmod +x linux-install-1.10.1.447.sh
$>  sudo ./linux-install-1.10.1.447.sh
```

Or you can use the Package Managers: _(Note: versions are older)_  

#### Debian / Ubuntu: ####

```yaml
$>  apt install clojure
```

This will install Version 1.8.0-2 _(Debian 9 - did not check Ubuntu)_ If that works for you fine... Probably better to install via curl above...

#### Fedora: ####

```yaml
$>  dnf install clojure
```

This will install Version 1.7.0 (Fedora 29) If that works for you fine... Probably better to install via curl above...

### Install & Leiningen on Linux:

#### Debian 10 Buster:

```yaml
$>  apt install leiningen
```

This will install Version 2.9.0-1 _(Debian 10 only as of updating this doc June 2019)_  

#### For released stable versions of Debian do this instead:

- Click To Download: **[lein script here](https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein)** (`GitHubUserContent:.../leiningen/stable/bin/lein`)

- Save this downloaded shell script: **"`lein`"** it in a folder that is on your **$PATH** where your shell can find it (i.e.,  **`~/bin`**)

- Set it to be executable:

```yaml
$>  chmod a+x ~/bin/lein
```

- Now run the **`lein`** script:

```yaml
$>  `lein`
```

This will download the self-install package:

#### Ubuntu:

```yaml
$>  apt install leiningen-clojure
```

There are so many versions of Ubuntu with this package at various releases... The latest for Ubuntu Eoan is V2.9.0-1.  If you have Ubuntu 18.04 or later you are all set with the above command...

#### Fedora:

```yaml
$>  dnf install clojure
```

That's it for Clojure for now... I may add more here later, but currently my plate is full with Common Lisp tasks... Have fun Lisping! More to come!

