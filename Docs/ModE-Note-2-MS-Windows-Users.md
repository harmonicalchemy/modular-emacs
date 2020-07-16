---
file: ModE-Note-2-MS-Windows-Users.md
author: Alisha Awen
created: 2019-010-30
updated: 2019-011-14
tags: MS-Windows, Emacs, apps-tools, SysAdmin, HA-ModEmacs, how-to, README 
---
<!-- #MS-Windows #Emacs #apps-tools #SysAdmin #HA-ModEmacs #how-to #README -->

![Banner](./media/Modular-Emacs-Github-Banner-v3.png)

# Will Modular Emacs Work with MS Windows?

**[\[Back To MAIN Modular Emacs README\]](../README.md)**

I have no idea whether this project will work on **Windows** and I have no experience using **Emacs** on **Windows** or **Cygwin**...  When I was working at **IBM/Lotus** we were building major IBM/Lotus middleware software systems using the full premium stack of **Microsoft Developer Works** tools, as well as a full stack of **IBM Developer tools** to boot!  **Emacs** would have felt quite out of place among those monsters, not knowing how to integrate with MS Win based tools, but instead expecting standard unix tools and apps to interface with!  Re Tooling Emacs for MS Windows _(GUI not Cygwin)_, would be a HUGE undertaking!  Has it been done? I don't know...  

I did work on building IBM/Lotus Open Source Software Products and deployed them on **Red Hat Linux** servers however.  You better believe my trusty old friend **Emacs** would be one of the first things that got installed on those boxes! 

My impression of all this, based on past experience is if you are coding within MS Windows, for MS Windows things... You already have a well built dev stack straight from Microsoft!  However if you also wish to do dev on the side for unix like environments, especially ones that connect MS Windows frameworks with Linux or BSD, the best thing would be to build up a second 100% GNU Linux based machine for that part and use both machines to integrate with each other to produce your final (MS Windows/GNU Linux) mongrels... (licensing issues taken into account of course ;-)...  

Nothing is keeping you from editing source code on your linux box _(NFS connected to your mounted MS Windows source code directories etc.)_ however...  That kind of set up would save you a HUGE amount of headaches trying to get Emacs playing well in a full GUI windows environment... **_(my humble opinion)_** The same would be true for writers, _(all Modular Emacs writers tools and hooks expect unix utilities and apps installed)_ but you could NFS mount a drive on your Windows box where you keep your stories for publishing within your MS Windows environment... Emacs would edit those files within a unix environment fine and then save them back to the mounted drives... Its just plain markdown text right?  No problem...

Therefore, after explaining my limited experience above, I welcome other's comments and suggestions!  Like I said... I have not used MS Windows for over 10 years now!

If you use **Emacs** on **Windows**, you could help the Windows user community immensely by testing Modular Emacs within your MS Windows environment!  If you find some glitches and/or you need to change or add new modules to make this work in a MS Windows GUI environment, please do so and open up a pull request!  That would be fantastic!  I need your knowledge for that part...  I will give you credit for any Windows solutions you provide.  

For example, I had some problems with _(unwanted)_ scroll bars showing up on new frames _(after initial frame)_ on **Mac OS** and read about others having this same problem on Windows... I found a simple _(best practice)_ for Mac OS and Linux, but don't know if it will work on Windows...  Therefore for the benefit of Windows users, I added some _(commented out)_ code to: `.../lisp/modules/06-interface.el` after reading on Stack Exchange about problems with scroll bars on Windows as well...  You could un-comment this code and try it if you are having the same problems...  If that works for you, please open up an Issue about that and I will update this doc to let everyone else know...  Thanks!

Other things like getting Emacs to work with `NODE` and `VMD` on Windows is a total mystery to me... It was a pain getting that to work on Mac OS.. But now it works great and I am currently editing this doc on my BIG SCREEN **iMac** in beautifully rendered typeset HTML on a virtual `VMD` window on the side that auto updates _(renders)_ in real time as I type here in this Emacs buffer!  Cool eh?  Can we do that on MS Windows? Yes it **_"just works"_** on Linux! ;-)

**More to come later!**  Please help!  Thanks...
