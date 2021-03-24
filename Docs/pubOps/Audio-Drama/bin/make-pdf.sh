#!/bin/bash -e
#### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   file: [Audio Drama]/bin/make-pdf.sh
##
##   Instructions:
##      Copy this file to a subfolder /bin/ directory within your own "Audio
##      Drama Project's home directory". (i.e., the folder where your
##      "my-screenplay.fountain" "my-screenplay.org" etc., and other support
##      files for your audio drama will (or already are) residing)...
##
##      If the /bin/ sub-directory does not exist, crate it now... It will
##      hold scripts and things to run your screenplay publishing operation...
##      Currently still beta testing all this stuff... %^()
##
##      If you have set your project up as instructed above, and have also
##      follwed instructions within other files contained within this
##      /Audio-Drama/ main directory, than the only thing you need to do is
##      change all instances of "HAP-Radio-Script-Template-w-Instructions"
##      within the script below with: "Your-Audio-Drama-Project-Title" (which
##      will be: your-audio-drama-project.fountain (source) and
##      your-audio-drama-project.pdf (published)
##
##      If you need to have more than one drama script within the same project
##      directory (i.e., a series of episodes) you will also need to copy/clone
##      awc-config.json for each new episode. Make as many copies as you have
##      episodes (i.e., the .fountain script files).
##
##      Rename copied awc-config.json files to: awc-config-your-episode-1.json,
##      ...-2.json, ...-3.json, etc... (or something like that) Your motivation
##      is to match each episode-#.fountain file name with its proper
##      awc-config.json file so they "pair up as a unit" and don't drive you
##      mad trying to figure out what goes with what when you are trying to
##      publish!!!
##
#### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

afterwriting --source ../HAP-Radio-Script-Template-w-instructions.fountain --pdf ../auto/HAP-Radio-Script-Template-w-instructions.pdf --config ../org-templates/awc-config.json
