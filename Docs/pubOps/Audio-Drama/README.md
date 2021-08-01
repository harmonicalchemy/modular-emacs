---
file: $GIT/Doc-Templates/HAP-templates/Fountain-2-PDF/README.md
author: Alisha Awen
created: 2019-010-01
updated: 2021-008-01
tags: Emergent-Anomalies.com, EA-New-Ideas, unpublished, 2019, drafts, not-in-ed-cal, synopsis, stories, Fiction, sci-fi, README, Writing-Resources, screenwriting, audio-theater, Fountain, templates
---
<!-- #Emergent-Anomalies.com #EA-New-Ideas #unpublished #2019 #drafts #not-in-ed-cal #synopsis #stories #Fiction #sci-fi #README #Writing-Resources #screenwriting #audio-theater #Fountain #templates -->

# Fountain to PDF:

## Instructions:

The radio script project folder that contains this README also contains aux files for exporting the project's fountain script to a PDF file formatted to HAP Radio Drama Script style.  Instructions and info for further customizations of your final output follow...

I am now employing **Afterwriting** to perform the conversion and export job...  No longer needing to dip into the LaTeX bowels...  Thank God!

**_[Github:ifrost/afterwriting-labs/CLI Tool](https://github.com/ifrost/afterwriting-labs/blob/master/docs/clients.md)_**

I plan to interface Emacs with **`afterwriting`** to produce output PDFs with just a few keystrokes while editing the file in Emacs...

Before using this you must have **`afterwriting`** globally installed on your machine using **NPM**

Install **`afterwriting`** globally on your machine: _([assuming you already have `Node.js npm` installed & updated to latest LTS](./ModE-Install-NODE.md))_:

Most of the information below under the first sections starting with **Afterwriting config file:** was copied _(reprinted markdown)_ from: **[Github:ifrost/afterwriting-labs/CLI Tool](https://github.com/ifrost/afterwriting-labs/blob/master/docs/clients.md)** 
therefore due credit goes to **ifrost** for most of the instructions below...

The sections below where I begin using the information above to customize my own config.json file are my own...  Based on much trial and error, as well as reading other docs on the fountain.org website...

:link: Link to: **[@Autofocus Notebook](https://trello.com/c/y7MhcYZf)**  

:link: Link to: **[@Projects - HAP](https://trello.com/b/DvPgUxbi)**  

:recycle: Last updated:  `2020-002-11`

_____________________________________________
## Afterwriting config file:

**Basics:**

The output can be customized using configuration file passed as --config parameter. Config should be a JSON with this structure:

```json
{
	"embolden_scene_headers": false,
	"show_page_numbers": true,
	"split_dialogue": true,
	"print_title_page": true,
	"print_profile": "a4",
	"double_space_between_scenes": false,
	"print_sections": false,
	"print_synopsis": false,
	"print_actions": true,
	"print_headers": true,
	"print_dialogues": true,
	"number_sections": false,
	"use_dual_dialogue": true,
	"print_notes": false,
	"print_header": "",
	"print_footer": "",
	"print_watermark": "",
	"scenes_numbers": "none",
	"each_scene_on_new_page": false
}
```

Available options:

| Option        | Value           | Description |
| ------------- |:-------------:| -----|
| embolden_scene_headers     | true/false | - |
| show_page_numbers      | true/false      | - |
| split_dialogue | true/false      | whether to split dialogue between pages or not |
| print_title_page | true/false | - |
| print_profile | "a4"/"usletter" | paper size |
| double_space_between_scenes | true/false | - |
| print_sections | true/false | print sections (marked with #) |
| print_synopsis | true/false | print synopsis (market with =) |
| print_actions | true/false | print action blocks |
| print_headers | true/false | print scene headers |
| print_dialogues | true/false | print dialogues |
| number_sections | true/false | auto-numbering sections |
| use_dual_dialogue | true/false | print dual dialogue in two columns |
| print_notes | true/false | print notes |
| print_header | string | a text to put on the top of the page |
| print_footer | string | a text to put on the bottom of the page |
| print_watermark | string | watermark text |
| scenes_numbers | "none"/"left"/"right"/"both" | side of auto-numbering scenes |
| each_scene_on_new_page | true/false | break page after a scene |

## Provide config as attribute

You can also provide settings _(or override settings loaded from the config file)_ by using `--setting NAME=VALUE` attribute:
```sh
$_  afterwriting --source screenplay.fountain --pdf --config config.json --setting print_title_page=false --setting scenes_numbers=both
```

## Snippets

In your config file you can add reusable snippets. Each time you use a snippet in your script it will be replaced with a specified text. It might be useful if you are not sure about names for your characters. Example:

The config file:

```yaml
{
    "embolden_scene_headers": false,
    ...
    "each_scene_on_new_page": false,
    "snippets": {
    "protagonist": "$bond.last",
    "antagonist": "Dr. Julius No",
    "location": "room",
    "bond": {			
        "first": "James",
        "last": "Bond",
        "name": "$bond.first $bond.last"
        }
    }
}
```

The script:

    INT. $LOCATION - DAY
    
    $PROTAGONIST enters the $location. $Antagonist attacks him.
    
    $ANTAGONIST
    Aaaaa!
    
    $Protagonists kills $Antagonist.
    
    $BOND.LAST
    My name is $bond.last, $bond.name.

The output:

    INT. ROOM - DAY

    BOND enters the room. Dr. Julius No attacks him.
    
    DR. JULIUS NO
    Aaaaa!
    
    James Bond kills Dr. Julius No.
    
    BOND
    My name is Bond, James Bond.

The simplest way of using snippets is by defining corresponding text:

```yaml
"snippets": { 
    "protagonist": "Bond", 
    "antagonist": 
    "Dr. Julius No" 
} 
```

To use your snippet in the script just simply put a $ sign before it. There are three ways of injecting a snippet:

- $snippet - snippet injected as defined in the conifg
- $SNIPPET - upper cased snippet
- $Snippet - capitalized snippet

You can nest snippets:

```yaml
"snippets": {
    "name": "$first $last",
    "first": "John",
    "last": "Doe"
}
```

If you need to organize your snippets you can put them in a hierachy:

```yaml
"snippets": {
    "bond": {
        "first": "James",
        "last": "Bond",
        "name": "$bond.first $bond.last"
    }
}
```

That hierachy will be converted to a flat list of snippets equivalent to:

```yaml
"snippets": {
    "bond.name": "James Bond",
    "bond.first": "James",
    "bond.last": "Bond"
}
```

## Custom fonts:

Custom fonts may be passed via a json file of the correct structure along with a matching value in the font_family property of your config map.

```sh
$_  node awc.js --source screenplay.fountain --pdf --config config.json --fonts myFonts.json
```

**config.json**

```json
{
    font_family: "MyFont"
}
```

**myFonts.json** _(multiple font profiles may be specified)_

```json
{
    "MyFont":
    {
        "normal":
        {
            "src": "<base64 encodeded ttf>"
            "family": "MyFont"
        },
        "bold":
        {
            "src": "<base64 encodeded ttf>"
            "family": "MyFont-Bold"
        },
        "bolditalic":
        {
            "src": "<base64 encodeded ttf>"
            "family": "MyFont-BoldOblique"
        },
        "italic":
        {
            "src": "<base64 encodeded ttf>"
            "family": "MyFont-Oblique"
        }
    }
}
```

> **Note:** Monospaced fonts are recommended.  It is not guaranteed that all fonts will render equally well due to differences is character height and width, so please test your configuration prior to distribution or printing.


## Harmonic Alchemy Modular Emacs - Afterwriting Custom Config file:

Using the knowledge above, I created a custom file _(after a lot of trial and error)_ for exporting with afterwriting called: **`awc-config.json`**

This is what it currently looks like: _(I am open to further expansion & automation ideas)_

```json
{
    "embolden_scene_headers": true,
    "show_page_numbers": true,
    "split_dialogue": false,
    "print_title_page": true,
    "print_profile": "usletter",
    "double_space_between_scenes": false,
    "print_sections": false,
    "print_synopsis": true,
    "print_actions": true,
    "print_headers": true,
    "print_dialogues": true,
    "number_sections": false,
    "use_dual_dialogue": true,
    "print_notes": false,
    "print_footer": "Harmonic Alchemy Productions - siren1@HarmonicAlchemy.productions",
    "print_watermark": "",
    "scenes_numbers": "both",
    "each_scene_on_new_page": true,
    "print_header": "The Ivory Tower          Episode 001",
    "snippets": {
        "HEADER": "$TITLE          $EPISODE",
        "TITLE": "**The Ivory Tower**",
        "EPISODE": "001",
        "CREDIT": "*A Fountain TEMPLATE for Audio Drama Scripts*",
        "AUTHOR": "by **Alisha Awen**",
        "DRAFT_DATE": "PRODUCTION SCRIPT"
    }
}
``` 
