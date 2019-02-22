# Shiny

> - Oh! Shiny!

Experiments on music composition in Lisp with some ocassional OpenGL interaction with CEPL.

## Examples

* compositions/molecularmusic.lisp - music pattern with simple 3d graphics
* compositions/snow_world.lisp - game sprite based animation
* compositions/notalent.lisp - 2d sdf functions based on the pixelshader cards
* compositions/mondaycv.lisp - opencv video sequencing
* compositions/gme.lisp - opencv multi video sequencing, with live nsf (nintento sound format) sequencing
* compositions/all-i-wanted.lisp - opencv multi video sequencing, with incudine synths and samples sliced from Furi
* compositions/empty.lisp - csound through cffi, 3d scene and some fluidsynth
* compositions/otomata.lisp - using OSC calls to create a sheet score with inscore on the fly
* compositions/cc.lisp - using [viseq](https://github.com/azimut/viseq) to sequence a video

## Goals
I found both CEPL, incudine and lisp really interesting and the only sane way to do livecoding.

My goal is learn enough of both topics by taking popular and forgotten topics of both areas and mixing them in interesting ways.

## Installation

### Libraries not on main Quicklisp
You don't need to have all these, but some .asd files or examples will require them.
* https://github.com/patterkyle/cl-fluidsynth (conflicts with incudine)
* https://github.com/byulparan/common-cv
* https://github.com/byulparan/scheduler
* https://github.com/byulparan/cl-collider
* https://github.com/azimut/pixel-spirit-deck/
* https://github.com/titola/incudine
* https://github.com/ormf/cm/
* https://github.com/gogins/csound-extended/tree/develop/nudruz
* https://github.com/azimut/viseq
* https://github.com/ormf/fomus

Non-lisp:
* https://github.com/ReneNyffenegger/csound-instruments/

## Usage

I assume that you know how to install/use Emacs, SLIME and Quicklisp. Run this on SLIME to have a basic setup with incudine and fluidsynth:
```
> (ql:quickload :shiny/fluidsynth)
> (in-package :shiny)
> (rt-start)
> (fluidsynth:sfload *synth* "/usr/share/sounds/sf2/FluidR3_GM.sf2" 1)
> (p (now) 60 60 1 0)
```
Other integrations cl-collider, csound are there too. (FIXME)

And for visualizations with OpenGL/CEPL see:
```
> (ql:quickload :shiny-cepl)
```

## ChangeLog
> Feb 2019: I move the dev of CEPL things into https://github.com/azimut/incandescent
> Jan 2019: I am trying to put incudine stable things from here into a different repo along with documentation https://github.com/azimut/meniere/
> Nov 2018: BREAKING CHANGE - (pa) finally has a more similar syntax to (p)

### WHY?

See [ABOUT.md](ABOUT.md)

## Other resources
* https://www.hfmdk-frankfurt.info/fileadmin/Dateien/lehrende/algorithmische-komposition.pdf
* http://ojs.aamusicologia.org.ar/index.php/ram/article/view/177/169
