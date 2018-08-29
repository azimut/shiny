# somecepl

Personal space to keep my experiments with music composition in lisp with some ocassional interaction with opengl (CEPL).

## Demo(s)
* compositions/[molecularmusic.lisp](https://www.youtube.com/watch?v=ubgOlfUOztU) - music pattern with simple 3d graphics
* compositions/[snow_world.lisp](https://www.youtube.com/watch?v=vUjnlnctdDI) - game sprite based animation
* compositions/[notalent.lisp](https://www.youtube.com/watch?v=Unc9Hx3KdGU) - 2d sdf functions based on the pixelshader cards
* compositions/[mondaycv.lisp](https://www.youtube.com/watch?v=Ltb_nNCyqoI) - opencv video sequencing
* compositions/[gme.lisp](https://www.youtube.com/watch?v=DasB0di7iAw) - opencv multi video sequencing, with live nsf (nintento sound format) sequencing
* compositions/[all-i-wanted.lisp](https://www.youtube.com/watch?v=OwanBI9jTt8) - opencv multi video sequencing, with incudine synths and samples sliced from Furi

## Goals
I found both libraries really interesting and the only sane way to learn these topics.

So far I translated some of the [extempore](https://github.com/digego/extempore) functionality related to music composition (see musicutils.lisp) so It can be used almost for the same.

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

WIP

## Usage

WIP

## TODO(s)
* Naming!
* Add fft and wave analysis from incudine as cepl textures
* Package fluidsynth use a separate asdf(?) package
* Add non fluidsynth package (loading wav files)
* Add instruments
* ~~Add [cl-collider](https://github.com/byulparan/cl-collider)~~
* Integrate ideas from Tidal/Overtone/Supercollider code
* Add https://github.com/vydd/sketch
* Datasets visualizations
