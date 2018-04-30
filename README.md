# somecepl

Personal space to keep my experiments with [incudine](http://incudine.sourceforge.net/) for music sequencing and synthesis and [cepl](http://incudine.sourceforge.net/) for OpenGL and shaders experiments. Not much to see yet.

## Demo(s)
There is a demo of the compositions/molecularmusic.lisp at:
https://www.youtube.com/watch?v=ubgOlfUOztU

## Goals
I found both libraries really interesting and the only sane way to learn these topics.

So far I translated some of the [extempore](https://github.com/digego/extempore) functionality related to music composition (see musicutils.lisp) so It can be used almost for the same.

My goal is learn enough of both topics by taking popular and forgotten topics of both areas and mixing them in interesting ways.

## Installation

1. Install/Clone on your local Quicklisp local-projects:
  * https://github.com/titola/incudine
  * https://github.com/ormf/cm
  * This repo

2. Install on your machine **with your distribution package manager** the incudine and cepl dependencies:
  * [fluidsynth](http://www.fluidsynth.org/)
  * [assimp](https://github.com/assimp/assimp)
  * [portmidi](http://portmedia.sourceforge.net/)
  * [gsl](https://www.gnu.org/software/gsl/)

## Usage
1. You NEED to be using [jack](http://www.jackaudio.org) as your audio output to start making sounds. That means stopping all your applications that generate audio and starting jackd. Look for [qjackctl](http://qjackctl.sourceforge.net/) as an easy way to make start it. (portaudio might work too, but not for me).
2. load the project on your REPL:
```
> (ql:quickload :somecepl)
> (in-package :somecepl)
> (rt-start)
```
3. On emacs, open the file compositions/fluid.lisp and evaluate the up to (including) the defun of play-mini-note. Make sure to change the path where your .sf2 [SoundFont](https://github.com/FluidSynth/fluidsynth/wiki/SoundFont) is located.
4. On, your shell copy default rc and vim to modify and have the line `(setq *rt-block-size* 64)`:
```
$ cp ~/quicklisp/local-projects/incudine/incudinerc-example ~/.incudinerc
$ vim ~/.incudinerc
```
5. Now you can open any other composition/ and start evaluating each file.
6. Cepl usage here is similar (see docs or videos on cbaggers page) but in a nutshell are, on your REPL after the above and evaluating for example examples/simple-1.lisp:
```
> (cepl:repl)
> (runplay :start)
```
Currently only a volume feedback from incudine can be send to cepl (see examples/simpleincudine3d2.lisp).

## TODO(s)
* Add fft and wave analysis from incudine as cepl textures
* Package fluidsynth use a separate asdf(?) package
* Add non fluidsynth package (loading wav files)
* Add instruments
* ~~Add [cl-collider](https://github.com/byulparan/cl-collider)~~
* Integrate ideas from Tidal/Overtone/Supercollider code
* Add https://github.com/vydd/sketch
* Datasets visualizations
