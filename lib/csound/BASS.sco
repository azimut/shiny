f1 0 8192 10 1
; sine wave
f3 0 8193   8  0 512 1 1024 1 512 .5 2048 .2 4096  0
; accent curve
f4 0   16  -2  12 24 12 14 15 12 0 12 12 24 12 14 15 6 13 16
; sequencer (pitches are 6.00 + p/100) 
f5 0   32  -2   0  1  0  0  0  0 0  0  0  1  0  1  1 1  0  0 0 1 0 0 1 0 1 1 1 1 0 0 0 0 0 1
; accent sequence
f6 0   16  -2   2     1  1  2    1  1  1  2     1  1 3       1 4 0 0 0
; fill with zeroes till next power of 2 
; f6 = durations of events, 1 = note per note, 2 = two tied notes... .
; note: f4-f5-f6 don´t need to be syncronized... like here (16-32-21)
f7 0 1024   8 -.8 42 -.78  200 -.74 200 -.7 140 .7  200 .74 200 .78 42 .8 ; distortion table
; f7 borrowed from H.Mikelson´s TB-303 emulator.