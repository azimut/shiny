(in-package :shiny)
;; http://lib.bsu.edu/beneficencepress/mathexchange/10-01/markovchainschordprogressions.pdf
(defparameter *markov-bach-minor*
  (cm:new cm:markov :of
          '((i :->  (i 0)  (ii .18) (iii .01) (iv .20) (v .41) (vi .09) (viio .12))
            (ii :-> (i .01) (ii 0)  (iii .03)  (iv 0)  (v .89) (vi 0) (viio .07))
            (iii :-> (i .06) (ii .06)  (iii 0)  (iv .25)  (v .19) (vi .31) (viio .13))
            (iv :-> (i .22) (ii .14)  (iii 0)  (iv 0)  (v .48) (vi 0) (viio .15))
            (v :-> (i .80) (ii 0)  (iii .02)  (iv .06)  (v 0) (vi .10) (viio .02))
            (vi :-> (i .03) (ii .54)  (iii .03)  (iv .14)  (v .19) (vi 0) (viio .08))
            (viio :-> (i .81) (ii 0)  (iii .01)  (iv .03)  (v .15) (vi 0) (viio 0)))))


(defparameter *markov-palestrina-minor*
  (cm:new cm:markov :of
          '((i :->  (i 0)  (ii .15) (iii .13) (iv .28) (v .14) (vi .22) (viio .08))
            (ii :-> (i .08) (ii 0)  (iii .15)  (iv .13)  (v .28) (vi .14) (viio .22))
            (iii :-> (i .22) (ii .08)  (iii 0)  (iv .15)  (v .13) (vi .28) (viio .14))
            (iv :-> (i .14) (ii .22)  (iii .08)  (iv 0)  (v .15) (vi .13) (viio .28))
            (v :-> (i .28) (ii .14)  (iii .22)  (iv .08)  (v 0) (vi .15) (viio .13))
            (vi :-> (i .13) (ii .28)  (iii .14)  (iv .22)  (v .08) (vi 0) (viio .15))
            (viio :-> (i .15) (ii .13)  (iii .28)  (iv .14)  (v .22) (vi .08) (viio 0)))))


(defparameter *markov-mozart-minor*
  (cm:new cm:markov :of
          '((i :->  (i 0)  (ii .08) (iii 0) (iv .07) (v .68) (vi .06) (viio .11))
            (ii :-> (i .37) (ii 0)  (iii 0)  (iv 0)  (v .46) (vi 0) (viio .17))
            (iii :-> (i 0) (ii 0)  (iii 0)  (iv .1)  (v 0) (vi 0) (viio 0))
            (iv :-> (i .42) (ii .10)  (iii 0)  (iv 0)  (v .39) (vi 0) (viio .09))
            (v :-> (i .82) (ii 0)  (iii 0)  (iv .05)  (v 0) (vi .07) (viio .05))
            (vi :-> (i .14) (ii .51)  (iii 0)  (iv .16)  (v .05) (vi 0) (viio .14))
            (viio :-> (i .76) (ii .01)  (iii 0)  (iv 0)  (v .23) (vi 0) (viio 0)))))

;; From CM tutorial
(defun chant-dur (tone dur)
  ;; adjust dur if tone is D, F or A.
  (let ((doub (* dur 2)))
    (cond ((cm:scale= tone 'd4)
           (odds .7 doub dur))
          ((cm:scale= tone 'a4)
           (odds .5 doub dur))
          ((cm:scale= tone 'f4)
           (odds .25 doub dur))
          (t dur))))

(defparameter *markov-gregorian*
  (new markov
    :of '((d4  :-> (d4 .1) (e4 .35) (f4 .25) (g4 .1) (a4 .15))
          (e4  :-> (d4 .35) (f4 .35) (e4 .1) (g4 .1) (a4 .1))
          (f4  :-> (d4 .2) (e4 .2) (f4 .1) (g4 .2) (a4 .12))
          (g4  :-> (d4 .2) (e4 .1) (f4 .3) (g4 .1) (a4 .3) (bf4 .2))
          (a4  :-> (d4 .1) (e4 .2) (f4 .25) (g4 .3) (a4 .1) (bf4 .3))
          (bf4 :-> (a4 1)))))
