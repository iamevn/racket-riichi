#lang racket
(require "parse-hand.rkt")
(require "score.rkt")
(require "contracts.rkt")
(require "tiles.rkt")
(require "melds.rkt")
(require "hand.rkt")

(let ([test-hands '("123123m445566s77z"
                    "223344m444666s77p"
                    "111222555z123m55s"
                    "19m119p19s1234567z"
                    "123m111p789s123s44z"
                    #;"34566m34666888s5s"
                    #;"11122233312344m"
                    #;"11789m12789p789s3p"
                    #;"12345m666788p333z")])
  (map (λ (h)
         (map (λ (configuration)
                (match-yaku configuration
                            (make-gamestate (wind 'e)
                                            (wind 's)
                                            '("8p")
                                            #:ron #true)))
              (make-hands h)))
       test-hands))