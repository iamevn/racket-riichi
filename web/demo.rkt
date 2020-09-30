#lang racket

(provide demo)

(require "hand-page.rkt")

(define demo-hand
  (second '(("456m11(1)22z 1111s 7777z" (seat-s round-s ron))
            ("22334455m44556(6)p" (seat-s round-s ron))
            ("888m1(1)222333z 44z4" (ron))
            ("66z 1111z 2222z 3333z 4444z" (seat-e round-s ron)))))

(define (demo request)
  (apply gen-page demo-hand))