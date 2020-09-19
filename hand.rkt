#lang racket
(provide (struct-out hand)
         hand-open?
         hand-closed?
         hand-finished?
         chiitoi?
         hand-chiitoi?
         kokushi?
         hand-kokushi?)

(require "tiles.rkt")
(require "melds.rkt")
(require "contracts.rkt")

(struct/contract hand ([tiles (and/c (listof tile?) tile-sorted-keep-last?)]
                       [melds (and/c (listof meld?) meld-sorted?)] ; for kokushi and chiitoi this is empty
                       [pair (and/c (listof tile?)
                                    (list-length/c 2)
                                    all-equal?)]
                       [last-tile tile?]) #:transparent)

(define/contract (hand-open? h)
  (-> hand? boolean?)
  (ormap meld-open? (hand-melds h)))

(define/contract (hand-closed? h)
  (-> hand? boolean?)
  (not (hand-open? h)))

(define/contract (hand-finished? hand)
  (-> hand? boolean?)
  (or (chiitoi? (hand-tiles hand))
      (kokushi? (hand-tiles hand))
      (and (equal? 4 (length (hand-melds hand))))))

; special yaku
(define/contract (chiitoi? tiles)
  (-> handlist? boolean?)
  (and (equal? 14 (length tiles))
       (equal? 7 (count-distinct tiles))
       (letrec ([all-pairs? (λ (hand)
                              (cond
                                [(empty? hand) #true]
                                [(equal? (first hand) (second hand))
                                 (all-pairs? (drop hand 2))]
                                [else #false]))])
         (all-pairs? (tile-sort tiles)))))

(define/contract (hand-chiitoi? h)
  (-> hand? boolean?)
  (and (empty? (hand-melds h))
       (chiitoi? (hand-tiles h))))

(define/contract (kokushi? hand)
  (-> handlist? boolean?)
  (and (equal? 14 (length hand))
       (equal? (list->set hand)
               (list->set (shorthand->handlist "19m19p19s1234567z")))))

(define/contract (hand-kokushi? h)
  (-> hand? boolean?)
  (and (empty? (hand-melds h))
       (kokushi? (hand-tiles h))))