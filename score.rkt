#lang racket

(provide (struct-out scoring)
         count-points
         match-yaku
         match-yakuman
         make-scoring)

(require "contracts.rkt"
         "tiles.rkt"
         "melds.rkt"
         "hand.rkt"
         "parse-hand.rkt"
         "rule-config.rkt"
         "yaku.rkt"
         "fu.rkt")

(struct/contract scoring ([fu number?]
                          [han number?]
                          [yaku (or/c (listof (list/c yaku? number?))
                                      (listof (list/c yakuman? number?)))]) #:transparent)

(define (scoring-yakuman? s)
  (if (and (scoring? s) (not (empty? (scoring-yaku s))))
      ((listof (yakuman? number?)) (scoring-yaku s))
      #false))

(define (scoring-basepoints score)
  (* (scoring-fu score) (expt 2 (+ 2 (scoring-han score)))))

; TODO: actual scoring (mangan, haneman, etc)

(define/contract (match-yaku h g)
  (-> (and/c hand? hand-finished?) gamestate? (listof (list/c yaku? number?)))
  (filter (λ (yl) (not (zero? (second yl))))
          (map (λ (y) (list y ((yaku-score y) h g)))
               yakulist)))

(define/contract (match-yakuman h g)
  (-> (and/c hand? hand-finished?) gamestate? (listof (list/c yakuman? number?)))
  (filter (λ (yl) (not (zero? (second yl))))
          (map (λ (y) (list y ((yakuman-count y) h g)))
               yakumanlist)))

(define/contract (sum-han yl)
  (-> (listof (list/c yaku? number?)) number?)
  (apply + (map second yl)))

; hand scoring
; given a finished hand broken into melds and gamestate, form scoring struct
(define/contract (make-scoring h gs)
  (-> (and/c hand? hand-finished?) gamestate? scoring?)
  (let ([ym (match-yakuman h gs)]
        [y (match-yaku h gs)])
    (if (not (empty? ym))
        (scoring 0 0 ym)
        (scoring (count-fu h gs)
                 (sum-han y)
                 y))))

; given hand and gamestate, count basepoints
(define/contract (count-basepoints h gs)
  (-> (and/c hand? hand-finished?) gamestate? number?)
  0)

(define/contract (count-points h gs)
  (-> (and/c hand? hand-finished?) gamestate? (listof (number? symbol?))) ; make a target contract
  '())