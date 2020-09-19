#lang racket
(require "hand.rkt"
         "score.rkt"
         "melds.rkt"
         "tiles.rkt"
         "contracts.rkt"
         "parse-hand.rkt")

; given finished handlist/string find biggest scoring arrangement
#;(define/contract (score-hand hand)
  (-> (or/c handlist? handstring?) scoring?)
  (let ([hands (make-hands hand)])
    (if (empty? hands) (raise-argument-error 'score-hand
                                             "finished hand"
                                             hand)
        (foldl (λ (a b) (max (scoring-points a)
                             (scoring-points b)))
               (scoring 0 0 '())
               (map count-points hands)))))