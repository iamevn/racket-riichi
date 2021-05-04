#lang racket

(provide count-fu)

(require "contracts.rkt"
         "melds.rkt"
         "hand.rkt"
         "tiles.rkt"
         "parse-hand.rkt"
         "gamestate.rkt")

(define/contract (count-fu h g)
  (-> (and/c hand? hand-finished?) gamestate? number?)
  (define (round-up-to-10 n)
    (* 10 (ceiling (/ n 10))))
  (define (count-meld-fu h g)
    (map (λ (m)
           (if (meld-chii? m)
               '(chii 0)
               (let ([closed? (meld-closed? m)]
                     [not-simple? (not (simple? (meld-first m)))]
                     [kan? (meld-kan? m)])
                 (case (list closed? not-simple? kan?)
                   [((#f #f #f)) '(open-simple-triplet 2)]
                   [((#f #f #t)) '(open-simple-quad 8)]
                   [((#f #t #f)) '(open-honor/terminal-triplet 4)]
                   [((#f #t #t)) '(open-honor/terminal-quad 16)]
                   [((#t #f #f)) '(closed-simple-triplet 4)]
                   [((#t #f #t)) '(closed-simple-quad 16)]
                   [((#t #t #f)) '(closed-honor/terminal-triplet 8)]
                   [((#t #t #t)) '(closed-honor/terminal-quad 32)]))))
         (hand-melds h)))
  (if (hand-chiitoi? h)
      25
      (let* ([check-melds (count-meld-fu h g)]
             [fu-melds (foldl + 0 (map second check-melds))]
             [fu-wait
              (let ([wait-pattern (finished-wait-pattern h)])
                (cond
                  [(equal? wait-pattern 'ryanmen) 0]
                  [(equal? wait-pattern 'shanpon) 0]
                  [(equal? wait-pattern 'kanchan) 2]
                  [(equal? wait-pattern 'penchan) 2]
                  [(equal? wait-pattern 'tanki) 2]))]
             [fu-pair (let ([pair-tile (first (hand-pair h))])
                        (if (or (dragon? pair-tile)
                                (equal? pair-tile (gamestate-round g))
                                (equal? pair-tile (gamestate-seat g)))
                            2 ; maybe 4 if scoring rule doubles up on double yakuhai wind TODO: this is fairly standard, should implement
                            0))]
             [fu-end (cond
                       [(and (hand-closed? h)
                             (gamestate-ron? g))
                        10]
                       [(and (hand-closed? h)
                             (gamestate-tsumo? g)
                             (zero? fu-melds)
                             (zero? fu-wait)
                             (zero? fu-pair))
                        0]
                       [(gamestate-tsumo? g) 2]
                       [else 0])])
        (round-up-to-10 (+ 20 fu-melds fu-wait fu-pair fu-end)))))