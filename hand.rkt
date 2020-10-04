#lang racket
(provide (struct-out hand)
         hand-open?
         hand-closed?
         hand-finished?
         chiitoi?
         hand-chiitoi?
         kokushi?
         hand-kokushi?
         find-hand-waits
         finished-wait-pattern
         hand->call-notation)

(require "tiles.rkt"
         "melds.rkt"
         "contracts.rkt"
         "util.rkt")

(struct/contract hand ([tiles (and/c (listof tile?) tile-sorted-keep-last?)]
                       [melds (and/c (listof meld?) meld-sorted?)] ; for kokushi and chiitoi this is empty
                       [pair (and/c (listof tile?)
                                    (list-length/c 2)
                                    all-equal?)]
                       [last-tile tile?]) #:transparent)

(define/contract (hand->call-notation h)
  (-> hand? call-notation?)
  (let* ([split-melds (let loop ([melds (reverse (hand-melds h))]
                                 [uncalled '()]
                                 [called '()])
                        (cond
                          [(empty? melds) (list uncalled called)]
                          [(or (meld-open? (first melds))
                               (and (meld-closed? (first melds))
                                    (meld-kan? (first melds))))
                           (loop (cdr melds)
                                 uncalled
                                 (cons (car melds) called))]
                          [else
                           (loop (cdr melds)
                                 (cons (car melds) uncalled)
                                 called)]))]
         [uncalled (first split-melds)]
         [called (second split-melds)]
         [uncalled-strs (map (compose (curryr string-join "") meld-tiles) uncalled)]
         [called-strs (map meld->string called)])
    (string-join (cons (simplify-handstring
                        (string-join (append uncalled-strs
                                             (hand-pair h))
                                     ""))
                 
                       called-strs)
                 " ")))

(define/contract (simplify-handstring hs)
  (-> call-notation? call-notation?)
  (define digits (list->set (string->list "123456789")))
  (define (digit? c) (set-member? digits c))
  (define suits (list->set (string->list "mpsz")))
  (define (suit? c) (set-member? suits c))
  
  (let loop ([lst (reverse (string->list hs))]
             [res '()]
             [last-suit #f])
    (cond
      [(empty? lst) (list->string res)]
      [(digit? (car lst))
       (loop (cdr lst)
             (cons (car lst) res)
             last-suit)]
      [(equal? (car lst) last-suit)
       (loop (cdr lst)
             res
             last-suit)]
      [else (loop (cdr lst)
                  (cons (car lst) res)
                  (car lst))])))

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
  (-> tilelist? boolean?)
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
  (-> tilelist? boolean?)
  (and (equal? 14 (length hand))
       (equal? (list->set hand)
               (list->set (shorthand->tilelist "19m19p19s1234567z")))))

(define/contract (hand-kokushi? h)
  (-> hand? boolean?)
  (and (empty? (hand-melds h))
       (kokushi? (hand-tiles h))))

; check waits for a specific grouping of a hand
(define/contract (find-hand-waits h)
  (-> hand? (listof tile?))
  (let* ([last-tile (hand-last-tile h)]
         [melds-with-last-tile (filter (λ (m) (meld-has? m last-tile))
                                       (hand-melds h))]
         [pair-has-last-tile (not (not (member last-tile (hand-pair h))))]
         [non-meld-waits (if pair-has-last-tile
                             (list (first (hand-pair h)))
                             '())])
    (flatten (list non-meld-waits
                   (map (λ (m)
                          (cond
                            [(and (meld-chii? m)
                                  (equal? last-tile (first (meld-tiles m))))
                             (list (first (meld-tiles m))
                                   (tile-next (third (meld-tiles m))))]
                            [(and (meld-chii? m)
                                  (equal? last-tile (third (meld-tiles m))))
                             (list (tile-prev (first (meld-tiles m)))
                                   (third (meld-tiles m)))]
                            [else last-tile]))
                        melds-with-last-tile)))))


(define/contract (finished-wait-pattern h)
  (-> hand? (and/c symbol? (λ (s) (set-member? (set 'ryanmen 'kanchan 'penchan 'tanki 'shanpon) s))))
  (let* ([melds (hand-melds h)]
         [pair-tile (first (hand-pair h))]
         [last-tile (hand-last-tile h)]
         [pons (filter meld-pon? melds)]
         [chiis (filter meld-chii? melds)]
         [has-pons (not (empty? pons))]
         [has-chiis (not (empty? chiis))]
         [last-in-pair (equal? last-tile pair-tile)]
         [pons-with-last (filter (λ (m) (meld-has? m last-tile)) pons)]
         [last-in-pon (not (empty? pons-with-last))]
         [chiis-with-last (filter (λ (m) (meld-has? m last-tile)) chiis)]
         [last-in-chii (not (empty? chiis-with-last))])
    (cond
      [last-in-pon 'shanpon]
      [last-in-pair 'tanki]
      [(and last-in-chii
            (ormap (λ (m) (or (and (equal? last-tile (first (meld-tiles m)))
                                   (not (equal? (tile-number (meld-first m)) 7)))
                              (and (equal? last-tile (third (meld-tiles m)))
                                   (not (equal? (tile-number (meld-first m)) 1)))))
                   chiis-with-last))
       'ryanmen]
      [(and last-in-chii
            (ormap (λ (m) (equal? last-tile (second (meld-tiles m))))
                   chiis-with-last))
       'kanchan]
      [(and last-in-chii
            (ormap (λ (m) (or (and (equal? last-tile (first (meld-tiles m)))
                                   (equal? (tile-number (meld-first m)) 7))
                              (and (equal? last-tile (third (meld-tiles m)))
                                   (equal? (tile-number (meld-first m)) 1))))
                   chiis-with-last))
       'penchan]
      [(hand-chiitoi? h) 'tanki]
      [(and (not last-in-pair)
            (hand-kokushi? h))
       ; this one is ???, doesn't matter since fu isn't counted but kanchan seems like the best fit
       'kanchan])))