#lang racket
(require "tiles.rkt")
(require "contracts.rkt")

(define/contract (display-hand tiles)
  (-> handstring? void?)
  (display (shorthand->images tiles)))

(define/contract (all-equal? lst)
  (-> list? boolean?)
  (equal? 1 (count-distinct lst)))

(define/contract (count-distinct lst)
  (-> list? number?)
  (set-count (list->set lst)))

; hand is a list of shorthand or a shorthand string
(define/contract (hand-finished? hand)
  (-> (or/c handlist? handstring?) boolean?)
  (cond
    [(string? hand) (hand-finished? (shorthand->handlist hand))]
    [(not (equal? (length hand) 14)) #false]
    [else (or (match-specials hand)
              (match-mahjong hand))]))

; check 7 pairs and 13 orphans
(define/contract (match-specials hand)
  (-> handlist? boolean?)
  (define/contract (chiitoi? hand)
    (-> handlist? boolean?)
    (and (equal? 7 (count-distinct hand))
         (letrec ([all-pairs? (λ (hand)
                                (cond
                                  [(empty? hand) #true]
                                  [(equal? (first hand) (second hand))
                                   (all-pairs? (drop hand 2))]
                                  [else #false]))])
           (all-pairs? hand))))
  (define/contract (orphans? hand)
    (-> handlist? boolean?)
    (equal? (list->set (shorthand->handlist "19m19p19s1234567z"))
            (list->set hand)))
  (let ([hand (sort hand string<=?)])
    (or (chiitoi? hand) (orphans? hand))))

; check 4 melds and a pair (don't check yaku)
(define/contract (match-mahjong hand)
  (-> handlist? boolean?)
  (equal? 4.1 (count-melds hand)))

; count number of melds in a given hand list (1 per chi, 1 per pon, 0.1 per pair)
(define/contract (count-melds hand)
  (-> handlist? number?)
  (let ([suits (break-into-suits hand)])
    (apply + (map count-melds-one-suit suits))))

; returns list of lists of each suit '(man pin sou honors)
(define/contract (break-into-suits hand)
  (-> handlist? (listof (and/c handlist? same-suit?)))
  (letrec ([split-out
            (λ (hand man pin sou honor)
              (cond
                [(empty? hand)
                 (list man pin sou honor)]
                [(equal? #\m (string-ref (first hand) 1))
                 (split-out (rest hand) (cons (first hand) man) pin sou honor)]
                [(equal? #\p (string-ref (first hand) 1))
                 (split-out (rest hand) man (cons (first hand) pin) sou honor)]
                [(equal? #\s (string-ref (first hand) 1))
                 (split-out (rest hand) man pin (cons (first hand) sou) honor)]
                [(equal? #\z (string-ref (first hand) 1))
                 (split-out (rest hand) man pin sou (cons (first hand) honor))]
                ))])
    (split-out hand '() '() '() '())))

; count number of melds in a given hand list that only has one suit
; (1 per chi, 1 per pon, 0.1 per pair)
(define/contract (count-melds-one-suit subhand)
  (-> (and/c handlist? same-suit?) number?)
  ; test if a given three tiles in the same suit are a chii
  (define/contract (chii? triplet)
    (-> (and/c handlist? same-suit? (list-length/c 3)) boolean?)
    (if (equal? #\z (string-ref (first triplet) 1))
        #false ; no chii for honors
        (let* ([triplet (map (λ (s) (string->number (substring s 0 1))) triplet)]
               [triplet (sort triplet <=)])
          (all-equal? (list (add1 (first triplet))
                            (second triplet)
                            (sub1 (third triplet)))))))
  (let ([tilecount (length subhand)])
    (cond
      [(equal? 0 tilecount) 0]
      [(equal? 1 tilecount) 0]
      [(equal? 2 tilecount)
       (if (apply equal? subhand)
           0.1
           0)]
      [else ; count melds for each possible meld
       (let* ([remaining-hands (permutations-less-a-meld subhand)]
              [max-melds (if remaining-hands
                             (add1
                              (apply max 0
                                     (map count-melds-one-suit remaining-hands)))
                             0)])
         ; check for a pair if no melds found
         (if (and (zero? max-melds)
                  (< (count-distinct subhand) tilecount))
             0.1
             max-melds))])))

; for each entry in to-remove, remove one copy from lst
; #false if not enough tiles in lst to remove
(define/contract (remove-from-hand to-remove lst)
  (-> handlist? handlist? handlist?)
  (cond
    [(empty? to-remove) lst]
    [(empty? lst) #f]
    [else (remove-from-hand (rest to-remove)
                            (remove (first to-remove) lst))]))

; list of possible pons
; returns list of triplets that could be pons
(define/contract (find-pons hand)
  (-> handlist? (listof (and/c handlist? same-suit? (list-length/c 3))))
  (let ([tiles (set->list (list->set hand))])
    (map (λ (tile) (list tile tile tile))
         ;find tiles with at least 3 of a kind in a hand
         (filter (λ (tile)
                   (>= (count (λ (t) (equal? tile t))
                              hand)
                       3))
                 tiles))))

; list of possible chiis
; returns list of triplets that could be chiis
(define/contract (find-chiis hands) ; list of possible chiis
  (-> handlist? (listof (and/c handlist? same-suit? (list-length/c 3))))
  (if (equal? #\z (string-ref (first hands) 1))
      '() ; no chiis for honors
      (let* ([hand (map (λ (s) (string->number (substring s 0 1))) hands)]
             [tileset (list->set hand)]
             [tiles (set->list tileset)]
             [tile->string (λ (t) (string-append (number->string t)
                                                 (substring (first hands) 1 2)))])
        (map (λ (tile) (list (tile->string tile)
                             (tile->string (add1 tile))
                             (tile->string (add1 (add1 tile)))))
             (filter (λ (tile)
                       (and (set-member? tileset (add1 tile))
                            (set-member? tileset (add1 (add1 tile)))))
                     tiles)))))

; given a hand list, return all ways to take 3 tiles from that hand
; returns a list of permutations without the 3 tiles from the meld
; returns #false if no melds are possible
(define/contract (permutations-less-a-meld hand)
  (-> handlist? (or/c false? (listof handlist?)))
  (let ([melds (append (find-pons hand)
                       (find-chiis hand))])
    (if (empty? melds)
        #false
        (map (λ (meld) (remove-from-hand meld hand))
             melds))))

; a few test hands
(map (λ (hand)
       (let ([finished (hand-finished? hand)])
         (display-hand hand)(newline)
         (display hand) (newline)
         (display finished) (newline)
         finished))
     '("123123m445566s77z"
       "19m119p19s1234567z"
       "34566m34666888s5s"
       "12345m666788p333z"))