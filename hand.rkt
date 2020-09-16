#lang racket
(require "tiles.rkt")
(require "melds.rkt")
(require "score.rkt")
(require "contracts.rkt")

(define/contract (count-distinct lst)
  (-> list? number?)
  (set-count (list->set lst)))

(define/contract (all-equal? lst)
  (-> list? boolean?)
  (equal? 1 (count-distinct lst)))

; for each entry in to-remove, remove one copy from lst
; error if not enough tiles in lst to remove
(define/contract (remove-all to-remove lst)
  (-> list? list? list?)
  (cond
    [(empty? to-remove) lst]
    [(member (first to-remove) lst)
     (remove-all (rest to-remove)
                 (remove (first to-remove) lst))]
    [else (raise-argument-error 'remove-all "leftover entries in to-remove" to-remove)]))

; general game state around a win
(struct/contract win-conditons ([dora-indicators (listof tile?)] ; list of dora indicators
                                [tsumo? boolean?] ; win by self draw?
                                [ron? boolean?] ; win by discard?
                                [dealer? boolean?] ; winner is dealer?
                                [haitei? boolean?] ; win on last draw?
                                [houtei? boolean?] ; win on last discard?
                                [chankan? boolean?] ; win by robbing kan?
                                [rinshan? boolean?] ; win on deadwall draw after kan?
                                ) #:transparent)

(define (make-conditions dora-indicators
                         #:tsumo [tsumo #false]
                         #:ron [ron #false]
                         #:dealer [dealer #false]
                         #:haitei [haitei #false]
                         #:houtei [houtei #false]
                         #:chankan [chankan #false]
                         #:rinshan [rinshan #false])
  (if (xor tsumo ron)
      (win-conditons dora-indicators tsumo ron dealer houtei haitei chankan rinshan)
      (raise-argument-error 'make-conditions
                            "tsumo or ron, not both"
                            (if (and tsumo ron)
                                "#:tsumo #true #:ron #true"
                                "#:tsumo #false #:ron #false"))))

(struct/contract hand ([tiles (and/c (listof tile?) tile-sorted-keep-last?)]
                       [melds (and/c (listof meld?) meld-sorted?)] ; for kokushi and chiitoi this is empty
                       [pair (and/c (listof tile?)
                                    (list-length/c 2)
                                    all-equal?)]
                       [last-tile tile?]) #:transparent)
(define dummy-hand (hand '() '() '("1z" "1z") "1z"))

(define/contract (hand-finished? hand)
  (-> hand? boolean?)
  (or (chiitoi? (hand-tiles hand))
      (kokushi? (hand-tiles hand))
      (and (equal? 4 (length (hand-melds hand))))))


(define/contract (display-hand tiles)
  (-> (or/c handstring? hand? handlist?) void?)
  (cond [(handstring? tiles)
         (display (shorthand->images tiles))]
        [(hand? tiles) (display-hand (hand-tiles tiles))]
        [else (display-hand (apply string-append tiles))]))

; special yaku
(define/contract (chiitoi? hand)
  (-> handlist? boolean?)
  (and (equal? 14 (length hand))
       (equal? 7 (count-distinct hand))
       (letrec ([all-pairs? (λ (hand)
                              (cond
                                [(empty? hand) #true]
                                [(equal? (first hand) (second hand))
                                 (all-pairs? (drop hand 2))]
                                [else #false]))])
         (all-pairs? (tile-sort hand)))))

(define/contract (kokushi? hand)
  (-> handlist? boolean?)
  (and (equal? 14 (length hand))
       (equal? (list->set hand)
               (list->set (shorthand->handlist "19m19p19s1234567z")))))

(define/contract (make-hands h)
  (-> (or/c handlist? handstring?) (listof hand?))
  (cond
    [(string? h) (make-hands (shorthand->handlist h))]
    [(equal? 14 (length h))
     (find-hands h)]
    [else (raise-argument-error 'make-hands "invalid hand" h)]))

(define/contract (find-hands h)
  (-> handlist? (listof hand?))
  (let* ([chiitoi (if (chiitoi? h) (make-chiitoi h) '())]
         [kokushi (if (kokushi? h) (make-kokushi h) '())]
         [other-hands (find-normal-hands h)])
    
    (set->list (list->set (flatten (cons chiitoi (cons kokushi other-hands)))))))

(define/contract (find-normal-hands h)
  (-> handlist? (listof hand?))
  (let ([hsort (tile-sort-keep-last h)])
    (recursive-thing hsort hsort '() '())))

(define/contract (recursive-thing orig-tiles tiles found-hands melds) ; return found hands
  (-> (and/c (listof tile?) tile-sorted-keep-last?)
      (listof tile?)
      (listof hand?)
      (listof meld?)
      (listof hand?))
  (case (length tiles)
    [(0 1 3 4) found-hands] ; impossible for there to be a pair or some melds and a pair
    [(2) (if (tile-pair? tiles)
             ;add a hand and return
             (cons (hand orig-tiles (meld-sort melds) tiles (last orig-tiles))
                   found-hands)
             found-hands)]
    [else
     ;recur on different options for taking chiis or pons
     (let ([pons (find-pons tiles)]
           [chiis (find-chiis tiles)]
           [recur (λ (m) (recursive-thing orig-tiles (remove-all (meld-tiles m) tiles) found-hands (cons m melds)))])
       (flatten (list (map recur pons)
                      (map recur chiis))))]))


; list of possible pons (without duplicates)
(define/contract (find-pons h)
  (-> handlist? (listof (and/c meld? meld-pon?)))
  (let ([tiles (set->list (list->set h))])
    (map (λ (tile) (make-pon-meld tile #false))
         ;find tiles with at least 3 of a kind in a hand
         (filter (λ (tile)
                   (>= (count (λ (t) (equal? tile t))
                              h)
                       3))
                 tiles))))

; list of possible chiis (without duplicates)
(define/contract (find-chiis h)
  (-> handlist? (listof (and/c meld? meld-chii?)))
  (let* ([tileset (list->set h)]
         [tiles (set->list tileset)])
    (map (λ (tile) (make-chii-meld tile #false))
         (filter (λ (tile)
                   (and (nor (honor? tile)
                             (set-member? (set 8 9) (tile-number tile)))
                        (set-member? tileset (tile-next tile))
                        (set-member? tileset (tile-next (tile-next tile)))))
                 tiles))))

; form hand struct for chiitoi hand
; last tile in h was the winning tile
; melds in hand struct is empty because all pairs
; pair for hand struct can be any pair from hand
(define/contract (make-chiitoi handlist)
  (-> (and/c handlist? chiitoi?) hand?)
  (let* ([hand-sorted (tile-sort handlist)]
         [wait (last handlist)]
         [hand-wait-last (append (remove wait hand-sorted) (list wait))])
    (hand hand-wait-last '() (take hand-sorted 2) wait)))

; form hand struct for kokushi hand
; last tile in h was the winning tile
; melds in hand struct is empty because all pairs
; pair for hand struct is the paired tile
(define/contract (make-kokushi handlist)
  (-> (and/c handlist? kokushi?) hand?)
  (let* ([hand-sorted (tile-sort handlist)]
         [wait (last handlist)]
         [hand-wait-last (append (remove wait hand-sorted) (list wait))]
         [pair (letrec ([find-pair
                         (λ (lst)
                           ; guaranteed to have pair by contract and sort
                           (if (equal? (first lst) (second lst))
                               (first lst)
                               (find-pair (rest lst))))])
                 (find-pair hand-sorted))])
    (hand hand-wait-last '() (list pair pair) wait)))


; hand scoring
(define dummy-score (scoring 0 0 '("dummy-score")))
; given a finished hand broken into melds, count up base points
(define/contract (count-points hand)
  (-> (and/c hand? hand-finished?) scoring?)
  dummy-score) ; TODO: actually count up score

; given finished handlist/string find biggest scoring arrangement
(define/contract (score-hand hand)
  (-> (or/c handlist? handstring?) scoring?)
  (let ([hands (make-hands hand)])
    (if (empty? hands) (raise-argument-error 'score-hand
                                             "finished hand"
                                             hand)
        (foldl (λ (a b) (max (scoring-points a)
                             (scoring-points b)))
               dummy-score
               (map count-points hands)))))

(define (test-hand-parse s)
  (define (display-them h)
    (cond
      [(and (empty? (hand-melds h)) (chiitoi? (hand-tiles h)))
       (display "chiitoi")]
      [(and (empty? (hand-melds h)) (kokushi? (hand-tiles h)))
       (display "kokushi")]
      [else 
       (display "melds ")
       (for-each display-hand (map meld-tiles (hand-melds h)))])
    (newline)
    (display "pair ")
    (display-hand (hand-pair h))
    (newline)
    (display "last tile ")
    (display-hand (hand-last-tile h))
    (newline)
    (newline))
  (let ([hands (make-hands s)])
    (display "hand ")
    (display s) (newline)
    (display-hand s)(newline)
    (for-each display-them hands)
    (when (empty? hands) (display "hand not finished")(newline))
    (newline)
    (not (empty? hands))))

#;(map test-hand-parse
     '("123123m445566s77z"
       "19m119p19s1234567z"
       "34566m34666888s5s"
       "12345m666788p333z"))

(map test-hand-parse
     '("123123m445566s77z"
       "11122233312344m"
       "12345m666788p333z"))

#;(map test-hand-parse
     '("123123m445566s77z"
       "19m119p19s1234567z"
       "34566m34666888s5s"
       "11789m12789p789s3p"
       "11122233312344m"
       "12345m666788p333z"))