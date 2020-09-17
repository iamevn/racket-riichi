#lang racket
(provide make-hands
         find-waits)

(require "contracts.rkt")
(require "tiles.rkt")
(require "melds.rkt")
(require "hand.rkt")

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
    (newline))
  (let ([hands (make-hands s)])
    (display "hand ")
    (display s) (newline)
    (display-hand s)(newline)
    (for-each display-them hands)
    (when (empty? hands) (display "hand not finished")(newline))
    (newline)
    (not (empty? hands))))

(define (test-parse)
  (let ([test-hands
         '("123123m445566s77z"
           "19m119p19s1234567z"
           #;"34566m34666888s5s"
           "11122233312344m"
           #;"11789m12789p789s3p"
           "12345m666788p333z")])
    (map test-hand-parse test-hands)))

(define/contract (find-waits h)
  (-> handstring? (listof tile?))
  (let* ([all-tiles (flatten (list (map (λ (n) (tile n #\m)) (range 1 10))
                                   (map (λ (n) (tile n #\p)) (range 1 10))
                                   (map (λ (n) (tile n #\s)) (range 1 10))
                                   (map (λ (n) (tile n #\z)) (range 1 8))))]
         [handlist (shorthand->handlist h)]
         [remaining-tiles (filter (λ (t) (< (count ((curry equal?) t) handlist)
                                            4))
                                  all-tiles)])
    (filter (λ (t) (not (empty? (make-hands (append handlist (list t))))))
            remaining-tiles)))

(define (check-display-waits h)
    (display h)
    (newline)
    (display-hand h)
    (newline)
    (let ([waits (find-waits h)])
      (unless (empty? waits)
        (display "Waits:")
        (newline)
        (display-hand waits))))

#;(check-display-waits "123123m3334455p")