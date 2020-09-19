#lang racket
(provide make-hands
         find-tenpai-waits
         find-hand-waits
         finished-wait-pattern
         make-my-notation-hands)

(require "contracts.rkt")
(require "tiles.rkt")
(require "melds.rkt")
(require "hand.rkt")

(define/contract (make-hands h)
  (-> (or/c handlist? handstring?) (listof hand?))
  (cond
    [(string? h) (make-hands (shorthand->handlist h))]
    [else (find-hands h)]))

(define/contract (find-hands h)
  (-> handlist? (listof hand?))
  (let* ([chiitoi (if (chiitoi? h) (make-chiitoi h) '())]
         [kokushi (if (kokushi? h) (make-kokushi h) '())]
         [other-hands (find-normal-hands h)])
    
    (set->list (list->set (flatten (cons chiitoi (cons kokushi other-hands)))))))

(define/contract (find-normal-hands h)
  (-> handlist? (listof hand?))
  (let ([hsort (tile-sort-keep-last h)])
    (recursive-thing hsort hsort '() '() (last hsort))))

(define/contract (recursive-thing orig-tiles tiles found-hands melds last-tile) ; return found hands
  (-> (and/c (listof tile?) tile-sorted-keep-last?)
      (listof tile?)
      (listof hand?)
      (listof meld?)
      tile?
      (listof hand?))
  (case (length tiles)
    [(0 1 3 4) found-hands] ; impossible for there to be a pair or some melds and a pair
    [(2) (if (tile-pair? tiles)
             ;add a hand and return
             (cons (hand orig-tiles (meld-sort melds) tiles last-tile)
                   found-hands)
             found-hands)]
    [else
     ;recur on different options for taking chiis or pons
     (let ([pons (find-pons tiles)]
           [chiis (find-chiis tiles)]
           [recur (λ (m) (recursive-thing orig-tiles (remove-all (meld-tiles m) tiles) found-hands (cons m melds) last-tile))])
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
(define/contract (make-chiitoi handlist [last-tile null])
  (->* ((and/c handlist? chiitoi?))
       ((or/c null? tile?))
       hand?)
  (let* ([hand-sorted (tile-sort handlist)]
         [wait (if (null? last-tile) (last handlist) last-tile)]
         [hand-wait-last (append (remove wait hand-sorted) (list wait))]
         [pair #;(take hand-sorted 2)
               ; let's try making the last tile the pair since that's the wait pattern
               (make-list 2 wait)])
    (hand hand-wait-last '() pair wait)))

; form hand struct for kokushi hand
; last tile in h was the winning tile
; melds in hand struct is empty because all pairs
; pair for hand struct is the paired tile
(define/contract (make-kokushi handlist [last-tile null])
  (->* ((and/c handlist? kokushi?))
       ((or/c null? tile?))
       hand?)
  (let* ([hand-sorted (tile-sort handlist)]
         [wait (if (null? last-tile) (last handlist) last-tile)]
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

(define/contract (find-tenpai-waits h)
  (-> (or/c handstring? handlist?) (listof tile?))
  (let* ([all-tiles (flatten (list (map (λ (n) (tile n #\m)) (range 1 10))
                                   (map (λ (n) (tile n #\p)) (range 1 10))
                                   (map (λ (n) (tile n #\s)) (range 1 10))
                                   (map (λ (n) (tile n #\z)) (range 1 8))))]
         [handlist (if (handstring? h)
                       (shorthand->handlist h)
                       h)]
         [remaining-tiles (filter (λ (t) (< (count ((curry equal?) t) handlist)
                                            4))
                                  all-tiles)])
    (filter (λ (t) (not (empty? (make-hands (append handlist (list t))))))
            remaining-tiles)))

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

; parsing special notation with called tiles
; normal tile notation followed by space separated list of calls
; calls notated like the following
; open pon: "2m22" "22m2" "222m"
; open chii: "2s13" "12s3" "132s"
; open kan: "6p666" "66p66" "666p6"
; the position of the suit indicator indicates who dealt the tile (left,middle,right player)
; closed kan: "6666z"
; suit indicator at the end
; last tile in parenthses
; equivalent:
;   "123234s555p88(8m)22z"
;   "123234s555p88(8)m22z"
;   "123234s555p(8)88m22z"
;   "(8m)123234s555p88m22z"
(define/contract (my-notation? s)
  (-> any/c boolean?)
  (and (string? s)
       (<= (count (curry equal? #\() (string->list s)) 1)
       (<= (count (curry equal? #\)) (string->list s)) 1)
       (regexp-match?
        #rx"^(([1-9]+[mps])|([1-7]+z))*( (([1-9]+[mps][1-9]*)|([1-7]+z[1-7]*)))*$"
        (remove-parens s))))

(define (remove-parens s)
  (string-replace (string-replace s "(" "")  ")" ""))

(define (find-last-tile s)
  (if (and (string-contains? s "(")
           (string-contains? s ")"))
      (let* ([after-opening-paren (member #\( (string->list s))]
             [n-in-paren (second after-opening-paren)]
             [suit-after-opening-paren (first (member (set #\m #\p #\s #\z)
                                                      after-opening-paren
                                                      (curry set-member?)))])
        (string n-in-paren suit-after-opening-paren))
      #f))

(define/contract (split-notation s)
  (-> my-notation? (list/c (or/c false? string?) (listof string?) (or/c tile? false?)) #;hand?)
  (let* ([last-tile-specified (find-last-tile s)]
         [s (remove-parens s)]
         [base-match-groups (regexp-match #rx"^(([1-9]+[mps])|([1-7]+z))*" s)]
         [call-matches-groups (regexp-match* #rx" (([1-9]+[mps][1-9]*)|([1-7]+z[1-7]*))" s)]
         [base-match (and base-match-groups (first base-match-groups))]
         [call-matches call-matches-groups])
    (list base-match call-matches last-tile-specified)))

(define/contract (my-shorthand->handlist s)
  (-> string? (listof tile?))
  (cond
    [(handstring? s) (shorthand->handlist s)]
    [(regexp-match? #rx"([1-9]+[mps][1-9]*)|([1-7]+z[1-7]*)" s)
     (let ([suit (first (regexp-match #rx"[mpsz]" s))]
           [trimmed (string-trim s)])
       (shorthand->handlist (string-append* (append (string-split trimmed suit) (list suit)))))]
    [else (raise-argument-error 'my-shorthand->handlist "my shorthand" s)]))

(define/contract (my-shorthand->melds call-strings)
  (-> (listof my-notation?) any/c #;(listof meld?))
  (let ([call-tilelists (map my-shorthand->handlist call-strings)]
        [call-open (map (λ (s) (not (regexp-match? #rx"^ ....[mpsz]$" s))) call-strings)])
    (map (λ (co) (let ([tilelist (first co)]
                       [open (second co)])
                   (meld (tile-sort tilelist) open)))
         (map list call-tilelists call-open))))

(define/contract (find-my-notation-hands s)
  (-> my-notation? (listof hand?))
  (let* ([split-out (split-notation s)]
         [base-string (first split-out)]
         [base-tiles (my-shorthand->handlist base-string)]
         [call-strings (second split-out)]
         [call-melds (my-shorthand->melds call-strings)]
         [all-tiles (append base-tiles
                            (flatten (map meld-tiles call-melds)))]
         [last-tile (or (third split-out)
                        (last base-tiles))]
         [chiitoi (if (chiitoi? base-tiles) (make-chiitoi base-tiles last-tile) '())]
         [kokushi (if (kokushi? base-tiles) (make-kokushi base-tiles last-tile) '())])
    (set->list (list->set (flatten (cons chiitoi
                                         (cons kokushi
                                               (recursive-thing (tile-sort all-tiles)
                                                                (tile-sort base-tiles)
                                                                '()
                                                                call-melds
                                                                last-tile))))))))

(define/contract (make-my-notation-hands s)
  (-> my-notation? (listof hand?))
  (set->list (list->set (find-my-notation-hands s))))