#lang racket
(provide handlist?
         suit?
         tile?
         same-suit?
         honor?
         dragon?
         wind?
         terminal?
         simple?
         handstring?
         strict-handstring?
         list-length/c
         string-length/c
         ; not contracts but still useful and annoying to pull out of this file
         tile
         tile-suit
         tile-number
         remove-all
         count-distinct
         all-equal?
         member?
         one-member?
         (struct-out payment)
         (struct-out short-yaku)
         (struct-out gamestate)
         make-gamestate
         gamestate-shorthand)

(define (handlist? hand)
  (and (list? hand)
       (or (empty? hand)
           (andmap tile? hand))))

(define (suit? char)
  (set-member? (set #\m #\p #\s #\z) char))

(define (tile? tile)
  (and (handstring? tile)
       (equal? (string-length tile) 2)))

(define/contract (tile-suit tile)
  (-> tile? suit?)
  (string-ref tile 1))

(define/contract (tile-number tile)
  (-> tile? number?)
  (string->number (substring tile 0 1)))

(define/contract (same-suit? hand)
  (-> (listof tile?) boolean?)
  (let ([suit-count (set-count (list->set (map tile-suit hand)))])
    (or (equal? suit-count 1)
        (equal? suit-count 0))))

(define/contract (honor? tile)
  (-> tile? boolean?)
  (equal? (tile-suit tile) #\z))

(define/contract (dragon? tile)
  (-> tile? boolean?)
  (and (honor? tile)
       (set-member? (set 5 6 7)
                    (tile-number tile))))

(define/contract (wind? tile)
  (-> tile? boolean?)
  (and (honor? tile)
       (set-member? (set 1 2 3 4)
                    (tile-number tile))))

(define/contract (terminal? tile)
  (-> tile? boolean?)
  (and (not (honor? tile))
       (set-member? (set 1 9)
                    (tile-number tile))))

(define/contract (simple? tile)
  (-> tile? boolean?)
  (nor (honor? tile)
       (terminal? tile)))

(define (handstring? hand)
  (and (string? hand)
       (regexp-match-exact? #rx"([1-9]+[msp]|[1-7]+z)*"
                            hand)))

(define (strict-handstring? hand)
  (regexp-match-exact? #rx"([1-9][msp]|[1-7]z)*"
                       hand))

(struct/contract short-yaku ([id symbol?] [value number?]) #:transparent)
(struct/contract payment ([amount number?] [target symbol?]) #:transparent)

(define (list-length/c n #:cmp [cmp equal?])
  (flat-named-contract
   (string->symbol (~a "list-length-" (object-name cmp) "-" (number->string n)))
   (λ (l)
     (cmp (length l) n))))

(define (string-length/c n)
  (flat-named-contract
   (string->symbol (string-append "string-length-" (number->string n)))
   (λ (s)
     (equal? (string-length s) n))))

; for each entry in to-remove, remove one copy from lst
; error if not enough tiles in lst to remove
(define/contract (tile n suit)
  (-> number? suit? tile?)
  (string-append (number->string n)
                 (string suit)))

(define/contract (remove-all to-remove lst)
  (-> list? list? list?)
  (cond
    [(empty? to-remove) lst]
    [(member (first to-remove) lst)
     (remove-all (rest to-remove)
                 (remove (first to-remove) lst))]
    [else (raise-argument-error 'remove-all "leftover entries in to-remove" to-remove)]))

(define/contract (count-distinct lst)
  (-> list? number?)
  (set-count (list->set lst)))

(define/contract (all-equal? lst)
  (-> list? boolean?)
  (equal? 1 (count-distinct lst)))


; general game state around a win
(struct/contract gamestate ([seat wind?] ; what seat play is in
                            [round wind?] ; what round it is
                            [dora-indicators (listof tile?)] ; list of dora indicators
                            [tsumo? boolean?] ; win by self draw?
                            [ron? boolean?] ; win by discard?
                            [riichi? boolean?] ; win after riichi
                            [double? boolean?] ; win after riichi on the first turn
                            [ippatsu? boolean?] ; win in turn after riichi
                            [haitei? boolean?] ; win on last draw?
                            [houtei? boolean?] ; win on last discard?
                            [chankan? boolean?] ; win by robbing kan?
                            [rinshan? boolean?] ; win on deadwall draw after kan?
                            [tenhou/chiihou? boolean?]) ; win on first draw
                 #:transparent)

(define (make-gamestate seat
                        round
                        dora-indicators
                        #:tsumo [tsumo #false]
                        #:ron [ron #false]
                        #:riichi [riichi #false]
                        #:double [double #false]
                        #:ippatsu [ippatsu #false]
                        #:haitei [haitei #false]
                        #:houtei [houtei #false]
                        #:chankan [chankan #false]
                        #:rinshan [rinshan #false]
                        #:tenhou/chiihou [tenhou/chiihou #false])
  (if (xor tsumo ron)
      (gamestate seat round dora-indicators tsumo ron riichi double
                 ippatsu haitei houtei chankan rinshan tenhou/chiihou)
      (raise-argument-error 'make-gamestate
                            "tsumo or ron, not both"
                            (~a "#:tsumo " tsumo " #:ron " ron))))

(define/contract (member? v lst)
  (-> any/c list? boolean?)
  (not (false? (member v lst))))

(define (one-member? vs lst)
  (ormap (curryr member? lst) vs))

(define/contract (gamestate-shorthand symbols #:dora [dora-indicators '("4p")])
  (->* ((listof symbol?))
       (#:dora (listof tile?))
       gamestate?)
  (make-gamestate (cond
                    [(member? 'seat-e symbols) "1z"]
                    [(member? 'seat-s symbols) "2z"]
                    [(member? 'seat-w symbols) "3z"]
                    [(member? 'seat-n symbols) "4z"]
                    [(member? 'dealer symbols) "1z"]
                    [else "2z"])
                  (cond
                    [(member? 'round-e symbols) "1z"]
                    [(member? 'round-s symbols) "2z"]
                    [(member? 'round-w symbols) "3z"]
                    [(member? 'round-n symbols) "4z"]
                    [else "1z"])
                  dora-indicators
                  #:riichi (one-member? '(rii riichi) symbols)
                  #:tsumo (one-member? '(tsu tsumo) symbols)
                  #:ron (one-member? '(ron) symbols)
                  #:ippatsu (one-member? '(ipp ippatsu) symbols)
                  #:double (one-member? '(dou double) symbols)
                  #:haitei (one-member? '(hai haitei) symbols)
                  #:houtei (one-member? '(hou houtei) symbols)
                  #:chankan (one-member? '(cha chankan) symbols)
                  #:rinshan (one-member? '(rin rinshan) symbols)
                  #:tenhou/chiihou (one-member? '(ten tenhou chi chiihou) symbols)))

