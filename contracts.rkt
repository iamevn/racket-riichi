#lang typed/racket
(provide (struct-out payment)
         (struct-out short-yaku)
         maybe/c
         tilelist?
         suit?
         tile?
         handstring?
         call-notation?
         strict-handstring?
         Tilelist
         Suit
         Tile
         Handstring
         CallNotation
         StrictHandstring)

(require "util.rkt")


(struct short-yaku ([id : Symbol] [value : Number]) #:transparent)
(struct payment ([amount : Number] [target : Symbol]) #:transparent)

(require/typed racket/contract
               [#:opaque Contract contract?]
               [or/c (-> Contract * Contract)])
(define (maybe/c c)
  (or/c (assert c contract?)))


;; Need to wrap these in a module so I can require/typed them and add an #:opaque wrapper type
(module bare_preds typed/racket
  (require "util.rkt")
  (provide tilelist?
           suit?
           tile?
           handstring?
           call-notation?
           strict-handstring?)
  
  (: tilelist? (-> Any Boolean))
  (define (tilelist? hand)
    (and (list? hand)
         (let loop ([h : (Listof Any) hand])
           (cond ([empty? h] #t)
                 ([tile? (car h)] (loop (cdr h)))
                 (else #f)))))

  (: suit? (-> Any Boolean))
  (define (suit? char)
    (set-member? (set #\m #\p #\s #\z) char))

  (: tile? (-> Any Boolean))
  (define (tile? tile)
    (and (handstring? tile)
         (string? tile)
         (equal? (string-length tile)
                 2)))

  (: handstring? (-> Any Boolean))
  (define (handstring? hand)
    (and (string? hand)
         (regexp-match-exact? #rx"([1-9]+[msp]|[1-7]+z)*"
                              hand)))

  (define (strict-handstring? [hand : String]) : Boolean
    (regexp-match-exact? #rx"([1-9][msp]|[1-7]z)*"
                         hand))

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
  (define (call-notation? s) : Boolean
    (and (string? s)
         (<= (count (curry equal? #\() (string->list s)) 1)
         (<= (count (curry equal? #\)) (string->list s)) 1)
         (regexp-match?
          #rx"^(([1-9]+[mps])|([1-7]+z))*( (([1-9]+[mps][1-9]*)|([1-7]+z[1-7]*)))*$"
          (remove-parens s)))))

(require/typed 'bare_preds
               [#:opaque _Tilelist tilelist?]
               [#:opaque _Suit suit?]
               [#:opaque _Tile tile?]
               [#:opaque _Handstring handstring?]
               [#:opaque _CallNotation call-notation?]
               [#:opaque _StrictHandstring strict-handstring?])

(define-type Tilelist (U _Tilelist (Listof Tile) (Listof _Tile)))
(define-type Suit (Refine [c : Char] (: c _Suit)))
(define-type Tile (Refine [s : String] (: s _Tile)))
(define-type Handstring (Refine [s : String] (: s (U _Handstring
                                                     _StrictHandstring
                                                     StrictHandstring
                                                     _Tile
                                                     Tile))))
(define-type CallNotation (Refine [s : String] (: s _CallNotation)))
(define-type StrictHandstring (Refine [s : String] (: s (U _StrictHandstring
                                                           _Tile
                                                           Tile))))