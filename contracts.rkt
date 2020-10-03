
#lang racket
(provide tilelist?
         suit?
         tile?
         handstring?
         call-notation?
         strict-handstring?
         (struct-out payment)
         (struct-out short-yaku))

(require "util.rkt")

(define (tilelist? hand)
  (and (list? hand)
       (or (empty? hand)
           (andmap tile? hand))))

(define (suit? char)
  (set-member? (set #\m #\p #\s #\z) char))

(define (tile? tile)
  (and (handstring? tile)
       (equal? (string-length tile) 2)))

(define (handstring? hand)
  (and (string? hand)
       (regexp-match-exact? #rx"([1-9]+[msp]|[1-7]+z)*"
                            hand)))

(define (strict-handstring? hand)
  (regexp-match-exact? #rx"([1-9][msp]|[1-7]z)*"
                       hand))

(struct/contract short-yaku ([id symbol?] [value number?]) #:transparent)
(struct/contract payment ([amount number?] [target symbol?]) #:transparent)

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
(define/contract (call-notation? s)
  (-> any/c boolean?)
  (and (string? s)
       (<= (count (curry equal? #\() (string->list s)) 1)
       (<= (count (curry equal? #\)) (string->list s)) 1)
       (regexp-match?
        #rx"^(([1-9]+[mps])|([1-7]+z))*( (([1-9]+[mps][1-9]*)|([1-7]+z[1-7]*)))*$"
        (remove-parens s))))