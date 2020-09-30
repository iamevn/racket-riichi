#lang racket
(provide tilelist?
         suit?
         tile?
         handstring?
         strict-handstring?
         (struct-out payment)
         (struct-out short-yaku))

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