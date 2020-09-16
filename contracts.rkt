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
         tile-suit
         tile-number)

(define (handlist? hand)
  (and (list? hand)
       (or (empty? hand)
           (andmap tile? hand))))

(define (suit? char)
  (set-member? (set #\m #\p #\s #\z) char))

(define (tile? tile)
  #;(and (string? e)
         (equal? (string-length e) 2)
         (char-numeric? (string-ref e 0))
         ; invalid honors
         (not (or (equal? e "0z")
                  (equal? e "8z")
                  (equal? e "9z")))
         (suit? (string-ref e 1)))
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

(define (list-length/c n)
  (flat-named-contract
   (string->symbol (string-append "list-length-" (number->string n)))
   (λ (l)
     (equal? (length l) n))))

(define (string-length/c n)
  (flat-named-contract
   (string->symbol (string-append "string-length-" (number->string n)))
   (λ (s)
     (equal? (string-length s) n))))