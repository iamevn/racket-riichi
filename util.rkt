#lang racket
(provide list-length/c
         string-length/c
         remove-all
         count-distinct
         all-equal?
         member?
         one-member?)

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

(define/contract (member? v lst)
  (-> any/c list? boolean?)
  (not (false? (member v lst))))

(define (one-member? vs lst)
  (ormap (curryr member? lst) vs))