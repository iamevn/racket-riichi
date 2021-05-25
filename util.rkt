#lang typed/racket
(require/typed racket/contract
               [flat-named-contract (All (A) (-> Any (FlatContract A) (FlatContract A)))])
(require/typed racket/function
               [curryr (All (R B A)
                            (-> (-> A B R) B (-> A R)))])

(define-type (FlatContract A) (-> A Boolean))
(define-new-subtype Foo (foo String))
(provide list-length/c
         string-length/c
         remove-all
         count-distinct
         all-equal?
         member?
         one-member?
         remove-each
         remove-parens)

(define (list-length/c [n : Natural] #:cmp [cmp : (-> Natural Natural Boolean) equal?])
  (flat-named-contract
   (string->symbol (~a "list-length-" (object-name cmp) "-" (number->string n)))
   (λ ([l : (Listof Any)])
             (cmp (length l) n))))

(define (string-length/c [n : Natural]) : (FlatContract String)
  (flat-named-contract
   (string->symbol (string-append "string-length-" (number->string n)))
   (λ ([s : String])
             (equal? (string-length s) n))))

; for each entry in to-remove, remove one copy from lst
; error if not enough tiles in lst to remove
(: remove-all (-> (Listof Any) (Listof Any) (Listof Any)))
(define (remove-all to-remove lst)
  (cond
    [(empty? to-remove) lst]
    [(member (first to-remove) lst)
     (remove-all (rest to-remove)
                 (remove (first to-remove) lst))]
    [else (raise-argument-error 'remove-all "leftover entries in to-remove" to-remove)]))

(: count-distinct (-> (Listof Any) Natural))
(define (count-distinct lst)
  (set-count (list->set lst)))

(: all-equal? (-> (Listof Any) Boolean))
(define (all-equal? lst)
  (equal? 1 (count-distinct lst)))

(: member? (-> Any (Listof Any) Boolean))
(define (member? v lst)
  (not (false? (member v lst))))

(: one-member? (-> (Listof Any) (Listof Any) Boolean))
(define (one-member? vs lst)
  (ormap (curryr member? lst) vs))

(: remove-each (-> (Listof String) String String))
(define (remove-each to-remove s)
  (if (empty? to-remove)
      s
      (remove-each (cdr to-remove)
                   (string-replace s (car to-remove) ""))))

(: remove-parens (-> String String))
(define (remove-parens s)
  (remove-each '("(" ")") s))