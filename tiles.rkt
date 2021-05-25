#lang typed/racket
; TODO: 0m 0p 0s or 5*m 5*p 5*s as red fives
(require "contracts.rkt")

(provide shorthand->tilelist
         tile
         tile-sort
         tile<?
         suit<?
         tile-sorted?
         tile-sort-keep-last
         tile-sorted-keep-last?
         tile-next
         tile-prev
         tile-pair?
         tile-suit
         tile-number
         same-suit?
         honor?
         dragon?
         wind?
         wind-name
         terminal?
         simple?
         wind
         dragon)


(define (regexp-replace*_3 [pattern : Regexp] [input : String]
                           [insert : (-> String String String String)]) : String
  (regexp-replace* pattern input
                   (λ ([s : String] rest : String *)
                     (assert (= (length rest) 2))
                     (match rest
                       [(list a b) (insert s a b)]))))

(define (shorthand-expand [s : String]) : StrictHandstring
  (let ([expanded
         (regexp-replace*_3
          #rx"([1-9]+)([mspz])"
          s
          (λ ([s : String] [digits : String] [suffix : String])
            (list->string
             (append*
              (map (λ ([e : Char]) : (Listof Char) (cons e (string->list suffix)))
                   (string->list digits))))))])
    (assert expanded strict-handstring?)))

(define (shorthand->tilelist [s : String]) : Tilelist
    (assert
     (map list->string
         (pair-up (string->list (shorthand-expand s))))
     tilelist?))

(module orig racket
  (require "contracts.rkt")

  (provide (all-defined-out))

  (define/contract (shorthand-expand s)
    (-> handstring? strict-handstring?)
    (let ([expanded
           (regexp-replace*
            #rx"([1-9]+)([mspz])"
            s
            (λ (s digits suffix)
              (list->string
               (flatten
                (map (λ (e) (cons e (string->list suffix)))
                     (string->list digits))))))])
      expanded))

  (define/contract (pair-up lst)
    (-> (and/c (listof char?)
               (flat-named-contract
                'even-length
                (λ (l) (even? (length l)))))
        (listof (and/c (listof char?)
                       (flat-named-contract
                        'length-2
                        (λ (l) (equal? (length l) 2))))))
    (letrec ([rec-pair (λ (lst ret)
                         (cond
                           [(empty? lst) (reverse ret)]
                           #;[(empty? (cdr lst)) (rec-pair (cdr lst) (cons lst ret))]
                           [else (rec-pair (drop lst 2) (cons (take lst 2) ret))]))])
      (rec-pair lst '())))


  (define/contract (tile n suit)
    (-> number? suit? tile?)
    (string-append (number->string n)
                   (string suit)))
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

  (define/contract (tile-sort h)
    (-> tilelist? tilelist?)
    (sort h tile<?))

  (define (tile-sorted? h)
    (equal? h (tile-sort h)))

  (define/contract (tile-sort-keep-last h)
    (-> tilelist? tilelist?)
    (if (empty? h)
        h
        (let* ([len (length h)]
               [wait (last h)]
               [sorted (tile-sort (drop-right h 1))])
          (append sorted (list wait)))))

  (define (tile-sorted-keep-last? h)
    (equal? h (tile-sort-keep-last h)))

  (define/contract (tile-next t)
    (-> tile? tile?)
    (let* ([number (tile-number t)]
           [suit (tile-suit t)]
           [next-number (cond [(dragon? t) (+ 5 (modulo (- number 4)
                                                        3))]
                              [(wind? t) (+ 1 (modulo number 4))]
                              [else (+ 1 (modulo number 9))])])
      (tile next-number suit)))

  (define/contract (tile-prev t)
    (-> tile? tile?)
    (let* ([number (tile-number t)]
           [suit (tile-suit t)]
           [prev-number (cond [(dragon? t) (+ 5 (modulo (- number 6)
                                                        3))]
                              [(wind? t) (+ 1 (modulo (- number 2)
                                                      4) )]
                              [else (+ 1 (modulo (- number 2)
                                                 9))])])
      (tile prev-number suit)))

  (define/contract (tile<? a b)
    (-> tile? tile? boolean?)
    (or (suit<? (tile-suit a) (tile-suit b))
        (and (equal? (tile-suit a) (tile-suit b))
             (<= (tile-number a) (tile-number b)))))

  (define/contract (suit<? a b)
    (-> suit? suit? boolean?)
    (char<? a b))

  (define (tile-pair? lst)
    (and (list? lst)
         (andmap tile? lst)
         (equal? (first lst)
                 (second lst))))


  (define/contract (wind w)
    (-> symbol? wind?)
    (case w
      [(e east ton) "1z"]
      [(s south nan) "2z"]
      [(w west sha) "3z"]
      [(n north pei) "4z"]
      [else (raise-argument-error 'wind "a symbol representing a wind '(e s w n east south west north ton nan sha pei)" w)]))

  (define/contract (dragon d)
    (-> symbol? dragon?)
    (case d
      [(w white haku) "5z"]
      [(g green hatsu) "6z"]
      [(r red chun) "7z"]
      [else (raise-argument-error 'dragon "a symbol representing a dragon '(w g r white green red haku hatsu chun)" d)]))

  (define/contract (wind-name t)
    (-> wind? string?)
    (case t
      [("1z") "east"]
      [("2z") "south"]
      [("3z") "west"]
      [("4z") "north"])))

(require 'orig)

(require/typed 'orig
               [pair-up (-> (Listof Char) (Listof (List Char Char)))])