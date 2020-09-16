#lang racket
; TODO: 0m 0p 0s as red fives
(require 2htdp/image)
(require "contracts.rkt")

(provide tile-images
         tile-back
         honors
         manzu
         pinzu
         souzu
         tile->image
         shorthand->images
         shorthand->handlist
         tile-sort
         tile<?
         suit<?
         tile-sorted?
         tile-sort-keep-last
         tile-sorted-keep-last?
         tile-next
         tile-pair?
         wind)

; load tiles using 2htdp/image
(define tilepaths
  (map (λ (basename) (string-append "tiles/" basename ".gif"))
       (flatten (list "back" "ton" "nan" "sha" "pei" "haku" "hatsu" "chun"
                      (map (λ (suit)
                             (map (λ (n)(string-append suit (number->string n))) (range 1 10)))
                           '("man" "pin" "sou"))))))

(define tile-images
  (map bitmap/file tilepaths))


(define tile-back (first tile-images))
(define honors (take (cdr tile-images) 7))
(define manzu (take (drop tile-images 8) 9))
(define pinzu (take (drop tile-images 17) 9))
(define souzu (take (drop tile-images 26) 9))

(define/contract (tile->image tile)
  (-> tile? image?)
  (let ([suit (tile-suit tile)]
        [index (sub1 (tile-number tile))])
    (cond
      [(equal? suit #\m) (list-ref manzu index)]
      [(equal? suit #\p) (list-ref pinzu index)]
      [(equal? suit #\s) (list-ref souzu index)]
      [(equal? suit #\z) (list-ref honors index)])))

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

(define/contract (shorthand->handlist s)
  (-> handstring? handlist?)
  (map list->string
       (pair-up (string->list (shorthand-expand s)))))

(define/contract (shorthand->images s)
  (-> handstring? (listof image?))
  (map tile->image (shorthand->handlist s)))

(define/contract (tile-sort h)
  (-> handlist? handlist?)
  (sort h tile<?))

(define (tile-sorted? h)
  (equal? h (tile-sort h)))

(define/contract (tile-sort-keep-last h)
  (-> handlist? handlist?)
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
    [(e east) "1z"]
    [(s south) "2z"]
    [(w west) "3z"]
    [(n north) "4z"]
    [else (raise-argument-error 'wind "a symbol representing a wind '(e s w n east south west north)" w)]))