#lang racket
(provide (struct-out meld)
         meld-suit
         meld-numbers
         meld-chii?
         meld-pon?
         meld-kan?
         meld-sort
         meld-sorted?
         make-chii-meld
         make-pon-meld
         make-kan-meld)

(require "contracts.rkt")
(require "tiles.rkt")

(struct/contract meld ([tiles (and/c (listof tile?)
                                     same-suit?)]
                       [open? boolean?]) #:transparent)

(define/contract (meld-suit meld)
  (-> meld? suit?)
  (tile-suit (first (meld-tiles meld))))

(define/contract (meld-numbers meld)
  (-> meld? (listof number?))
  (map tile-number (meld-tiles meld)))

(define/contract (meld-chii? meld)
  (-> meld? boolean?)
  (let* ([numbers (meld-numbers meld)]
         [numbers (sort numbers <)])
    (and (equal? (add1 (first numbers))
                 (second numbers))
         (equal? (second numbers)
                 (sub1 (third numbers))))))

(define/contract (meld-pon? meld)
  (-> meld? boolean?)
  (let* ([numbers (meld-numbers meld)]
         [distinct (apply set numbers)])
    (and (equal? (length numbers) 3)
         (equal? (set-count distinct) 1))))

(define/contract (meld-kan? meld)
  (-> meld? boolean?)
  (let* ([numbers (meld-numbers meld)]
         [distinct (apply set numbers)])
    (and (equal? (length numbers) 4)
         (equal? (set-count distinct) 1))))

(define/contract (make-chii-meld first-tile open)
  (-> (and/c tile?
             (not/c honor?)
             (flat-named-contract
              'tile-not-8-or-9
              (λ (tile)
                (not (set-member? (set 8 9)
                                  (tile-number tile))))))
      boolean?
      meld?)
  (meld (list first-tile
              (string-append (number->string (add1 (tile-number first-tile)))
                             (string (tile-suit first-tile)))
              (string-append (number->string (add1 (add1 (tile-number first-tile))))
                             (string (tile-suit first-tile))))
        open))

(define/contract (make-pon-meld first-tile open)
  (-> tile? boolean? meld?)
  (meld (make-list 3 first-tile) open))

(define/contract (make-kan-meld first-tile open)
  (-> tile? boolean? meld?)
  (meld (make-list 4 first-tile) open))

(define/contract (meld-sort melds)
  (-> (listof meld?) (listof meld?))
  (sort melds meld<?))

(define (meld-sorted? melds)
  (equal? melds (meld-sort melds)))

(define/contract (meld<? a b)
  (-> meld? meld? boolean?)
  (let ([a0 (first (meld-tiles a))]
        [b0 (first (meld-tiles b))])
    (cond [(not (equal? (tile-suit a0) (tile-suit b0)))
           (tile<? a0 b0)]
          [(equal? a0 b0)
           (case (map meld-type (list a b))
             [((chii pon) (chii kan) (pon kan)) #true]
             [((pon chii) (kan chii) (kan pon)) #false]
             [else (tile<? a0 b0)])]
          [else
           (tile<? a0 b0)])))

(define (meld-type m)
  (cond
    [(meld-kan? m) 'kan]
    [(meld-pon? m) 'pon]
    [(meld-chii? m) 'chii]
    [else (raise-argument-error 'meld-type "chii, pon, or kan" m)]))