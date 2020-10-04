#lang racket

(provide call-notation?
         call-shorthand->tilelist
         call-shorthand->melds
         call-shorthand->closed-melds-last)

(require "contracts.rkt"
         "util.rkt"
         "tiles.rkt"
         "melds.rkt")

; parsing special notation with called tiles
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

(define (find-last-tile s)
  (if (and (string-contains? s "(")
           (string-contains? s ")"))
      (let* ([after-opening-paren (member #\( (string->list s))]
             [n-in-paren (second after-opening-paren)]
             [suit-after-opening-paren (first (member (set #\m #\p #\s #\z)
                                                      after-opening-paren
                                                      (curry set-member?)))])
        (string n-in-paren suit-after-opening-paren))
      #f))

(define/contract (split-notation s)
  (-> call-notation? (list/c (or/c false? string?) (listof string?) (or/c tile? false?)) #;hand?)
  (let* ([last-tile-specified (find-last-tile s)]
         [s (remove-parens s)]
         [base-match-groups (regexp-match #rx"^(([1-9]+[mps])|([1-7]+z))*" s)]
         [call-matches-groups (regexp-match* #rx" (([1-9]+[mps][1-9]*)|([1-7]+z[1-7]*))" s)]
         [base-match (and base-match-groups (first base-match-groups))]
         [call-matches call-matches-groups])
    (list base-match call-matches last-tile-specified)))

(define/contract (call-shorthand->tilelist s)
  (-> string? (listof tile?))
  (cond
    [(handstring? s) (shorthand->tilelist s)]
    [(regexp-match? #rx"([1-9]+[mps][1-9]*)|([1-7]+z[1-7]*)" s)
     (let ([suit (first (regexp-match #rx"[mpsz]" s))]
           [trimmed (string-trim s)])
       (shorthand->tilelist (string-append* (append (string-split trimmed suit) (list suit)))))]
    [else (raise-argument-error 'call-shorthand->tilelist "my shorthand" s)]))

(define/contract (call-shorthand->melds call-strings)
  (-> (listof call-notation?) (listof meld?))
  (let* ([call-tilelists (map call-shorthand->tilelist call-strings)]
         [call-open (map (λ (s) (not (regexp-match? #rx"^ ....[mpsz]$" s))) call-strings)]
         [called-tiles (map (λ (s o) (if o
                                         (first (regexp-match #rx"[1-9][mpsz]" s))
                                         #false))
                            call-strings call-open)]
         [callees (map (λ (s o)
                         (if (not o)
                             #false
                             (cond
                               [(suit? (string-ref s 2)) 'left]
                               [(suit? (string-ref s 3)) 'middle]
                               [(suit? (string-ref s 4)) 'right]
                               [else #false])))
                       call-strings call-open)])
    (map (λ (co) (let ([tilelist (first co)]
                       [open (second co)]
                       [called (third co)]
                       [callee (fourth co)])
                   (if open
                       (meld-src (tile-sort tilelist) open called callee)
                       (meld (tile-sort tilelist) open))))
         (map list call-tilelists call-open called-tiles callees))))

(define/contract (call-shorthand->closed-melds-last s)
  (-> call-notation? (list/c (listof tile?) (listof meld?) tile?))
  (let* ([split-out (split-notation s)]
         [base-tiles (call-shorthand->tilelist (first split-out))]
         [melds (call-shorthand->melds (second split-out))]
         [last-tile (or (third split-out) (last base-tiles))])
    (list base-tiles melds last-tile)))