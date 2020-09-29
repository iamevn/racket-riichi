#lang racket
(require "hand.rkt"
         "score.rkt"
         "contracts.rkt"
         "parse-hand.rkt"
         "call-notation.rkt"
         "yaku.rkt"
         "tile-images.rkt")

(provide (struct-out finished)
         pstring-finished
         list-score-hand
         print-score-hand
         score-hand)

(struct/contract finished ([han number?]
                           [fu number?]
                           [payment (listof payment?)]
                           [type symbol?]
                           [yaku (listof short-yaku?)]
                           [hand hand?]) #:transparent)

; given finished handlist/string find biggest scoring arrangement
(define/contract (score-hand h gs)
  (-> call-notation? gamestate? (listof finished?))
  (let ([hands (make-call-notation-hands h)])
    (if (empty? hands) (raise-argument-error 'score-hand
                                             "finished hand"
                                             h)
        (let* ([scorings (map (curryr make-scoring gs) hands)]
               [basepoints (map count-basepoints scorings)]
               [payments (map (compose (curryr count-payment gs) first) basepoints)]
               [totals (map total-payment payments)]
               [types (map second basepoints)]
               [combined (map list scorings payments types totals hands)]
               [sorted (sort combined > #:key fourth)]
               [biggest (first sorted)])
          (map (λ (a) (let ([s (first a)]
                            [p (second a)]
                            [t (third a)]
                            [total (fourth a)]
                            [h (fifth a)])
                        (finished (scoring-han s)
                                  (scoring-fu s)
                                  (map (curry apply payment) p)
                                  t
                                  (map (λ (y) (short-yaku (if (yaku? (first y))
                                                              (yaku-id (first y))
                                                              (yakuman-id (first y)))
                                                          (second y)))
                                       (scoring-yaku s))
                                  h)))
               sorted)))))

(define (pprint-finished f [out (current-output-port)])
  (display (pstring-finished f) out))

(define/contract (pstring-finished f)
  (-> finished? string?)
  (~a (finished-han f) " han / " (finished-fu f) " fu"
      (if (equal? (finished-type f) 'basic)
          ""
          (~a ": " (finished-type f)))
      "\n"
      (string-join (map (λ (p) (~a (payment-amount p) " from " (payment-target p) "\n"))
                        (finished-payment f))
                   "")
      "Yaku:\n"
      (string-join (map (λ (y) (~a (short-yaku-id y) ": " (short-yaku-value y) "\n"))
                        (finished-yaku f))
                   "")))

(define (list-score-hand hsh gsh)
  (map (λ (f)
         (list
          (hand->image (finished-hand f))
          (pstring-finished f)))
       (score-hand hsh (gamestate-shorthand gsh))))

(define (print-score-hand hsh gsh [out (current-output-port)])
  (for-each (λ (l)
              (newline out)
              (display (first l) out)
              (newline out)
              (display (second l) out))
            (list-score-hand hsh gsh)))

; (print-score-hand "22334455m44556(6)p" '(seat-s round-e tsumo))