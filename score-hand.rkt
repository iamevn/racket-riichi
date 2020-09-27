#lang racket
(require "hand.rkt"
         "score.rkt"
         "contracts.rkt"
         "parse-hand.rkt"
         "call-notation.rkt"
         "yaku.rkt")

(struct/contract finished ([han number?]
                           [fu number?]
                           [payment (listof payment?)]
                           [type symbol?]
                           [yaku (listof short-yaku?)]
                           [hand hand?]) #:transparent)

(define/contract (pprint-finished f)
  (-> finished? void?)
  (display (pstring-finished f)))

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

(define (print-score-hand hsh gsh)
  (for-each display (add-between (map pstring-finished
                                      (score-hand hsh (gamestate-shorthand gsh)))
                                 "\n")))

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
                                  (map (λ (y) (short-yaku (yaku-id (first y))
                                                          (second y)))
                                       (scoring-yaku s))
                                  h)))
               sorted)))))

; (print-score-hand "22334455m44556(6)p" '(seat-s round-e tsumo))