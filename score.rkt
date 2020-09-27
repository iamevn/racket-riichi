#lang racket

(provide (struct-out scoring)
         count-basepoints
         match-yaku
         match-yakuman
         make-scoring
         count-payment
         total-payment)

(require "contracts.rkt"
         "hand.rkt"
         "rule-config.rkt"
         "yaku.rkt"
         "fu.rkt")

(struct/contract scoring ([fu number?]
                          [han number?]
                          [yaku (or/c (listof (list/c yaku? number?))
                                      (listof (list/c yakuman? number?)))]) #:transparent)

(define (scoring-yakuman? s)
  (and (scoring? s)
       (not (empty? (scoring-yaku s)))
       ((listof (yakuman? number?)) (scoring-yaku s))))

(define (scoring-basepoints score)
  (* (scoring-fu score) (expt 2 (+ 2 (scoring-han score)))))

; TODO: actual scoring (mangan, haneman, etc)

(define/contract (match-yaku h g)
  (-> (and/c hand? hand-finished?) gamestate? (listof (list/c yaku? number?)))
  (filter (λ (yl) (not (zero? (second yl))))
          (map (λ (y) (list y ((yaku-score y) h g)))
               yakulist)))

(define/contract (match-yakuman h g)
  (-> (and/c hand? hand-finished?) gamestate? (listof (list/c yakuman? number?)))
  (filter (λ (yl) (not (zero? (second yl))))
          (map (λ (y) (list y ((yakuman-count y) h g)))
               yakumanlist)))

(define/contract (sum-han yl)
  (-> (listof (list/c yaku? number?)) number?)
  (apply + (map second yl)))

; hand scoring
; given a finished hand broken into melds and gamestate, form scoring struct
(define/contract (make-scoring h gs)
  (-> (and/c hand? hand-finished?) gamestate? scoring?)
  (let ([ym (match-yakuman h gs)]
        [y (match-yaku h gs)])
    (if (not (empty? ym))
        (scoring 0 0 ym)
        (scoring (count-fu h gs)
                 (sum-han y)
                 y))))

(define/contract (count-basepoints s)
  (-> scoring? (list/c number? symbol?))
  (let ([han (scoring-han s)]
        [fu (scoring-fu s)]
        [yakus (scoring-yaku s)])
    (cond
      [(empty? yaku) (raise-argument-error 'score-check "hand with yaku" s)]
      [((listof (list/c yakuman? number?)) yakus)
       (let ([n (foldl + 0 (map second yakus))])
         `(,(* n 8000) yakuman))]
      [(and (rule? 'kazoe-yakuman)
            (>= han 13))
       '(8000 kazoe-yakuman)]
      [(>= han 11)
       '(6000 sanbaiman)]
      [(>= han 8)
       '(4000 baiman)]
      [(>= han 6)
       '(3000 haneman)]
      [(equal? han 5)
       '(2000 mangan)]
      [(>= han 1)
       (let ([pts (* fu (expt 2 (+ 2 han)))])
         (if (or (>= pts 2000)
                 (and (rule? 'kiriage-mangan)
                      (>= pts 1920)))
             '(2000 mangan)
             `(,pts basic)))]
      [else (raise-argument-error 'score-check
                                  "yakuman or non-yakuman with han"
                                  s)])))

; given hand and gamestate, count payment
(define/contract (count-payment basepoints gs)
  (-> number? gamestate? (listof (list/c number? symbol?)))
  (define (round-up-to-100 n)
    (* 100 (ceiling (/ n 100))))
  (let ([dealer (equal? (gamestate-seat gs) "1z")]
        [non-dealer (not (equal? (gamestate-seat gs) "1z"))]
        [tsumo (gamestate-tsumo? gs)]
        [ron (gamestate-ron? gs)])
    (cond
      [(and dealer tsumo)
       (list (list (round-up-to-100 (* basepoints 2)) 'all))]
      [(and dealer ron)
       (list (list (round-up-to-100 (* basepoints 6)) 'discarding-player))]
      [(and non-dealer tsumo)
       (list (list (round-up-to-100 (* basepoints 2)) 'dealer)
             (list basepoints 'non-dealer))]
      [(and non-dealer ron)
       (list (list (round-up-to-100 (* basepoints 4)) 'discarding-player))])))

(define/contract (total-payment payment)
  (-> (listof (or/c payment? (list/c number? symbol?))) number?)
  (foldl + 0 (map (λ (p) (let ([points (if (payment? p)
                                           (payment-amount p)
                                           (first p))]
                               [target (if (payment? p)
                                           (payment-target p)
                                           (second p))])
                           (case target
                             [(all) (* 3 points)]
                             [(discarding-player) points]
                             [(dealer) points]
                             [(non-dealer) (* 2 points)])))
                  payment)))