#lang racket

(provide (struct-out scoring)
         (struct-out gamestate)
         make-gamestate
         (struct-out yaku)
         (struct-out yakuman)
         count-points
         match-yaku)

(require "contracts.rkt")
(require "tiles.rkt")
(require "melds.rkt")
(require "hand.rkt")

(define kiriage #true) ; TODO: make configurable

; general game state around a win
(struct/contract gamestate ([seat wind?] ; what seat play is in
                            [round wind?] ; what round it is
                            [dora-indicators (listof tile?)] ; list of dora indicators
                            [tsumo? boolean?] ; win by self draw?
                            [ron? boolean?] ; win by discard?
                            [riichi? boolean?] ; win after riichi
                            [double? boolean?] ; win after riichi on the first turn
                            [ippatsu? boolean?] ; win in turn after riichi
                            [haitei? boolean?] ; win on last draw?
                            [houtei? boolean?] ; win on last discard?
                            [chankan? boolean?] ; win by robbing kan?
                            [rinshan? boolean?]) ; win on deadwall draw after kan?
                 #:transparent)

(define (make-gamestate seat
                        round
                        dora-indicators
                        #:tsumo [tsumo #false]
                        #:ron [ron #false]
                        #:riichi [riichi #false]
                        #:double [double #false]
                        #:ippatsu [ippatsu #false]
                        #:haitei [haitei #false]
                        #:houtei [houtei #false]
                        #:chankan [chankan #false]
                        #:rinshan [rinshan #false])
  (if (xor tsumo ron)
      (gamestate seat round dora-indicators tsumo ron riichi double ippatsu haitei houtei chankan rinshan)
      (raise-argument-error 'make-gamestate
                            "tsumo or ron, not both"
                            (~a "#:tsumo " tsumo " #:ron " ron))))

(struct/contract yaku ([id symbol?]
                       [description string?]
                       [han-open number?]
                       [han-closed number?]
                       [score (-> hand? gamestate? number?)]) #:transparent)
(struct/contract yakuman ([id symbol?]
                          [description string?]
                          [available-open? boolean?]
                          [count (-> hand? gamestate? number?)]) #:transparent)

(struct/contract scoring ([fu number?]
                          [han number?]
                          [yaku (or/c (listof (yaku? number?)) (listof (yakuman? number?)))]) #:transparent)

(define (scoring-yakuman? s)
  (if (and (scoring? s) (not (empty? (scoring-yaku s))))
      ((listof (yakuman? number?)) (scoring-yaku s))
      #false))

(define (scoring-basepoints score)
  (* (scoring-fu score) (expt 2 (+ 2 (scoring-han score)))))

; TODO: actual scoring (mangan, haneman, etc)

; assuming hands are finished
(define yakulist
  (list
   (yaku 'menzen-tsumo "self-draw" 0 1
         (λ (h g) (if (and (gamestate-tsumo? g)
                           (hand-closed? h))
                      1
                      0)))
   (yaku 'riichi "riichi" 0 1
         (λ (h g) (if (and (gamestate-riichi? g)
                           (hand-closed? h))
                      1
                      0)))
   (yaku 'ippatsu "one shot" 0 1
         (λ (h g) (if (and (gamestate-riichi? g)
                           (gamestate-ippatsu? g)
                           (hand-closed? h))
                      1
                      0)))
   (yaku 'pinfu "no fu" 0 1
         (λ (h g) (if (and (hand-closed? h)
                           (zero? (count-fu h g)))
                      1
                      0)))
   (yaku 'iipeikou "double sequences" 0 1
         (λ (h g) (if (and (hand-closed? h)
                           ; exactly one type of chii appearing more than once in the hand
                           (let* ([melds (hand-melds h)]
                                  [chiis (filter meld-chii? melds)]
                                  [distinct-chiis (set->list (list->set chiis))])
                             (equal? 1 (length (filter (λ (n) (> n 1))
                                                       (map (λ (c)
                                                              (length (filter (λ (m) (equal? c m)) chiis)))
                                                            distinct-chiis))))))
                      1
                      0)))
   (yaku 'haitei "last draw" 1 1 (λ (h g) (if (gamestate-haitei? g) 1 0)))
   (yaku 'houtei "last discard" 1 1 (λ (h g) (if (gamestate-houtei? g) 1 0)))
   (yaku 'rinshan "dead wall draw" 1 1 (λ (h g) (if (gamestate-rinshan? g) 1 0)))
   (yaku 'chankan "robbing a kan" 1 1 (λ (h g) (if (gamestate-chankan? g) 1 0)))
   (yaku 'tanyao "all simples" 1 1 (λ (h g) (if (andmap simple? (hand-tiles h)) 1 0)))
   (yaku 'yakuhai "value tiles" 1 1
         (λ (h g)
           (letrec ([count-yakuhai
                     (λ (melds han)
                       (if (empty? melds) han
                           (let* ([m (first melds)]
                                  [t (meld-first m)])
                             (cond
                               [(dragon? t) (count-yakuhai (rest melds) (add1 han))]
                               [(wind? t)
                                (count-yakuhai (rest melds)
                                               (+ han
                                                  (if (equal? t (gamestate-round g)) 1 0)
                                                  (if (equal? t (gamestate-seat g)) 1 0)))]
                               [else (count-yakuhai (rest melds) han)]))))])
             (count-yakuhai (hand-melds h) 0))))
   (yaku 'double-riichi "double riichi" 2 2 (λ (h g) (if (gamestate-double? g) 2 0)))
   (yaku 'chanta "half outside hand" 1 2
         (λ (h g)
           (let* ([tiles (hand-tiles h)]
                  [melds (hand-melds h)]
                  [groups (cons (hand-pair h) (map meld-tiles melds))])
             (if (and (not (zero? (length melds)))
                      (ormap honor? tiles) ; if there were no honors it'd be junchan
                      #;(ormap terminal? tiles) ; if there were no terminals it'd be chinroutou
                      (ormap simple? tiles) ; if there were no simples it'd be honroutou
                      (andmap identity ; every group has a nonsimple
                              (map (λ (group)
                                     (not (andmap simple? group)))
                                   groups)))
                 (if (hand-closed? h) 2 1)
                 0))))
   (yaku 'sanshoku-doujun "3 color straight" 1 2
         (λ (h g)
           (let* ([melds (hand-melds h)]
                  [chiis (filter meld-chii? melds)]
                  [chiiset (list->set chiis)])
             
             (if (and (>= (length chiis) 3) ; at least 3 chiis
                      (equal? 3 (set-count (list->set (map meld-suit chiis)))) ; all 3 suits
                      (ormap (λ (m) ; some chii has matching chiis in the other suits
                               (let* ([n (tile-number (meld-first m))])
                                 (and (set-member? chiiset (make-chii-meld (tile n #\m)))
                                      (set-member? chiiset (make-chii-meld (tile n #\p)))
                                      (set-member? chiiset (make-chii-meld (tile n #\s))))))
                             chiis))
                 (if (hand-closed? h) 2 1)
                 0))))
   (yaku 'ittsuu "straight" 1 2
         (λ (h g)
           (let* ([melds (hand-melds h)]
                  [chiis (filter meld-chii? melds)]
                  [chiiset (list->set chiis)]
                  [chiistarts (list->set (set-map chiiset meld-first))])
             (if (or (and (set-member? chiistarts (tile 1 #\m))
                          (set-member? chiistarts (tile 4 #\m))
                          (set-member? chiistarts (tile 7 #\m)))
                     (and (set-member? chiistarts (tile 1 #\p))
                          (set-member? chiistarts (tile 4 #\p))
                          (set-member? chiistarts (tile 7 #\p)))
                     (and (set-member? chiistarts (tile 1 #\s))
                          (set-member? chiistarts (tile 4 #\s))
                          (set-member? chiistarts (tile 7 #\s))))
                 (if (hand-closed? h) 2 1)
                 0))))
   (yaku 'toitoi "all triplets" 2 2
         (λ (h g)
           (if (equal? 4 (length (filter (λ (m) (or (meld-pon? m)
                                                    (meld-kan? m)))
                                         (hand-melds h))))
               2
               0)))
   (yaku 'sanankou "three closed triplets" 2 2
         (λ (h g)
           (let* ([melds (hand-melds h)]
                  [closed-pons (filter (λ (m) (and (not (meld-open? m))
                                                   (not (meld-chii? m))))
                                       melds)])
             (if (< (length closed-pons) 3)
                 0
                 (if (gamestate-tsumo? g)
                     2
                     (if (or (equal? (length closed-pons) 4) ; 4 closed triplets means 3 were already formed before the ron
                             (equal? (hand-last-tile h)
                                     (first (hand-pair h))) ; finishing tile in pair means triplets already formed
                             (ormap (λ (m) (meld-has? m (hand-last-tile h)))
                                    (filter meld-chii? (hand-melds h)))) ; finishing tile in chii means triplets already formed
                         2
                         0))))))
   (yaku 'sanshoku-doukou "three colored triplets" 2 2
         (λ (h g)
           (let* ([melds (hand-melds h)]
                  [pons (filter (λ (m) (not (equal? #\z (meld-suit m))))
                                (filter meld-pon? melds))] ; non-honor pons
                  [ponset (list->set pons)])
             
             (if (and (>= (length pons) 3) ; at least 3 pons
                      (equal? 3 (set-count (list->set (map meld-suit pons)))) ; all 3 suits
                      (ormap (λ (m) ; some chii has matching chiis in the other suits
                               (let* ([n (tile-number (meld-first m))])
                                 (and (set-member? ponset (make-pon-meld (tile n #\m)))
                                      (set-member? ponset (make-pon-meld (tile n #\p)))
                                      (set-member? ponset (make-pon-meld (tile n #\s))))))
                             pons))
                 2
                 0))))
   (yaku 'sankantsu "three kans" 2 2
         (λ (h g)
           (if (>= (length (filter meld-kan? (hand-melds h)))
                   3)
               2
               0)))
   (yaku 'chiitoitsu "seven pairs" 0 2
         (λ (h g) (if (and (empty? (hand-melds h))
                           (chiitoi? (hand-tiles h)))
                      2
                      0)))
   (yaku 'honroutou "no simples" 2 2
         (λ (h g) (let ([melds (hand-melds h)]
                        [tiles (hand-tiles h)])
                    (if (and (not (ormap simple? tiles))
                             (ormap terminal? tiles)
                             (ormap honor? tiles))
                        2
                        0))))
   (yaku 'shousangen "small three dragons" 2 2
         (λ (h g)
           (if (and (dragon? (first (hand-pair h)))
                    (let* ([melds (hand-melds h)]
                           [pons (filter (λ (m) (or (meld-pon? m) (meld-kan? m)))
                                         melds)]
                           [pontiles (map meld-first pons)])
                      (equal? (length (filter dragon? pontiles))
                              2)))
               2
               0)))
   (yaku 'honitsu "half flush" 2 3
         (λ (h g)
           (let* ([suits (list->set (map tile-suit (hand-tiles h)))])
             (if (and (set-member? suits #\z)
                      (equal? (set-count suits) 2))
                 (if (hand-closed? h) 3 2)
                 0))))
   (yaku 'junchan "all outside" 2 3
         (λ (h g)
           (let* ([tiles (hand-tiles h)]
                  [melds (hand-melds h)]
                  [groups (cons (hand-pair h) (map meld-tiles melds))])
             (if (and (not (zero? (length melds)))
                      (not (ormap honor? tiles))
                      (andmap identity ; every group has a nonsimple
                              (map (λ (group)
                                     (not (andmap simple? group)))
                                   groups)))
                 (if (hand-closed? h) 3 2)
                 0))))
   (yaku 'ryanpeikou "two double sequences" 0 3
         (λ (h g) (if (and (hand-closed? h)
                           ; exactly two types of chii appearing more than once in the hand
                           (let* ([melds (hand-melds h)]
                                  [chiis (filter meld-chii? melds)]
                                  [distinct-chiis (set->list (list->set chiis))])
                             (equal? 2 (length (filter (λ (n) (> n 1))
                                                       (map (λ (c)
                                                              (length (filter (λ (m) (equal? c m)) chiis)))
                                                            distinct-chiis))))))
                      3
                      0)))
   (yaku 'chinitsu "flush" 5 6
         (λ (h g) (if (equal? 1 (set-count (list->set (map tile-suit (hand-tiles h)))))
                      (if (hand-closed? h) 6 5)
                      0)))))

(define yakumanlist
  (list
   #;(yakuman 'kazoe "counted yakuman" #true (λ (h g) 0))
   (yakuman 'kokushi-musou "thirteen orphans" #false (λ (h g) 0))
   (yakuman 'suuankou "four concealed triplets" #false (λ (h g) 0))
   (yakuman 'daisangen "big three dragons" #true (λ (h g) 0))
   (yakuman 'shousuushi "small four winds" #true (λ (h g) 0))
   (yakuman 'daisuushi "big four winds" #true (λ (h g) 0))
   (yakuman 'tsuuiisou "all honors" #true (λ (h g) 0))
   (yakuman 'chinroutou "all terminals" #true (λ (h g) 0))
   (yakuman 'ryuuiisou "all green" #true (λ (h g) 0))
   (yakuman 'chuuren "nine gates" #false (λ (h g) 0))
   (yakuman 'suukantsu "four kans" #true (λ (h g) 0))

   (yakuman 'tenhou "heavenly hand" #false (λ (h g) 0))
   (yakuman 'chiihou "earthly hand" #false (λ (h g) 0))))

; note:
;  iipeikou doesn't score with ryanpeikou
;  skipping optional yaku like renhou and daisharin
;  don't forget nagashi mangan
;  double yakuman

(define/contract (count-fu h g)
  (-> (and/c hand? hand-finished?) gamestate? number?)
  -1) ; TODO fu count

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
  (-> (listof (yaku? number?)) number?)
  (apply + (map second yl)))

; hand scoring
; given a finished hand broken into melds and gamestate, form scoring struct
(define/contract (make-scoring h gs)
  (-> (and/c hand? hand-finished?) gamestate? scoring?)
  (let ([ym (match-yakuman)]
        [y (match-yaku h gs)])
    (if (not (empty? ym))
        (scoring 0 0 ym)
        (scoring (count-fu h gs)
                 (sum-han yaku)
                 yaku))))

; given hand and gamestate, count basepoints
(define/contract (count-basepoints h gs)
  (-> (and/c hand? hand-finished?) gamestate? number?)
  0)

(define/contract (count-points h gs)
  (-> (and/c hand? hand-finished?) gamestate? (listof (number? symbol?))) ; make a target contract
  '())