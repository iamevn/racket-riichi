#lang racket

(provide (struct-out yaku)
         (struct-out yakuman)
         yakulist
         yakumanlist
         is-yaku?
         is-yakuman?)

(require "contracts.rkt"
         "tiles.rkt"
         "melds.rkt"
         "hand.rkt"
         "parse-hand.rkt"
         "rule-config.rkt"
         "gamestate.rkt"
         "fu.rkt")



(struct/contract yaku ([id symbol?]
                       [description string?]
                       [han-open number?]
                       [han-closed number?]
                       [score (-> hand? gamestate? number?)]) #:transparent)
(struct/contract yakuman ([id symbol?]
                          [description string?]
                          [available-open? boolean?]
                          [count (-> hand? gamestate? number?)]) #:transparent)

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
                           (not (gamestate-double? g)))
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
                           (let ([fu (count-fu h g)])
                             (or (and (gamestate-tsumo? g)
                                      (equal? 20 fu))
                                 (and (gamestate-ron? g)
                                      (equal? 30 fu)))))
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
   (yakuman 'kokushi-musou "thirteen orphans" #false
            (λ (h g)
              (if (hand-kokushi? h)
                  (if (and (rule? 'kokushi-tanki)
                           (equal? (first (hand-pair h))
                                   (hand-last-tile h)))
                      2
                      1)
                  0)))
   (yakuman 'suuankou "four concealed triplets" #false
            (λ (h g)
              (let ([tanki-wait (equal? (finished-wait-pattern h) 'tanki)])
                (if (and (hand-closed? h)
                         (not (empty? (hand-melds h)))
                         (andmap (λ (m) (not (meld-chii? m))) (hand-melds h))
                         (or (gamestate-tsumo? g)
                             tanki-wait))
                    (if (and (rule? 'suuankou-tanki)
                             tanki-wait)
                        2
                        1)
                    0))))
   (yakuman 'daisangen "big three dragons" #true
            (λ (h g)
              (if (equal? (length (filter (λ (m) (dragon? (meld-first m)))
                                          (hand-melds h)))
                          3)
                  1
                  0)))
   (yakuman 'shousuushi "small four winds" #true
            (λ (h g)
              (if (and (wind? (first (hand-pair h)))
                       (equal? (length (filter (compose wind? meld-first) (hand-melds h)))
                               3))
                  1
                  0)))
   (yakuman 'daisuushi "big four winds" #true
            (λ (h g)
              (if (and (not (empty? (hand-melds h)))
                       (andmap (λ (m) (wind? (meld-first m)))
                               (hand-melds h)))
                  (if (rule? 'daisuushi-double)
                      2
                      1)
                  0)))
   (yakuman 'tsuuiisou "all honors" #true
            (λ (h g)
              (if (andmap honor? (hand-tiles h))
                  1
                  0)))
   (yakuman 'chinroutou "all terminals" #true
            (λ (h g)
              (if (andmap terminal? (hand-tiles h))
                  1
                  0)))
   (yakuman 'ryuuiisou "all green" #true
            (λ (h g)
              (let* ([greens (set "2s" "3s" "4s" "6s" "8s")]
                     [greens (if (rule? 'ryuuiisou-excl)
                                 greens
                                 (set-add greens "6z"))]
                     [tiles (hand-tiles h)])
                (if (and (or (not (rule? 'ryuuiisou-req))
                             (ormap (curry equal? "6z") tiles))
                         (andmap (curry set-member? greens) tiles))
                    1
                    0))))
   (yakuman 'chuuren "nine gates" #false
            (λ (h g)
              (if (and (hand-closed? h)
                       (equal? (set-count (list->set (map tile-suit (hand-tiles h))))
                               1)
                       (let check-chuuren ([tiles (map tile-number (hand-tiles h))]
                                           [pattern '(1 1 1 2 3 4 5 6 7 8 9 9 9)]
                                           [extra-found #false])
                         (cond
                           [(empty? tiles)
                            (and extra-found (empty? pattern))]
                           [(empty? pattern)
                            (if extra-found #false
                                (check-chuuren (rest tiles) pattern #true))]
                           [else (if (equal? (first tiles) (first pattern))
                                     (check-chuuren (rest tiles) (rest pattern) extra-found)
                                     (if extra-found
                                         #false
                                         (check-chuuren (rest tiles) pattern #true)))])))
                  (if (and (rule? 'chuuren-double)
                           ; 9 sided wait results in either four 1's, four 9's, or two of another tile
                           (let* ([t (hand-last-tile h)]
                                  [c (count (curry equal? t) (hand-tiles h))])
                             (if (or (equal? (tile-number t) 1)
                                     (equal? (tile-number t) 9))
                                 (equal? c 4)
                                 (equal? c 2))))
                      2
                      1)
                  0)))
   (yakuman 'suukantsu "four kans" #true
            (λ (h g)
              (if (and (not (empty? (hand-melds h)))
                       (andmap meld-kan? (hand-melds h)))
                  1
                  0)))

   (yakuman 'tenhou "heavenly hand" #false
            (λ (h g) (if (and (gamestate-tenhou/chiihou? g)
                              (equal? (gamestate-seat g)
                                      (wind 'e)))
                         1
                         0)))
   (yakuman 'chiihou "earthly hand" #false
            (λ (h g) (if (and (gamestate-tenhou/chiihou? g)
                              (not (equal? (gamestate-seat g)
                                           (wind 'e))))
                         1
                         0)))))

(define (is-yaku? s)
  (not (not (member s (map yaku-id yakulist)))))
(define (is-yakuman? s)
  (not (not (member s (map yakuman-id yakumanlist)))))
; note:
;  iipeikou doesn't score with ryanpeikou
;  skipping optional yaku like renhou and daisharin
;  don't forget nagashi mangan
;  double yakuman
;  sekinin banrai
;  optional daichiisei would cancel tsuuiisou or add on to it for double yakuman
