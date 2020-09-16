#lang racket

(provide (struct-out scoring)
         scoring-points
         dummy-score
         count-points)

(require "contracts.rkt")
(require "tiles.rkt")
(require "hand.rkt")

(struct/contract scoring ([fu number?]
                          [han number?]
                          [yaku (listof string?)]) #:transparent)

(define (scoring-points score)
  (* (scoring-fu score) (expt 2 (+ 2 (scoring-han score)))))

; TODO: actual scoring (mangan, haneman, etc)

; general game state around a win
(struct/contract gamestate ([dora-indicators (listof tile?)] ; list of dora indicators
                            [tsumo? boolean?] ; win by self draw?
                            [ron? boolean?] ; win by discard?
                            [dealer? boolean?] ; winner is dealer?
                            [haitei? boolean?] ; win on last draw?
                            [houtei? boolean?] ; win on last discard?
                            [chankan? boolean?] ; win by robbing kan?
                            [rinshan? boolean?]) ; win on deadwall draw after kan?
                 #:transparent)

(define (make-gamestate dora-indicators
                        #:tsumo [tsumo #false]
                        #:ron [ron #false]
                        #:dealer [dealer #false]
                        #:haitei [haitei #false]
                        #:houtei [houtei #false]
                        #:chankan [chankan #false]
                        #:rinshan [rinshan #false])
  (if (xor tsumo ron)
      (gamestate dora-indicators tsumo ron dealer houtei haitei chankan rinshan)
      (raise-argument-error 'make-gamestate
                            "tsumo or ron, not both"
                            (if (and tsumo ron)
                                "#:tsumo #true #:ron #true"
                                "#:tsumo #false #:ron #false"))))

; yakulist
(struct/contract yaku ([id symbol?]
                       [description string?]
                       [han-open number?]
                       [han-closed number?]
                       [scoring (-> hand? gamestate? scoring?)]) #:transparent)
(struct/contract yakuman ([id symbol?]
                          [description string?]
                          [available-open? boolean?]
                          [scoring (-> hand? gamestate? scoring?)]) #:transparent)

(define yakulist
  (list
   (yaku 'menzen-tsumo "self-draw" 0 1 (λ (h g) dummy-score))
   (yaku 'riichi "riichi" 0 1 (λ (h g) dummy-score))
   (yaku 'ippatsu "one shot" 0 1 (λ (h g) dummy-score))
   (yaku 'pinfu "no fu" 0 1 (λ (h g) dummy-score))
   (yaku 'iipeikou "double sequences" 0 1 (λ (h g) dummy-score))
   (yaku 'haitei "last draw" 1 1 (λ (h g) dummy-score))
   (yaku 'houtei "last discard" 1 1 (λ (h g) dummy-score))
   (yaku 'rinshan "dead wall draw" 1 1 (λ (h g) dummy-score))
   (yaku 'chankan "robbing a kan" 1 1 (λ (h g) dummy-score))
   (yaku 'tanyao "all simples" 1 1 (λ (h g) dummy-score))
   (yaku 'yakuhai "value tiles" 1 1 (λ (h g) dummy-score))
   (yaku 'double-riichi "double riichi" 2 2 (λ (h g) dummy-score))
   (yaku 'chanta "half outside hand" 1 2 (λ (h g) dummy-score))
   (yaku 'sanshoku-doujun "3 color straight" 1 2 (λ (h g) dummy-score))
   (yaku 'ittsuu "straight" 1 2 (λ (h g) dummy-score))
   (yaku 'toitoi "all triplets" 2 2 (λ (h g) dummy-score))
   (yaku 'sanankou "three closed triplets" 2 2 (λ (h g) dummy-score))
   (yaku 'sanshoku-doukou "three colored triplets" 2 2 (λ (h g) dummy-score))
   (yaku 'sankantsu "three kans" 2 2 (λ (h g) dummy-score))
   (yaku 'chiitoitsu "seven pairs" 0 2 (λ (h g) dummy-score))
   (yaku 'honroutou "outside hand" 2 2 (λ (h g) dummy-score))
   (yaku 'shousangen "small three dragons" 2 2 (λ (h g) dummy-score))
   (yaku 'honitsu "half flush" 2 3 (λ (h g) dummy-score))
   (yaku 'junchan "all terminals" 2 3 (λ (h g) dummy-score))
   (yaku 'ryanpeikou "two double sequences" 0 3 (λ (h g) dummy-score))
   (yaku 'chinitsu "flush" 5 6 (λ (h g) dummy-score))))

(define yakumanlist
  (list
   (yakuman 'kazoe "counted yakuman" #true (λ (h g) dummy-score))
   (yakuman 'kokushi-musou "thirteen orphans" #false (λ (h g) dummy-score))
   (yakuman 'suuankou "four concealed triplets" #false (λ (h g) dummy-score))
   (yakuman 'daisangen "big three dragons" #true (λ (h g) dummy-score))
   (yakuman 'shousuushi "small four winds" #true (λ (h g) dummy-score))
   (yakuman 'daisuushi "big four winds" #true (λ (h g) dummy-score))
   (yakuman 'tsuuiisou "all honors" #true (λ (h g) dummy-score))
   (yakuman 'chinroutou "all terminals" #true (λ (h g) dummy-score))
   (yakuman 'ryuuiisou "all green" #true (λ (h g) dummy-score))
   (yakuman 'chuuren "nine gates" #false (λ (h g) dummy-score))
   (yakuman 'suukantsu "four kans" #true (λ (h g) dummy-score))

   (yakuman 'tenhou "heavenly hand" #false (λ (h g) dummy-score))
   (yakuman 'chiihou "earthly hand" #false (λ (h g) dummy-score))))

; note:
;  iipeikou doesn't score with ryanpeikou
;  skipping optional yaku like renhou and daisharin
;  don't forget nagashi mangan
;  double yakuman


; hand scoring
(define dummy-score (scoring 0 0 '("dummy-score")))
; given a finished hand broken into melds, count up base points
(define/contract (count-points hand)
  (-> (and/c hand? hand-finished?) scoring?)
  dummy-score) ; TODO: actually count up score