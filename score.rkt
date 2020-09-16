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
(struct/contract win-conditons ([dora-indicators (listof tile?)] ; list of dora indicators
                                [tsumo? boolean?] ; win by self draw?
                                [ron? boolean?] ; win by discard?
                                [dealer? boolean?] ; winner is dealer?
                                [haitei? boolean?] ; win on last draw?
                                [houtei? boolean?] ; win on last discard?
                                [chankan? boolean?] ; win by robbing kan?
                                [rinshan? boolean?] ; win on deadwall draw after kan?
                                ) #:transparent)

(define (make-conditions dora-indicators
                         #:tsumo [tsumo #false]
                         #:ron [ron #false]
                         #:dealer [dealer #false]
                         #:haitei [haitei #false]
                         #:houtei [houtei #false]
                         #:chankan [chankan #false]
                         #:rinshan [rinshan #false])
  (if (xor tsumo ron)
      (win-conditons dora-indicators tsumo ron dealer houtei haitei chankan rinshan)
      (raise-argument-error 'make-conditions
                            "tsumo or ron, not both"
                            (if (and tsumo ron)
                                "#:tsumo #true #:ron #true"
                                "#:tsumo #false #:ron #false"))))

(struct/contract yaku ([id symbol?]
                       [han-open number?]
                       [han-closed number?]
                       [in-hand? any/c]) #:transparent)

(define (idk-some-yaku)
  ; closed only 1 han
  '(riichi ippatsu tsumo pinfu iipeikou)
  ; 1 han
  '(haitei houtei rinshan chankan tanyao yakuhai)
  ; 2 han
  '(double-riichi chanta sanshoku-doujun ittsuu toitoi sanankou sanshoku-doukou sankantsu honroutou shousangen)
  ; 3 han
  '(honitsu junchan ryanpeikou)
  ; 6 han
  '(chinitsu)
  ; yakuman
  '(kazoe kokushi daisangen suuankou shousuushi daisuushi tsuuiisou ryuuiisou chinroutou chuuren suukantsu tenhou chiihou)
  ; special
  '(nagashi)
  ; optional
  '(renhou daisharin))

; hand scoring
(define dummy-score (scoring 0 0 '("dummy-score")))
; given a finished hand broken into melds, count up base points
(define/contract (count-points hand)
  (-> (and/c hand? hand-finished?) scoring?)
  dummy-score) ; TODO: actually count up score