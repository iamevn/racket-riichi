#lang racket

(provide rule?)

(define/contract (rule? name)
  (-> symbol? boolean?)
  (case name
    [(kokushi-tanki) #true] ; pair wait kokushi is double
    [(suuankou-tanki) #true] ; pair wait suuankou is double
    [(daisuushi-double) #true] ; daisuushi is double
    [(ryuuiisou-req) #true] ; ryuuiisou requires green dragon
    [(ryuuiisou-excl) #false] ; ryuuiisou excludes green dragon
    [(chuuren-double) #true] ; chuuren is double with 9-sided wait
    [else (raise-argument-error 'rules "valid rule symbol" name)]))

; variations to add:
; kiriage-mangan, chiitoitsu-unique, double-riichi, haitei/houtei, haitei-rinshan (#false),
; ippatsu, kazoe-yakuman, kokushi-closed-chankan (#false), combine-yakuman, pinfu-tsumo, rinshan-fu
;
; optional yaku:
; daisharin, open-riichi, paarenchan, renhou
;
; pao:
; pao-daisangen/daisuushii, pao-suukantsu, pao-daiminkan-rinshan
;
; nagashi mangan???