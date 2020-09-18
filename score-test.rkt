#lang racket
(require rackunit)
(require "parse-hand.rkt")
(require "score.rkt")
(require "contracts.rkt")
(require "tiles.rkt")
(require "melds.rkt")
(require "hand.rkt")

(define/contract (member? v lst)
  (-> any/c list? boolean?)
  (not (false? (member v lst))))

(define positive-yaku-test-cases
  (map (λ (l)
         (let ([h (first l)]
               [g (second l)]
               [y (third l)])
           (list h
                 (make-gamestate (wind (if (member? 'dealer g)
                                           'e
                                           's))
                                 (wind 'e)
                                 '("4p")
                                 #:riichi (member? 'rii g)
                                 #:tsumo (member? 'tsu g)
                                 #:ron (member? 'ron g)
                                 #:ippatsu (member? 'ipp g)
                                 #:double (member? 'dou g)
                                 #:haitei (member? 'hai g)
                                 #:houtei (member? 'hou g)
                                 #:chankan (member? 'cha g)
                                 #:rinshan (member? 'rin g)
                                 #:tenhou/chiihou (member? 'ten g))
                 y)))
       '(("123123m7744p897s7p" (rii tsu) menzen-tsumo)
         ("123123m7744p897s7p" (rii tsu) riichi)
         ("123123m7744p897s7p" (rii ipp ron) ippatsu)
         ("234m45789p45688s3p" (tsu) pinfu)
         ("234m34(5)789p45688s" (ron) pinfu)
         ("445566p111234m55z" (tsu) iipeikou)
         ("123123m7744p897s7p" (tsu hai) haitei)
         ("123123m7744p897s7p" (ron hou) houtei)
         ("123123m4p897s4p 7777p" (tsu rin) rinshan)
         ("123123m7744p897s7p" (ron cha) chankan)
         ("33344m22256788p4m" (tsu) tanyao)
         ("444m22(2)88p 5p67 33m3" (ron) tanyao)
         ("123p55s66z999p111z5s" (tsu) yakuhai)
         ("123p55s66z999p3336z" (tsu) yakuhai)
         ("123p55s66(6z) 99p9 111z" (ron) yakuhai)
         ("123123m7744p897s7p" (rii dou tsu) double-riichi)
         ("11m789p123s22z123p1m" (tsu) chanta)
         ("4568m456p456s555z8m" (ron) sanshoku-doujun)
         ("123456789m333z22p" (ron) ittsuu)
         ("111m77p44s222z888p7p" (ron) toitoi)
         ("666m777p44s222z123p" (tsu) sanankou)
         ("777m888p33s11777z3s" (ron) sanankou)
         ("456777m5777s777p5s" (ron) sanshoku-doukou)
         ("456m22s 66p66 999s9 5555s" (tsu) sankantsu)
         ("1177p33z55m2z44s552z" (ron) chiitoitsu)
         ("1177p33z55m2z44s552z" (tsu) chiitoitsu)
         ("1177p33z(5)5m22z44s55z" (ron) chiitoitsu)
         ("11199m22z999s444z9m" (ron) honroutou)
         ("123345s55566677z" (ron) shousangen)
         ("66789p22444z1236p" (ron) honitsu)
         ("1999m789p123123s1m" (ron) junchan)
         ("456456m67868p22s7p" (tsu) ryanpeikou)
         ("12334556679996p" (ron) chinitsu)
         ;yakuman hands
         ("19m19s19p1234556(7)z" (ron) kokushi-musou)
         ("19m19s19p12345567z" (ron) kokushi-musou)
         ("19m19s19p1234567z9s" (ron) kokushi-musou)
         ("222m444p888m77z22(2z)" (tsu) suuankou)
         ("345m2(2s)555666z 7z77" (tsu) daisangen)
         ("888m1(1)222333z 44z4" (ron) shousuushi)
         ("5(5)p111222333z 444z" (tsu) daisuushi)
         ("1112255(5z) 222z 6z66" (tsu) tsuuiisou)
         ("111999m11(1)99s 11p1" (ron) chinroutou)
         ("22334466688(8s)66z" (ron) ryuuiisou)
         ("11123456789993m" (tsu) chuuren)
         ("4(4z) 66p66 222m2 7777z 444s4" (tsu) suukantsu)
         ("123123m7744p897s7p" (tsu ten dealer) tenhou)
         ("123123m7744p897s7p" (tsu ten) chiihou))))
; TODO: negative test cases
; TODO: combination test cases
; TODO: count points

(define-check (check-yaku? yl present)
  (let* ([h (first yl)]
         [g (second yl)]
         [y (third yl)]
         [configurations (make-my-notation-hands h)]
         [found-yaku (map (λ (configuration)
                            (match-yaku configuration g))
                          configurations)]
         [found-yakuman (map (λ (configuration)
                               (match-yakuman configuration g))
                             configurations)]
         [found-ids (append (map (λ (yl) (yaku-id (first yl)))
                                 (append* found-yaku))
                            (map (λ (yl) (yakuman-id (first yl)))
                                 (append* found-yakuman)))])
    (with-check-info (['hand-shorthand h]
                      ['gamestate g]
                      ['hand-configurations configurations]
                      ['match-yaku-result found-yaku]
                      ['yaku-ids found-ids])
      (if present
          (unless (member? y found-ids)
            (fail-check))
          (when (member? y found-ids)
            (fail-check))))))

(define score-tests
  (make-test-suite "Tests for yaku matcher"
                   (map (λ (tc)
                          (test-case (symbol->string (third tc))
                                     (check-yaku? tc #true)))
                        positive-yaku-test-cases)))

(define pinfu-test-cases
  (map (λ (l) (let ([h (first l)]
                    [g (second l)]
                    [present (third l)])
                (list h
                      (make-gamestate (wind 'e)
                                      (wind 's)
                                      '("4p")
                                      #:riichi (member? 'rii g)
                                      #:tsumo (member? 'tsu g)
                                      #:ron (member? 'ron g)
                                      #:ippatsu (member? 'ipp g)
                                      #:double (member? 'dou g)
                                      #:haitei (member? 'hai g)
                                      #:houtei (member? 'hou g)
                                      #:chankan (member? 'cha g)
                                      #:rinshan (member? 'rin g))
                      present)))
       '(("234m45789p45688s3p" (tsu) #true)
         ("234m45789p45688s3p" (ron) #true))))

(define pinfu-tests
  (make-test-suite "Tests for pinfu yaku"
                   (map (λ (tc)
                          (test-case (~a tc)
                                     (check-yaku? (list (first tc)
                                                        (second tc)
                                                        'pinfu)
                                                  (third tc))))
                        pinfu-test-cases)))

#; (count-fu (hand (tile-sort-keep-last (shorthand->handlist "456m1122z1111s77771z"))
                   (meld-sort (list (make-chii-meld "4m")
                                    (make-pon-meld "1z")
                                    (make-kan-meld "1s")
                                    (make-kan-meld "7z")))
                   '("2z" "2z")
                   "1z")
             (make-gamestate (wind 's)
                             (wind 's)
                             '("3z")
                             #:ron #true))