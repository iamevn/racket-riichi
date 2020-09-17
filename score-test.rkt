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
  (map (λ (l) (let ([h (first l)]
                    [g (second l)]
                    [y (third l)])
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
                      y)))
       '(("123123m7744p897s7p" (rii tsu) menzen-tsumo)
         ("123123m7744p897s7p" (rii tsu) riichi)
         ("123123m7744p897s7p" (rii ipp ron) ippatsu)
         #;("234m45789p45688s3p" (tsu) pinfu) ; pinfu unimplemented
         ("445566p111234m55z" (tsu) iipeikou)
         ("123123m7744p897s7p" (tsu hai) haitei)
         ("123123m7744p897s7p" (ron hou) houtei)
         #;("123123m4p897s4p 7777p" (tsu rin) rinshan) ; make-hands can't make kans
         ("123123m7744p897s7p" (ron cha) chankan)
         ("33344m22256788p4m" (tsu) tanyao)
         ("123p55s66z999p111z5s" (tsu) yakuhai)
         ("123p55s66z999p3336z" (tsu) yakuhai)
         ("123123m7744p897s7p" (rii dou tsu) double-riichi)
         ("11m789p123s22z123p1m" (tsu) chanta)
         ("4568m456p456s555z8m" (ron) sanshoku-doujun)
         ("123456789m333z22p" (ron) ittsuu)
         ("111m77p44s222z888p7p" (ron) toitoi)
         ("666m777p44s222z123p" (tsu) sanankou)
         ("777m888p33s11777z3s" (ron) sanankou)
         ("456777m5777s777p5s" (ron) sanshoku-doukou)
         #;("456m22s 6666p9999s5555s" (tsu) sankantsu) ; make-hands can't make kans
         ("1177p33z55m2z44s552z" (ron) chiitoitsu)
         ("1177p33z55m2z44s552z" (tsu) chiitoitsu)
         ("11199m22z999s444z9m" (ron) honroutou)
         ("123345s55566677z" (ron) shousangen)
         ("66789p22444z1236p" (ron) honitsu)
         ("1999m789p123123s1m" (ron) junchan)
         ("456456m67868p22s7p" (tsu) ryanpeikou)
         ("12334556679996p" (ron) chinitsu))))
; TODO: rest of yaku
; TODO: negative test cases


(define valid-yaku (list->set '(menzen-tsumo
                                riichi
                                ippatsu
                                pinfu
                                iipeikou
                                haitei
                                houtei
                                rinshan
                                chankan
                                tanyao
                                yakuhai
                                double-riichi
                                chanta
                                sanshoku-doujun
                                ittsuu
                                toitoi
                                sanankou
                                sanshoku-doukou
                                sankantsu
                                chiitoitsu
                                honroutou
                                shousangen
                                honitsu
                                junchan
                                ryanpeikou
                                chinitsu)))

(define (valid-yaku? s) (set-member? valid-yaku s))

(check-true (andmap (λ (yt) (valid-yaku? (third yt))) positive-yaku-test-cases))

(define-check (check-yaku? yl)
  (let* ([h (first yl)]
         [g (second yl)]
         [y (third yl)]
         [configurations (make-hands h)]
         [found-yaku (map (λ (configuration)
                            (match-yaku configuration g))
                          configurations)]
         [found-ids (map (λ (yl) (yaku-id (first yl)))
                         (append* found-yaku))])
    (with-check-info (['hands h]
                      ['gamestate g]
                      ['match-yaku-result found-yaku]
                      ['yaku-ids found-ids])
      (unless (member? y found-ids)
        (fail-check)))))

(define score-tests
  (make-test-suite "Tests for yaku matcher"
                   (map (λ (tc)
                          (test-case (symbol->string (third tc))
                                     (check-yaku? tc)))
                        positive-yaku-test-cases)))