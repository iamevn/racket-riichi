#lang racket

(provide build-yaku-test-case)

(require rackunit
         "parse-hand.rkt"
         "score.rkt"
         "contracts.rkt"
         "hand.rkt"
         "yaku.rkt"
         "gamestate.rkt"
         "util.rkt"
         "fu.rkt")

(define/contract (build-testcase-gamestate tc)
  (-> (and/c list? (list-length/c 2 #:cmp >=)) (and/c list? (list-length/c 2 #:cmp >=)))
  (list* (first tc)
         (gamestate-shorthand (second tc))
         (drop tc 2)))

; TODO: negative test cases
; TODO: combination test cases
; TODO: count points

(define-check (check-yaku yl expected)
  (let* ([h (first yl)]
         [g (second yl)]
         [y (third yl)]
         [configurations (make-call-notation-hands h)]
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
      (if expected
          (unless (member? y found-ids)
            (fail-check))
          (when (member? y found-ids)
            (fail-check))))))

(define (build-yaku-test-case testspec)
  (let ([h (first testspec)]
        [g (gamestate-shorthand (second testspec))]
        [y (third testspec)]
        [expected (if (>= (length testspec) 4) (fourth testspec) #true)])
    (test-case (symbol->string y)
               (check-yaku (list h g y) expected))))

(define-test-suite yaku-present-tests
  (test-suite "Standard hands"
              (map build-yaku-test-case
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
                     ("456777m5777s5s 7777p" (ron) sanshoku-doukou)
                     ("456777m55s 7777p 7s77" (ron) sanshoku-doukou)
                     ("456m22s 66p66 999s9 5555s" (tsu) sankantsu)
                     ("1177p33z55m2z44s552z" (ron) chiitoitsu)
                     ("1177p33z55m2z44s552z" (tsu) chiitoitsu)
                     ("1177p33z(5)5m22z44s55z" (ron) chiitoitsu)
                     ("11199m22z999s444z9m" (ron) honroutou)
                     ("123345s55566677z" (ron) shousangen)
                     ("66789p22444z1236p" (ron) honitsu)
                     ("1999m789p123123s1m" (ron) junchan)
                     ("456456m67868p22s7p" (tsu) ryanpeikou)
                     ("12334556679996p" (ron) chinitsu))))
  (test-suite "Yakuman hands"
              (map build-yaku-test-case
                   '(("19m19s19p1234556(7)z" (ron) kokushi-musou)
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
                     ("123123m7744p897s7p" (tsu ten) chiihou)
                     ; negative test cases
                     ("11227(7s)2233m77p77z" (seat-e round-e ron) suuankou #false)
                     ("11227(7s)2233m77p77z" (seat-e round-e ron) daisuushi #false)
                     ("11227(7s)2233m77p77z" (seat-e round-e ron) suukantsu #false)))))

(define pinfu-tests
  (test-suite "Pinfu hands"
              (map (λ (tc)
                     (test-case (~a tc)
                                (let ([h (first tc)]
                                      [g (gamestate-shorthand (second tc))]
                                      [expected (third tc)])
                                  (check-yaku (list h g 'pinfu) expected))))
                   '(("234m45789p45688s3p" (tsu) #true)
                     ("234m45789p45688s3p" (ron) #true)))))


(define-check (check-fu-raw h g expected)
  (let* ([hands (make-call-notation-hands h)]
         [found-fu (map (λ (configuration) (count-fu configuration g)) hands)])
    (unless (member? expected found-fu)
      (fail-check))))

(define-check (check-fu h g expected)
  (let* ([hands (make-call-notation-hands h)]
         [scorings (map (λ (configuration) (make-scoring configuration g)) hands)]
         [found-fu (map scoring-fu scorings)])
    (with-check-info (['hands hands]
                      ['scorings scorings]
                      ['found-fu found-fu])
      (unless (member? expected found-fu)
        (fail-check)))))

(define fu-tests
  (test-suite "Fu count tests"
              (map (λ (tc)
                     (test-case (~a tc)
                                (apply check-fu-raw (build-testcase-gamestate tc))
                                (apply check-fu (build-testcase-gamestate tc))))
                   '(("456m11(1)22z 1111s 7777z" (seat-s round-s ron) 110)
                     ("234s1(1z) 999p9 3333z 1111p" (seat-e round-e tsu) 110)
                     ("1168(7)s456m789p 77z7" (seat-e round-e ron) 30)
                     ("11227(7s)2233m77p77z" (seat-e round-e ron) 25)
                     ("34534522m45645(6)p" (seat-e round-e tsu) 20)
                     ("34534522m45645(6)p" (seat-e round-e tsu) 25)))))

(define-check (check-parse-last-tile h expected)
  (let ([hands (make-call-notation-hands h)])
    (unless (andmap (compose (curry equal? expected) hand-last-tile) hands)
      (fail-check))))

(define last-tile-tests
  (test-suite "Hand notation for last tile tests"
              (map (λ (tc)
                     (test-case (~a #\" (first tc) #\" " -> " (second tc))
                                (apply check-parse-last-tile tc)))
                   '(("44556(6)m223344p44s" "6m")
                     ("19m19p19s12(3)45567z" "3z")
                     ("123m456p222s333z22s" "2s")
                     ("123m456p222s22s 33z3" "2s")
                     ("12344m555s 1(1p)1 222z" "1p")
                     ("1(2)3456789m22255z" "2m")))))

(define-check (check-han h g expected)
  (let* ([hands (make-call-notation-hands h)]
         [scorings (map (λ (configuration) (make-scoring configuration g)) hands)]
         [found-han (map scoring-han scorings)])
    (unless (member? expected found-han)
      (fail-check (~a "Unable to find scoring with " expected " han. Found: " found-han)))
    (unless (empty? (filter (curry < expected) found-han))
      (fail-check (~a expected " is not biggest scoring. Found scorings with han: " found-han)))))

(define han-tests
  (test-suite "Han count tests"
              (map (λ (tc)
                     (test-case (~a tc)
                                (let ([h (first tc)]
                                      [g (gamestate-shorthand (second tc))]
                                      [expected (third tc)])
                                  (check-han h g expected))))
                   '(("444(7)89m555p234s22z" (rii tsu round-e seat-e) 2)
                     ("345p4(4)m 5m555 3s333 44s44" (tsu round-e seat-e) 3)
                     ("6662(2)m555p444s 77p7" (tsu round-e seat-e) 5)
                     ("19p19s19m1234(4)567z" (ron) 0)))))

(define-check (check-base-score s expected)
  (let ([score (count-basepoints s)])
    (unless (equal? score expected)
      (fail-check (~a "Expected score of " expected ", got: " score)))))

(define (make-dummy-yaku-scoring value is-yakuman)
  (if is-yakuman
      (list (yakuman 'dummy-yakuman "yakuman for testing" #true (λ (h g) value)) value)
      (list (yaku 'dummy-yaku "yaku for testing" value value (λ (h g) value)) value)))
(define base-score-tests
  (test-suite "Base score tests"
              (map (λ (tc)
                     (test-case (~a tc)
                                (let* ([han (first tc)]
                                       [fu (second tc)]
                                       [yl (map (curry apply make-dummy-yaku-scoring) (third tc))]
                                       [expected (fourth tc)])
                                  (check-base-score (scoring han fu yl) expected))))
                   '((30 3 ((2 #f)) (960 basic))
                     (10 2 ((1 #t)) (8000 yakuman))
                     (10 2 ((2 #t)) (16000 yakuman))
                     (10 2 ((1 #t) (2 #t)) (24000 yakuman))))))


(define-test-suite full-suite
  last-tile-tests
  fu-tests
  pinfu-tests
  yaku-present-tests
  han-tests
  base-score-tests)

(module+ main
  #;(begin (require rackunit/gui)
           (test/gui full-suite))
  (begin (require rackunit/text-ui)
           (run-tests full-suite))
  #;(begin (require rackunit/text-ui)
         (require profile)
         (profile (run-tests full-suite)))
  #;(begin (require rackunit/text-ui)
         (require contract-profile)
         (contract-profile (run-tests full-suite))))