#lang racket
(require "yaku.rkt")

(define/contract (yaku-compatible? a b)
  (-> (or/c is-yaku? is-yakuman?) (or/c is-yaku? is-yakuman?) boolean?)
  (cond
    [(and (is-yaku? a)
          (is-yaku? b))
     (hash-ref (hash-ref yaku-compatibility-hash a (make-hash)) b #f)]
    [(and (is-yakuman? a)
          (is-yakuman? b))
     (hash-ref (hash-ref yakuman-compatibility-hash a (make-hash)) b #f)]
    [else #f]))

(define/contract (incompatible-yaku s)
  (-> (or/c is-yaku? is-yakuman?) (or/c (listof is-yaku?) (listof is-yakuman?)))
  (filter (curry (compose not yaku-compatible?) s)
          (if (is-yakuman? s)
              yakuman-id
              yaku-id)))

(define/contract (compatible-yaku s)
  (-> (or/c is-yaku? is-yakuman?) (or/c (listof is-yaku?) (listof is-yakuman?)))
  (filter (curry yaku-compatible? s)
          (if (is-yakuman? s)
              (map yakuman-id yakumanlist)
              (map yaku-id yakulist))))

; from http://arcturus.su/wiki/Yaku_compatibility
(define yaku-compatibility-hash
  (let ([compatibility-table
         '((id id #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t)
           (id id #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t)
           (#t #t id #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #f #t)
           (#t #t #t id #t #t #t #t #t #t #t -- #t #t #t #t #t #t #t #t #t #t #t #t #f #f)
           (#t #t #t #t id #t #t #f #f #t #t #t #t #t #f #f #t #f #f -- #t #t #t #t #t #t)
           (#t #t #t #t #t id #t #t #f #t #f #f #f #f #t #t #t #f #f #t #t #f #f #t #t #t)
           (#t #t #t #t #t #t id #t #t #t #f #f #f #f #t #t #f #t #f #t #t #f #t #t #t #t)
           (#t #t #t #t #f #t #t id #t #f #f #f #f #f #f #f #f #f #f #t #t #f #t #t #t #t)
           (#t #t #t #t #f #f #t #t #t #t #t #t #t #t #t #f #f #t #t #t #f #f #t #t #t #t)
           (#t #t #t #t #t #t #t #f #t id #f #f #f #f #t #t #f #f #f #f #f #f #t #t #t #t)
           (#t #t #t #t #t #f #f #f #t #f id #t #t #t #t #t #f #f #t #f #f #f #t #t #t #t)
           (#t #t #t -- #t #f #f #f #t #f #t id #t #t -- -- #f #t #t #t #t #f #t #t #t #f)
           (#t #t #t #t #t #f #f #f #t #f #t #t id #t #t #t #f #t #t #t #t #f #t #t #t #t)
           (#t #t #t #t #t #f #f #f #t #f #t #t #t id #t #t #f #t #t #t #t #f #t #t #t #t)
           (#t #t #t #t #f #t #t #f #t #t #t -- #t #t id #f #t #t #f #t -- -- #t #t #t #t)
           (#t #t #t #t #f #t #t #f #f #t #t -- #t #t #f id #t #f -- -- #t #f #t #t #t #t)
           (#t #t #t #t #t #t #f #f #f #f #f #f #f #f #t #t id #f #f #t #t #f #f #t #t #f)
           (#t #t #t #t #f #f #t #f #t #f #f #t #t #t #t #f #f id #t #t #f #f #t #t #t #t)
           (#t #t #t #t #f #f #f #f #t #f #t #t #t #t #f -- #f #t id #t #f #t #t #t #t #f)
           (#t #t #t #t -- #t #t #t #t #f #f #t #t #t #t -- #t #t #t id #f #t #t #t #t #t)
           (#t #t #t #t #t #t #t #t #f #f #f #t #t #t -- #t #t #f #f #f id #t #t #t #t #t)
           (#t #t #t #t #t #f #f #f #f #f #f #f #f #f -- #f #f #f #t #t #t id #f #t #t #f)
           (#t #t #f #t #t #f #t #t #t #t #t #t #t #t #t #t #f #t #t #t #t #f id #f #f #f)
           (#t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #f id #f #f)
           (#t #t #f #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #f #f id #f)
           (#t #t #t #f #t #t #t #t #t #t #t #f #t #t #t #t #f #t #f #t #t #f #f #f #f id))]
        [table-yaku 
         '(riichi
           double-riichi
           ippatsu
           menzen-tsumo
           tanyao
           pinfu
           iipeikou
           ittsuu
           yakuhai
           sanshoku-doujun
           sanshoku-doukou
           toitoi
           sanankou
           sankantsu
           chanta
           junchan
           ryanpeikou
           shousangen
           honroutou
           honitsu
           chinitsu
           chiitoitsu
           rinshan
           haitei
           houtei
           chankan)])

    (make-hash (map (λ (name row) (cons name
                                        (make-hash (map cons table-yaku
                                                        (map (curry equal? #t) row)))))
                    table-yaku
                    compatibility-table))))

; from http://arcturus.su/wiki/Yaku_compatibility
(define yakuman-compatibility-hash
  (let ([compatibility-table
         '((id #f #f #f #f #f #f #f #f #f #t #t)
           (#f id #t #f #f #t #f #f #f #t #t #t)
           (#f #t id #t #t #t #t #t #f #t #t #t)
           (#f #f #t id id #t #f #f #f #t #t #t)
           (#f #f #t id id #t #f #f #f #t #t #t)
           (#f #t #t #t #t id #f #f #f #t #t #t)
           (#f #f #t #f #f #f id #f #f #t #t #t)
           (#f #f #t #f #f #f #f id #f #t #t #t)
           (#f #f #f #f #f #f #f #f id #f #t #t)
           (#f #t #t #t #t #t #t #t #f id #f #f)
           (#t #t #t #t #t #t #t #t #t #f id id)
           (#t #t #t #t #t #t #t #t #t #f id id))]
        [table-yakuman 
         '(kokushi-musou
           daisangen
           suuankou
           shousuushi
           daisuushi
           tsuuiisou
           ryuuiisou
           chinroutou
           chuuren
           suukantsu
           tenhou
           chiihou)])

    (make-hash (map (λ (name row) (cons name
                                        (make-hash (map cons table-yakuman
                                                        (map (curry equal? #t) row)))))
                    table-yakuman
                    compatibility-table))))


; I think I counted this right, with multiple yakuman rules (daisuushi being double yakuman,
; suuankou being double if it's a pair wait, combined yakuman adding up) the biggest possible hands
; are suuankou + daisuushi + tsuuiisou + tenhou/chiihou/suukantsu with a pair wait for 6x yakuman.
; the next biggest hands are 5x yakuman and are either those without the pair wait or swap the
; daisuushi for shousuushi or daisangen

(define (biggest-possible #:at-least [at-least 5])
  (define (flatten-set s)
    (if (foldl equal? #t (set-map s set?))
        (if (> (set-count s) 1)
            s
            (if (equal? (set-count s) 1)
                (flatten-set (first (set->list s)))
                s))
        (set s)))
  
  (define (yakuman-combinations)
    (define found-cache (make-hash))
    (define (find-groups group)
      (if (hash-has-key? found-cache group)
          (hash-ref found-cache group)
          (let* ([compatible (apply set-intersect
                                    (set-map group
                                             (compose list->set compatible-yaku)))]
                 [new-found (set-subtract compatible group)])
            (cond [(set-empty? new-found)
                   (hash-set! found-cache group (set group))
                   (set group)]
                  [else (let* ([to-check (set-map new-found (curry set-add group))]
                               [res (map find-groups to-check)]
                               [max-count (argmax set-count res)]
                               [just-maxes (list->set (filter (compose (curry equal?) set-count)
                                                              res))]
                               [flattened-maxes (flatten-set just-maxes)])
                          (hash-set! found-cache group flattened-maxes)
                          flattened-maxes)]))))

    (for-each (λ (y) (find-groups (set y))) (hash-keys yakuman-compatibility-hash))

    (hash-keys found-cache))
  
  (define (yakuman-value y)
    (case y
      [(shousuushi) 'single]
      [(daisuushi) 'double]
      [(suuankou) 'double-if-tanki]
      [(suukantsu) 'single]
      [(tsuuiisou) 'single]
      [(daisangen) 'single]
      [(tenhou) 'single]
      [(chiihou) 'single]
      ; misses kokushi or 9-gates with multisided wait but those aren't compatible with enough other
      ; yakuman to make the list of biggest possible hands
      [else 'single]))
  
  (define (yman-sort-cmp a b)
    (let* ([ylst '(suuankou daisuushi shousuushi daisangen tsuuiisou tenhou chiihou suukantsu)]
           [ao (or (index-of ylst a) -1)]
           [bo (or (index-of ylst b) -1)])
      (if (and (equal? ao bo))
          (symbol<? a b)
          (< ao bo))))
  
  
  (filter (compose (curry <= at-least) third)
          (sort (map (λ (yl) (let ([yl (sort yl yman-sort-cmp)]
                                   [yv (map yakuman-value yl)])
                               (list yl yv (+ (count (curry equal? 'single) yv)
                                              (* 2 (count (curry equal? 'double) yv))
                                              (* 2 (count (curry equal? 'double-if-tanki) yv))))))
                     (map set->list (yakuman-combinations)))
                >
                #:key third)))