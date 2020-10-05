#lang racket

(provide score)

(require web-server/http
         net/url-string
         "hand-page.rkt")

(define last-exn '())
; will need to handle bad hands, needs the right error code
(define (score request)
  (let* ([query (url-query (request-uri request))]
         [query-hash (make-hash query)]
         [hand (hash-ref query-hash 'hand)]
         [raw-gamestate (hash-ref query-hash 'gamestate)]
         [gamestate (map string->symbol (string-split raw-gamestate ","))]
         [dora (string->number (hash-ref query-hash 'dora))])
    (with-handlers ([exn:fail:contract?
                     (λ (e)
                       (set! last-exn e)
                       (displayln e)
                       (response/xexpr
                        `(html
                          (body
                           (p "hand not finished:" (br)
                              ,hand (br)
                              ,gamestate (br))
                           (div ,@(add-between (string-split (exn-message e)
                                                             "\n")
                                               '(br)))))))])
      (gen-page hand gamestate #:dora dora))))
; /score?hand=1234567(8)9m22z%20444p&gamestate=seat-e,round-e,ron
; /score?hand=123123m3453456(6)p&gamestate=seat-e,round-s,tsumo
; /score?hand=12322m444p78(9)p333z&gamestate=seat-s,round-e,ron,riichi,ippatsu