#lang racket

(provide img-encode
         gen-page)

(require "../score-hand.rkt"
         "../gamestate.rkt"
         "../hand.rkt"
         net/base64
         file/convertible
         web-server/http)

; from https://stackoverflow.com/questions/44863550/how-to-draw-a-picture-in-web-app-using-racket
(define (img-encode img)
  (~a "data:image/png;base64,"
      (base64-encode (convert img 'png-bytes))))

(define (gen-page hand gamestate #:dora [dora 0])
  (response/xexpr
   `(html
     (body
      (p ,hand (br)
         ,(gamestate-shorthand->string gamestate) (br)
         (br))
      ,@(map (λ (img-and-text)
               (let* ([hand-image (first img-and-text)]
                      [hand-str (hand->call-notation (finished-hand (third img-and-text)))]
                      [text (second img-and-text)]
                      [split-text (string-split text "\n")])
                 `(div
                   (img ([src ,(~a "hand/" hand-str)]))
                   (p ,@(add-between split-text '(br)))
                   (br))))
             (list-score-hand hand gamestate #:dora dora))))))
