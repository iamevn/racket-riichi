#lang racket
(require web-server/http
         web-server/servlet-env
         web-server/servlet/setup
         net/url-structs
         net/url-string)
(require "score-hand.rkt"
         "contracts.rkt"
         net/base64
         file/convertible)

; from https://stackoverflow.com/questions/44863550/how-to-draw-a-picture-in-web-app-using-racket
(define (img-encode img)
  (~a "data:image/png;base64,"
      (base64-encode (convert img 'png-bytes))))

(define (gen-page hand gamestate)
  (response/xexpr
   `(html
     (body
      (p ,hand (br)
         ,(gamestate-shorthand->string gamestate) (br)
         (br))
      ,@(map (λ (img-and-text)
               (let* ([hand-image (first img-and-text)]
                      [text (second img-and-text)]
                      [split-text (string-split text "\n")])
                 `(div
                   (img ([src ,(img-encode hand-image)]))
                   (p ,@(add-between split-text '(br)))
                   (br))))
             (list-score-hand hand gamestate))))))

(define (score request)
  (let* ([query (url-query (request-uri request))]
         [query-hash (make-hash query)]
         [hand (hash-ref query-hash 'hand)]
         [raw-gamestate (hash-ref query-hash 'gamestate)]
         [gamestate (map string->symbol (string-split raw-gamestate ","))])
    (gen-page hand gamestate)))
; /score?hand=1234567(8)9m22z%20444p&gamestate=seat-e,round-e,ron
; /score?hand=123123m3453456(6)p&gamestate=seat-e,round-s,tsumo

(define (demo request)
  (sleep 10)
  #;(gen-page "456m11(1)22z 1111s 7777z" '(seat-s round-s ron))
  #;(gen-page "22334455m44556(6)p" '(seat-s round-s ron))
  #;(gen-page "888m1(1)222333z 44z4" '(ron))
  (gen-page "66z 1111z 2222z 3333z 4444z" '(seat-e round-s ron)))

(define (hello request)
  (response/xexpr
   `(html (body (p "hello world!" (br) (br) "demo page " (a ([href "demo"]) "here"))))))

(define (four-oh-four request)
  (response/xexpr
   `(html (body (p "unrecognized path:" (br) ,(url->string (request-uri request)))))))

(define r '()) ; for inspecting requests in repl
(define (route request)
  (unless (equal? (path/param-path (first (url-path (request-uri request))))
                  "favicon.ico") ; skip caching favicon request
      (set! r request)) ; for inspecting requests in repl
  (let* ([uri (request-uri request)]
         [path (url-path uri)]
         [first-path (if (zero? (length path))
                         ""
                         (path/param-path (first path)))])
    (case first-path
      [("hello" "") (hello request)]
      [("demo") (demo request)]
      [("score") (score request)]
      [else (four-oh-four request)])))

(define server
  (thread
   (λ ()
     (serve/servlet route
                    #:stateless? #t
                    #:servlet-path "/demo"
                    #:servlet-regexp #rx""
                    #:launch-browser? #t
                    #:listen-ip #f))))