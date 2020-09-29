#lang racket
(require web-server/http
         web-server/lang/web
         web-server/servlet-dispatch
         web-server/servlet-env
         web-server/lang/abort-resume
         web-server/web-server
         web-server/servlet/setup
         (prefix-in pathprocedure: web-server/dispatchers/dispatch-pathprocedure)
         net/url-structs
         net/url-string)
(require "score-hand.rkt"
         net/base64
         file/convertible)

; from https://stackoverflow.com/questions/44863550/how-to-draw-a-picture-in-web-app-using-racket
(define (img-encode img)
  (format "data:image/png;base64,~a"
          (base64-encode (convert img 'png-bytes))))

(define (gen-page hand gamestate)
  (let ([scorelist (list-score-hand hand gamestate)])
    (λ (make-url)
      (response/xexpr
       (list 'html
             (append*
              `(body
                (p ,(~a hand " south round, south seat, ron")
                   (br)
                   (br)))
              (map (λ (img-and-text)
                     (let* ([img (first img-and-text)]
                            [text (second img-and-text)]
                            [split-text (string-split text "\n")]
                            [text-list-with-br (add-between split-text '(br))])
                       `((img ([src ,(img-encode img)]))
                         ,(cons 'p text-list-with-br)
                         (br))))
                   scorelist)))))))

(define (demo request)
  (send/suspend/dispatch
   ;(gen-page "456m11(1)22z 1111s 7777z" '(seat-s round-s ron))
   (gen-page "22334455m44556(6)p" '(seat-s round-s ron))))

(define (hello request)
  (response/xexpr
   `(html (body (p "hello world!" (br) (br) "demo page " (a ((href "demo")) "here"))))))


(define r '())
(define (route request)
  (display "updating r")(newline)(set! r request)(display r)(newline)
  (let* ([uri (request-uri request)]
         [path (url-path uri)]
         [first-path (if (zero? (length path))
                         ""
                         (path/param-path (first path)))])
    (cond
      [(or (equal? first-path "hello")
           (equal? first-path ""))
       (hello request)]
      [(equal? first-path "demo")
       (demo request)]
      [else (response/xexpr `(html (body (p "unrecognized path:" (br) ,(url->string uri)))))])))

(define server
  (thread
   (λ ()
     (serve/servlet route
                    #:stateless? #t
                    #:servlet-path "/demo"
                    #:servlet-regexp #rx""
                    #:launch-browser? #t
                    #:listen-ip #f))))