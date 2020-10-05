#lang racket
(require web-server/http
         web-server/servlet-env
         web-server/dispatchers/dispatch
         net/url-string
         racket/runtime-path
         "score-page.rkt"
         "build-hand-page.rkt"
         "demo-page.rkt"
         "hand.rkt")

(define (hello request)
  (response/xexpr
   `(html (body (p "hello world!")
                (br) (br) "demo page " (a ([href "demo"]) "here")))))

(define (route request)
  (let* ([uri (request-uri request)]
         [path (url-path uri)]
         [first-path (if (zero? (length path))
                         ""
                         (path/param-path (first path)))])
    (case first-path
      [("hello" "") (hello request)]
      [("demo") (demo request)]
      [("score") (score request)]
      [("build-hand") (build-hand request)]
      [("hand") (hand-image request)]
      [else (next-dispatcher)])))


(define-runtime-path staticdir "static")

(define port (if (getenv "PORT")
                 (string->number (getenv "PORT"))
                 8000))

(serve/servlet route
               #:stateless? #t
               #:servlet-path "/"
               #:servlet-regexp #rx""
               #:extra-files-paths (list staticdir)
               #:launch-browser? #f
               #:listen-ip #f
               #:port port)
; useful: /conf/refresh-servlets
; from https://docs.racket-lang.org/web-server/faq.html#%28part._update-servlets%29