#lang racket
(require web-server/http
         web-server/servlet-env
         web-server/dispatchers/dispatch
         net/url-string
         racket/runtime-path
         "score.rkt"
         "build-hand.rkt"
         "demo.rkt")

(define (hello request)
  (response/xexpr
   `(html (body (p "hello world!" (br) (br) "demo page " (a ([href "demo"]) "here"))))))

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
      [else (next-dispatcher)])))


(define-runtime-path staticdir "static")

(define server
  (thread
   (λ ()
     (serve/servlet route
                    #:stateless? #t
                    #:servlet-path "/build-hand"
                    #:servlet-regexp #rx""
                    #:extra-files-paths (list staticdir)
                    #:launch-browser? #t
                    #:listen-ip #f))))
; useful: /conf/refresh-servlets
; from https://docs.racket-lang.org/web-server/faq.html#%28part._update-servlets%29