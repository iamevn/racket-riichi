#lang racket

(provide hand-image)

(require web-server/http
         net/url-string
         file/convertible
         "../tile-images.rkt"
         "../call-notation.rkt")

(define (four-oh-four request)
  (response/xexpr
   #:code 404
   #:message #"Invalid hand"
   `(html
     (body
      (p "Invalid hand")))))

(define (hand-image request)
  (let* ([uri (request-uri request)]
         [path (url-path uri)])
    (if (not (equal? (length path) 2))
        (four-oh-four request)
        (let* ([path (path/param-path (second path))]
               [hs (path->handstring path)])
          (if (not hs)
              (four-oh-four request)
              (response
               200 #"OK"
               (current-seconds)
               #"image/png"
               '()
               (λ (client-out)
                 (write-bytes (convert (call-notation->image hs)
                                       'png-bytes)
                              client-out))))))))

(define (path->handstring path)
  (let ([stripped (regexp-replace #rx"[.]png$" path "")])
    (and (non-empty-string? stripped)
         (call-notation? stripped)
         stripped)))