#lang racket
(provide (struct-out scoring)
         scoring-points)

(struct/contract scoring ([fu number?]
                          [han number?]
                          [yaku (listof string?)]) #:transparent)

(define (scoring-points score)
  (* (scoring-fu score) (expt 2 (+ 2 (scoring-han score)))))

; TODO: actual scoring (mangan, haneman, etc)