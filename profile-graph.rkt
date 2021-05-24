#lang racket
(require net/sendurl
         profile-flame-graph/flame-graph
         profile/sampler
         racket/file
         racket/port
         racket/system)
(provide profile-snapshots)
(define (profile-snapshots thunk)
  (define sampler (create-sampler (current-thread) 0.05 #:use-errortrace? #f))
  (define (run) (for/last ([i (in-range 1)]) (thunk)))
  (begin0 (with-handlers ([void (λ (e) (eprintf "profiled thunk error: ~a\n"
                                                (if (exn? e)
                                                    (exn-message e)
                                                    (format "~e" e))))])
            (run)))
  (sampler 'stop)
  (sampler 'get-snapshots))