#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; macro magic below ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module hashlist racket
  (provide for/hashlist
           for*/hashlist
           hashlist-set
           hashlist-set!
           hashlist-update
           hashlist-update!)
  
  (define-for-syntax (split-for-body stx body-stx)
    (let ([lst (syntax->list body-stx)])
      (if lst
          (let loop ([exprs lst] [pre-kw null] [post-kw null])
            (cond
              [(null? exprs)
               (if (null? post-kw)
                   (if (null? pre-kw)
                       (raise-syntax-error #f
                                           "missing body"
                                           stx)
                       (raise-syntax-error #f
                                           (format "missing body form after ~a clause"
                                                   (syntax-e (cadr pre-kw)))
                                           stx
                                           (cadr pre-kw)))
                   (list (reverse pre-kw) (reverse post-kw)))]
              [(memq (syntax-e (car exprs)) '(#:break #:final))
               (if (pair? (cdr exprs))
                   (loop (cddr exprs) 
                         (append (list* (cadr exprs) (car exprs) post-kw)
                                 pre-kw)
                         null)
                   (raise-syntax-error #f
                                       (format "missing expression after ~a" (syntax-e (car exprs)))
                                       stx
                                       (car exprs)))]
              [else
               (loop (cdr exprs) pre-kw (cons (car exprs) post-kw))]))
          (raise-syntax-error #f "bad syntax" stx))))

  (define-for-syntax (for-variant-stx stx derived-id-stx fold-bind-stx wrap rhs-wrap combine)
    (with-syntax ([derived-id derived-id-stx]
                  [fold-bind fold-bind-stx])
      (syntax-case stx ()
        ;; When there's a bindings clause...
        [(_ (bind ...) expr1 expr ...)
         (with-syntax ([(bind ...)
                        (let loop ([bs (syntax->list #'(bind ...))])
                          (if (null? bs)
                              null
                              (syntax-case (car bs) ()
                                [[ids rhs]
                                 (or (identifier? #'ids)
                                     (andmap identifier? (or (syntax->list #'ids) '(#f))))
                                 (cons #`[ids #,(rhs-wrap #'rhs)]
                                       (loop (cdr bs)))]
                                [kw
                                 (memq (syntax-e #'kw) '(#:when #:unless #:break #:final))
                                 (cons (car bs)
                                       (if (null? (cdr bs))
                                           null
                                           (cons (cadr bs) (loop (cddr bs)))))]
                                [_
                                 ;; a syntax error; let the /derived form
                                 ;; handle it, and no need to wrap any more:
                                 bs])))]
                       [((middle-expr ...) (end-expr ...))
                        (split-for-body stx #'(expr1 expr ...))])
           (quasisyntax/loc stx
             #,(wrap (quasisyntax/loc stx
                       (derived-id #,stx fold-bind (bind ...)
                                   middle-expr ...
                                   #,(combine (syntax/loc stx (let () end-expr ...))))))))]
        ;; Let `derived-id' complain about the missing bindings and body expression:
        [(_ . rest)
         #`(derived-id #,stx fold-bind . rest)])))

  (define-syntax define-syntax-via-derived
    (syntax-rules ()
      [(_ id derived-id fold-bind wrap rhs-wrap combine)
       (define-syntax (id stx)
         (for-variant-stx stx #'derived-id #'fold-bind wrap rhs-wrap combine))]))

  (define-syntax define-for-variants
    (syntax-rules ()
      [(_ (for for*) fold-bind wrap rhs-wrap combine)
       (begin
         (define-syntax-via-derived for for/fold/derived fold-bind wrap rhs-wrap combine)
         (define-syntax-via-derived for* for*/fold/derived fold-bind wrap rhs-wrap combine))]))

  (define-for-variants (for/hash for*/hash)
    ([table #hash()]) ; fold-bind
    (lambda (x) x) ; wrap
    (lambda (rhs) rhs) ; rhs-wrap
    (lambda (x) ; combine
      #`(let-values ([(key val) #,x])
          (hash-set table key val))))

  (define-for-variants (for/hashlist for*/hashlist)
    ([table #hash()])
    (lambda (x) x)
    (lambda (rhs) rhs)
    (lambda (x)
      #`(let-values ([(key val) #,x])
          (hashlist-set table key val))))

  (define (hashlist-set hash key v)
    (if (hash-has-key? hash key)
        (hash-update hash key (curry cons v))
        (hash-set hash key (list v))))

  (define (hashlist-set! hash key v)
    (if (hash-has-key? hash key)
        (hash-update! hash key (curry cons v))
        (hash-set! hash key (list v))))

  (define hashlist-update hashlist-set)
  (define hashlist-update! hashlist-set!))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; macro magic finished ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(module profile-graph racket
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
    (sampler 'get-snapshots)))

(require drracket/check-syntax)
(require 'hashlist)

(define (rooted-in? root path)
  (let ([root-dirs (explode-path root)]
        [path-dirs (explode-path path)])
    (and (>= (length path-dirs) (length root-dirs))
         (for/and ([a root-dirs]
                   [b path-dirs])
           (equal? a b)))))
(define project-path?
  (curry rooted-in? (current-directory)))
; alternatively: (string->path "C:\\Users\\Evan\\Desktop\\programming\\racket-riichi\\")

#;(begin
    (define hash
      (for/hashlist ([v (in-list (show-content "tests.rkt"))])
        (let ([id (vector-ref v 0)])
          (values id v))))

    (define identifier-paths
      (for/hash ([v (in-list (hash-ref hash 'syncheck:add-jump-to-definition))])
        (values (vector-ref v 3) (vector-ref v 4))))

    (let ([identifiers-from-multiple-paths
           (filter (λ (v) (let ([id (vector-ref v 3)]
                                [path (vector-ref v 4)])
                            (not (and (hash-has-key? identifier-paths id)
                                      (equal? path (hash-ref identifier-paths id))))))
                   (hash-ref hash 'syncheck:add-jump-to-definition))])
      (when (not (empty? identifiers-from-multiple-paths))
        identifiers-from-multiple-paths))


    #;(for/hash ([id (in-hash-keys identifier-paths)]
                 #:when (project-path? (hash-ref identifier-paths id)))
        (let* ([path (hash-ref identifier-paths id)]
               [exploded (explode-path path)]
               [file (path->string (last exploded))])
          (values id file)))

    #;(for/hashlist ([id (in-hash-keys identifier-paths)]
                     #:when (project-path? (hash-ref identifier-paths id)))
        (let* ([path (hash-ref identifier-paths id)]
               [exploded (explode-path path)]
               [file (path->string (last exploded))])
          (values path id))))

(define project-files
  (list "score.rkt" "yaku.rkt" "fu.rkt"
        "rule-config.rkt" "gamestate.rkt" "parse-hand.rkt"
        "hand.rkt" "call-notation.rkt" "melds.rkt"
        "tiles.rkt" "contracts.rkt" "util.rkt"
        "score-hand.rkt" "tile-images.rkt" "yaku-compatibility.rkt"
        "tests.rkt" "bench.rkt" "info.rkt"
        "web\\build-hand-page.rkt"
        "web\\demo-page.rkt"
        "web\\hand.rkt"
        "web\\hand-page.rkt"
        "web\\riichi-server.rkt"
        "web\\score-page.rkt"))

(define (gen-project-deps)
  (for/hash ([file project-files])
    (values file
            (let* ([identifier-paths
                    (for/hash ([v (in-list (show-content file))]
                               #:when (equal? (vector-ref v 0) 'syncheck:add-jump-to-definition))
                      (values (vector-ref v 3) (vector-ref v 4)))])
              (for/hashlist ([id (in-hash-keys identifier-paths)]
                             #:when (project-path? (hash-ref identifier-paths id)))
                (let* ([path (hash-ref identifier-paths id)]
                       [exploded (explode-path path)]
                       [file (path->string (last exploded))])
                  (values path id)))))))

(define-struct edge (src dst id) #:transparent)

(define (nodes-and-edges project-deps)
  (define nodes '())
  (define edges '())
  (define project-prefix-len (string-length (path->string (current-directory))))
  (hash-for-each
   project-deps
   (λ (source-file file-deps)
     (set! nodes (cons source-file nodes))
     (hash-for-each
      file-deps
      (λ (dep-path dep-ids)
        (let ([dep-file (substring (path->string dep-path) project-prefix-len)])
          (for ([dep-id dep-ids])
            (set! edges (cons (edge source-file dep-file (symbol->string dep-id)) edges))))))))
  (values nodes edges))

(define (gen-js project-deps)
  (let-values ([(nodes edges) (nodes-and-edges project-deps)])

    (write-string "const g = new Graph();")
    (newline)
    (newline)
    (write-string "// Files (nodes)")
    (newline)
    (for ([node (in-list nodes)])
      (write-string (~a "g.addNode('" node "');"))
      (newline))
    (newline)
    (write-string "// Imported identifiers (edges)")
    (newline)
    (for ([edge (in-list edges)])
      (write-string (~a "g.addEdge('" (edge-src edge) "', '" (edge-dst edge)
                        "', {directed: true, style: {label: '" (edge-id edge) "'}});"))
      (newline))
    (newline)
    (write-string "const layouter = new Graph.Layout.Spring(g);")
    (newline)
    (write-string "layouter.layout();")
    (newline)
    (write-string "const renderer = new Graph.Renderer.Raphael('canvas', g, 1280, 720);")
    (newline)
    (write-string "renderer.draw();")
    (newline)))

#;(gen-js (gen-project-deps))

(require 'profile-graph)
(require profile-flame-graph/flame-graph)

(define (graph pf #:filename [filename "profile.svg"]
               #:args [args '()])
  (let ([tmp (make-temporary-file)])
    (with-output-to-file tmp #:exists 'replace
      (thunk (print-stacks (samples->stacks pf))))
    (with-output-to-file filename #:exists 'replace
      (thunk (system (string-join (list* "flamegraph.pl" (path->string tmp) args)))))))

#;(define snapshots (profile-snapshots gen-project-deps))
#;(graph snapshots)