#lang racket
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
(define hashlist-update! hashlist-set!)