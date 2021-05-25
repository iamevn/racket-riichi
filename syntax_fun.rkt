#lang racket
(require drracket/check-syntax)
(require "hashlist.rkt")

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
        "web\\score-page.rkt"
        "syntax_fun.rkt"
        "hashlist.rkt"
        "profile-graph.rkt"))

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

(require "profile-graph.rkt")
(require profile-flame-graph/flame-graph)

(define (graph-snapshots pf #:filename [filename "profile.svg"]
                         #:args [args '()])
  (let ([tmp (make-temporary-file)])
    (with-output-to-file tmp #:exists 'replace
      (thunk (print-stacks (samples->stacks pf))))
    (with-output-to-file filename #:exists 'replace
      (thunk (system (string-join (list* "flamegraph.pl" (path->string tmp) args)))))))

(define (make-profile-graph)
  (graph-snapshots (profile-snapshots gen-project-deps)))
#;(define snapshots (profile-snapshots gen-project-deps))
#;(graph-snapshots snapshots)

(define (deps->dot project-deps #:combine-edges? [combine-edges? #true])
  (define (-.rkt f) (string-trim f ".rkt" #:left? #f #:right? #t))
  (define (replace-backslash f #:replacement [replacement "/"])
    (string-replace f "\\" replacement #:all? #t))
  (define (quote-wrap f) (~a #\" f #\"))
  (define clean (compose quote-wrap replace-backslash))
  
  (let-values ([(nodes edges) (nodes-and-edges project-deps)])
    (write-string "digraph G {") (newline)
    (write-string "  overlap=\"scale\"") (newline)
    (write-string "  splines=true") (newline)
    (write-string "  node[shape=\"box\"]") (newline)
    (newline)
    (if combine-edges?
        (let ([h (for/hashlist ([edge (in-list edges)])
                   (values (list (edge-src edge) (edge-dst edge))
                           (edge-id edge)))])
          (for ([(e l) (in-hash h)])
            (let ([src (clean (first e))]
                  [dst (clean (second e))]
                  [label (string-join l "\\n")])
              (write-string (~a "  " src
                                " -> " dst
                                " [label=\"" label "\"];"))
              (newline))))
        (for ([edge (in-list edges)])
          (write-string (~a "  " (clean (edge-src edge))
                            " -> " (clean (edge-dst edge))
                            " [label=\"" (edge-id edge) "\"];"))
          (newline)))
    (write-string "}")
    (newline)))

(define prdeps (gen-project-deps))
(with-output-to-file "module_deps.gv" #:exists 'replace
  (thunk (deps->dot prdeps)))