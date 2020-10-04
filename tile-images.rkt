#lang racket

(require 2htdp/image
         racket/runtime-path
         "contracts.rkt"
         "util.rkt"
         "tiles.rkt"
         "hand.rkt"
         "melds.rkt"
         "call-notation.rkt")

(provide tile-images
         tile-back
         honors
         manzu
         pinzu
         souzu
         tile->image
         call-notation->image
         shorthand->images
         hand->image
         meld->image)

(define-runtime-path tiledir "tiles")
; load tiles using 2htdp/image
(define tilepaths
  (map (λ (basename) (build-path tiledir (~a basename ".gif")))
       (flatten (list "back" "ton" "nan" "sha" "pei" "haku" "hatsu" "chun"
                      (map (λ (suit)
                             (map (λ (n)(string-append suit (number->string n))) (range 1 10)))
                           '("man" "pin" "sou"))))))

(define tile-images
  (map bitmap/file tilepaths))


(define tile-back (first tile-images))
(define honors (take (cdr tile-images) 7))
(define manzu (take (drop tile-images 8) 9))
(define pinzu (take (drop tile-images 17) 9))
(define souzu (take (drop tile-images 26) 9))
(define spacer
  (let ([height (image-height tile-back)]
        [width (* 0.5 (image-width tile-back))])
    (rectangle width height 0 'transparent)))

(define/contract (tile->image tile)
  (-> tile? image?)
  (let ([suit (tile-suit tile)]
        [index (sub1 (tile-number tile))])
    (cond
      [(equal? suit #\m) (list-ref manzu index)]
      [(equal? suit #\p) (list-ref pinzu index)]
      [(equal? suit #\s) (list-ref souzu index)]
      [(equal? suit #\z) (list-ref honors index)])))


(define/contract (shorthand->images s)
  (-> handstring? (listof image?))
  (map tile->image (shorthand->tilelist s)))

(define/contract (hand->image h)
  (-> hand? image?)
  (let* ([m (hand-melds h)])
    (if (empty? m)
        (apply (curry beside/align "bottom")
               (map tile->image (hand-tiles h)))
        (apply (curry beside/align "bottom")
               (add-between (append (map meld->image m) (list (apply beside (map tile->image (hand-pair h)))))
                            spacer)))))

(define/contract (meld->image m)
  (-> meld? image?)
  (let ([images (map tile->image (meld-tiles m))])
    (cond
      [(and (meld-kan? m)
            (meld-closed? m))
       ; this is wrong, the stacked sideways tiles are for added kans
       #;(beside/align "bottom"
                       tile-back
                       (above (rotate -90 (second images))
                              (rotate -90 (third images)))
                       tile-back)
       (beside/align "bottom"
                     tile-back
                     (second images)
                     (third images)
                     tile-back)]
      [(and (meld-kan? m)
            (meld-open? m))
       (if (meld-src? m)
           (let ([callee (meld-src-callee m)])
             (beside/align "bottom"
                           (if (equal? callee 'left)
                               (rotate -90 (first images))
                               (first images))
                           (if (equal? callee 'middle)
                               (rotate -90 (second images))
                               (second images))
                           (third images)
                           (if (equal? callee 'right)
                               (rotate -90 (fourth images))
                               (fourth images))))
           (beside/align "bottom"
                         (first images)
                         (second images)
                         (third images)
                         (fourth images)))]
      [(or (meld-chii? m)
           (meld-pon? m))
       (if (meld-src? m)
           (let* ([callee (meld-src-callee m)]
                  [called (meld-src-called-tile m)]
                  [uncalled (remove called (meld-tiles m))]
                  [called-img (rotate -90 (tile->image called))]
                  [uncalled-imgs (map tile->image uncalled)])
             (beside/align-bottom*
              (case callee
                [(left) (cons called-img uncalled-imgs)]
                [(middle) (cons (car uncalled-imgs) (cons called-img (cdr uncalled-imgs)))]
                [(right) (append uncalled-imgs (list called-img))])))
           (apply beside images))])))

(define/contract (call-notation->image s)
  (-> call-notation? image?)
  (let* ([parsed (call-shorthand->closed-melds-last s)]
         [base (first parsed)]
         [melds (second parsed)]
         [base-img (beside/align-bottom* (map tile->image base))]
         [meld-imgs (map meld->image melds)])
    (if (empty? melds)
        base-img
        (beside/align-bottom* (add-between (list* base-img meld-imgs)
                                           spacer)))))

(define/contract (beside/align-bottom* lst)
  (-> list? image?)
  (if (equal? (length lst) 1)
      (first lst)
      (apply (curry beside/align "bottom") lst)))