#lang racket

(provide build-hand)

(require web-server/http
         net/url-string
         "../score-hand.rkt"
         "hand-page.rkt")

; link up js
; non-text hand builder
; frame for score? load data as json or something and render in-page?
(define (build-hand request)
  (let* ([query (url-query (request-uri request))]
         [query-hash (make-hash query)])
    (response/xexpr
     `(html
       (head (title "Riichi Hand Builder")
             (script ([src "/js/build-hand.js"])))
       (body
        (input ([type "button"]
                [id "score"]
                [value "Score hand"]))
        (fieldset
         (legend "Hand shorthand")
         (input ([type "text"]
                 [id "input-hand"]
                 [required "required"]
                 [minlength "15"]
                 [size "32"]
                 [placeholder "12322m444p78(9)p333z"])))
        (fieldset
         (legend "Game state")
         (div (div (label ([for "finish-type"]) "Finish type:")
                   (select ([name "finish"]
                            [id "finish-type"])
                           (option ([value "tsumo"]) "tsumo")
                           (option ([value "ron"]) "ron")))
              (div (label ([for "round"]) "Round wind:")
                   (select ([name "round"]
                            [id "round"])
                           (option ([value "1z"]) "east")
                           (option ([value "2z"]) "south")
                           (option ([value "3z"]) "west")
                           (option ([value "4z"]) "north")))
              (div (label ([for "seat"]) "Seat wind:")
                   (select ([name "seat"]
                            [id "seat"])
                           (option ([value "1z"]) "east")
                           (option ([value "2z"]) "south")
                           (option ([value "3z"]) "west")
                           (option ([value "4z"]) "north")))
              ,@(map (λ (s)
                       `(div (input ([type "checkbox"]
                                     [id ,s]))
                             (label ([for ,s]) ,s)))
                     (list "riichi" "double" "ippatsu"
                           "haitei" "houtei"
                           "chankan" "rinshan"
                           "tenhou" "chiihou"))))
        (fieldset
         (legend "Instructions")
         (p "Type a hand into the text box and set the
             game state in the selects and checkboxes.
             Click [Score hand] button to score it."
            (br))
         (p "Shorthand works like this:" (br)
            (l (li "Tiles are a number and a suit like 2m or 5s")
               (li "Suits are either m, p, s, or z. Corresponding to manzu 
                    (characters), pinzu (dots),  souzu (bamboo), and jihai (honors) 
                    respectively.")
               (li "123m and 1m2m3m are equivalent. Adjacent tiles of the same
                    suit may omit the suit until the end of the group")
               (li "The final drawn/called tile which finished the
                    hand is marked by surrounding that tile with parentheses.")
               (li "Called melds are separated from the rest of the tiles by spaces.")
               (li "The position of the suit marker within a meld indicates
                    who the meld was called from.")
               (li "For a kan, the suit indicator being on the far right indicates
                    a closed kan.")))
         (p "For example, \"12(3)p44s 11z1 687s 6666p\" is a hand with" (br)
            (img ([src ,(img-encode
                         (first (first (list-score-hand "12(3)p44s 11z1 687s 6666p"
                                                        '(seat-e tsumo)))))]))
            (l (li "An open pon of east wind from the player across the table")
               (li "An open chii of 6,7,8 bamboo with the 7 called from the player to the right")
               (li "A closed kan of 6 pin")
               (li "And a 3 pin as the tile that finished the hand.")))))))))