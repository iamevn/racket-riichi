#lang racket
(provide (struct-out gamestate)
         make-gamestate
         gamestate-shorthand
         gamestate-shorthand->string)

(require "tiles.rkt"
         "util.rkt"
         "contracts.rkt")

; general game state around a win
(struct/contract gamestate ([seat wind?] ; what seat play is in
                            [round wind?] ; what round it is
                            [dora-indicators (listof tile?)] ; list of dora indicators
                            [tsumo? boolean?] ; win by self draw?
                            [ron? boolean?] ; win by discard?
                            [riichi? boolean?] ; win after riichi
                            [double? boolean?] ; win after riichi on the first turn
                            [ippatsu? boolean?] ; win in turn after riichi
                            [haitei? boolean?] ; win on last draw?
                            [houtei? boolean?] ; win on last discard?
                            [chankan? boolean?] ; win by robbing kan?
                            [rinshan? boolean?] ; win on deadwall draw after kan?
                            [tenhou/chiihou? boolean?]) ; win on first draw
                 #:transparent)

(define (make-gamestate seat
                        round
                        dora-indicators
                        #:tsumo [tsumo #false]
                        #:ron [ron #false]
                        #:riichi [riichi #false]
                        #:double [double #false]
                        #:ippatsu [ippatsu #false]
                        #:haitei [haitei #false]
                        #:houtei [houtei #false]
                        #:chankan [chankan #false]
                        #:rinshan [rinshan #false]
                        #:tenhou/chiihou [tenhou/chiihou #false])
  (if (xor tsumo ron)
      (gamestate seat round dora-indicators tsumo ron riichi double
                 ippatsu haitei houtei chankan rinshan tenhou/chiihou)
      (raise-argument-error 'make-gamestate
                            "tsumo or ron, not both"
                            (~a "#:tsumo " tsumo " #:ron " ron))))


(define/contract (gamestate-shorthand symbols #:dora [dora-indicators '("4p")])
  (->* ((listof symbol?))
       (#:dora (listof tile?))
       gamestate?)
  (make-gamestate (cond
                    [(member? 'seat-e symbols) "1z"]
                    [(member? 'seat-s symbols) "2z"]
                    [(member? 'seat-w symbols) "3z"]
                    [(member? 'seat-n symbols) "4z"]
                    [(member? 'dealer symbols) "1z"]
                    [else "2z"])
                  (cond
                    [(member? 'round-e symbols) "1z"]
                    [(member? 'round-s symbols) "2z"]
                    [(member? 'round-w symbols) "3z"]
                    [(member? 'round-n symbols) "4z"]
                    [else "1z"])
                  dora-indicators
                  #:riichi (one-member? '(rii riichi) symbols)
                  #:tsumo (one-member? '(tsu tsumo) symbols)
                  #:ron (one-member? '(ron) symbols)
                  #:ippatsu (one-member? '(ipp ippatsu) symbols)
                  #:double (one-member? '(dou double) symbols)
                  #:haitei (one-member? '(hai haitei) symbols)
                  #:houtei (one-member? '(hou houtei) symbols)
                  #:chankan (one-member? '(cha chankan) symbols)
                  #:rinshan (one-member? '(rin rinshan) symbols)
                  #:tenhou/chiihou (one-member? '(ten tenhou chi chiihou) symbols)))


(define/contract (gamestate-shorthand->string g)
  (-> (listof symbol?) string?)
  (gamestate->string (gamestate-shorthand g)))

(define/contract (gamestate->string g)
  (-> gamestate? string?)
  (let* ([round (wind-name (gamestate-round g))]
         [seat (wind-name (gamestate-seat g))])
    (string-join
     `(,(~a round " round")
       ,(~a seat " seat")
       ;dora
       ,@(map first
              (filter (λ (v) ((second v) g))
                      `(("tsumo" ,gamestate-tsumo?)
                        ("ron" ,gamestate-ron?)
                        ("riichi" ,gamestate-riichi?)
                        ("double riichi" ,gamestate-double?)
                        ("ippatsu" ,gamestate-ippatsu?)
                        ("haitei" ,gamestate-haitei?)
                        ("houtei" ,gamestate-houtei?)
                        ("chankan" ,gamestate-chankan?)
                        ("rinshan kaihou" ,gamestate-rinshan?)
                        ("tenhou" ,(λ (g) (and (equal? "1z" (gamestate-seat g))
                                               (gamestate-tenhou/chiihou? g))))
                        ("chiihou" ,(λ (g) (and (not (equal? "1z" (gamestate-seat g)))
                                                (gamestate-tenhou/chiihou? g))))))))
     ", ")))