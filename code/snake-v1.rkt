;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname snake-v1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CODE STANDARDS ;;;;;;;;;;;;;;;;;;;;;;;;;
; - Constants always UPPERCASE
; - Functions always Capitalize


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LIBRARIES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 2htdp/universe)
(require 2htdp/image)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; IMAGES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; App background
(define Background (empty-scene 1000 1000))


; a SnakeUnit is an Image
; It represents the little rectangles that the snake is made off
(define SnakeUnit (rectangle 25 25 "solid" "green"))

(define AppleUnit (rectangle 25 25 "solid" "red"))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; THE GAME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;; POSITION ;;;;;;;;;;

; a Position is a Posn as such:
; - (make-posn -1 -1)
; - (make-posn (- (posn-x Position) 26) (- (posn-y Position) 26))
; represents the position of a element of the snake



;;;;;;;;;; LENGTH ;;;;;;;;;;

; a Length is a Number as such:
; - the Number 3
; - Length + 1
; represents the length of the snake
; Header (define (length state) 3)

; Examples
(check-expect (dimension Snake1) (make-snake (snake-position Snake1) 3 (snake-direction Snake1)))
(check-expect (dimension Snake2) (make-snake (snake-position Snake2) 5 (snake-direction Snake2)))
(check-expect (dimension Snake3) (make-snake (snake-position Snake3) 6 (snake-direction Snake3)))
(check-expect (dimension Snake4) (make-snake (snake-position Snake4) 7 (snake-direction Snake4)))
(check-expect (dimension Snake5) (make-snake (snake-position Snake5) 4 (snake-direction Snake5)))

; Template
;(define (dimension state)
;  (cond
;    [(and (= (snake-length state) 3) (equal? (make-posn -26 -26) (snake-position state))) ... state ...] ; base case where the n = 3 (the minimun snake's lenght and the position is "center"
;    [(or (= (snake-length state) 3) (equal? (make-posn -26 -26) (snake-position state))) ... state ...]  ; base case where the n = 3 (the minimun snake's lenght or the position is "center"
;    [else ... state ...]))                                                                               ; recursive case

; Code
(define (dimension state)
  (cond
    [(and (= (snake-length state) 3) (equal? (make-posn -27 -27) (snake-position state))) state] ; snake direction, length and direction are the same
    [else (make-snake (snake-position state)                                                     ; snake position is the same
          (add1 (snake-length state))                                                            ; snake length become one more bigger
          (snake-direction state))]))                                                            ; snake direction is the same



;;;;;;;;;; DIRECTION ;;;;;;;;;;

; a Direction is one of these String
; - "up"
; - "down"
; - "left"
; - "right"

(define Up "up")
(define Down "down")
(define Left "left")
(define Right "right")



;;;;;;;;;; SNAKE ;;;;;;;;;;

; a Snake is a struct
; (make-snake position length direction)
; where:
; - position is a Position
; - length is a Length
; - direction is a Direction 

(define-struct snake [position length direction])

(define Snake1 (make-snake (make-posn -27 -27) 3 Up))
(define Snake2 (make-snake (make-posn -27 -27) 4 Down))
(define Snake3 (make-snake (make-posn -27 -53) 5 Left))
(define Snake4 (make-snake (make-posn -53 -27) 6 Right))
(define Snake5 (make-snake (make-posn -1 -1) 3 Left))



;;;;;;;;;; APPLE ;;;;;;;;;;

; an Apple is a Position
; it represents the position of the apple at a particular moment

(define Apple1 (make-posn -27 -27))
(define Apple2 (make-posn -1 -27))



;;;;;;;;;; GAME ;;;;;;;;;;

; a Maybe<Game> is one of:
; - #true : the game is running
; - #false : the game is on pause

(define Maybe<Game>_t #true)
(define Maybe<Game>_f #false)

;;;;;;;;;; QUIT ;;;;;;;;;;

; a quit is one of:
; - #false : the game is on and is running
; - #true : the game is off and "close"



;;;;;;;;;; APPSTATE ;;;;;;;;;;

; an AppState is a Struct
; (make-appstate snake apple game quit)
; where:
; - snake is a Snake
; - apple is an Apple
; - game is a Maybe<Game>
; - quit is a Boolean

(define-struct appstate [snake apple game quit])



; change-snake-direction : Direction Snake -> Snake
; changes the direction of the snake

; Example
(check-expect (change-snake-direction "down" Snake1) (make-snake (make-posn -27 -27) 3 "down"))
(check-expect (change-snake-direction "up" Snake2) (make-snake (make-posn -27 -27) 4 "up"))
(check-expect (change-snake-direction "left" Snake3) (make-snake (make-posn -27 -53) 5 "left"))
(check-expect (change-snake-direction "right" Snake4) (make-snake (make-posn -53 -27) 6 "right"))
(check-expect (change-snake-direction "up" Snake5) (make-snake (make-posn -1 -1) 3 "up"))

; Template
;(define (change-snake-direction dir snake)
;  (cond
;    [(string=? dir "down") ... state ...]
;    [(string=? dir "right") ... state ...]
;    [(string=? dir "left") ... state ...]
;    [(string=? dir "up") ... state ...]))

; Code
(define (change-snake-direction dir snake)
  (cond
    [(string=? dir "down") (make-snake (snake-position snake) (snake-length snake) "down")]
    [(string=? dir "right") (make-snake (snake-position snake) (snake-length snake) "right")]
    [(string=? dir "left") (make-snake (snake-position snake) (snake-length snake) "left")]
    [(string=? dir "up") (make-snake (snake-position snake) (snake-length snake) "up")]))

; update-snake-position : Direction Snake -> Snake
; updates snake position based on snake direction

(define (update-snake-direction dir snake)
  (cond
    [(string=? dir "down") (make-snake (snake-position snake) (snake-length snake) "down")]
    [(string=? dir "right") (make-snake (snake-position snake) (snake-length snake) "right")]
    [(string=? dir "left") (make-snake (snake-position snake) (snake-length snake) "left")]
    [(string=? dir "up") (make-snake (snake-position snake) (snake-length snake) "up")]))


;(define (snake-game AppState)
;  (big-bang AppState
;    [to-draw draw]             ; draw the snake
;    [on-tick move-snake time]  ; uptade snake's position and "time" incrase each tick
;    [on-key handle-mouse]      ; change snake's direction or reset game or quit the game
;    [stop-when end?]))         ; quit the application