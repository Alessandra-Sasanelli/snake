;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname snake-v1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CODE STANDARDS ;;;;;;;;;;;;;;;;;;;;;;;;;

; - Constants always UPPERCASE
; - Functions always Capitalize


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LIBRARIES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 2htdp/universe)
(require 2htdp/image)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; IMAGES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; App background
(define Background (empty-scene 500 500 "black"))


; a SnakeUnit is an Image
; It represents the little rectangles that the snake is made off
(define SnakeUnit (rectangle 24 24 "solid" "green"))

; a AppleUnit is an Image
; It represents the little rectangles that the apple is made off
(define AppleUnit (rectangle 24 24 "solid" "red"))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; THE GAME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;; POSITION ;;;;;;;;;;

; a Position is Posn as such:
; - (make-posn -1 -1)
; - (make-posn (- (posn-x Position) 25) (- (posn-y Position) 25))
; represents the position of a element of the snake

; a List<Positions> is a one of:
; - (cons Position '())
; - (cons Position (cons List<Position> '()))



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
(define (dimension Snake)
  (cond
    [(and (= (snake-length Snake) 3) (equal? (make-posn -26 -26) (snake-position Snake))) Snake] ; snake direction, length and direction are the same
    [else (make-snake (snake-position Snake)                                                     ; snake position is the same
          (add1 (snake-length Snake))                                                            ; snake length become one more bigger
          (snake-direction Snake))]))                                                            ; snake direction is the same



;;;;;;;;;; DIRECTION ;;;;;;;;;;

; a Direction is one of these String
; - "up"
; - "left"
; - "down"
; - "right"

(define Up "up")
(define Left "left")
(define Down "down")
(define Right "right")



;;;;;;;;;; SNAKE ;;;;;;;;;;

; a Snake is a struct
; (make-snake position length direction)
; where:
; - position is a Position
; - length is a Length
; - direction is a Direction 

(define-struct snake [position length direction])

(define SnakeDefault (make-snake (make-posn -225 -225) 3 Right))
(define Snake1 (make-snake (make-posn -26 -26) 3 Up))
(define Snake2 (make-snake (make-posn -26 -26) 4 Down))
(define Snake3 (make-snake (make-posn -26 -52) 5 Left))
(define Snake4 (make-snake (make-posn -52 -26) 6 Right))
(define Snake5 (make-snake (make-posn -1 -1) 3 Left))



;;;;;;;;;; APPLE ;;;;;;;;;;

; an Apple is a Position
; it represents the position of the apple at a particular moment

(define Apple1 (make-posn -26 -26))
(define Apple2 (make-posn -1 -26))



;;;;;;;;;; GAME ;;;;;;;;;;

; a Game is one of:
; - #true : the game is running
; - #false : the game is lost and the application is on pause

(define Game_t #true)
(define Game_f #false)



;;;;;;;;;; QUIT ;;;;;;;;;;

; a quit is one of:
; - #false : the game is on and is running
; - #true : the game is off and "close"

(define quit_f #false)
(define quit_t #true)



;;;;;;;;;;;;;;;;;;;; APPSTATE ;;;;;;;;;;;;;;;;;;;;

; an AppState is a Struct
; (make-appstate snake apple game quit)
; where:
; - snake is a Snake
; - apple is an Apple
; - game is a Game
; - quit is a Quit

(define-struct appstate [snake apple game quit])

(define Default (make-appstate SnakeDefault Apple1 Game_t quit_f))
(define E1 (make-appstate Snake1 Apple1 Game_t quit_f))
(define E2 (make-appstate Snake2 Apple1 Game_t quit_f))
(define E3 (make-appstate Snake3 Apple1 Game_t quit_f))
(define E4 (make-appstate Snake4 Apple1 Game_t quit_f))
(define E5 (make-appstate Snake5 Apple1 Game_t quit_f))
(define E6 (make-appstate Snake1 Apple1 Game_f quit_f))
(define E7 (make-appstate Snake1 Apple1 Game_t quit_t))





;;;;;;;;;;;;;;;;;;;; DRAW ;;;;;;;;;;;;;;;;;;;;

; draw : AppState -> AppState
; draw the snake and the apple's position on the background


;(define (draw state)
;  (overlay/xy AppleUnit -1 -1 (overlay/xy SnakeUnit -1 -1 Background)))

(define (draw state)
  (overlay/xy AppleUnit (posn-x Apple2) (posn-y Apple2) (overlay/xy SnakeUnit (x-cord (appstate-snake state)) (y-cord (appstate-snake state)) Background)))

(define (x-cord snake)
  (posn-x (snake-position snake)))

(define (y-cord snake)
  (posn-y (snake-position snake)))


;;;;;;;;;;;;;;;;;;;; TICK ;;;;;;;;;;;;;;;;;;;;

; tick : AppState Number -> Number
; change the position of the snake and apple's position, if it was eaten by snake, and the tick is become faster and faster every time the snake eats an apple




;;;;;;;;;;;;;;;;;;;; HANDLE KEYBOARD ;;;;;;;;;;;;;;;;;;;;

; an handle-keyboard is one of:
; - up    : call the fuction to change the snake's direction to up
; - right : call the fuction to change the snake's direction to right
; - down  : call the fuction to change the snake's direction to down
; - left  : call the fuction to change the snake's direction to left
; - r     : call the fuction to reset the game
; - esc   : call the fuction to close main application 
; - Any   : return the prevoius AppState

; Example
(check-expect (handle-keyboard E2 "up") (make-appstate (make-snake (make-posn -26 -26) 4 Up) Apple1 Game_t quit_f))
(check-expect (handle-keyboard E1 "right") (make-appstate (make-snake (make-posn -26 -26) 3 Right) Apple1 Game_t quit_f))
(check-expect (handle-keyboard E1 "down") (make-appstate (make-snake (make-posn -26 -26) 3 Down) Apple1 Game_t quit_f))
(check-expect (handle-keyboard E1 "left") (make-appstate (make-snake (make-posn -26 -26) 3 Left) Apple1 Game_t quit_f))
(check-expect (handle-keyboard E1 "r") Default)
(check-expect (handle-keyboard E1 "escape") E7)
(check-expect (handle-keyboard E7 "escape") E7)
(check-expect (handle-keyboard E1 "") E1)
(check-expect (handle-keyboard E1 " ") E1)
(check-expect (handle-keyboard E1 "ciao") E1)
(check-expect (handle-keyboard E1 "b") E1)
(check-expect (handle-keyboard E1 #false) E1)
(check-expect (handle-keyboard E1 #true) E1)
(check-expect (handle-keyboard E1 2) E1)

; Template
;(define (handle-mouse key state)
;  (cond
;    [(not (string? key)) ... state ...]
;    [(string=? key "up") ... state ...]
;    [(string=? key "right") ... state ...]
;    [(string=? key "down") ... state ...]
;    [(string=? key "left") ... state ...]
;    [(string=? key "r") ... state ...]
;    [(string=? key "escape") ... state ...]
;    [else ... state ...]))

; Code
(define (handle-keyboard state key)
  (cond
    [(not (string? key)) state]                                  ; for any possible not-key input which are not a string, the output is the same AppState as before
    [(string=? key "up") (change-snake-direction key state)]     ; change the direction to up
    [(string=? key "right") (change-snake-direction key state)]  ; change the direction to right
    [(string=? key "down") (change-snake-direction key state)]   ; change the direction to down
    [(string=? key "left") (change-snake-direction key state)]   ; change the direction to left
    [(string=? key "r") (reset state)]                           ; reset the game
    [(string=? key "escape") (quit state)]                       ; quit the game
    [else state]))                                               ; for any possible not-key input which are a string, the output is the same AppState as before


;;;;;;;;;; CHANGE SNAKE DIRECTION ;;;;;;;;;;
; change-snake-direction : Direction AppState -> AppState
; changes the direction of the snake
; Header (define (change-snake-direction dir state) )

; Example
(check-expect (change-snake-direction "up" E2) (make-appstate (make-snake (make-posn -26 -26) 4 Up) Apple1 Game_t quit_f))
(check-expect (change-snake-direction "right" E1) (make-appstate (make-snake (make-posn -26 -26) 3 Right) Apple1 Game_t quit_f))
(check-expect (change-snake-direction "down" E1) (make-appstate (make-snake (make-posn -26 -26) 3 Down) Apple1 Game_t quit_f))
(check-expect (change-snake-direction "left" E1) (make-appstate (make-snake (make-posn -26 -26) 3 Left) Apple1 Game_t quit_f))


; Template
;(define (change-snake-direction dir state)
;  (cond
;    [(string=? dir "up") ... state ...]
;    [(string=? dir "left") ... state ...]
;    [(string=? dir "down") ... state ...]
;    [(string=? dir "right") ... state ...]))

; Code
(define (change-snake-direction dir state)
  (cond
    [(string=? dir "up") (make-appstate (update-snake-position dir (appstate-snake state)) (appstate-apple state) (appstate-game state) (appstate-quit state))]     ; change the direction to up
    [(string=? dir "right") (make-appstate (update-snake-position dir (appstate-snake state)) (appstate-apple state) (appstate-game state) (appstate-quit state))]  ; change the direction to right
    [(string=? dir "down") (make-appstate (update-snake-position dir (appstate-snake state)) (appstate-apple state) (appstate-game state) (appstate-quit state))]   ; change the direction to down
    [(string=? dir "left") (make-appstate (update-snake-position dir (appstate-snake state)) (appstate-apple state) (appstate-game state) (appstate-quit state))])) ; change the direction to left



;;;;;;;;;; UPDATE SNAKE DIRECTION ;;;;;;;;;;

; update-snake-position : Direction Snake -> Snake
; updates snake position based on snake direction

; Examples
(check-expect (update-snake-position "up" Snake2) (make-snake (make-posn -26 -26) 4 "up"))
(check-expect (update-snake-position "right" Snake4) (make-snake (make-posn -52 -26) 6 "right"))
(check-expect (update-snake-position "down" Snake1) (make-snake (make-posn -26 -26) 3 "down"))
(check-expect (update-snake-position "left" Snake3) (make-snake (make-posn -26 -52) 5 "left"))
(check-expect (update-snake-position "up" Snake5) (make-snake (make-posn -1 -1) 3 "up"))

; Template
;(define (update-snake-position dir snake)
;  (cond
;    [(string=? dir "up") ... snake ...]
;    [(string=? dir "left") ... snake ...]
;    [(string=? dir "right") ... snake ...]
;    [(string=? dir "down") ... snake ...]

; Code
(define (update-snake-position dir snake)
  (cond
    [(string=? dir "up") (make-snake (snake-position snake) (snake-length snake) "up")]        ; change the direction to up
    [(string=? dir "right") (make-snake (snake-position snake) (snake-length snake) "right")]  ; change the direction to right
    [(string=? dir "down") (make-snake (snake-position snake) (snake-length snake) "down")]    ; change the direction to down
    [(string=? dir "left") (make-snake (snake-position snake) (snake-length snake) "left")]))  ; change the direction to left


;;;;;;;;;; RESET ;;;;;;;;;;

; a reset is a Boolean
; reset : AppState -> AppState
; if the r button is pressed, the main application is reseted
; Header (define (end? state) #false)

; Examples
(check-expect (reset E1) Default)
(check-expect (reset E2) Default)
(check-expect (reset E3) Default)
(check-expect (reset E4) Default)
(check-expect (reset E5) Default)
(check-expect (reset E6) E6)

; Template
;(define (reset state)
;  (cond
;    [(boolean=? (appstate-quit state) #true) ... state ...]
;    [else ... state ...]))

; Code
(define (reset state)
  (cond
    [(boolean=? (appstate-game state) #true) Default] ; the game continue to run
    [else state]))                                    ; the game is reseted



;;;;;;;;;; QUIT ;;;;;;;;;;

; a quit is an AppState
; quit : AppState -> BAppState
; if the escape button is pressed, the quit change to quit the application
; Header (define (end? state) #false)

; Examples
(check-expect (quit E1) E7)
(check-expect (quit E2) (make-appstate Snake2 Apple1 Game_t quit_t))
(check-expect (quit E3) (make-appstate Snake3 Apple1 Game_t quit_t))
(check-expect (quit E4) (make-appstate Snake4 Apple1 Game_t quit_t))
(check-expect (quit E5) (make-appstate Snake5 Apple1 Game_t quit_t))


; Templates
; (define (quit state) ... state ...)

; Code
(define (quit state) (make-appstate
                      (appstate-snake state) ; the snake remain the same
                      (appstate-apple state) ; the apple remain the same
                      (appstate-game state)  ; the game remain the same
                      #true))                ; the quit change to quit the application



;;;;;;;;;; END ;;;;;;;;;;

; a end? is a Boolean
; end? : AppState -> Boolean
; if the escape button is pressed, the main application is closed
; Header (define (end? state) #false)

; Examples
(check-expect (end? E1) #false)
(check-expect (end? E2) #false)
(check-expect (end? E3) #false)
(check-expect (end? E4) #false)
(check-expect (end? E5) #false)
(check-expect (end? (make-appstate Snake1 Apple1 Game_t quit_t)) #true)
(check-expect (end? (make-appstate Snake2 Apple1 Game_t quit_t)) #true)
(check-expect (end? (make-appstate Snake3 Apple1 Game_t quit_t)) #true)
(check-expect (end? (make-appstate Snake4 Apple1 Game_t quit_t)) #true)
(check-expect (end? (make-appstate Snake5 Apple1 Game_t quit_t)) #true)


; Template
;(define (end? state)
;  (cond
;    [(boolean=? (appstate-quit state) #false) ... state ...]
;    [else ... state ...]))

; Code
(define (end? state)
  (cond
    [(boolean=? (appstate-quit state) #false) #false] ; the application remains on
    [else #true]))                                    ; the application turn off





;;;;;;;;;;;;;;;;;;;; MAIN APPLICATION ;;;;;;;;;;;;;;;;;;;;

(define (snake-game AppState)
  (big-bang AppState
    [to-draw draw]             ; draw the snake
;    [on-tick move-snake time]  ; uptade snake's position and "time" incrase each tick
    [on-key handle-keyboard]   ; change snake's direction or reset game or quit the game
    [stop-when end?]))         ; quit the application