;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname snake-final) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; Convenzioni
;  - constants UPPERCASE
;  - anything else lowercase using kebab-case

;; Libraries

(require 2htdp/universe)
(require 2htdp/image)

;; DATA TYPES

; a SnakeUnit is an Image
; It represents the little rectangles that the snake is made off

; The SnakeHead is an Image
; It represents the head of the snake
; it is an overlay of an eye, a tongue and a SnakeUnit

; an AppleUnit is an Image
; It represents the little rectangles that the apple is made off

; a Direction is one of these String
; - "up"
; - "left"
; - "down"
; - "right"

; a Game is one of:
; - #true : the game is running
; - #false : the game is lost and the application is on pause

; a Quit is one of:
; - #false : the game is on and is running
; - #true : the game is off and "close"

; a List<Posn> is one of:
;  - (cons Posn (cons Posn (cons Posn ’())))
;  - (cons Posn List<Posn>)

; a Length is a Number as such:
; - the Number 3
; - Length + 1
; represents the length of the snake

; a Snake is a struct
; (make-snake position length direction)
; where:
; - position is a List<Posn>    --> represents the positions of the elements that make up the snake
; - length is a Length          --> the lenght of the snake
; - direction is a Direction    --> the direction of the head
(define-struct snake [position lenght direction])

; an Apple is a Position
; it represents the position of the apple at a particular moment

; an AppState is a Struct
; (make-appstate snake apple game quit)
; where:
; - snake is a Snake
; - apple is an Apple
; - game is a Game
; - quit is a Quit
(define-struct appstate [snake apple freepositions game quit])

;; CONSTANTS
; app scene background
(define BACKGROUND (empty-scene 501 501 'white))

; SnakeUnit
(define SNAKEUNIT (rectangle 24 24 'solid 'green))

; Elements to draw the SnakeHead
(define EYE (circle 3 "solid" "black"))
(define TONGUE (rectangle 3 10 "solid" "red"))
(define SNAKEHEAD (place-image EYE 7 12 (place-image EYE 17 12 (place-image TONGUE 12 3 SNAKEUNIT))))

; AppleUnit
(define APPLEUNIT (rectangle 24 24 "solid" "red"))

; Directions
(define UP "up")
(define LEFT "left")
(define DOWN "down")
(define RIGHT "right")

; the game
(define GAME-T #true)
(define GAME-F #false)

; the quit
(define QUIT-F #false)
(define QUIT-T #true)

; make-positions: Number Number Number List<Posn> -> List<Posn>
; computes all the positions on the background before the game starts
(define (make-positions n x y lop)
  (cond
    [(= n 400) lop]
    [else
     (cond
       [(= (posn-x (first lop)) 488)
        (make-positions
         (+ n 1)
         1
         (+ y 1)
         (cons (make-posn 13 (+ (* 12 (+ (* y 2) 1)) (+ y 1))) lop))]
       [else
        (make-positions
         (+ n 1)
         (+ x 1)
         y
         (cons (make-posn (+ (* 12 (+ (* x 2) 1)) (+ x 1)) (posn-y (first lop))) lop))]
       )
     ]))

; compute-available-pos: Snake Apple List<Posn> -> List<Posn>
; compute available positions on the background
(define (compute-available-pos snake apple lop)
  (cond
    [(or (empty? (rest lop)) (empty? (rest (snake-position snake)))) '()]
    [(equal? apple (first lop)) (compute-available-pos snake apple (rest lop))]
    [(equal? (first (snake-position snake)) (first lop))
     (compute-available-pos
      (make-snake
       (rest (snake-position snake))
       (snake-lenght snake)
       (snake-direction snake)) apple (rest lop))]
    [else
     (cons (first lop) (compute-available-pos snake apple (rest lop)))]))

; All the positions on the background
(define BACKGROUNDPOS (make-positions 1 1 1 (list (make-posn 13 13))))

;; FUNCTIONS
; compute-apple-position: Number Number List<Posn> -> Apple
; computes the apple position based on available positions
(define (compute-apple-position n acc lop)
  (cond
    [(= n acc) (first lop)]
    [else
     (compute-apple-position n (+ acc 1) (rest lop))]))

; draw-snake: Snake -> Image
; draws the snake on the background
(define (draw-snake snake)
  (cond
    [(empty? (rest (snake-position snake)))
     (place-image
      SNAKEUNIT
      (posn-x (first (snake-position snake)))
      (posn-y (first (snake-position snake)))
      BACKGROUND)]
    [else
     (place-image
      SNAKEUNIT
      (posn-x (first (snake-position snake)))
      (posn-y (first (snake-position snake)))
      (draw-snake
       (make-snake (rest (snake-position snake)) (snake-lenght snake) (snake-direction snake))))
     ]))

; draw-appstate: AppState -> Image
; draws the appstate
(define (draw-appstate appstate)
  (place-image
   APPLEUNIT
   (posn-x (appstate-apple appstate))
   (posn-y (appstate-apple appstate))
   (draw-snake (appstate-snake appstate))))

; increment-pos: Number -> Number
; increments the x or the y position of a posn
(define (increment-pos xy)
  (+ xy 25))

; decrement-pos: Number -> Number
; decrements the x or the y position of a posn
(define (decrement-pos xy)
  (- xy 25))

; last: List<Posn> -> List<Posn>
; returns the last element of a list
(define (last lop)
  (cond
    [(empty? (rest lop)) (first lop)]
    [else
     (last (rest lop))]))

; change-snake-direction: String Snake -> Snake
; changes snake's head direction
(define (change-snake-direction direction snake)
  (cond
    [(string=? direction UP)
     (make-snake (snake-position snake) (snake-lenght snake) UP)]
    [(string=? direction DOWN)
     (make-snake (snake-position snake) (snake-lenght snake) DOWN)]
    [(string=? direction LEFT)
     (make-snake (snake-position snake) (snake-lenght snake) LEFT)]
    [(string=? direction RIGHT)
     (make-snake (snake-position snake) (snake-lenght snake) RIGHT)]
    [else snake]))

; move-snake: Direction List<Posn> -> List<Posn>
(define (move-snake d lop)
     (cond
       [(string=? d UP)
        (if (empty? (rest lop))
            (cons (make-posn (posn-x (first lop)) (decrement-pos (posn-y (first lop)))) '())
            (cons
             (make-posn (posn-x (first lop)) (decrement-pos (posn-y (first lop))))
             (move-snake d (rest lop))))]
       [(string=? d DOWN)
        (if (empty? (rest lop))
            (cons (make-posn (posn-x (first lop)) (increment-pos (posn-y (first lop)))) '())
            (cons
             (make-posn (posn-x (first lop)) (increment-pos (posn-y (first lop))))
             (move-snake d (rest lop))))]
       [(string=? d LEFT)
        (if (empty? (rest lop))
            (cons (make-posn (decrement-pos (posn-x (first lop))) (posn-y (first lop))) '())
            (cons
             (make-posn (decrement-pos (posn-x (first lop))) (posn-y (first lop)))
             (move-snake d (rest lop))))]
       [(string=? d RIGHT)
        (if (empty? (rest lop))
            (cons (make-posn (increment-pos (posn-x (first lop))) (posn-y (first lop))) '())
            (cons
             (make-posn (increment-pos (posn-x (first lop))) (posn-y (first lop)))
             (move-snake d (rest lop))))]))


; move: AppState -> AppState
(define (move appstate)
  (make-appstate
   (make-snake
    (move-snake (snake-direction (appstate-snake appstate)) (snake-position (appstate-snake appstate))) (snake-lenght (appstate-snake appstate)) (snake-direction (appstate-snake appstate)))
   (appstate-apple appstate)
   (compute-available-pos
    (make-snake
    (move-snake (snake-direction (appstate-snake appstate)) (snake-position (appstate-snake appstate))) (snake-lenght (appstate-snake appstate)) (snake-direction (appstate-snake appstate)))
    (appstate-apple appstate)
    BACKGROUNDPOS)
   (appstate-game appstate)
   (appstate-quit appstate)))

; time-tick: AppState -> Number
; changes the speed of the snake based on snake lenght
(define (time-tick state) (/ 2.7 (snake-lenght (appstate-snake state))))

; reset: AppState -> AppState
; changes the game in the appstate
(define (reset state)
  (cond
    [(boolean=? (appstate-game state) #true) DEFAULT] ; the game continue to run
    [else state]))

; quit: AppState -> AppState
; changes the quit in the appstate
(define (quit state) (make-appstate
                      (appstate-snake state) ; the snake remain the same
                      (appstate-apple state) ; the apple remain the same
                      (appstate-game state)  ; the game remain the same
                      #true))  

; handle-keyboard: AppState KeyboardEvent -> AppState
; handles the keyboard events
(define (handle-keyboard state key)
  (cond
    [(not (string? key)) state]                                                                          ; for any possible not-key input which are not a string, the output is the same AppState as before
    [(or (and (string=? key "up") (string=? (snake-direction (appstate-snake state)) "down"))            ; if the direction is opposite of the key, the state is the same
         (and (string=? key "right") (string=? (snake-direction (appstate-snake state)) "left"))         ; if the direction is opposite of the key, the state is the same
         (and (string=? key "down") (string=? (snake-direction (appstate-snake state)) "up"))            ; if the direction is opposite of the key, the state is the same
         (and (string=? key "left") (string=? (snake-direction (appstate-snake state)) "right"))) state] ; if the direction is opposite of the key, the state is the same
    [(or (string=? key "up") (string=? key "right") (string=? key "down") (string=? key "left"))
     (make-appstate
      (change-snake-direction key (appstate-snake state))
      (appstate-apple state)
      (compute-available-pos (change-snake-direction key (appstate-snake state)) (appstate-apple state) BACKGROUNDPOS)
      (appstate-game state)
      (appstate-quit state))]                                                                                     
    [(string=? key "r") (reset state)]                                                                   ; reset the game
    [(string=? key "escape") (quit state)]                                                               ; quit the game
    [else state]))

; check-position-out: Posn List<Posn> -> Boolean
; checks wheather the given posn is into backgroundpos
(define (check-position-out pos lop)
  (cond
    [(empty? (rest lop)) (if (equal? pos (first lop)) #true #false)]
    [else
     (if (equal? pos (first lop)) #true (check-position-out pos (rest lop)))]))


; end?: AppState -> Boolean
; stops the game
(define (end? state)
  (cond
    [(check-position-out (first (snake-position (appstate-snake state))) BACKGROUNDPOS) #true] ; if the snake is over the left limit the application turn off
    [(boolean=? (appstate-quit state) #false) #false]                                                ; the application remains on
    [else #true]))   

;; MAIN APPLICATION
; the first Snake
(define SNAKE1 (make-snake (list (make-posn 63 13) (make-posn 38 13) (make-posn 13 13)) 3 RIGHT))

; the first example of an Apple
(define APPLE1 (compute-apple-position (random 401) 1 (compute-available-pos SNAKE1 (make-posn 0 0) BACKGROUNDPOS)))

; the default AppState
(define DEFAULT (make-appstate SNAKE1 APPLE1 (compute-available-pos SNAKE1 APPLE1 BACKGROUNDPOS) GAME-F QUIT-F))

(define (snake-game appstate)
  (big-bang appstate
    [to-draw draw-appstate]                                      ; draw the snake
    [on-key handle-keyboard]                                     ; change snake's direction or reset game or quit the game
    [on-tick move (time-tick appstate)]                          ; uptade snake's position and "time" incrase each tick
;    [stop-when end?]                                            ; quit the application
    ))                          