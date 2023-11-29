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

; a length is a Number as such:
; - the Number 3
; - length + 1
; represents the length of the snake

; a Snake is a struct
; (make-snake position length direction)
; where:
; - position is a List<Posn>    --> represents the positions of the elements that make up the snake
; - length is a Length          --> the length of the snake
; - direction is a Direction    --> the direction of the head
; - breakpoint is a List<Posn>  --> represents at which positions has the snake changed direction        
(define-struct snake [position length direction breakpoint])

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
; eye
(define EYE (circle 3 'solid 'black))
; tongue
(define TONGUE (rectangle 10 3 'solid 'red))
; head
(define HEAD (put-image SNAKEUNIT 0 12 (circle 12 'solid 'green)))
;snake head
(define SNAKEHEAD (place-image EYE 10 8 (place-image EYE 10 16 (place-image TONGUE 20 12 HEAD))))
; tail
(define TAIL (put-image SNAKEUNIT 24 12 (circle 12 'solid 'green)))

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


;;;;;;;;;; CHECK POSITION OUT ;;;;;;;;;;
; check-position-out: Posn List<Posn> -> Boolean
; checks wheather the given posn is into backgroundpos
; Header (define (check-position-out pos lop) #false

;Examples
;the left limit
(check-expect (check-position-out (make-posn 13 13) BACKGROUNDPOS) #false)
(check-expect (check-position-out (make-posn -12 13) BACKGROUNDPOS) #true)
; the right limit
(check-expect (check-position-out (make-posn 488 13) BACKGROUNDPOS) #false)
(check-expect (check-position-out (make-posn 513 13) BACKGROUNDPOS) #true)
; the top limit
(check-expect (check-position-out (make-posn 13 13) BACKGROUNDPOS) #false)
(check-expect (check-position-out (make-posn 13 -12) BACKGROUNDPOS) #true)
; the bottom limit
(check-expect (check-position-out (make-posn 13 488) BACKGROUNDPOS) #false)
(check-expect (check-position-out (make-posn 13 513) BACKGROUNDPOS) #true)

; Template
;(define (check-position-out pos lop)
;  (cond
;    [(empty? (rest lop)) ... boolean ...]
;    [else ... boolean ...]))

; Code
(define (check-position-out pos lop)
  (cond
    [(empty? (rest lop)) (if (equal? pos (first lop)) #false #true)]             ; if rest of list is empty, check if pos = first of list
    [else
     (if (equal? pos (first lop)) #false (check-position-out pos (rest lop)))])) ; if el pos = first of lop, return false, owhtewise, go ahead

(define (delete-el pos lop)
  (cond
    [(empty? (rest lop)) (if (equal? pos (first lop)) '() (cons (first lop) '()))]                   ; Alessandra commenta tu cosa fa
    [else
     (if (equal? pos (first lop)) (rest lop) (cons (first lop) (delete-el pos (rest lop))))])) ; Alessandra commenta tu cosa fa

; compute-available-pos: List<Posn> List<Posn> -> List<Posn>
; compute available positions on the background
(define (compute-available-pos snake lop)
  (cond
    [(or (empty? lop) (empty? snake)) lop]
    [else
     (compute-available-pos
      (rest snake)
      (delete-el (first snake) lop))]))

; All the positions on the background
(define BACKGROUNDPOS (make-positions 1 1 1 (list (make-posn 13 13))))

;; FUNCTIONS
; compute-apple-position: Number Number List<Posn> -> Apple
; computes the apple position based on available positions
(define (compute-apple-position n acc lop)
  (cond
    [(= n acc) (first lop)]
    [(empty? lop) (make-posn 13 13)]
    [else
     (compute-apple-position n (+ acc 1) (rest lop))]))

; the first Snake
(define SNAKE1 (make-snake (list (make-posn 13 13) (make-posn 38 13) (make-posn 63 13)) 3 RIGHT '()))

; the first example of an Apple
(define APPLE1 (compute-apple-position (random 401) 1 (compute-available-pos (cons (make-posn 0 0)(snake-position SNAKE1)) BACKGROUNDPOS)))

; draw-snake: Snake -> Image
; draws the snake on the background
(define (draw-snake snake)
  (cond
    [(empty? (rest (snake-position snake)))
     (place-image
      SNAKEHEAD
      (posn-x (first (snake-position snake)))
      (posn-y (first (snake-position snake)))
      BACKGROUND)]
    [(equal? (length (snake-position snake)) (snake-length snake))
     (place-image
      TAIL
      (posn-x (first (snake-position snake)))
      (posn-y (first (snake-position snake)))
      (draw-snake
       (make-snake (rest (snake-position snake)) (snake-length snake) (snake-direction snake) (snake-breakpoint snake))))]
    [else
     (place-image
      SNAKEUNIT
      (posn-x (first (snake-position snake)))
      (posn-y (first (snake-position snake)))
      (draw-snake
       (make-snake (rest (snake-position snake)) (snake-length snake) (snake-direction snake) (snake-breakpoint snake))))
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
     (make-snake
      (snake-position snake)
      (snake-length snake)
      UP
      (cons (first (snake-position snake)) (snake-breakpoint snake)))]
    [(string=? direction DOWN)
     (make-snake
      (snake-position snake)
      (snake-length snake)
      DOWN
      (cons (first (snake-position snake)) (snake-breakpoint snake)))] 
    [(string=? direction LEFT)
     (make-snake
      (snake-position snake)
      (snake-length snake)
      LEFT
      (cons (first (snake-position snake)) (snake-breakpoint snake)))]
    [(string=? direction RIGHT)
     (make-snake
      (snake-position snake)
      (snake-length snake)
      RIGHT
      (cons (first (snake-position snake)) (snake-breakpoint snake)))]
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
    (move-snake (snake-direction (appstate-snake appstate)) (snake-position (appstate-snake appstate)))
    (snake-length (appstate-snake appstate))
    (snake-direction (appstate-snake appstate))
    (snake-breakpoint (appstate-snake appstate)))
   (appstate-apple appstate)
   (compute-available-pos
    (cons (appstate-apple appstate) (snake-position (appstate-snake appstate)))
    BACKGROUNDPOS)
   (appstate-game appstate)
   (appstate-quit appstate)))

; time-tick: AppState -> Number
; changes the speed of the snake based on snake length
(define (time-tick state) (/ 2.7 (snake-length (appstate-snake state))))

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
                      (appstate-freepositions state) ; the apple remain the same
                      (appstate-game state)  ; the game remain the same
                      #true))  

;;;;;;;;;; HANDLE KEYBOARD ;;;;;;;;;;
; handle-keyboard: AppState KeyboardEvent -> AppState
; handles the keyboard events
; Header (define (handle-keyboard state key) (make-appstate SNAKE1 APPLE1 (compute-available-pos SNAKE1 APPLE1 BACKGROUNDPOS) GAME-T QUIT-F))

; Examples
(check-expect (handle-keyboard (make-appstate SNAKE1 APPLE1 (compute-available-pos (cons APPLE1 (snake-position SNAKE1)) BACKGROUNDPOS) GAME-T QUIT-F) "")
              (make-appstate SNAKE1 APPLE1 (compute-available-pos (cons APPLE1 (snake-position SNAKE1)) BACKGROUNDPOS) GAME-T QUIT-F))

; Template
;(define (handle-keyboard state key)
;  (cond
;    [(not (string? key)) ... state ...]
;    [(or (and (string=? key "up") (string=? (snake-direction (appstate-snake state)) "down"))
;         (and (string=? key "right") (string=? (snake-direction (appstate-snake state)) "left"))
;         (and (string=? key "down") (string=? (snake-direction (appstate-snake state)) "up"))
;         (and (string=? key "left") (string=? (snake-direction (appstate-snake state)) "right"))) ... state ...]
;    [(or (string=? key "up") (string=? key "right") (string=? key "down") (string=? key "left")) ... state ...]
;    [(string=? key "r") ... state ...]
;    [(string=? key "escape") ... state ...]
;    [else ... state ...]))

; Code
(define (handle-keyboard state key)
  (cond
    [(not (string? key)) state]                                                                                        ; for any possible not-string key input, the output is the same AppState as before
    [(or (and (string=? key "up") (string=? (snake-direction (appstate-snake state)) "down"))                          ; if the direction is opposite of the key, the state is the same
         (and (string=? key "right") (string=? (snake-direction (appstate-snake state)) "left"))                       ; if the direction is opposite of the key, the state is the same
         (and (string=? key "down") (string=? (snake-direction (appstate-snake state)) "up"))                          ; if the direction is opposite of the key, the state is the same
         (and (string=? key "left") (string=? (snake-direction (appstate-snake state)) "right"))) state]               ; if the direction is opposite of the key, the state is the same
    [(or (string=? key "up") (string=? key "right") (string=? key "down") (string=? key "left"))                       ; if the input is one of 'up', 'right', 'down' or 'left'
     (make-appstate                                                                                                    ; create a new appstate where :
      (change-snake-direction key (appstate-snake state))                                                                ; the new snake is returned by the fuction to change the snake;'s direction
      (appstate-apple state)                                                                                             ; the apple's appstate is the same
      (compute-available-pos
       (cons (appstate-apple state) (snake-position (change-snake-direction key (appstate-snake state)))) BACKGROUNDPOS)   ; the possible positions' appstate is the same
      (appstate-game state)                                                                                              ; the game's appstate is the same
      (appstate-quit state))]                                                                                            ; the quit's appstate is the same
    [(string=? key "r") (reset state)]                                                                                   ; reset the game
    [(string=? key "escape") (quit state)]                                                                               ; quit the game
    [else state]))                                                                                                     ; for any other input the appstate is the same

;;;;;;;;;; END ;;;;;;;;;;
; end?: AppState -> Boolean
; stops the game
; Header (define (end? state) #true)

; Example
(check-expect (end? DEFAULT) #false)
(check-expect (end? (make-appstate SNAKE1 APPLE1 (compute-available-pos (cons APPLE1 (snake-position SNAKE1)) BACKGROUNDPOS) GAME-T QUIT-T)) #true)
;the left limit
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 13 13) (make-posn 38 13) (make-posn 63 13)) 3 LEFT '())
                     APPLE1
                     (compute-available-pos (cons APPLE1 (snake-position SNAKE1)) BACKGROUNDPOS)
                     GAME-T
                     QUIT-F))
              #false)
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn -12 13) (make-posn 13 13) (make-posn 38 13)) 3 LEFT '())
                     APPLE1
                     (compute-available-pos (cons APPLE1 (snake-position SNAKE1)) BACKGROUNDPOS)
                     GAME-T
                     QUIT-F))
              #true)
; the right limit
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 488 13) (make-posn 463 13) (make-posn 438 13)) 3 RIGHT '())
                     APPLE1
                     (compute-available-pos (cons APPLE1 (snake-position SNAKE1)) BACKGROUNDPOS)
                     GAME-T
                     QUIT-F))
              #false)
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 513 13) (make-posn 488 13) (make-posn 438 13)) 3 RIGHT '())
                     APPLE1
                     (compute-available-pos (cons APPLE1 (snake-position SNAKE1)) BACKGROUNDPOS)
                     GAME-T
                     QUIT-F))
              #true)
; the top limit
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 13 13) (make-posn 13 38) (make-posn 13 63)) 3 UP '())
                     APPLE1
                     (compute-available-pos (cons APPLE1 (snake-position SNAKE1)) BACKGROUNDPOS)
                     GAME-T
                     QUIT-F))
              #false)
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 13 -12) (make-posn 13 13) (make-posn 13 38)) 3 UP '())
                     APPLE1
                     (compute-available-pos (cons APPLE1 (snake-position SNAKE1)) BACKGROUNDPOS)
                     GAME-T
                     QUIT-F))
              #true)
; the bottom limit
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 13 488) (make-posn 13 463) (make-posn 13 438)) 3 DOWN '())
                     APPLE1
                     (compute-available-pos (cons APPLE1 (snake-position SNAKE1)) BACKGROUNDPOS)
                     GAME-T
                     QUIT-F))
              #false)
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 13 513) (make-posn 13 488) (make-posn 13 463)) 3 DOWN '())
                     APPLE1
                     (compute-available-pos (cons APPLE1 (snake-position SNAKE1)) BACKGROUNDPOS)
                     GAME-T
                     QUIT-F))
              #true)

; Template
;(define (end? state)
;  (cond
;    [(check-position-out (first (snake-position (appstate-snake state))) BACKGROUNDPOS) ... boolean ...]
;    [(boolean=? (appstate-quit state) #false) ... boolean ...]
;    [else ... boolean ...]))

; Code
(define (end? state)
  (cond
    [(check-position-out (first (snake-position (appstate-snake state))) BACKGROUNDPOS) #true] ; if the snake is over background's limits, the application turn off
    [(boolean=? (appstate-quit state) #false) #false]                                          ; the application remains on
    [else #true]))                                                                             ; for any other case the application turn off

;;;;;;;;;; MAIN APPLICATIONS ;;;;;;;;;;

; the default AppState
(define DEFAULT (make-appstate SNAKE1 APPLE1 (compute-available-pos (cons APPLE1 (snake-position SNAKE1)) BACKGROUNDPOS) GAME-T QUIT-F))

(define (snake-game appstate)
  (big-bang appstate
    [to-draw draw-appstate]                                      ; draw the snake
    [on-key handle-keyboard]                                     ; change snake's direction or reset game or quit the game
    [on-tick move 0.2]; (time-tick appstate)]                    ; uptade snake's position and "time" incrase each tick
    ;[display-mode 'fullscreen]
    [stop-when end?]                                             ; quit the application
    ))                          