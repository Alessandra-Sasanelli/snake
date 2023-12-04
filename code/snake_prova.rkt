;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname snake_prova) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; Convenzioni
;  - constants UPPERCASE
;  - anything else lowercase using kebab-case

;; Libraries

(require 2htdp/universe)
(require 2htdp/image)

;; Our external files
(require "external-files/snake.rkt")
(require "external-files/positions.rkt")





;;;;;;;;;;;;;;;;;;;; DATA TYPES ;;;;;;;;;;;;;;;;;;;;

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
; it represents the direction of a SnakeUnit

; a Game is one of:
; - #true    : the game is running
; - #false   : the game is lost and the application is on pause

; a Quit is one of:
; - #false   : the game is on and is running
; - #true    : the game is off and "close"

; a List<Posn> is one of:
;  - '()
;  - (cons Posn List<Posn>)
; represents a list of positions

; a Breakpoint is a Posn
; it represents the positions where the player changed direction to the Snake

; a List<Breakpoint> is one of:
; - '()                                  --> the empty list 
; - (cons Breakpoint List<Breakpoint>)   --> recursive case
; represents a list of breakpoints

; a Length is a Number as such:
; - the Number 3
; - Length + 1
; represents the length of the snake

; a Snake is a struct
; (make-snake position length direction)
; where:
; - position is a List<Posn>           --> represents the positions of the elements that make up the snake
; - length is a Length                 --> the length of the snake
; - direction is a Direction           --> the direction of the head
; - breakpoint is a List<Breakpoints>  --> list of the points where the snake's direction changed
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
(define-struct appstate [snake apple game quit])





;;;;;;;;;;;;;;;;;;;; CONSTANTS ;;;;;;;;;;;;;;;;;;;;
; app scene background
(define BACKGROUND (bitmap "../resources/background2.jpeg")) ;(empty-scene 501 501 'white))

; SnakeUnit
(define SNAKEUNIT (rectangle 24 24 "solid" 'green))

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
(define APPLEUNIT (bitmap "../resources/apple.png"))

; Directions
(define UP "up")
(define RIGHT "right")
(define DOWN "down")
(define LEFT "left")

; the game
(define GAME-T #true)
(define GAME-F #false)

; the quit
(define QUIT-T #true)
(define QUIT-F #false)

; All the positions on the background
(define BACKGROUNDPOS (make-positions 1 1 1 (list (make-posn 13 13))))

; the first Snake
(define SNAKE1 (make-snake (list (make-posn 13 13) (make-posn 38 13) (make-posn 63 13)) 3 RIGHT '()))

; the first example of an Apple
(define APPLE1 (compute-apple-position (random 401) 1 (compute-available-pos (cons (make-posn 0 0)(snake-position SNAKE1)) BACKGROUNDPOS)))





;;;;;;;;;;;;;;;;;;;; FUNCTIONS ;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;; LAST ;;;;;;;;;;
; last: List<Any> -> List<Any>
; returns the last element of a list
; Header (define (last lop) 1)

; Examples
(check-expect (last (list 1 2 3)) 3)
(check-expect (last (list 1)) 1)
(check-expect (last (list #false "ciao")) "ciao")
(check-expect (last (list 589 48 984 989 9892 -394 392 3 4259 02 0)) 0)
(check-expect (last (list #false #true "boolean" #true)) #true)

; Code
(define (last lop)
  (cond
    [(empty? (rest lop)) (first lop)] ; base case: when rest is empty, just return the element
    [else
     (last (rest lop))]))             ; recursive case: discard all other elements

;;;;;;;;;; ROTATE ELEMENT ;;;;;;;;;;
; rotate-el: Direction Image -> Image
; rotate the image so that whe the snake changes direction its head and tail are in the right direction
; Header (define (rotate-el d img) SNAKEHEAD)

; Examples
(check-expect (rotate-el UP SNAKEHEAD) (rotate 90 SNAKEHEAD))
(check-expect (rotate-el RIGHT SNAKEHEAD) (rotate 0 SNAKEHEAD))
(check-expect (rotate-el DOWN SNAKEHEAD) (rotate 270 SNAKEHEAD))
(check-expect (rotate-el LEFT SNAKEHEAD) (rotate 180 SNAKEHEAD))
(check-expect (rotate-el 0 TAIL) TAIL)

; Code
(define (rotate-el d img)
  (rotate
   (cond
     [(equal? d UP) 90]    ; rotate 90 counterclockwise
     [(equal? d RIGHT) 0]  ; rotate 0 counterclockwise
     [(equal? d DOWN) 270] ; rotate 270 counterclockwise
     [(equal? d LEFT) 180] ; rotate 180 counterclockwise
     [else 0])             ; do nothing
   img))


;;;;;;;;;; DRAW SNAKE ;;;;;;;;;;
; draw-snake: Snake -> Image
; draws the snake on the background
; Header (define (draw-snake snake) (place-image TAIL 413 113 (place-image SNAKEUNIT 388 113 (place-image SNAKEHEAD 363 113 BACKGROUND))))

; Examples
(check-expect (draw-snake SNAKE1) (place-image SNAKEHEAD 63 13 (place-image SNAKEUNIT 38 13 (place-image TAIL 13 13 BACKGROUND))))

(check-expect (draw-snake (make-snake (list (make-posn 363 113) (make-posn 388 113) (make-posn 413 113)) 3 RIGHT '()))
              (place-image TAIL 363 113 (place-image SNAKEUNIT 388 113 (place-image SNAKEHEAD 413 113 BACKGROUND))))

(check-expect (draw-snake (make-snake (list (make-posn 113 113) (make-posn 138 113) (make-posn 163 113) (make-posn 188 113)) 4 RIGHT '()))
              (place-image TAIL 113 113 (place-image SNAKEUNIT 163 113 (place-image SNAKEUNIT 138 113 (place-image SNAKEHEAD 188 113 BACKGROUND)))))

(check-expect (draw-snake (make-snake (list (make-posn 313 88) (make-posn 338 88) (make-posn 313 88) (make-posn 288 88) (make-posn 263 88)) 5 RIGHT '()))
              (place-image TAIL 313 88 (place-image SNAKEUNIT 338 88 (place-image SNAKEUNIT 313 88 (place-image SNAKEUNIT 288 88 (place-image SNAKEHEAD 263 88 BACKGROUND))))))

(check-expect (draw-snake (make-snake (list (make-posn 413 63) (make-posn 438 63) (make-posn 463 63)) 3 RIGHT '()))
              (place-image TAIL 413 63 (place-image SNAKEUNIT 438 63 (place-image SNAKEHEAD 463 63 BACKGROUND))))

; Code
(define (draw-snake snake)
  (cond
    [(empty? (rest (snake-position snake)))
     (place-image
      (rotate-el (snake-direction snake) SNAKEHEAD)
      (posn-x (first (snake-position snake)))
      (posn-y (first (snake-position snake)))
      BACKGROUND)]
    [(equal? (length (snake-position snake)) (snake-length snake))
     (place-image
      (rotate-el (direction-by-posn (first (snake-position snake)) (second (snake-position snake))) TAIL)
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
       (make-snake (rest (snake-position snake)) (snake-length snake) (snake-direction snake) (snake-breakpoint snake))))]))


;;;;;;;;;; DRAW APPSTATE ;;;;;;;;;;
; draw-appstate: AppState -> Image
; draws the appstate
; Header (define (draw-appstate state) )

; Examples
(check-expect (draw-appstate (make-appstate
                              (make-snake (list (make-posn 363 113) (make-posn 388 113) (make-posn 413 113)) 3 RIGHT '())
                              (compute-apple-position 1 1 (compute-available-pos (cons (make-posn 488 488) (snake-position SNAKE1)) BACKGROUNDPOS))
                              GAME-T
                              QUIT-F))
              (place-image APPLEUNIT 463 488 (place-image TAIL 363 113 (place-image SNAKEUNIT 388 113 (place-image SNAKEHEAD 413 113 BACKGROUND)))))

(check-expect (draw-appstate (make-appstate
                              (make-snake (list (make-posn 113 113) (make-posn 163 113) (make-posn 138 113) (make-posn 188 113)) 4 RIGHT '())
                              (compute-apple-position 100 1 (compute-available-pos (cons (make-posn 488 363) (snake-position SNAKE1)) BACKGROUNDPOS))
                              GAME-T
                              QUIT-F))
              (place-image APPLEUNIT 13 388 (place-image TAIL 113 113 (place-image SNAKEUNIT 163 113 (place-image SNAKEUNIT 138 113 (place-image SNAKEHEAD 188 113 BACKGROUND))))))

(check-expect (draw-appstate (make-appstate
                              (make-snake (list (make-posn 263 88) (make-posn 338 88) (make-posn 313 88) (make-posn 288 88) (make-posn 363 88)) 5 RIGHT '())
                              (compute-apple-position 250 1 (compute-available-pos (cons (make-posn 238 188) (snake-position SNAKE1)) BACKGROUNDPOS))
                              GAME-T
                              QUIT-F))
              (place-image APPLEUNIT 263 188 (place-image TAIL 263 88 (place-image SNAKEUNIT 338 88 (place-image SNAKEUNIT 313 88 (place-image SNAKEUNIT 288 88 (place-image SNAKEHEAD 363 88 BACKGROUND)))))))

(check-expect (draw-appstate (make-appstate
                              (make-snake (list (make-posn 413 63) (make-posn 438 63) (make-posn 463 63)) 3 RIGHT '())
                              (compute-apple-position 364 1 (compute-available-pos (cons (make-posn 388 38) (snake-position SNAKE1)) BACKGROUNDPOS))
                              GAME-T
                              QUIT-F))
              (place-image APPLEUNIT 413 38 (place-image TAIL 413 63 (place-image SNAKEUNIT 438 63 (place-image SNAKEHEAD 463 63 BACKGROUND)))))

(check-expect (draw-appstate (make-appstate
                              SNAKE1
                              (compute-apple-position 396 1 (compute-available-pos (cons (make-posn 88 13) (snake-position SNAKE1)) BACKGROUNDPOS))
                              GAME-T
                              QUIT-F))
              (place-image APPLEUNIT 113 13 (place-image TAIL 13 13 (place-image SNAKEUNIT 38 13 (place-image SNAKEHEAD 63 13 BACKGROUND)))))

; Code
(define (draw-appstate state)                          ; draw a image of the appstate at moment
  (place-image APPLEUNIT                               ; an Apple
               (posn-x (appstate-apple state))         ; x coordinate
               (posn-y (appstate-apple state))         ; y coordinate
               (draw-snake (appstate-snake state))))   ; a Snake call its own function to draw itself


;;;;;;;;;; CHANGE SNAKE DIRECTION ;;;;;;;;;;
; change-snake-direction: String Snake -> Snake
; changes snake's head direction
; Header (define (change-snake-direction direction snake) snake)

; Examples
(check-expect (change-snake-direction UP SNAKE1) (make-snake (snake-position SNAKE1)
                                                             (snake-length SNAKE1)
                                                             UP
                                                             (cut-breakpoints (snake-length SNAKE1) (first (snake-position SNAKE1)) (cons (last (snake-position SNAKE1)) (snake-breakpoint SNAKE1)))))
(check-expect (change-snake-direction RIGHT SNAKE1) (make-snake (snake-position SNAKE1)
                                                                (snake-length SNAKE1)
                                                                RIGHT
                                                                (cut-breakpoints (snake-length SNAKE1) (first (snake-position SNAKE1)) (cons (last (snake-position SNAKE1)) (snake-breakpoint SNAKE1)))))
(check-expect (change-snake-direction DOWN SNAKE1) (make-snake (snake-position SNAKE1)
                                                               (snake-length SNAKE1)
                                                               DOWN
                                                               (cut-breakpoints (snake-length SNAKE1) (first (snake-position SNAKE1)) (cons (last (snake-position SNAKE1)) (snake-breakpoint SNAKE1)))))
(check-expect (change-snake-direction LEFT (make-snake (list (make-posn 13 13) (make-posn 13 63) (make-posn 13 88)) 3 DOWN '()))
              (make-snake (list (make-posn 13 13) (make-posn 13 63) (make-posn 13 88))
                          3
                          LEFT
                          (cut-breakpoints 3 (first (list (make-posn 13 13) (make-posn 13 63) (make-posn 13 88))) (cons (last (list (make-posn 13 13) (make-posn 13 63) (make-posn 13 88))) '()))))
(check-expect (change-snake-direction " " SNAKE1) SNAKE1)

; Code 
(define (change-snake-direction direction snake)
  (cond                                                                                                                                     ; check in which direction the snake is going
    [(or (string=? direction UP)
         (string=? direction DOWN)
         (string=? direction RIGHT)
         (string=? direction LEFT))
     (make-snake                                                                                                                            ; dipend the snake's direction, make a snake where:
      (snake-position snake)                                                                                                                  ; the snake's position is the same
      (snake-length snake)                                                                                                                    ; the snake's length is the same
      direction                                                                                                                              ; the snake's direction changes according to the input
      (cut-breakpoints (snake-length snake) (first (snake-position snake)) (cons (last (snake-position snake)) (snake-breakpoint snake))))]   ; the snake's breakpoint is a list with the position of the head and the point at which the change of direction was made 
    [else snake]))                                                                                                                          ; for any other case returns the same appstate as before


;;;;;;;;;; MOVE SNAKE ;;;;;;;;;;
; move-snake: Direction List<Posn> -> List<Posn>
; Header (define (move-snake d lop) lop)

; Examples
(check-expect (move-snake (make-snake (snake-position (appstate-snake DEFAULT))
                                      (snake-length (appstate-snake DEFAULT))
                                      (snake-direction (appstate-snake DEFAULT))
                                      (snake-breakpoint (appstate-snake DEFAULT))))
              (make-snake (list (make-posn 38 13) (make-posn 63 13) (make-posn 88 13)) 3 RIGHT '()))

(check-expect (move-snake (make-snake (list (make-posn 13 38) (make-posn 13 63) (make-posn 13 88)) 3 DOWN '())) 
              (make-snake (list (make-posn 13 63) (make-posn 13 88) (make-posn 13 113)) 3 DOWN '()))              

(check-expect (move-snake (make-snake (list (make-posn 13 38) (make-posn 38 38) (make-posn 63  38)) 3 RIGHT '()))
              (make-snake (list (make-posn 38 38) (make-posn 63 38) (make-posn 88 38)) 3 RIGHT '()))

(check-expect (move-snake (make-snake (list (make-posn 13 88) (make-posn 13 63) (make-posn 13  38)) 3 UP '()))
              (make-snake (list (make-posn 13 63) (make-posn 13 38) (make-posn 13 13)) 3 UP '()))

(check-expect (move-snake (make-snake (list (make-posn 38 38) (make-posn 63 38) (make-posn 88 38)) 3 LEFT '()))
              (make-snake (list (make-posn 63 38) (make-posn 38 38) (make-posn 63 38)) 3 LEFT '()))

; Code
(define (move-snake snake)
  (make-snake
   (update-positions (snake-direction snake) (snake-position snake) (snake-breakpoint snake))
   (snake-length snake)
   (snake-direction snake)
   (cut-breakpoints (snake-length snake) (first (snake-position snake)) (snake-breakpoint snake))))


;;;;;;;;;; MOVE ;;;;;;;;;;
; move: AppState -> AppState
; moves the snake using an auxiliary function
; Header (define (move appstate)

; Examples
(check-expect (move DEFAULT) (make-appstate
                              (make-snake (list (make-posn 38 13) (make-posn 63 13) (make-posn 88 13)) 3 RIGHT '())
                              APPLE1
                              GAME-T
                              QUIT-F))

(check-expect (move (make-appstate
                     (make-snake (list (make-posn 13 38) (make-posn 13 63) (make-posn 13  88)) 3 DOWN '())
                     APPLE1
                     GAME-T
                     QUIT-F))
              (make-appstate
               (make-snake (list (make-posn 13 63) (make-posn 13 88) (make-posn 13 113)) 3 DOWN '())
               APPLE1
               GAME-T
               QUIT-F))

(check-expect (move (make-appstate
                     (make-snake (list (make-posn 13 38) (make-posn 38 38) (make-posn 63  38)) 3 RIGHT '())
                     APPLE1
                     GAME-T
                     QUIT-F))
              (make-appstate
               (make-snake (list (make-posn 38 38) (make-posn 63 38) (make-posn 88 38)) 3 RIGHT '())
               APPLE1
               GAME-T
               QUIT-F))

(check-expect (move (make-appstate
                     (make-snake (list (make-posn 13 88) (make-posn 13 63) (make-posn 13  38)) 3 UP '())
                     APPLE1
                     GAME-T
                     QUIT-F))
              (make-appstate
               (make-snake (list (make-posn 13 63) (make-posn 13 38) (make-posn 13 13)) 3 UP '())
               APPLE1
               GAME-T
               QUIT-F))

(check-expect (move (make-appstate
                     (make-snake (list (make-posn 88 38) (make-posn 63 38) (make-posn 38 38)) 3 LEFT '())
                     APPLE1
                     GAME-T
                     QUIT-F))
              (make-appstate
               (make-snake (list (make-posn 63 38) (make-posn 38 38) (make-posn 13 38)) 3 LEFT '())
               APPLE1
               GAME-T
               QUIT-F))

; Code
(define (move appstate) (make-appstate
                         (move-snake (appstate-snake appstate))
                         (appstate-apple appstate)
                         (appstate-game appstate)
                         (appstate-quit appstate)))


;;;;;;;;;; TICK ;;;;;;;;;;
; time-tick: AppState -> Number
; changes the speed of the snake based on snake length
; Header (define (time-tick state) 2.7)

; Examples
(check-expect (time-tick (make-appstate
                          (make-snake (list (make-posn 413 113) (make-posn 388 113) (make-posn 363 113)) 3 UP '())
                          APPLE1
                          GAME-T
                          QUIT-F)) 0.9)

(check-expect (time-tick (make-appstate
                          (make-snake (list (make-posn 188 113) (make-posn 163 113) (make-posn 138 113) (make-posn 113 113)) 4 UP '())
                          APPLE1
                          GAME-T
                          QUIT-F)) 0.675)

(check-expect (time-tick (make-appstate
                          (make-snake (list (make-posn 363 88) (make-posn 338 88) (make-posn 313 88) (make-posn 288 88) (make-posn 263 88)) 5 UP '())
                          APPLE1
                          GAME-T
                          QUIT-F)) 0.54)

(check-expect (time-tick (make-appstate
                          (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 UP '())
                          APPLE1
                          GAME-T
                          QUIT-F)) 0.9)

(check-expect (time-tick (make-appstate
                          SNAKE1
                          APPLE1
                          GAME-T
                          QUIT-F)) 0.9)

; Code
(define (time-tick state) (/ 2.7 (snake-length (appstate-snake state)))) ; longer the snake is, faster the game will be because the tick will decrease


;;;;;;;;;; RESET ;;;;;;;;;; 
; reset: AppState -> AppState
; changes the game in the appstate
; Header (define (reset state) DEFAULT

; Examples
(check-expect (reset DEFAULT) DEFAULT)

(check-expect (reset (make-appstate
                      (make-snake (list (make-posn 188 113) (make-posn 163 113) (make-posn 138 113) (make-posn 113 113)) 4 RIGHT '())
                      APPLE1
                      GAME-F
                      QUIT-F))
              (make-appstate
               (make-snake (list (make-posn 188 113) (make-posn 163 113) (make-posn 138 113) (make-posn 113 113)) 4 RIGHT '())
               APPLE1
               GAME-F
               QUIT-F))

(check-expect (reset (make-appstate
                      (make-snake (list (make-posn 413 113) (make-posn 388 113) (make-posn 363 113)) 3 RIGHT '())
                      APPLE1
                      GAME-T
                      QUIT-F)) DEFAULT)

(check-expect (reset (make-appstate
                      (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 RIGHT '())
                      APPLE1
                      GAME-F
                      QUIT-F))
              (make-appstate
               (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 RIGHT '())
               APPLE1
               GAME-F
               QUIT-F))

(check-expect (reset (make-appstate
                      (make-snake (list (make-posn 363 88) (make-posn 338 88) (make-posn 313 88) (make-posn 288 88) (make-posn 263 88)) 5 RIGHT '())
                      APPLE1
                      GAME-T
                      QUIT-F)) DEFAULT)

; Code
(define (reset state)
  (cond
    [(boolean=? (appstate-game state) #true) DEFAULT] ; the game continue to run
    [else state]))                                    ; the game is reset


;;;;;;;;;; QUIT ;;;;;;;;;;
; quit: AppState -> AppState
; changes the quit in the appstate
; Header (define (quit state) #true)

; Examples
(check-expect (quit DEFAULT) (make-appstate SNAKE1 APPLE1 GAME-T QUIT-T))

(check-expect (quit (make-appstate
                     (make-snake (list (make-posn 188 113) (make-posn 163 113) (make-posn 138 113) (make-posn 113 113)) 4 RIGHT '())
                     APPLE1
                     GAME-T
                     QUIT-F))
              (make-appstate
               (make-snake (list (make-posn 188 113) (make-posn 163 113) (make-posn 138 113) (make-posn 113 113)) 4 RIGHT '())
               APPLE1
               GAME-T
               QUIT-T))

(check-expect (quit (make-appstate
                     (make-snake (list (make-posn 413 113) (make-posn 388 113) (make-posn 363 113)) 3 RIGHT '())
                     APPLE1
                     GAME-T
                     QUIT-F))
              (make-appstate
               (make-snake (list (make-posn 413 113) (make-posn 388 113) (make-posn 363 113)) 3 RIGHT '())
               APPLE1
               GAME-T
               QUIT-T))

(check-expect (quit (make-appstate
                     (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 RIGHT '())
                     APPLE1
                     GAME-T
                     QUIT-F))
              (make-appstate
               (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 RIGHT '())
               APPLE1
               GAME-T
               QUIT-T))

(check-expect (quit (make-appstate
                     (make-snake (list (make-posn 363 88) (make-posn 338 88) (make-posn 313 88) (make-posn 288 88) (make-posn 263 88)) 5 RIGHT '())
                     APPLE1
                     GAME-T
                     QUIT-F))
              (make-appstate
               (make-snake (list (make-posn 363 88) (make-posn 338 88) (make-posn 313 88) (make-posn 288 88) (make-posn 263 88)) 5 RIGHT '())
               APPLE1
               GAME-T
               QUIT-T))

; Code
(define (quit state) (make-appstate
                      (appstate-snake state) ; the snake remain the same
                      (appstate-apple state) ; the apple remain the same
                      (appstate-game state)  ; the game remain the same
                      #true))                ; the quit change to quit the application


;;;;;;;;;; HANDLE KEYBOARD ;;;;;;;;;;
; handle-keyboard: AppState KeyboardEvent -> AppState
; handles the keyboard events
; Header (define (handle-keyboard state key) (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F))

; Examples
; not-string key input
(check-expect (handle-keyboard (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F) #false)
              (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F))
(check-expect (handle-keyboard (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F) #true)
              (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F))
(check-expect (handle-keyboard (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F) 2)
              (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F))
; opposite
(check-expect (handle-keyboard (make-appstate
                                (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 DOWN '())
                                APPLE1
                                GAME-T
                                QUIT-F) "up")
              (make-appstate
               (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 DOWN '())
               APPLE1
               GAME-T
               QUIT-F))
(check-expect (handle-keyboard (make-appstate
                                (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 LEFT '())
                                APPLE1
                                GAME-T
                                QUIT-F) "right")
              (make-appstate
               (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 LEFT '())
               APPLE1
               GAME-T
               QUIT-F))
(check-expect (handle-keyboard (make-appstate
                                (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 UP '())
                                APPLE1
                                GAME-T
                                QUIT-F) "down")
              (make-appstate
               (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 UP '())
               APPLE1
               GAME-T
               QUIT-F))
(check-expect (handle-keyboard (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F) "left")
              (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F))
; change direction
(check-expect (handle-keyboard (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F) "up")
              (make-appstate
               (make-snake (snake-position SNAKE1) (snake-length SNAKE1) UP (cons (make-posn 63 13) '()))
               APPLE1
               GAME-T
               QUIT-F))

(check-expect (handle-keyboard (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F) "right")
              (make-appstate
               (make-snake (snake-position SNAKE1) (snake-length SNAKE1) RIGHT (cons (make-posn 63 13) '()))
               APPLE1
               GAME-T
               QUIT-F))

(check-expect (handle-keyboard (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F) "down")
              (make-appstate
               (make-snake (snake-position SNAKE1) (snake-length SNAKE1) DOWN (cons (make-posn 63 13) '()))
               APPLE1
               GAME-T
               QUIT-F))

(check-expect (handle-keyboard (make-appstate
                                (make-snake (list (make-posn 13 13) (make-posn 13 63) (make-posn 13 88)) 3 DOWN '())
                                APPLE1
                                GAME-T
                                QUIT-F) "left")
              (make-appstate
               (make-snake (list (make-posn 13 13) (make-posn 13 63) (make-posn 13 88))(snake-length SNAKE1) LEFT (cons (make-posn 13 88) '()))
               APPLE1
               GAME-T
               QUIT-F))

; reset
(check-expect (handle-keyboard (make-appstate SNAKE1 APPLE1 GAME-F QUIT-F) "r")
              (make-appstate SNAKE1 APPLE1 GAME-F QUIT-F))
(check-expect (handle-keyboard (make-appstate
                                (make-snake (list (make-posn 363 88) (make-posn 338 88) (make-posn 313 88) (make-posn 288 88) (make-posn 263 88)) 5 UP '())
                                APPLE1
                                GAME-T
                                QUIT-F) "r")
              DEFAULT)
; quit
(check-expect (handle-keyboard (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F) "escape")
              (make-appstate SNAKE1 APPLE1 GAME-T QUIT-T))
(check-expect (handle-keyboard (make-appstate
                                (make-snake (list (make-posn 363 88) (make-posn 338 88) (make-posn 313 88) (make-posn 288 88) (make-posn 263 88)) 5 UP '())
                                APPLE1
                                GAME-T
                                QUIT-F)
                               "escape")
              (make-appstate
               (make-snake (list (make-posn 363 88) (make-posn 338 88) (make-posn 313 88) (make-posn 288 88) (make-posn 263 88)) 5 UP '())
               APPLE1
               GAME-T
               QUIT-T))
; any other input
(check-expect (handle-keyboard (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F) "")
              (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F))
(check-expect (handle-keyboard (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F) " ")
              (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F))
(check-expect (handle-keyboard (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F) "ciao")
              (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F))
(check-expect (handle-keyboard (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F) "a")
              (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F))
(check-expect (handle-keyboard (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F) "return")
              (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F))

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
      (appstate-game state)                                                                                              ; the game's appstate is the same
      (appstate-quit state))]                                                                                            ; the quit's appstate is the same

    [(string=? key "r") (reset state)]                                                                                   ; reset the game

    [(string=? key "escape") (quit state)]                                                                               ; quit the game

    [else state]))                                                                                                     ; for any other input the appstate is the same


;;;;;;;;;; CHECK SNAKE HIT ITSELF ;;;;;;;;;;
; check-eat-snake : AppState -> AppState
; check if the snake hit itself during the game
; Header (define (check-eat-snake state) #true)

; Examples
(check-expect (check-eat-snake DEFAULT) #false)
(check-expect (check-eat-snake (make-appstate
                                (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 438 38) (make-posn 463 38) (make-posn 463 63)) 5 DOWN '())
                                APPLE1
                                GAME-T
                                QUIT-F)) #true)
(check-expect (check-eat-snake (make-appstate
                                (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 438 38) (make-posn 463 38) (make-posn 463 13)) 5 UP '())
                                APPLE1
                                GAME-T
                                QUIT-F)) #false)
(check-expect (check-eat-snake (make-appstate
                                (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63) (make-posn 413 88) (make-posn 438 88) (make-posn 438 63)) 6 UP '())
                                APPLE1
                                GAME-T
                                QUIT-F)) #true)
(check-expect (check-eat-snake (make-appstate
                                (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63) (make-posn 388 63) (make-posn 363 63)) 5 LEFT '())
                                APPLE1
                                GAME-T
                                QUIT-F)) #false)

; Code
(define (check-eat-snake state)
  (cond
    [(empty? (rest (snake-position (appstate-snake state)))) #false]
    [(equal? (last (snake-position (appstate-snake state))) (first (snake-position (appstate-snake state)))) #true]
    [else (check-eat-snake
           (make-appstate
            (make-snake (rest (snake-position (appstate-snake state)))
                        (snake-length (appstate-snake state))
                        (snake-direction (appstate-snake state))
                        (snake-breakpoint (appstate-snake state)))
            (appstate-apple state)
            (appstate-game state)
            (appstate-quit state)))]))


;;;;;;;;;; END ;;;;;;;;;;
; end?: AppState -> Boolean
; stops the game
; Header (define (end? state) #true)

; Example
(check-expect (end? DEFAULT) #false)
(check-expect (end? (make-appstate SNAKE1 APPLE1 GAME-T QUIT-T)) #true)
;the left limit
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 13 13) (make-posn 38 13) (make-posn 63 13)) 3 LEFT '())
                     APPLE1
                     GAME-T
                     QUIT-F))
              #false)
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 38 13) (make-posn 13 13) (make-posn -12 13)) 3 LEFT '())
                     APPLE1
                     GAME-T
                     QUIT-F))
              #true)
; the right limit
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 488 13) (make-posn 463 13) (make-posn 438 13)) 3 RIGHT '())
                     APPLE1
                     GAME-T
                     QUIT-F))
              #false)
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 438 13) (make-posn 488 13) (make-posn 513 13)) 3 RIGHT '())
                     APPLE1
                     GAME-T
                     QUIT-F))
              #true)
; the top limit
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 13 13) (make-posn 13 38) (make-posn 13 63)) 3 UP '())
                     APPLE1
                     GAME-T
                     QUIT-F))
              #false)
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 13 38) (make-posn 13 13) (make-posn 13 -12)) 3 UP '())
                     APPLE1
                     GAME-T
                     QUIT-F))
              #true)
; the bottom limit
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 13 488) (make-posn 13 463) (make-posn 13 438)) 3 DOWN '())
                     APPLE1
                     GAME-T
                     QUIT-F))
              #false)
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 13 463) (make-posn 13 488) (make-posn 13 513)) 3 DOWN '())
                     APPLE1
                     GAME-T
                     QUIT-F))
              #true)
; snake hit itself
(check-expect (end? DEFAULT) #false)
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 438 38) (make-posn 463 38) (make-posn 463 63)) 5 DOWN '())
                     APPLE1
                     GAME-T
                     QUIT-F)) #true)
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 438 38) (make-posn 463 38) (make-posn 463 13)) 5 UP '())
                     APPLE1
                     GAME-T
                     QUIT-F)) #false)
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63) (make-posn 413 88) (make-posn 438 88) (make-posn 438 63)) 6 UP '())
                     APPLE1
                     GAME-T
                     QUIT-F)) #true)
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63) (make-posn 388 63) (make-posn 363 63)) 5 LEFT '())
                     APPLE1
                     GAME-T
                     QUIT-F)) #false)

; Code
(define (end? state)
  (cond
    [(check-position-out (last (snake-position (appstate-snake state))) BACKGROUNDPOS) #true] ; if the snake is over background's limits, the application turn off
    [(check-eat-snake state) #true]                                                           ; if the snake hits itself, the application turn off
    [(boolean=? (appstate-quit state) #false) #false]                                         ; the application remains on
    [else #true]))                                                                            ; for any other case the application turn off


;;;;;;;;;; MAIN APPLICATIONS ;;;;;;;;;;
; the default AppState
(define DEFAULT (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F))

(define (snake-game appstate)
  (big-bang appstate
    [to-draw draw-appstate]                                  ; draw the snake and apple on the background
    [on-key handle-keyboard]                                 ; change snake's direction or reset game or quit the game
    [on-tick move 0.2]; (time-tick appstate)]                ; uptade snake's position and "time" incrase each tick
    ;[display-mode 'fullscreen]                              ; the display automatically becomes full screen
    [stop-when end?]))                                       ; quit the application