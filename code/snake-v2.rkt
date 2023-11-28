;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname snake-v2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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
; - length is a Length          --> the length of the snake
; - direction is a Direction    --> the direction of the head
(define-struct snake [position length direction])

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

;; CONSTANTS
; app scene background
(define BACKGROUND (empty-scene 501 501 'white))

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
(define APPLEUNIT (rectangle 24 24 "solid" 'red))

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


;;;;;;;;;; MAKE POSITIONS ;;;;;;;;;;
; make-positions: Number Number Number List<Posn> -> List<Posn>
; computes all the positions on the background before the game starts
; Header (define (make-positions n x y lop) (make-posn 488 488))

; Examples
; the list is too long so we decide to do not any examples

; Template
;(define (make-positions n x y lop)
;  (cond
;    [(= n 400) ... lop ...]
;    [else
;     (cond
;       [(= (posn-x (first lop)) 488) ... (make-positions posn) ...]
;       [else ... (make-positions posn) ...])]))

; Code
(define (make-positions n x y lop)
  (cond
    [(= n 400) lop]                                                                         ; if n = 400 returns the list of all possible positions
    [else                                                                                   ; otherwise create a list of all possible posn
     (cond
       [(= (posn-x (first lop)) 488)                                                        ; if the x posn is equal to 488
        (make-positions                                                                     ; call the recursive where the inputs are:
         (+ n 1)                                                                              ; n increased by one
         1                                                                                    ; the new x is one
         (+ y 1)                                                                              ; y increased by one
         (cons (make-posn 13 (+ (* 12 (+ (* y 2) 1)) (+ y 1))) lop))]                         ; new lop with new posn
       [else
        (make-positions                                                                     ; otherwise call the recursive where the inputs are:
         (+ n 1)                                                                              ; n increased by one
         (+ x 1)                                                                              ; x increased by one
         y                                                                                    ; the y is the same
         (cons (make-posn (+ (* 12 (+ (* x 2) 1)) (+ x 1)) (posn-y (first lop))) lop))])]))   ; new lop wih new posn


;;;;;;;;;; COMPUTE AVAIBLE POSITIONS ;;;;;;;;;;
; compute-available-pos: Snake Apple List<Posn> -> List<Posn>
; compute available positions on the background
; Header (define (compute-avaible-pos snake apple lop) list)

; Examples
; the list is too long so we decide to do not any examples

; Template
;(define (compute-available-pos snake apple lop)
;  (cond
;    [(or (empty? (rest lop)) (empty? (rest (snake-position snake)))) ... lop ...]
;    [(equal? apple (first lop))
;     ... (compute-available-pos ... snake ... apple ... (rest lop) ...) ...]
;    [(equal? (first (snake-position snake)) (first lop))
;     ... (compute-available-pos ... snake ... apple ... (rest lop) ...)]
;    [else ... cons ...]))

; Code
(define (compute-available-pos snake apple lop)
  (cond
    [(or (empty? (rest lop)) (empty? (rest (snake-position snake)))) '()]       ; base case where the snake it finish
    [(equal? apple (first lop)) (compute-available-pos snake apple (rest lop))] ; if both apple's position and first posn are same call the recursive excludiing this posn as free
    [(equal? (first (snake-position snake)) (first lop))                        ; if both head of the snake's position and first posn are same,
     (compute-available-pos                                                     ; call the recursive excludiing this posn as free, where :
      (make-snake                                                                 ; the new snake is :
       (rest (snake-position snake))                                                ; the rest of the snake
       (snake-length snake)                                                         ; the snake's length is the same
       (snake-direction snake))                                                     ; the snake's direction is the same
      apple                                                                       ; the apple is the same and
      (rest lop))]                                                                ; give the rest of possible free posn
    [else
     (cons (first lop) (compute-available-pos snake apple (rest lop)))]))       ; the positions is free so add to a possible positions


;;;;;;;;;; ALL BACKGROUND POSITIONS ;;;;;;;;;;
; All the positions on the background
(define BACKGROUNDPOS (make-positions 1 1 1 (list (make-posn 13 13))))

;; FUNCTIONS

;;;;;;;;;; COMPUTE APPLE POSITIONS ;;;;;;;;;;
; compute-apple-position: Number Number List<Posn> -> Apple
; computes the apple position based on available positions
; Heade (define (compute-apple-position n acc lop) (make-posn 13 13))

; Examples
(check-expect (compute-apple-position 1 1 (compute-available-pos SNAKE1 (make-posn 0 0) BACKGROUNDPOS)) (make-posn 488 488))
(check-expect (compute-apple-position 100 1 (compute-available-pos SNAKE1 (make-posn 0 0) BACKGROUNDPOS)) (make-posn 13 388))
(check-expect (compute-apple-position 250 1 (compute-available-pos SNAKE1 (make-posn 0 0) BACKGROUNDPOS)) (make-posn 263 188))
(check-expect (compute-apple-position 364 1 (compute-available-pos SNAKE1 (make-posn 0 0) BACKGROUNDPOS)) (make-posn 413 38))
(check-expect (compute-apple-position 397 1 (compute-available-pos SNAKE1 (make-posn 0 0) BACKGROUNDPOS)) (make-posn 88 13))

; Template
;(define (compute-apple-position n acc lop)
;  (cond
;    [(= n acc) ... (first lop) ...]
;    [else ... (compute-apple-position ... (rest lop)) ...]))

; Code
(define (compute-apple-position n acc lop)
  (cond
    [(= n acc) (first lop)]                             ; case limit where n is equal to the accumulator and there are not other possible free positions
    [else
     (compute-apple-position n (+ acc 1) (rest lop))])) ; create a new possible position


;;;;;;;;;; DRAW SNAKE ;;;;;;;;;;
; draw-snake: Snake -> Image
; draws the snake on the background
; Header (define (draw-snake snake) (place-image TAIL x y (place-image SNAKEUNIT x y (place-image SNAKEHEAD x y BACKGROUND))

; Examples
(check-expect (draw-snake SNAKE1) (place-image TAIL 13 13 (place-image SNAKEUNIT 38 13 (place-image SNAKEHEAD 63 13 BACKGROUND))))

(check-expect (draw-snake (make-snake (list (make-posn 413 113) (make-posn 388 113) (make-posn 363 113)) 3 UP))
              (place-image TAIL 363 113 (place-image SNAKEUNIT 388 113 (place-image SNAKEHEAD 413 113 BACKGROUND))))

(check-expect (draw-snake (make-snake (list (make-posn 188 113) (make-posn 163 113) (make-posn 138 113) (make-posn 113 113)) 4 UP))
              (place-image TAIL 113 113 (place-image SNAKEUNIT 138 113 (place-image SNAKEUNIT 163 113 (place-image SNAKEHEAD 188 113 BACKGROUND)))))

(check-expect (draw-snake (make-snake (list (make-posn 363 88) (make-posn 338 88) (make-posn 313 88) (make-posn 288 88) (make-posn 263 88)) 5 UP))
              (place-image TAIL 263 88 (place-image SNAKEUNIT 288 88 (place-image SNAKEUNIT 313 88 (place-image SNAKEUNIT 338 88 (place-image SNAKEHEAD 363 88 BACKGROUND))))))

(check-expect (draw-snake (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 UP))
              (place-image TAIL 413 63 (place-image SNAKEUNIT 438 63 (place-image SNAKEHEAD 463 63 BACKGROUND))))

; Template
;(define (draw-snake snake)
;  (cond
;    [(empty? (rest (snake-position snake))) ... image ...]
;    [(equal? (length (snake-position snake)) (snake-length snake)) ... image ...]
;    [else ... image ...]))

; Code
(define (draw-snake snake)
  (cond
    [(empty? (rest (snake-position snake)))                                                         ; base case where if the rest is empty place the tail at the end of the snake
     (place-image
      TAIL
      (posn-x (first (snake-position snake)))
      (posn-y (first (snake-position snake)))
      BACKGROUND)]                                                                                  ; draw all the snake on the background
    [(equal? (length (snake-position snake)) (snake-length snake))                                  ; the first case where it places the snake's head at the beginning of the snake
     (place-image
      SNAKEHEAD
      (posn-x (first (snake-position snake)))
      (posn-y (first (snake-position snake)))
      (draw-snake                                                                                   ; after place the head it calls itself to continue the snake
       (make-snake (rest (snake-position snake)) (snake-length snake) (snake-direction snake))))]
    [else
     (place-image                                                                                   ; in all other case it places the snake unit between
      SNAKEUNIT
      (posn-x (first (snake-position snake)))
      (posn-y (first (snake-position snake)))
      (draw-snake                                                                                   ; and at the end it calls itself to continue the snake
       (make-snake (rest (snake-position snake)) (snake-length snake) (snake-direction snake))))]))


;;;;;;;;;; DRAW APPSTATE ;;;;;;;;;;
; draw-appstate: AppState -> Image
; draws the appstate
; Header (define (draw-appstate state) )

; Examples
(check-expect (draw-appstate (make-appstate
                              (make-snake (list (make-posn 413 113) (make-posn 388 113) (make-posn 363 113)) 3 UP)
                              (compute-apple-position 1 1 (compute-available-pos SNAKE1 (make-posn 0 0) BACKGROUNDPOS))
                              GAME-T
                              QUIT-F))
              (place-image APPLEUNIT 488 488 (place-image TAIL 363 113 (place-image SNAKEUNIT 388 113 (place-image SNAKEHEAD 413 113 BACKGROUND)))))

(check-expect (draw-appstate (make-appstate
                              (make-snake (list (make-posn 188 113) (make-posn 163 113) (make-posn 138 113) (make-posn 113 113)) 4 UP)
                              (compute-apple-position 100 1 (compute-available-pos SNAKE1 (make-posn 0 0) BACKGROUNDPOS))
                              GAME-T
                              QUIT-F))
              (place-image APPLEUNIT 13 388 (place-image TAIL 113 113 (place-image SNAKEUNIT 138 113 (place-image SNAKEUNIT 163 113 (place-image SNAKEHEAD 188 113 BACKGROUND))))))
              
(check-expect (draw-appstate (make-appstate
                              (make-snake (list (make-posn 363 88) (make-posn 338 88) (make-posn 313 88) (make-posn 288 88) (make-posn 263 88)) 5 UP)
                              (compute-apple-position 250 1 (compute-available-pos SNAKE1 (make-posn 0 0) BACKGROUNDPOS))
                              GAME-T
                              QUIT-F))
              (place-image APPLEUNIT 263 188 (place-image TAIL 263 88 (place-image SNAKEUNIT 288 88 (place-image SNAKEUNIT 313 88 (place-image SNAKEUNIT 338 88 (place-image SNAKEHEAD 363 88 BACKGROUND)))))))

(check-expect (draw-appstate (make-appstate
                              (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 UP)
                              (compute-apple-position 364 1 (compute-available-pos SNAKE1 (make-posn 0 0) BACKGROUNDPOS))
                              GAME-T
                              QUIT-F))
              (place-image APPLEUNIT 413 38 (place-image TAIL 413 63 (place-image SNAKEUNIT 438 63 (place-image SNAKEHEAD 463 63 BACKGROUND)))))

(check-expect (draw-appstate (make-appstate
                              SNAKE1
                              (compute-apple-position 397 1 (compute-available-pos SNAKE1 (make-posn 0 0) BACKGROUNDPOS))
                              GAME-T
                              QUIT-F))
              (place-image APPLEUNIT 88 13 (place-image TAIL 13 13 (place-image SNAKEUNIT 38 13 (place-image SNAKEHEAD 63 13 BACKGROUND)))))

; Templates
;(define (draw-appstate state) ... image ...)

; Code
(define (draw-appstate state)                        ; draw a image of :
  (place-image APPLEUNIT                             ; an Apple
               (posn-x (appstate-apple state))         ; x coordinate
               (posn-y (appstate-apple state))         ; y coordinate
               (draw-snake (appstate-snake state)))) ; a Snake call its own function to draw itself


;;;;;;;;;; INCREMENT POSN ;;;;;;;;;;
; increment-pos: Number -> Number
; increments the x or the y position of a posn
; Header (define (increments-pos xy) 38)

; Examples
(check-expect (increment-pos 13) 38)
(check-expect (increment-pos 138) 163)
(check-expect (increment-pos 563) 588)
(check-expect (increment-pos 613) 638)
(check-expect (increment-pos 188) 213)

; Template
; (define (increment-pos xy) ... number ...)

; Code
(define (increment-pos xy)
  (+ xy 25))


;;;;;;;;;; DECREMENT POSN ;;;;;;;;;;
; decrement-pos: Number -> Number
; decrements the x or the y position of a posn
; Header (define (decrements-pos xy) 38)

; Examples
(check-expect (decrement-pos 413) 388)
(check-expect (decrement-pos 88) 63)
(check-expect (decrement-pos 588) 563)
(check-expect (decrement-pos 763) 738)
(check-expect (decrement-pos 38) 13)

; Template
; (define (decrement-pos xy) ... number ...)

; Code
(define (decrement-pos xy)
  (- xy 25))


;;;;;;;;;; LAST ;;;;;;;;;;
; last: List<Posn> -> List<Posn>
; returns the last element of a list
; Header (define (last lop) 1)

; Examples
(check-expect (last (list 1 2 3)) 3)
(check-expect (last (list 1)) 1)
(check-expect (last (list #false "ciao")) "ciao")
(check-expect (last (list 589 48 984 989 9892 -394 392 3 4259 02 0)) 0)
(check-expect (last (list #false #true "boolean" #true)) #true)

; Template
;(define (last lop)
;  (cond
;    [(empty? (rest lop)) ... (first lop) ...]
;    [else ... (last (rest lop)) ...]))

; Code
(define (last lop)
  (cond
    [(empty? (rest lop)) (first lop)] ; base case where the list is composed by one element
    [else
     (last (rest lop))]))             ; recursive case to deleted all other element

;;;;;;;;;; UPDATE POSITIONS ;;;;;;;;;;
; update-positions: Direction List<Posn> -> List<Posn>
; updates a list of position to new list of positions 
(define (update-positions dir lop)
  (cond
    [(empty? (rest lop))
     (cond
       [(string=? dir UP) (cons (make-posn (posn-x (first lop)) (decrement-pos (posn-y (first lop)))) '())]
       [(string=? dir DOWN) (cons (make-posn (posn-x (first lop)) (increment-pos (posn-y (first lop)))) '())]
       [(string=? dir RIGHT) (cons (make-posn (increment-pos (posn-x (first lop))) (posn-y (first lop))) '())]
       [(string=? dir LEFT) (cons (make-posn (decrement-pos (posn-x (first lop))) (posn-y (first lop))) '())]
       )]
    [else
     (cond
       [(and (string=? dir UP) (= (posn-x (first lop)) (posn-x (last lop))))
        (cons (make-posn (posn-x (first lop)) (decrement-pos (posn-y (first lop)))) (update-positions dir (rest lop)))]
       [(and (string=? dir DOWN) (= (posn-x (first lop)) (posn-x (last lop))))
        (cons (make-posn (posn-x (first lop)) (increment-pos (posn-y (first lop)))) (update-positions dir (rest lop)))]
       [(and (string=? dir RIGHT) (= (posn-y (first lop)) (posn-y (last lop))))
        (cons (make-posn (increment-pos (posn-x (first lop))) (posn-y (first lop))) (update-positions dir (rest lop)))]
       [(and (string=? dir LEFT) (= (posn-y (first lop)) (posn-y (last lop))))
        (cons (make-posn (decrement-pos (posn-x (first lop))) (posn-y (first lop))) (update-positions dir (rest lop)))]
       [else (cons (first lop) (move-snake dir (rest lop)))]
       )]))


;;;;;;;;;; CHANGE SNAKE DIRECTION ;;;;;;;;;;
; change-snake-direction: String Snake -> Snake
; changes snake's head direction
(define (change-snake-direction direction snake)
  (cond
    [(string=? direction UP)
     (make-snake (update-positions direction (snake-position snake)) (snake-length snake) UP)]
    [(string=? direction DOWN)
     (make-snake (update-positions direction (snake-position snake)) (snake-length snake) DOWN)]
    [(string=? direction LEFT)
     (make-snake (update-positions direction (snake-position snake)) (snake-length snake) LEFT)]
    [(string=? direction RIGHT)
     (make-snake (update-positions direction (snake-position snake)) (snake-length snake) RIGHT)]
    [else snake]))


;;;;;;;;;; MOVE SNAKE ;;;;;;;;;;
; move-snake: Direction List<Posn> -> List<Posn>
(define (move-snake dir lop)
  (cond
    [(string=? dir UP)
     (if (empty? (rest lop))
         (cons (make-posn (posn-x (first lop)) (decrement-pos (posn-y (first lop)))) '())
         (cons (make-posn (posn-x (first lop)) (decrement-pos (posn-y (first lop)))) (move-snake dir (rest lop))))]
    [(string=? dir DOWN)
     (if (empty? (rest lop))
         (cons (make-posn (posn-x (first lop)) (increment-pos (posn-y (first lop)))) '())
         (cons (make-posn (posn-x (first lop)) (increment-pos (posn-y (first lop)))) (update-positions dir (rest lop))))]
    [(string=? dir LEFT)
     (if (empty? (rest lop))
         (cons (make-posn (decrement-pos (posn-x (first lop))) (posn-y (first lop))) '())
         (cons (make-posn (decrement-pos (posn-x (first lop))) (posn-y (first lop))) (update-positions dir (rest lop))))]
    [(string=? dir RIGHT)
     (if (empty? (rest lop))
         (cons (make-posn (increment-pos (posn-x (first lop))) (posn-y (first lop))) '())
         (cons (make-posn (increment-pos (posn-x (first lop))) (posn-y (first lop))) (update-positions dir (rest lop))))]))


;;;;;;;;;; MOVE ;;;;;;;;;;
; move: AppState -> AppState
(define (move state)
  (make-appstate
   (make-snake
    (move-snake (snake-direction (appstate-snake state)) (snake-position (appstate-snake state))) (snake-length (appstate-snake state)) (snake-direction (appstate-snake state)))
   (appstate-apple state)
   (appstate-game state)
   (appstate-quit state)))


;;;;;;;;;; TICK ;;;;;;;;;;
; time-tick: AppState -> Number
; changes the speed of the snake based on snake length
; Header (define (time-tick state) 2.7)

; Examples
(check-expect (time-tick (make-appstate
                              (make-snake (list (make-posn 413 113) (make-posn 388 113) (make-posn 363 113)) 3 UP)
                              APPLE1
                              GAME-T
                              QUIT-F)) 0.9)

(check-expect (time-tick (make-appstate
                              (make-snake (list (make-posn 188 113) (make-posn 163 113) (make-posn 138 113) (make-posn 113 113)) 4 UP)
                              APPLE1
                              GAME-T
                              QUIT-F)) 0.675)
              
(check-expect (time-tick (make-appstate
                              (make-snake (list (make-posn 363 88) (make-posn 338 88) (make-posn 313 88) (make-posn 288 88) (make-posn 263 88)) 5 UP)
                              APPLE1
                              GAME-T
                              QUIT-F)) 0.54)

(check-expect (time-tick (make-appstate
                              (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 UP)
                              APPLE1
                              GAME-T
                              QUIT-F)) 0.9)

(check-expect (time-tick (make-appstate
                              SNAKE1
                              APPLE1
                              GAME-T
                              QUIT-F)) 0.9)

; Template
; (define (time-tick state) ... number ...)

; Code
(define (time-tick state) (/ 2.7 (snake-length (appstate-snake state)))) ; longer the snake is, faster the game will be because the tick will decrease


;;;;;;;;;; RESET ;;;;;;;;;;
; reset: AppState -> AppState
; changes the game in the appstate
; Header (define (reset state) DEFAULT

; Examples
(check-expect (reset DEFAULT) DEFAULT)

(check-expect (reset (make-appstate
                              (make-snake (list (make-posn 188 113) (make-posn 163 113) (make-posn 138 113) (make-posn 113 113)) 4 UP)
                              APPLE1
                              GAME-F
                              QUIT-F))
              (make-appstate
               (make-snake (list (make-posn 188 113) (make-posn 163 113) (make-posn 138 113) (make-posn 113 113)) 4 UP)
               APPLE1
               GAME-F
               QUIT-F))

(check-expect (reset (make-appstate
                              (make-snake (list (make-posn 413 113) (make-posn 388 113) (make-posn 363 113)) 3 UP)
                              APPLE1
                              GAME-T
                              QUIT-F)) DEFAULT)

(check-expect (reset (make-appstate
                              (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 UP)
                              APPLE1
                              GAME-F
                              QUIT-F))
              (make-appstate
               (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 UP)
               APPLE1
               GAME-F
               QUIT-F))

(check-expect (reset (make-appstate
                              (make-snake (list (make-posn 363 88) (make-posn 338 88) (make-posn 313 88) (make-posn 288 88) (make-posn 263 88)) 5 UP)
                              APPLE1
                              GAME-T
                              QUIT-F)) DEFAULT)

; Template
;(define (reset state)
;  (cond
;    [(boolean=? (appstate-game state) #true) ... state ...]
;    [else ... state ...]))

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
                              (make-snake (list (make-posn 188 113) (make-posn 163 113) (make-posn 138 113) (make-posn 113 113)) 4 UP)
                              APPLE1
                              GAME-T
                              QUIT-F))
              (make-appstate
               (make-snake (list (make-posn 188 113) (make-posn 163 113) (make-posn 138 113) (make-posn 113 113)) 4 UP)
               APPLE1
               GAME-T
               QUIT-T))

(check-expect (quit (make-appstate
                              (make-snake (list (make-posn 413 113) (make-posn 388 113) (make-posn 363 113)) 3 UP)
                              APPLE1
                              GAME-T
                              QUIT-F))
              (make-appstate
               (make-snake (list (make-posn 413 113) (make-posn 388 113) (make-posn 363 113)) 3 UP)
               APPLE1
               GAME-T
               QUIT-T))

(check-expect (quit (make-appstate
                              (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 UP)
                              APPLE1
                              GAME-T
                              QUIT-F))
              (make-appstate
               (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 UP)
               APPLE1
               GAME-T
               QUIT-T))

(check-expect (quit (make-appstate
                              (make-snake (list (make-posn 363 88) (make-posn 338 88) (make-posn 313 88) (make-posn 288 88) (make-posn 263 88)) 5 UP)
                              APPLE1
                              GAME-T
                              QUIT-F))
              (make-appstate
               (make-snake (list (make-posn 363 88) (make-posn 338 88) (make-posn 313 88) (make-posn 288 88) (make-posn 263 88)) 5 UP)
               APPLE1
               GAME-T
               QUIT-T))

; Template
;(define (quit state) ... state ...)

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
                                (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 DOWN)
                                APPLE1
                                GAME-T
                                QUIT-F) "up")
              (make-appstate
               (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 DOWN)
               APPLE1
               GAME-T
               QUIT-F))
(check-expect (handle-keyboard (make-appstate
                                (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 LEFT)
                                APPLE1
                                GAME-T
                                QUIT-F) "right")
              (make-appstate
               (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 LEFT)
               APPLE1
               GAME-T
               QUIT-F))
(check-expect (handle-keyboard (make-appstate
                                (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 UP)
                                APPLE1
                                GAME-T
                                QUIT-F) "down")
              (make-appstate
               (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 UP)
               APPLE1
               GAME-T
               QUIT-F))
(check-expect (handle-keyboard (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F) "left")
              (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F))
; change direction

; reset
(check-expect (handle-keyboard (make-appstate SNAKE1 APPLE1 GAME-F QUIT-F) "r")
              (make-appstate SNAKE1 APPLE1 GAME-F QUIT-F))
(check-expect (handle-keyboard (make-appstate
                              (make-snake (list (make-posn 363 88) (make-posn 338 88) (make-posn 313 88) (make-posn 288 88) (make-posn 263 88)) 5 UP)
                              APPLE1
                              GAME-T
                              QUIT-F) "r")
              DEFAULT)
; quit
(check-expect (handle-keyboard (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F) "escape")
              (make-appstate SNAKE1 APPLE1 GAME-T QUIT-T))
(check-expect (handle-keyboard (make-appstate
                                (make-snake (list (make-posn 363 88) (make-posn 338 88) (make-posn 313 88) (make-posn 288 88) (make-posn 263 88)) 5 UP)
                                APPLE1
                                GAME-T
                                QUIT-F)
                               "escape")
              (make-appstate
               (make-snake (list (make-posn 363 88) (make-posn 338 88) (make-posn 313 88) (make-posn 288 88) (make-posn 263 88)) 5 UP)
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
;(check-expect (handle-keyboard (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F) "delete")
;              (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F))
;(check-expect (handle-keyboard (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F) "q")
;              (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F))

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
      (appstate-game state)                                                                                              ; the game's appstate is the same
      (appstate-quit state))]                                                                                            ; the quit's appstate is the same
    
    [(string=? key "r") (reset state)]                                                                                   ; reset the game
    
    [(string=? key "escape") (quit state)]                                                                               ; quit the game
    
    [else state]))                                                                                                     ; for any other input the appstate is the same

;;;;;;;;;; CHECK POSITION OUT ;;;;;;;;;;
; check-position-out: Posn List<Posn> -> Boolean
; checks wheather the given posn is into backgroundpos
; Header (define (check-position-out pos lop) #false

;Examples
(check-expect (check-position-out (make-posn 363 88) BACKGROUNDPOS) #false)
(check-expect (check-position-out (make-posn 463 63) BACKGROUNDPOS) #false)
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
    [(empty? (rest lop)) (if (equal? pos (first lop)) #false #true)]             ; Alessandra commenta tu cosa fa
    [else
     (if (equal? pos (first lop)) #false (check-position-out pos (rest lop)))])) ; Alessandra commenta tu cosa fa


;;;;;;;;;; END ;;;;;;;;;;
; end?: AppState -> Boolean
; stops the game
; Header (define (end? state) #true)

; Example
(check-expect (end? DEFAULT) #false)
(check-expect (end? (make-appstate SNAKE1 APPLE1 GAME-T QUIT-T)) #true)
;the left limit
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 13 13) (make-posn 38 13) (make-posn 63 13)) 3 LEFT)
                     APPLE1
                     GAME-T
                     QUIT-F))
              #false)
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn -12 13) (make-posn 13 13) (make-posn 38 13)) 3 LEFT)
                     APPLE1
                     GAME-T
                     QUIT-F))
              #true)
; the right limit
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 488 13) (make-posn 463 13) (make-posn 438 13)) 3 RIGHT)
                     APPLE1
                     GAME-T
                     QUIT-F))
              #false)
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 513 13) (make-posn 488 13) (make-posn 438 13)) 3 RIGHT)
                     APPLE1
                     GAME-T
                     QUIT-F))
              #true)
; the top limit
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 13 13) (make-posn 13 38) (make-posn 13 63)) 3 UP)
                     APPLE1
                     GAME-T
                     QUIT-F))
              #false)
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 13 -12) (make-posn 13 13) (make-posn 13 38)) 3 UP)
                     APPLE1
                     GAME-T
                     QUIT-F))
              #true)
; the bottom limit
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 13 488) (make-posn 13 463) (make-posn 13 438)) 3 DOWN)
                     APPLE1
                     GAME-T
                     QUIT-F))
              #false)
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 13 513) (make-posn 13 488) (make-posn 13 463)) 3 DOWN)
                     APPLE1
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
; the first Snake
(define SNAKE1 (make-snake (list (make-posn 63 13) (make-posn 38 13) (make-posn 13 13)) 3 RIGHT))

; the first example of an Apple
(define APPLE1 (compute-apple-position (random 401) 1 (compute-available-pos SNAKE1 (make-posn 0 0) BACKGROUNDPOS)))

; the default AppState
(define DEFAULT (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F))

(define (snake-game appstate)
  (big-bang appstate
    [to-draw draw-appstate]                                  ; draw the snake and apple on the background
    [on-key handle-keyboard]                                 ; change snake's direction or reset game or quit the game
    [on-tick move 0.2]; (time-tick appstate)]                ; uptade snake's position and "time" incrase each tick
    ;[display-mode 'fullscreen]                              ; the display automatically becomes full screen
    [stop-when end?]))                                       ; quit the application                          