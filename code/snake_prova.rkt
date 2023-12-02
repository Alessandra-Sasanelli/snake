;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname snake_prova) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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
; - breakpoint is a List<Posn>  --> list of the point where the snake's direction changed
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

;; CONSTANTS
; game scene background
;(define BACKGROUND (bitmap "background.jpg")) ;(empty-scene 501 501 'white))
(define BACKGROUND (bitmap "../resources/background2.jpeg")) ;(empty-scene 501 501 'white))

;app scene background
;(define CANVAS (bitmap "canvas.png"));(rectangle 1500 800 'solid 'red))
;(define CANVAS (bitmap "canvas2.png"));(rectangle 1500 800 'solid 'red))
(define CANVAS (rectangle 1500 800 'solid 'red))

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

; Code
(define (check-position-out pos lop)
  (cond
    [(empty? (rest lop))
     (cond
      [(equal? pos (first lop)) #false]          ; check if pos is equal to the first element of the list
      [else #true])]
    [(equal? pos (first lop)) #false]            ; if the pos is eqaul to the first element of the lop return false
    [else (check-position-out pos (rest lop))])) ; owhtewise, go ahead


;;;;;;;;;; DELETE ELEMENT ;;;;;;;;;;
; delete-el : Posn List<Posn> -> List<Posn>
; delete all the posn which are occupated by something else
; Header (define (delete-el pos lop) '())

; Examples
(check-expect (delete-el (make-posn 0 0) (list (make-posn 0 0))) '())
(check-expect (delete-el (make-posn 1 2) (list (make-posn 1 2) (make-posn 0 2) (make-posn 20 10))) (list (make-posn 0 2) (make-posn 20 10)))
(check-expect (delete-el (make-posn 38 413) (list (make-posn 63 13) (make-posn 313 38) (make-posn 88 63))) (list (make-posn 63 13) (make-posn 313 38) (make-posn 88 63)))
(check-expect (delete-el (make-posn 288 463) (list (make-posn 38 363) (make-posn 88 238) (make-posn 288 463))) (list (make-posn 38 363) (make-posn 88 238)))
(check-expect (delete-el (make-posn 288 463) (list (make-posn 113 13) (make-posn 288 463))) (list (make-posn 113 13)))

; Code
(define (delete-el pos lop)
  (cond
    [(empty? (rest lop)) (if (equal? pos (first lop)) '() (cons (first lop) '()))] ; ALESSANDRA COMMENTA TU COSA FARE
    [(equal? pos (first lop)) (rest lop)]                                          ; ALESSANDRA COMMENTA TU COSA FARE
    [else (cons (first lop) (delete-el pos (rest lop)))]))                         ; ALESSANDRA COMMENTA TU COSA FARE

;;;;;;;;;; COMPUTE AVAILABLE POSITION ;;;;;;;;;;
; compute-available-pos: List<Posn> List<Posn> -> List<Posn>
; compute available positions on the background
; Header (define (compute-available-pos snake lop) lop)

; Examples
; the list is too long so we decide to do not any examples

; Code
(define (compute-available-pos snake lop)
  (cond
    [(or (empty? lop) (empty? snake)) lop] ; base case where the all the available positions are checked
    [else
     (compute-available-pos
      (rest snake)
      (delete-el (first snake) lop))]))    ; recursive case to call a function to delete the occupied position


;;;;;;;;;; ALL BACKGROUND POSITIONS ;;;;;;;;;;
; All the positions on the background
(define BACKGROUNDPOS (make-positions 1 1 1 (list (make-posn 13 13))))

;; FUNCTIONS

;;;;;;;;;; COMPUTE APPLE POSITIONS ;;;;;;;;;;
; compute-apple-position: Number Number List<Posn> -> Apple
; computes the apple position based on available positions
; Heade (define (compute-apple-position n acc lop) (make-posn 13 13))

; Examples
(check-expect (compute-apple-position 1 1 (compute-available-pos (cons (make-posn 488 488) (snake-position SNAKE1)) BACKGROUNDPOS)) (make-posn 463 488))
(check-expect (compute-apple-position 100 1 (compute-available-pos (cons (make-posn 488 363) (snake-position SNAKE1)) BACKGROUNDPOS)) (make-posn 13 388))
(check-expect (compute-apple-position 250 1 (compute-available-pos (cons (make-posn 238 188) (snake-position SNAKE1)) BACKGROUNDPOS)) (make-posn 263 188))
(check-expect (compute-apple-position 364 1 (compute-available-pos (cons (make-posn 388 38) (snake-position SNAKE1)) BACKGROUNDPOS)) (make-posn 413 38))
(check-expect (compute-apple-position 396 1 (compute-available-pos (cons (make-posn 88 13) (snake-position SNAKE1)) BACKGROUNDPOS)) (make-posn 113 13))

; Code
(define (compute-apple-position n acc lop)
  (cond
    [(= n acc) (first lop)]                             ; case limit where n is equal to the accumulator and there are not other possible free positions
    [else
     (compute-apple-position n (+ acc 1) (rest lop))])) ; create an apple's position


; the first Snake
(define SNAKE1 (make-snake (list (make-posn 13 13) (make-posn 38 13) (make-posn 63 13)) 3 RIGHT '()))

; the first example of an Apple
(define APPLE1 (compute-apple-position (random 401) 1 (compute-available-pos (cons (make-posn 0 0)(snake-position SNAKE1)) BACKGROUNDPOS)))

;;;;;;;;;; DRAW SNAKE ;;;;;;;;;;
; draw-snake: Snake -> Image
; draws the snake on the background
; Header (define (draw-snake snake) (place-image TAIL 413 113 (place-image SNAKEUNIT 388 113 (place-image SNAKEHEAD 363 113 BACKGROUND))))

; Examples
(check-expect (draw-snake SNAKE1) (place-image SNAKEHEAD 63 13 (place-image SNAKEUNIT 38 13 (place-image TAIL 13 13 BACKGROUND))))

(check-expect (draw-snake (make-snake (list (make-posn 413 113) (make-posn 388 113) (make-posn 363 113)) 3 RIGHT '()))
              (place-image TAIL 413 113 (place-image SNAKEUNIT 388 113 (place-image SNAKEHEAD 363 113 BACKGROUND))))

(check-expect (draw-snake (make-snake (list (make-posn 188 113) (make-posn 163 113) (make-posn 138 113) (make-posn 113 113)) 4 RIGHT '()))
              (place-image TAIL 188 113 (place-image SNAKEUNIT 163 113 (place-image SNAKEUNIT 138 113 (place-image SNAKEHEAD 113 113 BACKGROUND)))))

(check-expect (draw-snake (make-snake (list (make-posn 363 88) (make-posn 338 88) (make-posn 313 88) (make-posn 288 88) (make-posn 263 88)) 5 RIGHT '()))
              (place-image TAIL 363 88 (place-image SNAKEUNIT 338 88 (place-image SNAKEUNIT 313 88 (place-image SNAKEUNIT 288 88 (place-image SNAKEHEAD 263 88 BACKGROUND))))))

(check-expect (draw-snake (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 RIGHT '()))
              (place-image TAIL 463 63 (place-image SNAKEUNIT 438 63 (place-image SNAKEHEAD 413 63 BACKGROUND))))

; Code
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
       (make-snake (rest (snake-position snake)) (snake-length snake) (snake-direction snake) (snake-breakpoint snake))))]))


;;;;;;;;;; DRAW APPSTATE ;;;;;;;;;;
; draw-appstate: AppState -> Image
; draws the appstate
; Header (define (draw-appstate state) )

; Examples
(check-expect (draw-appstate (make-appstate
                              (make-snake (list (make-posn 413 113) (make-posn 388 113) (make-posn 363 113)) 3 RIGHT '())
                              (compute-apple-position 1 1 (compute-available-pos (cons (make-posn 488 488) (snake-position SNAKE1)) BACKGROUNDPOS))
                              GAME-T
                              QUIT-F))
              (place-image APPLEUNIT 463 488 (place-image TAIL 413 113 (place-image SNAKEUNIT 388 113 (place-image SNAKEHEAD 363 113 BACKGROUND)))))

(check-expect (draw-appstate (make-appstate
                              (make-snake (list (make-posn 188 113) (make-posn 163 113) (make-posn 138 113) (make-posn 113 113)) 4 RIGHT '())
                              (compute-apple-position 100 1 (compute-available-pos (cons (make-posn 488 363) (snake-position SNAKE1)) BACKGROUNDPOS))
                              GAME-T
                              QUIT-F))
              (place-image APPLEUNIT 13 388 (place-image TAIL 188 113 (place-image SNAKEUNIT 163 113 (place-image SNAKEUNIT 138 113 (place-image SNAKEHEAD 113 113 BACKGROUND))))))
              
(check-expect (draw-appstate (make-appstate
                              (make-snake (list (make-posn 363 88) (make-posn 338 88) (make-posn 313 88) (make-posn 288 88) (make-posn 263 88)) 5 RIGHT '())
                              (compute-apple-position 250 1 (compute-available-pos (cons (make-posn 238 188) (snake-position SNAKE1)) BACKGROUNDPOS))
                              GAME-T
                              QUIT-F))
              (place-image APPLEUNIT 263 188 (place-image TAIL 363 88 (place-image SNAKEUNIT 338 88 (place-image SNAKEUNIT 313 88 (place-image SNAKEUNIT 288 88 (place-image SNAKEHEAD 263 88 BACKGROUND)))))))

(check-expect (draw-appstate (make-appstate
                              (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 RIGHT '())
                              (compute-apple-position 364 1 (compute-available-pos (cons (make-posn 388 38) (snake-position SNAKE1)) BACKGROUNDPOS))
                              GAME-T
                              QUIT-F))
              (place-image APPLEUNIT 413 38 (place-image TAIL 463 63 (place-image SNAKEUNIT 438 63 (place-image SNAKEHEAD 413 63 BACKGROUND)))))

(check-expect (draw-appstate (make-appstate
                              SNAKE1
                              (compute-apple-position 396 1 (compute-available-pos (cons (make-posn 88 13) (snake-position SNAKE1)) BACKGROUNDPOS))
                              GAME-T
                              QUIT-F))
              (place-image APPLEUNIT 113 13 (place-image TAIL 13 13 (place-image SNAKEUNIT 38 13 (place-image SNAKEHEAD 63 13 BACKGROUND)))))

; Code
(define (draw-appstate state)                        ; draw a image of :
  (place-image APPLEUNIT                             ; an Apple
               (posn-x (appstate-apple state))         ; x coordinate
               (posn-y (appstate-apple state))         ; y coordinate
               (draw-snake (appstate-snake state)))) ; a Snake call its own function to draw itself


;;;;;;;;;; DRAW GAMES ;;;;;;;;;;
; draw-game : AppState -> Image
; draw the entire appstate into a general background
; Header (define (draw-snake state) background)

; Examples


; Code
(define (draw-game state)
  (overlay/align 'center 'center (draw-appstate state)
                 (overlay/align/offset 'center 'center (bitmap "../resources/frame.png") 10 5 CANVAS)))



;;;;;;;;;; EATING ;;;;;;;;;;
; eating : AppState -> AppState
; if the apple is eaten by snake, add 1 length and change he apple's position
; Header (define (eating state) (make-appstate (make-snake (list (make-posn 13 13) (make-posn 38 13) (make-posn 63 13) (make-posn 88 13)) 4 RIGHT '()) (make-posn 13 13) GAME-T QUIT-F))

; Examples

; Code
(define (eating state)
  (cond
    [(and (equal? (posn-x (first (snake-position (appstate-snake state)))) (posn-x (appstate-apple state)))
          (equal? (posn-y (first (snake-position (appstate-snake state)))) (posn-y (appstate-apple state))))
     (make-appstate
      (draw-snake
       (make-snake
        (cons (snake-position (appstate-snake state)) (make-posn (appstate-apple state)))
        (add1 (snake-length (appstate-snake state)))
        (snake-position (appstate-snake state))
        (snake-breakpoint (appstate-snake state))))
       (compute-apple-position state)
       (appstate-game state)
       (appstate-quit state))]
    [else state]))


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

; Code
(define (last lop)
  (cond
    [(empty? (rest lop)) (first lop)] ; base case where the list is composed by one element
    [else
     (last (rest lop))]))             ; recursive case to deleted all other element


;;;;;;;;;; CHANGE SNAKE DIRECTION ;;;;;;;;;;
; change-snake-direction: String Snake -> Snake
; changes snake's head direction
; Header (define (change-snake-direction direction snake) snake)

; Examples
(check-expect (change-snake-direction "up" SNAKE1) (make-snake (snake-position SNAKE1)
                                                               (snake-length SNAKE1)
                                                               UP
                                                               (cons (make-posn 13 13) '())))
(check-expect (change-snake-direction "right" SNAKE1) (make-snake (snake-position SNAKE1)
                                                               (snake-length SNAKE1)
                                                               RIGHT
                                                               (cons (make-posn 13 13) '())))
(check-expect (change-snake-direction "down" SNAKE1) (make-snake (snake-position SNAKE1)
                                                               (snake-length SNAKE1)
                                                               DOWN
                                                               (cons (make-posn 13 13) '())))
(check-expect (change-snake-direction "left" (make-snake (list (make-posn 13 13) (make-posn 13 63) (make-posn 13 88)) 3 DOWN '()))
              (make-snake (list (make-posn 13 13) (make-posn 13 63) (make-posn 13 88))
                          (snake-length SNAKE1)
                          LEFT
                          (cons (make-posn 13 13) '())))
(check-expect (change-snake-direction " " SNAKE1) SNAKE1)

; Code
(define (change-snake-direction direction snake)
  (cond
    [(string=? direction UP) (make-snake                                                          ; if the direction is up, make a snake where:
                              (snake-position snake)                                                ; the snake's position is the same
                              (snake-length snake)                                                  ; the snake's length is the same
                              UP                                                                    ; the snake's direction changes to up
                              (cons (first (snake-position snake)) (snake-breakpoint snake)))]      ; the snake's breakpoint is a list with the position of the head and the point at which the change of direction was made
    
    [(string=? direction RIGHT) (make-snake                                                       ; if the direction is up, make a snake where:
                                 (snake-position snake)                                             ; the snake's position is the same
                                 (snake-length snake)                                               ; the snake's length is the same
                                 RIGHT                                                              ; the snake's direction changes to right
                                 (cons (first (snake-position snake)) (snake-breakpoint snake)))]   ; the snake's breakpoint is a list with the position of the head and the point at which the change of direction was made
    
    [(string=? direction DOWN) (make-snake                                                        ; if the direction is up, make a snake where:
                                (snake-position snake)                                              ; the snake's position is the same
                                (snake-length snake)                                                ; the snake's length is the same
                                DOWN                                                                ; the snake's direction changes to down
                                (cons (first (snake-position snake)) (snake-breakpoint snake)))]    ; the snake's breakpoint is a list with the position of the head and the point at which the change of direction was made
    
    [(string=? direction LEFT) (make-snake                                                        ; if the direction is up, make a snake where:
                                (snake-position snake)                                              ; the snake's position is the same
                                (snake-length snake)                                                ; the snake's length is the same
                                LEFT                                                                ; the snake's direction changes to left
                                (cons (first (snake-position snake)) (snake-breakpoint snake)))]    ; the snake's breakpoint is a list with the position of the head and the point at which the change of direction was made
    
    [else snake]))                                                                                ; for any other case returns the same appstate as before

;;;;;;;;;; MOVE SNAKE ;;;;;;;;;;
; move-snake: Direction List<Posn> -> List<Posn>
; Header (define (move-snake d lop) lop)

; Examples
(check-expect (move-snake RIGHT (snake-position (appstate-snake DEFAULT))) (list (make-posn 38 13) (make-posn 63 13) (make-posn 88 13)))

(check-expect (move-snake UP (list (make-posn 13 38) (make-posn 13 63) (make-posn 13  88)))
              (list (make-posn 13 13) (make-posn 13 38) (make-posn 13 63)))

(check-expect (move-snake RIGHT (list (make-posn 13 38) (make-posn 38 38) (make-posn 63  38)))
              (list (make-posn 38 38) (make-posn 63 38) (make-posn 88 38)))

(check-expect (move-snake DOWN (list (make-posn 13 38) (make-posn 13 63) (make-posn 13  88)))
              (list (make-posn 13 63) (make-posn 13 88) (make-posn 13 113)))

(check-expect (move-snake LEFT (list (make-posn 38 38) (make-posn 63 38) (make-posn 88  38)))
              (list (make-posn 13 38) (make-posn 38 38) (make-posn 63 38)))

; Code
(define (move-snake d lop)
  (cond
    [(string=? d UP)
     (cond
       [(empty? (rest lop)) (cons (make-posn (posn-x (first lop)) (decrement-pos (posn-y (first lop)))) '())]
       [else (cons (make-posn
                    (posn-x (first lop))
                    (decrement-pos (posn-y (first lop))))
                   (move-snake d (rest lop)))])]
    
    [(string=? d RIGHT)
     (cond
       [(empty? (rest lop)) (cons (make-posn (increment-pos (posn-x (first lop))) (posn-y (first lop))) '())]
       [else (cons (make-posn
                    (increment-pos (posn-x (first lop)))
                    (posn-y (first lop)))
                   (move-snake d (rest lop)))])]
    
    [(string=? d DOWN)
     (cond
       [(empty? (rest lop)) (cons (make-posn (posn-x (first lop)) (increment-pos (posn-y (first lop)))) '())]
       [else (cons (make-posn
                    (posn-x (first lop))
                    (increment-pos (posn-y (first lop))))
                    (move-snake d (rest lop)))])]
    
    [(string=? d LEFT)
     (cond
       [(empty? (rest lop)) (cons (make-posn (decrement-pos (posn-x (first lop))) (posn-y (first lop))) '())]
       [ else (cons (make-posn
                     (decrement-pos (posn-x (first lop)))
                     (posn-y (first lop)))
                    (move-snake d (rest lop)))])]))


;;;;;;;;;; MOVE ;;;;;;;;;;
; move: AppState -> AppState
; moves the snake using an auxiliary function
; Header (define (move appstate)

; Examples
(check-expect (move DEFAULT) (make-appstate
                              (make-snake (list (make-posn 38 13) (make-posn 63 13) (make-posn 88 13)) 3 "right" '())
                              APPLE1
                              GAME-T
                              QUIT-F))

(check-expect (move (make-appstate
                     (make-snake (list (make-posn 13 38) (make-posn 13 63) (make-posn 13  88)) 3 UP '())
                     APPLE1
                     GAME-T
                     QUIT-F))
              (make-appstate
               (make-snake (list (make-posn 13 13) (make-posn 13 38) (make-posn 13 63)) 3 UP '())
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
                     (make-snake (list (make-posn 38 38) (make-posn 63 38) (make-posn 88  38)) 3 LEFT '())
                     APPLE1
                     GAME-T
                     QUIT-F))
              (make-appstate
               (make-snake (list (make-posn 13 38) (make-posn 38 38) (make-posn 63 38)) 3 LEFT '())
               APPLE1
               GAME-T
               QUIT-F))

; Code
(define (move appstate) (make-appstate
                         (make-snake
                          (move-snake (snake-direction (appstate-snake appstate)) (snake-position (appstate-snake appstate)))
                          (snake-length (appstate-snake appstate))
                          (snake-direction (appstate-snake appstate))
                          (snake-breakpoint (appstate-snake appstate)))
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
               (make-snake (snake-position SNAKE1) (snake-length SNAKE1) UP (cons (make-posn 13 13) '()))
               APPLE1
               GAME-T
               QUIT-F))

(check-expect (handle-keyboard (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F) "right")
              (make-appstate
               (make-snake (snake-position SNAKE1) (snake-length SNAKE1) RIGHT (cons (make-posn 13 13) '()))
               APPLE1
               GAME-T
               QUIT-F))

(check-expect (handle-keyboard (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F) "down")
              (make-appstate
               (make-snake (snake-position SNAKE1) (snake-length SNAKE1) DOWN (cons (make-posn 13 13) '()))
               APPLE1
               GAME-T
               QUIT-F))

(check-expect (handle-keyboard (make-appstate
                                (make-snake (list (make-posn 13 13) (make-posn 13 63) (make-posn 13 88)) 3 DOWN '())
                                APPLE1
                                GAME-T
                                QUIT-F) "left")
              (make-appstate
                (make-snake (list (make-posn 13 13) (make-posn 13 63) (make-posn 13 88))(snake-length SNAKE1) LEFT (cons (make-posn 13 13) '()))
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
                     (make-snake (list (make-posn -12 13) (make-posn 13 13) (make-posn 38 13)) 3 LEFT '())
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
                     (make-snake (list (make-posn 513 13) (make-posn 488 13) (make-posn 438 13)) 3 RIGHT '())
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
                     (make-snake (list (make-posn 13 -12) (make-posn 13 13) (make-posn 13 38)) 3 UP '())
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
                     (make-snake (list (make-posn 13 513) (make-posn 13 488) (make-posn 13 463)) 3 DOWN '())
                     APPLE1
                     GAME-T
                     QUIT-F))
              #true)

; Code
(define (end? state)
  (cond
    [(check-position-out (first (snake-position (appstate-snake state))) BACKGROUNDPOS) #true] ; if the snake is over background's limits, the application turn off
    [(boolean=? (appstate-quit state) #false) #false]                                          ; the application remains on
    [else #true]))                                                                             ; for any other case the application turn off


;;;;;;;;;; MAIN APPLICATIONS ;;;;;;;;;;

; the default AppState
(define DEFAULT (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F))

(define (snake-game appstate)
  (big-bang appstate
    [to-draw draw-game]                                  ; draw the snake and apple on the background
    [on-key handle-keyboard]                                 ; change snake's direction or reset game or quit the game
    [on-tick move 0.2]; (time-tick appstate)]                ; uptade snake's position and "time" incrase each tick
    ;[display-mode .fullscreen]                              ; the display automatically becomes full screen
    [stop-when end?]))                                       ; quit the application