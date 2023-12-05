;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; This file contains all the functions relative to the snake
(require racket/base)
(require 2htdp/image)
(require "positions.rkt")
(require "generals.rkt")

(provide TAIL
         SNAKEUNIT
         SNAKEHEAD
         SNAKE1
         make-snake
         snake-position
         snake-length
         snake-direction
         move-snake
         change-snake-direction
         draw-snake
         check-eat-snake)

;;;;;;;;;; COSTANTS ;;;;;;;;;;
; Directions
(define UP "up")
(define RIGHT "right")
(define DOWN "down")
(define LEFT "left")

; Snake struct and one examples
(define-struct snake [position length direction])
(define SNAKE1 (make-snake (list (make-posn 13 13) (make-posn 38 13) (make-posn 63 13)) 3 RIGHT))

; SnakeUnit
(define SNAKEUNIT (rectangle 24 24 "solid" 'green)) ; (bitmap "../resources/SNAKEUNIT.png")

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

; background
(define BACKGROUND (bitmap "../../resources/snake-background.png"))

;;;;;;;;;; DATA TYPES ;;;;;;;;;;;;;;
; a SnakeUnit is an Image
; It represents the little rectangles that the snake is made off

; The SnakeHead is an Image
; It represents the head of the snake
; it is an overlay of an eye, a tongue and a SnakeUnit

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
;(define-struct snake [position length direction])

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

(check-expect (draw-snake (make-snake (list (make-posn 363 113) (make-posn 388 113) (make-posn 413 113)) 3 RIGHT))
              (place-image TAIL 363 113 (place-image SNAKEUNIT 388 113 (place-image SNAKEHEAD 413 113 BACKGROUND))))

(check-expect (draw-snake (make-snake (list (make-posn 113 113) (make-posn 138 113) (make-posn 163 113) (make-posn 188 113)) 4 RIGHT))
              (place-image TAIL 113 113 (place-image SNAKEUNIT 163 113 (place-image SNAKEUNIT 138 113 (place-image SNAKEHEAD 188 113 BACKGROUND)))))

(check-expect (draw-snake (make-snake (list (make-posn 313 88) (make-posn 338 88) (make-posn 313 88) (make-posn 288 88) (make-posn 263 88)) 5 RIGHT))
              (place-image TAIL 313 88 (place-image SNAKEUNIT 338 88 (place-image SNAKEUNIT 313 88 (place-image SNAKEUNIT 288 88 (place-image SNAKEHEAD 263 88 BACKGROUND))))))

(check-expect (draw-snake (make-snake (list (make-posn 413 63) (make-posn 438 63) (make-posn 463 63)) 3 RIGHT))
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
       (make-snake (rest (snake-position snake)) (snake-length snake) (snake-direction snake))))]
    [else
     (place-image
      SNAKEUNIT
      (posn-x (first (snake-position snake)))
      (posn-y (first (snake-position snake)))
      (draw-snake
       (make-snake (rest (snake-position snake)) (snake-length snake) (snake-direction snake))))]))

;;;;;;;;;; CHANGE SNAKE DIRECTION ;;;;;;;;;;
; change-snake-direction: String Snake -> Snake
; changes snake's head direction
; Header (define (change-snake-direction direction snake) snake)

; Examples
(check-expect (change-snake-direction UP SNAKE1) (make-snake (snake-position SNAKE1)
                                                               (snake-length SNAKE1)
                                                               UP
                                                               ))
(check-expect (change-snake-direction RIGHT SNAKE1) (make-snake (snake-position SNAKE1)
                                                               (snake-length SNAKE1)
                                                               RIGHT
                                                               ))
(check-expect (change-snake-direction DOWN SNAKE1) (make-snake (snake-position SNAKE1)
                                                               (snake-length SNAKE1)
                                                               DOWN
                                                               ))
(check-expect (change-snake-direction LEFT (make-snake (list (make-posn 13 13) (make-posn 13 63) (make-posn 13 88)) 3 DOWN))
              (make-snake (list (make-posn 13 13) (make-posn 13 63) (make-posn 13 88))
                          3
                          LEFT))
(check-expect (change-snake-direction " " SNAKE1) SNAKE1)

; Code 
(define (change-snake-direction direction snake)
  (cond
    [(or (string=? direction UP)
         (string=? direction DOWN)
         (string=? direction RIGHT)
         (string=? direction LEFT))
     (make-snake                                                                                                                                         ; if the direction is up, make a snake where:
      (snake-position snake)                                                                                                                              ; the snake's position is the same
      (snake-length snake)                                                                                                                               ; the snake's length is the same
       direction                                                                                                                                         ; the snake's direction changes to up
      )]
    [else snake]))                                                                                                                                       ; for any other case returns the same appstate as before

;;;;;;;;;; MOVE SNAKE ;;;;;;;;;;
; move-snake: Direction List<Posn> -> List<Posn>
; Header (define (move-snake d lop) lop)

; Examples
(check-expect (move-snake (make-snake (snake-position SNAKE1)
                                      (snake-length SNAKE1)
                                      (snake-direction SNAKE1)
                                      ))
                          (make-snake (list (make-posn 38 13) (make-posn 63 13) (make-posn 88 13)) 3 RIGHT))

(check-expect (move-snake (make-snake (list (make-posn 13 38) (make-posn 13 63) (make-posn 13 88)) 3 DOWN)) 
              (make-snake (list (make-posn 13 63) (make-posn 13 88) (make-posn 13 113)) 3 DOWN))              

(check-expect (move-snake (make-snake (list (make-posn 13 38) (make-posn 38 38) (make-posn 63  38)) 3 RIGHT))
              (make-snake (list (make-posn 38 38) (make-posn 63 38) (make-posn 88 38)) 3 RIGHT))

(check-expect (move-snake (make-snake (list (make-posn 13 88) (make-posn 13 63) (make-posn 13  38)) 3 UP))
              (make-snake (list (make-posn 13 63) (make-posn 13 38) (make-posn 13 13)) 3 UP))

(check-expect (move-snake (make-snake (list (make-posn 38 38) (make-posn 63 38) (make-posn 88 38)) 3 RIGHT))
              (make-snake (list (make-posn 63 38) (make-posn 88 38) (make-posn 113 38)) 3 RIGHT)) 

; Code
(define (move-snake snake)
  (make-snake
   (update-positions (snake-direction snake) (snake-position snake))
   (snake-length snake)
   (snake-direction snake)
   ))


;;;;;;;;;; CHECK SNAKE HIT ITSELF ;;;;;;;;;;
; check-eat-snake : AppState -> AppState
; check if the snake hit itself during the game
; Header (define (check-eat-snake state) #true)

; Examples
(check-expect (check-eat-snake SNAKE1) #false)
(check-expect (check-eat-snake 
                                (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 438 38) (make-posn 463 38) (make-posn 463 63)) 5 DOWN)
                                ) #true)
(check-expect (check-eat-snake 
                                (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 438 38) (make-posn 463 38) (make-posn 463 13)) 5 UP)
                                ) #false)
(check-expect (check-eat-snake 
                                (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63) (make-posn 413 88) (make-posn 438 88) (make-posn 438 63)) 6 UP)
                                ) #true)
(check-expect (check-eat-snake 
                                (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63) (make-posn 388 63) (make-posn 363 63)) 5 LEFT)
                                ) #false)

; Code
(define (check-eat-snake snake)
  (cond
    [(empty? (rest (snake-position snake))) #false]
    [(equal? (last (snake-position snake)) (first (snake-position snake))) #true]
    [else (check-eat-snake
            (make-snake (rest (snake-position snake))
                        (snake-length snake)
                        (snake-direction snake)))]))
