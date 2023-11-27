;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname snake-final) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Convenzioni
;  - constants UPPERCASE
;  - anything else lowercase using kebab-case

;; Libraries

(require 2htdp/universe)
(require 2htdp/image)

;; Data Types

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
(define-struct appstate [snake apple game quit])

;; Constants

; app scene background
(define BACKGROUND (empty-scene 501 501 "white"))

; SnakeUnit
(define SNAKEUNIT (rectangle 24 24 "solid" "green"))

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

; all positions on the board

; make-positions is a function to compute all the positions on the background
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

; All the positions on the background
(define BACKGROUNDPOS (make-positions 1 1 1 (list (make-posn 13 13))))


;; MAIN APPLICATION

;(define (snake-game appstate)
;  (big-bang appstate
;    [to-draw draw]                              ; draw the snake
;    [on-key handle-keyboard]                    ; change snake's direction or reset game or quit the game
;    [on-tick move-snake  (time-tick appstate)]  ; uptade snake's position and "time" incrase each tick
;    [stop-when end?]))                          ; quit the application