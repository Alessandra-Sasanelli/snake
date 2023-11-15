;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname snake-v1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; libraries for animations and pictures
(require 2htdp/universe)
(require 2htdp/image)

;; App background
(define BACKGROUND (empty-scene 1000 1000))

; a SnakeUnit is an Image
; It represents the little rectangles that the snake is made off
(define SNAKEUNIT (rectangle 25 25 "solid" "green"))

(define APPLEUNIT (rectangle 25 25 "solid" "red"))

; a Position is a Posn as such:
; - (make-posn -1 -1)
; - (make-posn (- (posn-x Position) 26) (- (posn-y Position) 26))
; represents the position of a element of the snake

; an Apple is a Position
; it represents the position of the apple at a particular moment
(define APPLE1 (make-posn -27 -27))
(define APPLE2 (make-posn -1 -27))

; a Direction is one of these String
; - "up"
; - "down"
; - "left"
; - "right"

(define UP "up")
(define DOWN "down")
(define LEFT "left")
(define RIGHT "right")

; a Length is a Number as such:
; - the Number 3
; - Length + 1
; represents the length of the snake

; a Snake is a struct
; (make-snake position length direction)
; where:
; - position is a Position
; - length is a Length
; - direction is a Direction 

(define-struct snake [position length direction])

(define SNAKE1 (make-snake (make-posn 26 26) 3 UP))
(define SNAKE2 (make-snake (make-posn 26 26) 4 DOWN))
(define SNAKE3 (make-snake (make-posn 26 26) 5 LEFT))
(define SNAKE4 (make-snake (make-posn 26 26) 6 RIGHT))

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

(check-expect (change-snake-direction "down" (make-snake (make-posn 26 26) 3 "up")) (make-snake (make-posn 26 26) 3 "down"))
(check-expect (change-snake-direction "up" (make-snake (make-posn 26 26) 3 "left")) (make-snake (make-posn 26 26) 3 "up"))
(check-expect (change-snake-direction "left" (make-snake (make-posn 26 26) 3 "up")) (make-snake (make-posn 26 26) 3 "left"))
(check-expect (change-snake-direction "right" (make-snake (make-posn 26 26) 3 "up")) (make-snake (make-posn 26 26) 3 "right"))

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