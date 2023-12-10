;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname generals) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; Here are present some constants and some general functions
(require racket/base)
(require 2htdp/image)

(provide BACKGROUND
         GAMEBACK
         GAME-OVER
         HOME
         APPLEUNIT
         EATEN
         UP
         DOWN
         RIGHT
         LEFT
         GAME-T
         GAME-F
         QUIT-T
         QUIT-F
         last)

;;;;;;;;;;;;;;;;;;;; DATA TYPES ;;;;;;;;;;;;;;;;;;;;
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

(define BACKGROUND (bitmap "../../resources/images/snake-background.png"))
(define GAMEBACK (bitmap "../../resources/images/game-background.png"))
; Home background
(define HOME (bitmap "../../resources/images/Snake.png"))
; the writing game over
(define GAME-OVER (bitmap "../../resources/images/gameover.png"))

; AppleUnit
(define APPLEUNIT (bitmap "../../resources/images/apple.png"))

(define EATEN "../resources/sounds/apple-eaten.wav")

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

(define NUMBERS (list
                 (make-posn 0 "../../resources/numbers/0.png")
                 (make-posn 1 "../../resources/numbers/1.png")
                 (make-posn 2 "../../resources/numbers/2.png")
                 (make-posn 3 "../../resources/numbers/3.png")
                 (make-posn 4 "../../resources/numbers/4.png")
                 (make-posn 5 "../../resources/numbers/5.png")
                 (make-posn 6 "../../resources/numbers/6.png")
                 (make-posn 7 "../../resources/numbers/7.png")
                 (make-posn 8 "../../resources/numbers/8.png")
                 (make-posn 9 "../../resources/numbers/9.png")))

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

; number->path: Number -> String
; takes in a number and returns it as a path
(define (number->path num)
  (posn-y (first (filter (lambda (pos) (equal? (posn-x pos) num)) NUMBERS))))

; string->image: String<Number> Number -> Image
; takes in a number and returns it as an image
(define (number->image str n)
  (cond
    [(= n (- (string-length str) 1)) (bitmap/file (number->path (string->number (string (string-ref str n)))))]
    [else
     (overlay/offset (bitmap/file (number->path (string->number (string (string-ref str n)))))
                     60 0
     (number->image str (add1 n)))]))