;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname snake-final) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; Convenzioni
;  - constants UPPERCASE
;  - anything else lowercase using kebab-case

;; Libraries
(require 2htdp/universe)
(require 2htdp/image)

;; Our external files
(require "external-files/snake.rkt")
(require "external-files/positions.rkt")
(require "external-files/generals.rkt")


;;;;;;;;;;;;;;;;;;;; DATA TYPES ;;;;;;;;;;;;;;;;;;;;
; an AppleUnit is an Image
; It represents the little rectangles that the apple is made off

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
; AppleUnit
(define APPLEUNIT (bitmap "../resources/apple.png"))

; the first example of an Apple
(define APPLE1 (compute-apple-position (random 401) 1 (compute-available-pos (cons (make-posn 0 0)(snake-position SNAKE1)) BACKGROUNDPOS)))

; General background
(define GAMEBACK (bitmap "../resources/game-background.png"))

;;;;;;;;;;;;;;;;;;;; FUNCTIONS ;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;; DRAW APPSTATE ;;;;;;;;;;
; draw-appstate: AppState -> Image
; draws the appstate
; Header (define (draw-appstate state) )

; Examples
(check-expect (draw-appstate (make-appstate
                              (make-snake (list (make-posn 363 113) (make-posn 388 113) (make-posn 413 113)) 3 RIGHT)
                              (compute-apple-position 1 1 (compute-available-pos (cons (make-posn 488 488) (snake-position SNAKE1)) BACKGROUNDPOS))
                              GAME-T
                              QUIT-F))
              (overlay/align 'center 'center (place-image APPLEUNIT 463 488 (place-image TAIL 363 113 (place-image SNAKEUNIT 388 113 (place-image SNAKEHEAD 413 113 BACKGROUND)))) GAMEBACK))

(check-expect (draw-appstate (make-appstate
                              (make-snake (list (make-posn 113 113) (make-posn 163 113) (make-posn 138 113) (make-posn 188 113)) 4 RIGHT)
                              (compute-apple-position 100 1 (compute-available-pos (cons (make-posn 488 363) (snake-position SNAKE1)) BACKGROUNDPOS))
                              GAME-T
                              QUIT-F))
              (overlay/align 'center 'center (place-image APPLEUNIT 13 388 (place-image TAIL 113 113 (place-image SNAKEUNIT 163 113 (place-image SNAKEUNIT 138 113 (place-image SNAKEHEAD 188 113 BACKGROUND))))) GAMEBACK))

(check-expect (draw-appstate (make-appstate
                              (make-snake (list (make-posn 263 88) (make-posn 338 88) (make-posn 313 88) (make-posn 288 88) (make-posn 363 88)) 5 RIGHT)
                              (compute-apple-position 250 1 (compute-available-pos (cons (make-posn 238 188) (snake-position SNAKE1)) BACKGROUNDPOS))
                              GAME-T
                              QUIT-F))
              (overlay/align 'center 'center (place-image APPLEUNIT 163 188 (place-image TAIL 263 88 (place-image SNAKEUNIT 338 88 (place-image SNAKEUNIT 313 88 (place-image SNAKEUNIT 288 88 (place-image SNAKEHEAD 363 88 BACKGROUND)))))) GAMEBACK))

(check-expect (draw-appstate (make-appstate
                              (make-snake (list (make-posn 413 63) (make-posn 438 63) (make-posn 463 63)) 3 RIGHT)
                              (compute-apple-position 364 1 (compute-available-pos (cons (make-posn 388 38) (snake-position SNAKE1)) BACKGROUNDPOS))
                              GAME-T
                              QUIT-F))
              (overlay/align 'center 'center (place-image APPLEUNIT 313 38 (place-image TAIL 413 63 (place-image SNAKEUNIT 438 63 (place-image SNAKEHEAD 463 63 BACKGROUND)))) GAMEBACK))

(check-expect (draw-appstate (make-appstate
                              SNAKE1
                              (compute-apple-position 396 1 (compute-available-pos (cons (make-posn 88 13) (snake-position SNAKE1)) BACKGROUNDPOS))
                              GAME-T
                              QUIT-F))
              (overlay/align 'center 'center (place-image APPLEUNIT 13 13 (place-image TAIL 188 238 (place-image SNAKEUNIT 213 238 (place-image SNAKEHEAD 238 238 BACKGROUND)))) GAMEBACK))

; Code
(define (draw-appstate state)                          ; draw a image of the appstate at moment
  (overlay/align 'center 'center (place-image APPLEUNIT                      ; an Apple
                                              (posn-x (appstate-apple state))         ; x coordinate
                                              (posn-y (appstate-apple state))         ; y coordinate
                                              (draw-snake (appstate-snake state)))    ; a Snake call its own function to draw itself
                 GAMEBACK))


;;;;;;;;;; EATING ;;;;;;;;;;
; eating : AppState -> AppState
; when the apple is eating by the snake, it will become more longer and the apple's position will uptate
; Header (define (eating state) state)

; Examples
; the apple's position is randomic so it is impossible to do some examples because we cannot predict its position

; Code
(define (eating state)
    (make-appstate
     (move-snake
      (make-snake (cons (first (snake-position (appstate-snake state))) (snake-position (appstate-snake state)))
                  (add1 (snake-length (appstate-snake state)))
                  (snake-direction (appstate-snake state))))
      (compute-apple-position (random 401) 1 (compute-available-pos (cons (appstate-apple state) (snake-position (appstate-snake state))) BACKGROUNDPOS))
      (appstate-game state)
      (appstate-quit state)))


;;;;;;;;;; MOVE ;;;;;;;;;;
; move: AppState -> AppState
; moves the snake using an auxiliary function
; Header (define (move appstate)

; Examples
(check-expect (move DEFAULT) (make-appstate
                              (make-snake (list (make-posn 213 238) (make-posn 238 238) (make-posn 263 238)) 3 RIGHT)
                              APPLE1
                              GAME-T
                              QUIT-F))

(check-expect (move (make-appstate
                     (make-snake (list (make-posn 13 38) (make-posn 13 63) (make-posn 13  88)) 3 DOWN)
                     APPLE1
                     GAME-T
                     QUIT-F))
              (make-appstate
               (make-snake (list (make-posn 13 63) (make-posn 13 88) (make-posn 13 113)) 3 DOWN)
               APPLE1
               GAME-T
               QUIT-F))

(check-expect (move (make-appstate
                     (make-snake (list (make-posn 13 38) (make-posn 38 38) (make-posn 63  38)) 3 RIGHT)
                     APPLE1
                     GAME-T
                     QUIT-F))
              (make-appstate
               (make-snake (list (make-posn 38 38) (make-posn 63 38) (make-posn 88 38)) 3 RIGHT)
               APPLE1
               GAME-T
               QUIT-F))

(check-expect (move (make-appstate
                     (make-snake (list (make-posn 13 88) (make-posn 13 63) (make-posn 13  38)) 3 UP)
                     APPLE1
                     GAME-T
                     QUIT-F))
              (make-appstate
               (make-snake (list (make-posn 13 63) (make-posn 13 38) (make-posn 13 13)) 3 UP)
               APPLE1
               GAME-T
               QUIT-F))

(check-expect (move (make-appstate
                     (make-snake (list (make-posn 88 38) (make-posn 63 38) (make-posn 38 38)) 3 LEFT)
                     APPLE1
                     GAME-T
                     QUIT-F))
              (make-appstate
               (make-snake (list (make-posn 63 38) (make-posn 38 38) (make-posn 13 38)) 3 LEFT)
               APPLE1
               GAME-T
               QUIT-F))

; Code
(define (move state)
  (cond
    [(equal? (last (snake-position (appstate-snake state))) (appstate-apple state)) (eating state)]
    [else
     (make-appstate                         ; every tick the appstate is moved and it creates a new update where it is made by :
      (move-snake (appstate-snake state))   ; the new position of the snake
      (appstate-apple state)                ; the apple's state is the same
      (appstate-game state)                 ; the game's state is the same
      (appstate-quit state))]))               ; the quit's state is the same


;;;;;;;;;; TICK ;;;;;;;;;;
; time-tick: AppState -> Number
; changes the speed of the snake based on snake length
; Header (define (time-tick state) 2.7)

; Examples
(check-expect (time-tick (make-appstate
                              (make-snake (list (make-posn 413 113) (make-posn 388 113) (make-posn 363 113)) 3 UP)
                              APPLE1
                              GAME-T
                              QUIT-F)) 0.4)

(check-expect (time-tick (make-appstate
                              (make-snake (list (make-posn 188 113) (make-posn 163 113) (make-posn 138 113) (make-posn 113 113)) 4 UP)
                              APPLE1
                              GAME-T
                              QUIT-F)) 0.3)
              
(check-expect (time-tick (make-appstate
                              (make-snake (list (make-posn 363 88) (make-posn 338 88) (make-posn 313 88) (make-posn 288 88) (make-posn 263 88)) 5 UP)
                              APPLE1
                              GAME-T
                              QUIT-F)) 0.24)

(check-expect (time-tick (make-appstate
                              (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 UP)
                              APPLE1
                              GAME-T
                              QUIT-F)) 0.4)

(check-expect (time-tick (make-appstate
                              SNAKE1
                              APPLE1
                              GAME-T
                              QUIT-F)) 0.4)

; Code
(define (time-tick state) (/ 1.2 (snake-length (appstate-snake state)))) ; longer the snake is, faster the game will be because the tick will decrease


;;;;;;;;;; RESET ;;;;;;;;;;
; reset: AppState -> AppState
; changes the game in the appstate
; Header (define (reset state) DEFAULT

; Examples
(check-expect (reset DEFAULT) DEFAULT)

(check-expect (reset (make-appstate
                              (make-snake (list (make-posn 188 113) (make-posn 163 113) (make-posn 138 113) (make-posn 113 113)) 4 RIGHT)
                              APPLE1
                              GAME-F
                              QUIT-F))
              (make-appstate
               (make-snake (list (make-posn 188 113) (make-posn 163 113) (make-posn 138 113) (make-posn 113 113)) 4 RIGHT)
               APPLE1
               GAME-F
               QUIT-F))

(check-expect (reset (make-appstate
                              (make-snake (list (make-posn 413 113) (make-posn 388 113) (make-posn 363 113)) 3 RIGHT)
                              APPLE1
                              GAME-T
                              QUIT-F)) DEFAULT)

(check-expect (reset (make-appstate
                              (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 RIGHT)
                              APPLE1
                              GAME-F
                              QUIT-F))
              (make-appstate
               (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 RIGHT)
               APPLE1
               GAME-F
               QUIT-F))

(check-expect (reset (make-appstate
                              (make-snake (list (make-posn 363 88) (make-posn 338 88) (make-posn 313 88) (make-posn 288 88) (make-posn 263 88)) 5 RIGHT)
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
                              (make-snake (list (make-posn 188 113) (make-posn 163 113) (make-posn 138 113) (make-posn 113 113)) 4 RIGHT)
                              APPLE1
                              GAME-T
                              QUIT-F))
              (make-appstate
               (make-snake (list (make-posn 188 113) (make-posn 163 113) (make-posn 138 113) (make-posn 113 113)) 4 RIGHT)
               APPLE1
               GAME-T
               QUIT-T))

(check-expect (quit (make-appstate
                              (make-snake (list (make-posn 413 113) (make-posn 388 113) (make-posn 363 113)) 3 RIGHT)
                              APPLE1
                              GAME-T
                              QUIT-F))
              (make-appstate
               (make-snake (list (make-posn 413 113) (make-posn 388 113) (make-posn 363 113)) 3 RIGHT)
               APPLE1
               GAME-T
               QUIT-T))

(check-expect (quit (make-appstate
                              (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 RIGHT)
                              APPLE1
                              GAME-T
                              QUIT-F))
              (make-appstate
               (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 RIGHT)
               APPLE1
               GAME-T
               QUIT-T))

(check-expect (quit (make-appstate
                              (make-snake (list (make-posn 363 88) (make-posn 338 88) (make-posn 313 88) (make-posn 288 88) (make-posn 263 88)) 5 RIGHT)
                              APPLE1
                              GAME-T
                              QUIT-F))
              (make-appstate
               (make-snake (list (make-posn 363 88) (make-posn 338 88) (make-posn 313 88) (make-posn 288 88) (make-posn 263 88)) 5 RIGHT)
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
(check-expect (handle-keyboard (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F) "up")
              (make-appstate
               (make-snake (snake-position SNAKE1) (snake-length SNAKE1) UP)
               APPLE1
               GAME-T
               QUIT-F))

(check-expect (handle-keyboard (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F) "right")
              (make-appstate
               (make-snake (snake-position SNAKE1) (snake-length SNAKE1) RIGHT)
               APPLE1
               GAME-T
               QUIT-F))

(check-expect (handle-keyboard (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F) "down")
              (make-appstate
               (make-snake (snake-position SNAKE1) (snake-length SNAKE1) DOWN)
               APPLE1
               GAME-T
               QUIT-F))

(check-expect (handle-keyboard (make-appstate
                                (make-snake (list (make-posn 13 13) (make-posn 13 63) (make-posn 13 88)) 3 DOWN)
                                APPLE1
                                GAME-T
                                QUIT-F) "left")
              (make-appstate
                (make-snake (list (make-posn 13 13) (make-posn 13 63) (make-posn 13 88))(snake-length SNAKE1) LEFT)
                APPLE1
                GAME-T
                QUIT-F))

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

; Code
(define (handle-keyboard state key)
  (cond
    [(not (string? key)) state]                                                                          ; for any possible not-string key input, the output is the same AppState as before
    
    [(or (and (string=? key "up") (string=? (snake-direction (appstate-snake state)) "down"))            ; if the direction is opposite of the key, the state is the same
         (and (string=? key "right") (string=? (snake-direction (appstate-snake state)) "left"))        
         (and (string=? key "down") (string=? (snake-direction (appstate-snake state)) "up"))           
         (and (string=? key "left") (string=? (snake-direction (appstate-snake state)) "right"))) state]

    [(or (string=? key "up") (string=? key "right") (string=? key "down") (string=? key "left"))         ; if the input is one of 'up', 'right', 'down' or 'left'
     (make-appstate                                                                                      ; create a new appstate where :
      (change-snake-direction key (appstate-snake state))                                                  ; the new snake is returned by the function to change the snake's direction
      (appstate-apple state)                                                                               ; the apple's appstate is the same
      (appstate-game state)                                                                                ; the game's appstate is the same
      (appstate-quit state))]                                                                              ; the quit's appstate is the same

    [(string=? key "r") (reset state)]                                                                   ; reset the game

    [(string=? key "escape") (quit state)]                                                               ; quit the game

    [else state]))                                                                                       ; for any other input the appstate is the same


;;;;;;;;;; END ;;;;;;;;;;
; end?: AppState -> Boolean
; stops the game
; Header (define (end? state) #true)

; Example
(check-expect (end? DEFAULT) #false)
(check-expect (end? (make-appstate SNAKE1 APPLE1 GAME-T QUIT-T)) #true)
; the top limit
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 13 13) (make-posn 13 38) (make-posn 13 63)) 3 UP)
                     APPLE1
                     GAME-T
                     QUIT-F))
              #false)
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 13 38) (make-posn 13 13) (make-posn 13 -12)) 3 UP)
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
                     (make-snake (list (make-posn 438 13) (make-posn 488 13) (make-posn 513 13)) 3 RIGHT)
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
                     (make-snake (list (make-posn 13 463) (make-posn 13 488) (make-posn 13 513)) 3 DOWN)
                     APPLE1
                     GAME-T
                     QUIT-F))
              #true)
;the left limit
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 13 13) (make-posn 38 13) (make-posn 63 13)) 3 LEFT)
                     APPLE1
                     GAME-T
                     QUIT-F))
              #false)
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 38 13) (make-posn 13 13) (make-posn -12 13)) 3 LEFT)
                     APPLE1
                     GAME-T
                     QUIT-F))
              #true)
; snake hit itself
(check-expect (end? DEFAULT) #false)
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 438 38) (make-posn 463 38) (make-posn 463 63)) 5 DOWN)
                     APPLE1
                     GAME-T
                     QUIT-F)) #true)
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 438 38) (make-posn 463 38) (make-posn 463 13)) 5 UP)
                     APPLE1
                     GAME-T
                     QUIT-F)) #false)
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63) (make-posn 413 88) (make-posn 438 88) (make-posn 438 63)) 6 UP)
                     APPLE1
                     GAME-T
                     QUIT-F)) #true)
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63) (make-posn 388 63) (make-posn 363 63)) 5 LEFT)
                     APPLE1
                     GAME-T
                     QUIT-F)) #false)
 
; Code
(define (end? state)
  (cond
    [(check-position-out (last (snake-position (appstate-snake state))) BACKGROUNDPOS) #true] ; if the snake is over background's limits, the application turn off
    [(check-eat-snake (appstate-snake state)) #true]                                          ; if the snake hits itself, the application turn off
    [(boolean=? (appstate-quit state) #false) #false]                                         ; the application remains on
    [else #true]))                                                                            ; for any other case the application turn off


;;;;;;;;;; MAIN APPLICATIONS ;;;;;;;;;;
; the default AppState
(define DEFAULT (make-appstate SNAKE1 APPLE1 GAME-T QUIT-F))

(define (snake-game appstate)
  (big-bang appstate
    [to-draw draw-appstate]                                  ; draw the snake and apple on the background
    [on-key handle-keyboard]                                 ; change snake's direction or reset game or quit the game
    [on-tick move 0.2]                      ; uptade snake's position and "time" incrase each tick
    ;[display-mode 'fullscreen]                              ; the display automatically becomes full screen
    [name "Snake Game"]
    [stop-when end?]))                                       ; quit the application