;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname snake-final) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; Libraries
(require racket/base)
(require 2htdp/universe)
(require 2htdp/image
         (only-in racket/gui/base play-sound))

;; Our external files
(require "external-files/snake.rkt")
(require "external-files/positions.rkt")
(require "external-files/generals.rkt")


;;;;;;;;;;;;;;;;;;;; DATA TYPES ;;;;;;;;;;;;;;;;;;;;

; an Integer is a Number as such:
; - the Number 0
; - Integer + 1

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
; - tick is an Integer -> represents the ticks from the game beginning
; - rate is an Integer -> represents the speed levels
(define-struct appstate [snake apple game quit tick rate] #:transparent)


;;;;;;;;;;;;;;;;;;;; CONSTANTS ;;;;;;;;;;;;;;;;;;;;

; the first example of an Apple
(define APPLE (compute-apple-position (random 401) 1 (compute-available-pos (cons (make-posn 0 0)(snake-position SNAKE)) BACKGROUNDPOS)))

; on-tick constant
(define FASTSPEED 0.08)

; counts every tick of the clock
(define TICK 0)

; speed levels
(define RATE 10)

; GAMEOVRPLAYED: Boolean
; represents weather the game-over mp3 has been played or not
(define GAMEOVRPLAYED #false)

(define (reset-gameover)
  (set! GAMEOVRPLAYED #false))

;;;;;;;;;;;;;;;;;;;; FUNCTIONS ;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;; END ;;;;;;;;;;
; end?: AppState -> Boolean
; stops the game
; Header (define (end? state) #true)

; Example
(check-expect (end? DEFAULT) #false)
(check-expect (end? (make-appstate SNAKE APPLE GAME-T QUIT-T 0 0)) #false)
; the top limit
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 13 13) (make-posn 13 38) (make-posn 13 63)) 3 UP)
                     APPLE
                     GAME-T
                     QUIT-F 0 0))
              #false)
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 13 38) (make-posn 13 13) (make-posn 13 -12)) 3 UP)
                     APPLE
                     GAME-T
                     QUIT-F 0 0))
              #true)
; the right limit
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 488 13) (make-posn 463 13) (make-posn 438 13)) 3 RIGHT)
                     APPLE
                     GAME-T
                     QUIT-F 0 0))
              #false)
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 438 13) (make-posn 488 13) (make-posn 513 13)) 3 RIGHT)
                     APPLE
                     GAME-T
                     QUIT-F 0 0))
              #true)
; the bottom limit
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 13 488) (make-posn 13 463) (make-posn 13 438)) 3 DOWN)
                     APPLE
                     GAME-T
                     QUIT-F 0 0))
              #false)
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 13 463) (make-posn 13 488) (make-posn 13 513)) 3 DOWN)
                     APPLE
                     GAME-T
                     QUIT-F 0 0))
              #true)
;the left limit
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 13 13) (make-posn 38 13) (make-posn 63 13)) 3 LEFT)
                     APPLE
                     GAME-T
                     QUIT-F 0 0))
              #false)
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 38 13) (make-posn 13 13) (make-posn -12 13)) 3 LEFT)
                     APPLE
                     GAME-T
                     QUIT-F 0 0))
              #true) 
; snake hit itself
(check-expect (end? DEFAULT) #false)
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 438 38) (make-posn 463 38) (make-posn 463 63)) 5 DOWN)
                     APPLE
                     GAME-T
                     QUIT-F 0 0)) #true)
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 438 38) (make-posn 463 38) (make-posn 463 13)) 5 UP)
                     APPLE
                     GAME-T
                     QUIT-F 0 0)) #false)
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63) (make-posn 413 88) (make-posn 438 88) (make-posn 438 63)) 6 UP)
                     APPLE
                     GAME-T
                     QUIT-F 0 0)) #true)
(check-expect (end? (make-appstate
                     (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63) (make-posn 388 63) (make-posn 363 63)) 5 LEFT)
                     APPLE
                     GAME-T
                     QUIT-F 0 0)) #false)

(define (end? state)
  (if (or (check-position-out (last (snake-position (appstate-snake state))) BACKGROUNDPOS) ; return true if the snake hit the border
      (check-eat-snake (appstate-snake state)))
      (begin (if (not GAMEOVRPLAYED) (play-sound DEATH #true) #false)
             (begin
               (set! GAMEOVRPLAYED #true) #true))
      (begin (reset-gameover) #false)))


;;;;;;;;;; DRAW END ;;;;;;;;;;
; draw-end: AppState -> Image
; draws the game over screen
; Header (define (draw-game state) BACKGROUND)

; Code
(define (draw-end state)
  (place-image/align
   (number->image (number->string (* 100 (- (snake-length (appstate-snake state)) 3))) 0) ; a score point on the right
   670 100 'right 'center                                                                   ; x and y of point
   (overlay/align 'center 'center GAME-OVER GAMEBACK)))                                   ; put the write on the background


;;;;;;;;;; DRAW APPSTATE ;;;;;;;;;;
; draw-appstate: AppState -> Image
; draws the appstate
; Header (define (draw-appstate state) BACKGROUND)

; Examples
(check-expect (draw-appstate (make-appstate
                              (make-snake (list (make-posn 363 113) (make-posn 388 113) (make-posn 413 113)) 3 RIGHT)
                              (compute-apple-position 1 1 (compute-available-pos (cons (make-posn 488 488) (snake-position SNAKE)) BACKGROUNDPOS))
                              GAME-T
                              QUIT-F
                              0
                              0))
              (place-image/align (number->image (number->string (* 100 (- 3 3))) 0) 670 100 'right 'center
                                 (overlay/align 'center 'center (place-image APPLEUNIT 463 488 (place-image TAIL 363 113 (place-image SNAKEUNIT 388 113 (place-image SNAKEHEAD 413 113 BACKGROUND)))) GAMEBACK)))

(check-expect (draw-appstate (make-appstate
                              (make-snake (list (make-posn 113 113) (make-posn 163 113) (make-posn 138 113) (make-posn 188 113)) 4 RIGHT)
                              (compute-apple-position 100 1 (compute-available-pos (cons (make-posn 488 363) (snake-position SNAKE)) BACKGROUNDPOS))
                              GAME-T
                              QUIT-F
                              0
                              0))
              (place-image/align (number->image (number->string (* 100 (- 4 3))) 0) 670 100 'right 'center
                                 (overlay/align 'center 'center (place-image APPLEUNIT 13 388 (place-image TAIL 113 113 (place-image SNAKEUNIT 163 113 (place-image SNAKEUNIT 138 113 (place-image SNAKEHEAD 188 113 BACKGROUND))))) GAMEBACK)))

(check-expect (draw-appstate (make-appstate
                              (make-snake (list (make-posn 263 88) (make-posn 338 88) (make-posn 313 88) (make-posn 288 88) (make-posn 363 88)) 5 RIGHT)
                              (compute-apple-position 250 1 (compute-available-pos (cons (make-posn 238 188) (snake-position SNAKE)) BACKGROUNDPOS))
                              GAME-T
                              QUIT-F
                              0
                              0))
              (place-image/align (number->image (number->string (* 100 (- 5 3))) 0) 670 100 'right 'center
                                 (overlay/align 'center 'center (place-image APPLEUNIT 163 188 (place-image TAIL 263 88 (place-image SNAKEUNIT 338 88 (place-image SNAKEUNIT 313 88 (place-image SNAKEUNIT 288 88 (place-image SNAKEHEAD 363 88 BACKGROUND)))))) GAMEBACK)))

(check-expect (draw-appstate (make-appstate
                              (make-snake (list (make-posn 413 63) (make-posn 438 63) (make-posn 463 63)) 3 RIGHT)
                              (compute-apple-position 364 1 (compute-available-pos (cons (make-posn 388 38) (snake-position SNAKE)) BACKGROUNDPOS))
                              GAME-T
                              QUIT-F
                              0
                              0))
              (place-image/align (number->image (number->string (* 100 (- 3 3))) 0) 670 100 'right 'center
                                 (overlay/align 'center 'center (place-image APPLEUNIT 313 38 (place-image TAIL 413 63 (place-image SNAKEUNIT 438 63 (place-image SNAKEHEAD 463 63 BACKGROUND)))) GAMEBACK)))

(check-expect (draw-appstate (make-appstate
                              SNAKE
                              (compute-apple-position 396 1 (compute-available-pos (cons (make-posn 88 13) (snake-position SNAKE)) BACKGROUNDPOS))
                              GAME-T
                              QUIT-F
                              0
                              0))
              (place-image/align (number->image (number->string (* 100 (- 3 3))) 0) 670 100 'right 'center
                                 (overlay/align 'center 'center (place-image APPLEUNIT 13 13 (place-image TAIL 188 238 (place-image SNAKEUNIT 213 238 (place-image SNAKEHEAD 238 238 BACKGROUND)))) GAMEBACK)))

; Code
(define (draw-appstate state)                                                             ; draw a image of the appstate with :
  (place-image/align
   (number->image (number->string (* 100 (- (snake-length (appstate-snake state)) 3))) 0) ; a score point on the right
   670 100 'right 'center                                                                   ; x and y of point
   (overlay/align 'center 'center
                  (place-image APPLEUNIT                                                  ; an Apple
                               (posn-x (appstate-apple state))                              ; its x coordinate
                               (posn-y (appstate-apple state))                              ; its y coordinate
                               (draw-snake (appstate-snake state)))                       ; a Snake call its own function to draw itself
                  GAMEBACK)))                                                             ; over the background of game's table


;;;;;;;;;; DRAW GAME ;;;;;;;;;;
; draw-game: AppState -> Image
; decide to draw wheather the home or the game
; Header (define (draw-game DEFAULT) HOME)

; Examples
(check-expect (draw-game DEFAULT) HOME)

(check-expect (draw-game (make-appstate
                          (make-snake (list (make-posn 363 113) (make-posn 388 113) (make-posn 413 113)) 3 RIGHT)
                          (compute-apple-position 1 1 (compute-available-pos (cons (make-posn 488 488) (snake-position SNAKE)) BACKGROUNDPOS))
                          GAME-T
                          QUIT-F
                          0
                          0))
              (place-image/align (number->image (number->string (* 100 (- 3 3))) 0) 670 100 'right 'center
                                 (overlay/align 'center 'center (place-image APPLEUNIT 463 488 (place-image TAIL 363 113 (place-image SNAKEUNIT 388 113 (place-image SNAKEHEAD 413 113 BACKGROUND)))) GAMEBACK)))

(check-expect (draw-game (make-appstate
                          (make-snake (list (make-posn 113 113) (make-posn 163 113) (make-posn 138 113) (make-posn 188 113)) 4 RIGHT)
                          (compute-apple-position 100 1 (compute-available-pos (cons (make-posn 488 363) (snake-position SNAKE)) BACKGROUNDPOS))
                          GAME-F
                          QUIT-F
                          0
                          0))
              HOME)

(check-expect (draw-game (make-appstate
                          (make-snake (list (make-posn 263 88) (make-posn 338 88) (make-posn 313 88) (make-posn 288 88) (make-posn 363 88)) 5 RIGHT)
                          (compute-apple-position 250 1 (compute-available-pos (cons (make-posn 238 188) (snake-position SNAKE)) BACKGROUNDPOS))
                          GAME-T
                          QUIT-F
                          0
                          0))
              (place-image/align (number->image (number->string (* 100 (- 5 3))) 0) 670 100 'right 'center
                                 (overlay/align 'center 'center (place-image APPLEUNIT 163 188 (place-image TAIL 263 88 (place-image SNAKEUNIT 338 88 (place-image SNAKEUNIT 313 88 (place-image SNAKEUNIT 288 88 (place-image SNAKEHEAD 363 88 BACKGROUND)))))) GAMEBACK)))

(check-expect (draw-game (make-appstate
                          SNAKE
                          (compute-apple-position 396 1 (compute-available-pos (cons (make-posn 88 13) (snake-position SNAKE)) BACKGROUNDPOS))
                          GAME-F
                          QUIT-F
                          0
                          0))
              HOME) 

; Code
(define (draw-game state)
  (cond
    [(end? state) (draw-end state)]    ; the game is lost
    [(not (appstate-game state)) HOME] ; the game is off
    [else (draw-appstate state)]))     ; the game is running


;;;;;;;;;; START ;;;;;;;;;;
; start: AppState -> AppState
; starts the game, changing the canvas
; Header (define (start state) DEFAULT)

; Examples
(check-expect (start DEFAULT) (make-appstate
                               (appstate-snake DEFAULT)
                               (appstate-apple DEFAULT)
                               #true
                               (appstate-quit DEFAULT)
                               0
                               10))
(check-expect (start (make-appstate
                      (make-snake (list (make-posn 313 313) (make-posn 338 313) (make-posn 363 313)) 3 RIGHT)
                      (appstate-apple DEFAULT)
                      #false
                      (appstate-quit DEFAULT)
                      0
                      0))
              (make-appstate
               (make-snake (list (make-posn 313 313) (make-posn 338 313) (make-posn 363 313)) 3 RIGHT)
               (appstate-apple DEFAULT)
               #true
               (appstate-quit DEFAULT)
               0
               0))
(check-expect (start (make-appstate
                      (make-snake (list (make-posn 313 313) (make-posn 338 313) (make-posn 363 313)) 3 RIGHT)
                      (appstate-apple DEFAULT)
                      #false
                      (appstate-quit DEFAULT)
                      0
                      0))
              (make-appstate
               (make-snake (list (make-posn 313 313) (make-posn 338 313) (make-posn 363 313)) 3 RIGHT)
               (appstate-apple DEFAULT)
               #true
               (appstate-quit DEFAULT)
               0
               0))
(check-expect (start (make-appstate
                      (make-snake (list (make-posn 313 313) (make-posn 338 313) (make-posn 363 313)) 3 RIGHT)
                      (appstate-apple DEFAULT)
                      #false
                      (appstate-quit DEFAULT)
                      0
                      0))
              (make-appstate
               (make-snake (list (make-posn 313 313) (make-posn 338 313) (make-posn 363 313)) 3 RIGHT)
               (appstate-apple DEFAULT)
               #true
               (appstate-quit DEFAULT)
               0
               0))
(check-expect (start (make-appstate
                      (make-snake (list (make-posn 313 313) (make-posn 338 313) (make-posn 363 313)) 3 RIGHT)
                      (appstate-apple DEFAULT)
                      #false
                      (appstate-quit DEFAULT)
                      0
                      0))
              (make-appstate
               (make-snake (list (make-posn 313 313) (make-posn 338 313) (make-posn 363 313)) 3 RIGHT)
               (appstate-apple DEFAULT)
               #true
               (appstate-quit DEFAULT)
               0
               0))

; Code
(define (start state)
  (make-appstate           ; retrn a new appstate where :
   (appstate-snake state)    ; the snake's appstate is the same
   (appstate-apple state)    ; the apple's appstate is the same
   #true                     ; the game is on
   (appstate-quit state)     ; the quit's appstate is the same
   (appstate-tick state)     ; the tick's appstate is the same
   (appstate-rate state)))   ; the rate's appstate is the same


;;;;;;;;;; RESET ;;;;;;;;;;
; reset: AppState -> AppState
; changes the game in the appstate
; Header (define (reset state) DEFAULT)

; Code
(define (reset state)
  (make-appstate                                                                                                                      ; return a new appstate where :
   SNAKE                                                                                                                                ; the snake it the default one
   (compute-apple-position (random 401) 1 (compute-available-pos (cons (appstate-apple state)(snake-position SNAKE)) BACKGROUNDPOS))    ; compute a new apple's position
   #false                                                                                                                               ; the game become false
   #false                                                                                                                               ; the quit become false
   (appstate-tick state)                                                                                                                ; the tick is the same
   10))                                                                                                                                 ; the rate become ten


;;;;;;;;;; HANDLE KEYBOARD ;;;;;;;;;;
; handle-keyboard: AppState KeyboardEvent -> AppState
; handles the keyboard events
; Header (define (handle-keyboard state key) (make-appstate SNAKE APPLE GAME-T QUIT-F 0 0))

; Examples
; not-string key input
(check-expect (handle-keyboard (make-appstate SNAKE APPLE GAME-T QUIT-F 0 0) #false)
              (make-appstate SNAKE APPLE GAME-T QUIT-F 0 0))
(check-expect (handle-keyboard (make-appstate SNAKE APPLE GAME-T QUIT-F 0 0) #true)
              (make-appstate SNAKE APPLE GAME-T QUIT-F 0 0))
; start the game
(check-expect (handle-keyboard (make-appstate
                                (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 DOWN)
                                APPLE
                                GAME-F
                                QUIT-F
                                0 0) "s")
              (make-appstate
               (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 DOWN)
               APPLE
               GAME-T
               QUIT-F 0 0))
(check-expect (handle-keyboard (make-appstate
                                (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 LEFT)
                                APPLE
                                GAME-F
                                QUIT-F 0 0) "s")
              (make-appstate
               (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 LEFT)
               APPLE
               GAME-T
               QUIT-F 0 0))
(check-expect (handle-keyboard (make-appstate
                                (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 UP)
                                APPLE
                                GAME-F
                                QUIT-F 0 0) "s")
              (make-appstate
               (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 UP)
               APPLE
               GAME-T
               QUIT-F 0 0))
(check-expect (handle-keyboard (make-appstate SNAKE APPLE GAME-F QUIT-F 0 0) "s")
              (make-appstate SNAKE APPLE GAME-T QUIT-F 0 0))
; opposite
(check-expect (handle-keyboard (make-appstate
                                (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 DOWN)
                                APPLE
                                GAME-T
                                QUIT-F 0 0) "up")
              (make-appstate
               (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 DOWN)
               APPLE
               GAME-T
               QUIT-F 0 0))
(check-expect (handle-keyboard (make-appstate
                                (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 LEFT)
                                APPLE
                                GAME-T
                                QUIT-F 0 0) "right")
              (make-appstate
               (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 LEFT)
               APPLE
               GAME-T
               QUIT-F 0 0))
(check-expect (handle-keyboard (make-appstate
                                (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 UP)
                                APPLE
                                GAME-T
                                QUIT-F 0 0) "down")
              (make-appstate
               (make-snake (list (make-posn 463 63) (make-posn 438 63) (make-posn 413 63)) 3 UP)
               APPLE
               GAME-T
               QUIT-F 0 0))
(check-expect (handle-keyboard (make-appstate SNAKE APPLE GAME-T QUIT-F 0 0) "left")
              (make-appstate SNAKE APPLE GAME-T QUIT-F 0 0))
; change direction
(check-expect (handle-keyboard (make-appstate SNAKE APPLE GAME-T QUIT-F 0 0) "up")
              (make-appstate
               (make-snake (snake-position SNAKE) (snake-length SNAKE) UP)
               APPLE
               GAME-T
               QUIT-F 0 0))

(check-expect (handle-keyboard (make-appstate SNAKE APPLE GAME-T QUIT-F 0 0) "right")
              (make-appstate
               (make-snake (snake-position SNAKE) (snake-length SNAKE) RIGHT)
               APPLE
               GAME-T
               QUIT-F 0 0))

(check-expect (handle-keyboard (make-appstate SNAKE APPLE GAME-T QUIT-F 0 0) "down")
              (make-appstate
               (make-snake (snake-position SNAKE) (snake-length SNAKE) DOWN)
               APPLE
               GAME-T
               QUIT-F 0 0))

(check-expect (handle-keyboard (make-appstate
                                (make-snake (list (make-posn 13 13) (make-posn 13 63) (make-posn 13 88)) 3 DOWN)
                                APPLE
                                GAME-T
                                QUIT-F 0 0) "left")
              (make-appstate
               (make-snake (list (make-posn 13 13) (make-posn 13 63) (make-posn 13 88))(snake-length SNAKE) LEFT)
               APPLE
               GAME-T
               QUIT-F 0 0))
; quit
(check-expect (handle-keyboard (make-appstate SNAKE APPLE GAME-T QUIT-F 0 0) "escape")
              (make-appstate SNAKE APPLE GAME-T QUIT-T 0 0))
(check-expect (handle-keyboard (make-appstate
                                (make-snake (list (make-posn 363 88) (make-posn 338 88) (make-posn 313 88) (make-posn 288 88) (make-posn 263 88)) 5 UP)
                                APPLE
                                GAME-T
                                QUIT-F 0 0)
                               "escape")
              (make-appstate
               (make-snake (list (make-posn 363 88) (make-posn 338 88) (make-posn 313 88) (make-posn 288 88) (make-posn 263 88)) 5 UP)
               APPLE
               GAME-T
               QUIT-T 0 0))
; any other input
(check-expect (handle-keyboard (make-appstate SNAKE APPLE GAME-T QUIT-F 0 0) "")
              (make-appstate SNAKE APPLE GAME-T QUIT-F 0 0))
(check-expect (handle-keyboard (make-appstate SNAKE APPLE GAME-T QUIT-F 0 0) " ")
              (make-appstate SNAKE APPLE GAME-T QUIT-F 0 0))
(check-expect (handle-keyboard (make-appstate SNAKE APPLE GAME-T QUIT-F 0 0) "a")
              (make-appstate SNAKE APPLE GAME-T QUIT-F 0 0))
(check-expect (handle-keyboard (make-appstate SNAKE APPLE GAME-T QUIT-F 0 0) "return")
              (make-appstate SNAKE APPLE GAME-T QUIT-F 0 0))

; Code
(define (handle-keyboard state key)
  (cond    
    [(not (string? key)) state]                                                                          ; for any possible not-string key input, the output is the same AppState as before
    
    [(string=? key "s") (start state)]                                                                   ; start the game
    
    [(or (and (string=? key "up") (string=? (snake-direction (appstate-snake state)) "down"))            ; if the direction is opposite of the key, the state is the same
         (and (string=? key "right") (string=? (snake-direction (appstate-snake state)) "left"))        
         (and (string=? key "down") (string=? (snake-direction (appstate-snake state)) "up"))           
         (and (string=? key "left") (string=? (snake-direction (appstate-snake state)) "right"))) state]
    
    [(or (string=? key "up") (string=? key "right") (string=? key "down") (string=? key "left"))         ; if the input is one of 'up', 'right', 'down' or 'left'
     (make-appstate                                                                                      ; create a new appstate where :
      (change-snake-direction key (appstate-snake state))                                                  ; the new snake is returned by the function to change the snake's direction
      (appstate-apple state)                                                                               ; the apple's appstate is the same
      (appstate-game state)                                                                                ; the game's appstate is the same
      (appstate-quit state)                                                                                ; the quit's appstate is the same
      (appstate-tick state)                                                                                ; the tick's appstate is the same
      (appstate-rate state))]                                                                              ; the rate's appstate is the same
    
    [(string=? key "r") (reset state)]                                                                   ; reset the game
    
    [(string=? key "escape")
     (make-appstate                                                                                     ; create a new appstate where :
      (appstate-snake state)                                                                               ; the snake's appstate is the same
      (appstate-apple state)                                                                               ; the apple's appstate is the same
      (appstate-game state)                                                                                ; the game's appstate is the same
      #true                                                                                                ; the quit become true so quit the game
      (appstate-tick state)                                                                                ; the tick's appstate is the same
      (appstate-rate state))]                                                                              ; the rate's appstate is the same
    
    [else state]))                                                                                       ; for any other input the appstate is the same


;;;;;;;;;; EATING ;;;;;;;;;;
; eating : AppState -> AppState
; when the apple is eaten by the snake,
; it will become more longer and the apple's position will update
; It will also play the eaten sound
; Header (define (eating DEFAULT) DEFAULT)

; Code
(define (eating state)
  (begin (play-sound EATEN #true)
         (make-appstate                                                                                                                                      ; create a new appstate where :
          (move-snake                                                                                                                                          ; the snake moves
           (make-snake                                                                                                                                         ; and it is composed by:
            (cons (first (snake-position (appstate-snake state))) (snake-position (appstate-snake state)))                                                       ; both position of previous snake and apple
            (add1 (snake-length (appstate-snake state)))                                                                                                         ; the snake's length is greater than one
            (snake-direction (appstate-snake state))))                                                                                                           ; the snake's direction is the same
          (compute-apple-position (random 401) 1 (compute-available-pos (cons (appstate-apple state) (snake-position (appstate-snake state))) BACKGROUNDPOS))  ; the apple's position is changed
          (appstate-game state)                                                                                                                                ; the game's appstate is the same
          (appstate-quit state)                                                                                                                                ; the game's appstate is the same
          (appstate-tick state)                                                                                                                                ; the game's appstate is the same
          (if (and (> (sub1 (appstate-rate state)) 0) (= 0 (remainder (- 3 (snake-length (appstate-snake state))) 3)))                                         ; then check whether to increase the speed :
              (sub1 (appstate-rate state))                                                                                                                          ; the rate is less than one
              (appstate-rate state)))))                                                                                                                             ; the rate's appstate is the same


;;;;;;;;;; MOVE ;;;;;;;;;;
; move: AppState -> AppState
; moves the snake using an auxiliary function
; write state: TICK (adds one everytime it is called)
; read state:  RATE 
; Header (define (move state) DEFAULT)

; Code
(define (move state)
  (cond                                                                                                        ; if the game is off, the snake must not move
    [(end? state) (make-appstate                                                                               ; giving an appstate as before where :
                   (appstate-snake state)                                                                        ; the snaek's state is the same
                   (appstate-apple state)                                                                        ; the apple's state is the same
                   #false                                                                                        ; the game is false
                   #false                                                                                        ; the quit is false
                   (appstate-tick state)                                                                         ; the tick's state is the same
                   (appstate-rate state))]                                                                       ; the rate's state is the same
    [(not (appstate-game state)) state]                                                                        ; 
    [else (cond
            [(= (remainder (appstate-tick state) (appstate-rate state)) 0)
             (cond
               [(equal? (last (snake-position (appstate-snake state))) (appstate-apple state)) (eating state)] ; if the snake is eating call the function to upgrade both its length and apple's position
               [else  (make-appstate                                                                           ; every tick the appstate is moved and it creates a new update where it is made by :
                       (move-snake (appstate-snake state))                                                       ; the new position of the snake
                       (appstate-apple state)                                                                    ; the apple's state is the same
                       (appstate-game state)                                                                     ; the game's state is the same
                       (appstate-quit state)                                                                     ; the quit's state is the same
                       (add1 (appstate-tick state))                                                              ; the tick's state is greater than one
                       (appstate-rate state))])]                                                                 ; the rate's state is the same
            [else (make-appstate                                                                               ; otherwise give the same appstate as before but add 1 to tick
                   (appstate-snake state)
                   (appstate-apple state)
                   (appstate-game state)
                   (appstate-quit state)
                   (add1 (appstate-tick state))
                   (appstate-rate state))])]))


;;;;;;;;;; QUIT ;;;;;;;;;;
; quit: AppState -> AppState
; changes the quit in the appstate
; Header (define (quit state) DEFAULT)

; Code
(define (quit state)
  (appstate-quit state)) ; return the quit's appstate


;;;;;;;;;; MAIN APPLICATIONS ;;;;;;;;;;

; the default AppState
(define DEFAULT (make-appstate SNAKE APPLE GAME-F QUIT-F TICK RATE))

(define (snake-game appstate) 
  (big-bang appstate
    [to-draw draw-game]         ; draw the home; then snake, apple and score and finally game over on the background
    [on-key handle-keyboard]    ; start the game and then change snake's direction, reset game or quit the game
    [on-tick move FASTSPEED]    ; update snake's position
    [name "Snake Game"]         ; give a name to the game's display
    [close-on-stop #true]       ; close the window when the application closes
    [stop-when quit]))          ; quit the application