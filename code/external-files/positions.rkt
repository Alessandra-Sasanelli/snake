;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname positions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require racket/base)

; export all file's functions
(provide make-positions
         increment-pos
         decrement-pos
         check-position-out
         compute-available-pos
         update-positions
         cut-breakpoints
         direction-by-posn
         compute-apple-position)

;;;;;;;;;; COSTANTS ;;;;;;;;;;
; Directions
(define UP "up")
(define RIGHT "right")
(define DOWN "down")
(define LEFT "left")

; Snake struct and one examples
(define-struct snake [position length direction breakpoint])
(define SNAKE1 (make-snake (list (make-posn 13 13) (make-posn 38 13) (make-posn 63 13)) 3 RIGHT '()))


;;;;;;;;;; MAKE POSITIONS ;;;;;;;;;;
; make-positions: Number Number Number List<Posn> -> List<Posn>
; computes all the positions on the background before the game starts
; Header (define (make-positions n x y lop) List<Posn>)

; Examples
(check-expect (make-positions 395 18 2 (list (make-posn 32 21))) (list (make-posn 63 63) (make-posn 38 63) (make-posn 13 63) (make-posn 488 21) (make-posn 463 21) (make-posn 32 21)))
(check-expect (make-positions 396 23 9 (list (make-posn 32 21))) (list (make-posn 663 21) (make-posn 638 21) (make-posn 613 21) (make-posn 588 21) (make-posn 32 21)))
(check-expect (make-positions 397 12 8 (list (make-posn 32 21))) (list (make-posn 363 21) (make-posn 338 21) (make-posn 313 21) (make-posn 32 21)))
(check-expect (make-positions 398 25 4 (list (make-posn 32 21))) (list (make-posn 663 21) (make-posn 638 21) (make-posn 32 21)))
(check-expect (make-positions 399 25 12 (list (make-posn 32 21))) (list (make-posn 638 21) (make-posn 32 21)))

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


; All the positions on the background
(define BACKGROUNDPOS (make-positions 1 1 1 (list (make-posn 13 13))))


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


;;;;;;;;;; CHECK POSITION OUT ;;;;;;;;;;
; check-position-out: Posn List<Posn> -> Boolean
; checks wheather the given posn is into backgroundpos
; Header (define (check-position-out pos lop) #false)

; Examples
(check-expect (check-position-out (make-posn 363 88) BACKGROUNDPOS) #false)
(check-expect (check-position-out (make-posn 463 63) BACKGROUNDPOS) #false)
; the left limit
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
       [(equal? pos (first lop)) #false]         ; check if pos is equal to the first element of the list
       [else #true])]
    [(equal? pos (first lop)) #false]            ; if the pos is eqaul to the first element of the lop return false
    [else (check-position-out pos (rest lop))])) ; owhtewise, go ahead


;;;;;;;;;; DELETE ELEMENT ;;;;;;;;;;
; delete-el : Posn List<Posn> -> List<Posn>
; delete elements from lop if they're equal to given pos
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
    [(empty? (rest lop)) (if (equal? pos (first lop)) '() (cons (first lop) '()))] ; if rest is empty --> if first equal to pos, return '()
    [(equal? pos (first lop)) (rest lop)]                                          ; if pos equal to first --> return rest
    [else (cons (first lop) (delete-el pos (rest lop)))]))                         ; else, recursive case: delete-el gets called on rest 


;;;;;;;;;; COMPUTE AVAILABLE POSITION ;;;;;;;;;;
; compute-available-pos: List<Posn> List<Posn> -> List<Posn>
; computes available positions on the background
; Header (define (compute-available-pos lop-snake lop) '())

; Examples
(check-expect (compute-available-pos (list (make-posn 13 13) (make-posn 38 13) (make-posn 63 13)) (list (make-posn 113 13) (make-posn 13 13))) (list (make-posn 113 13)))
(check-expect (compute-available-pos (list (make-posn 38 13) (make-posn 38 68) (make-posn 38 88)) (list (make-posn 468 268) (make-posn 88 113))) (list (make-posn 468 268) (make-posn 88 113)))
(check-expect (compute-available-pos (list (make-posn 63 88) (make-posn 8 113) (make-posn 3 168)) (list (make-posn 413 388) (make-posn 363 88))) (list (make-posn 413 388) (make-posn 363 88)))
(check-expect (compute-available-pos (list (make-posn 88 88) (make-posn 88 63) (make-posn 88 38)) (list (make-posn 88 88) (make-posn 488 368))) (list (make-posn 488 368)))
(check-expect (compute-available-pos (list (make-posn 113 113) (make-posn 113 88) (make-posn 113 63)) (list (make-posn 113 113) (make-posn 113 88))) '())

; Code
(define (compute-available-pos lop-snake lop)
  (cond
    [(or (empty? lop) (empty? lop-snake)) lop] ; base case where the all the available positions are checked
    [else
     (compute-available-pos
      (rest lop-snake)
      (delete-el (first lop-snake) lop))]))    ; recursive case to call a function to delete the occupied position


;;;;;;;;;; COMPUTE APPLE POSITIONS ;;;;;;;;;;
; compute-apple-position: Number Number List<Posn> -> Apple
; computes the apple position based on available positions
; Header (define (compute-apple-position n acc lop) (make-posn 13 13))

; Examples
(check-expect (compute-apple-position 1 1 (compute-available-pos (cons (make-posn 488 488) (snake-position SNAKE1)) BACKGROUNDPOS)) (make-posn 463 488))
(check-expect (compute-apple-position 100 1 (compute-available-pos (cons (make-posn 488 363) (snake-position SNAKE1)) BACKGROUNDPOS)) (make-posn 13 388))
(check-expect (compute-apple-position 250 1 (compute-available-pos (cons (make-posn 238 188) (snake-position SNAKE1)) BACKGROUNDPOS)) (make-posn 263 188))
(check-expect (compute-apple-position 364 1 (compute-available-pos (cons (make-posn 388 38) (snake-position SNAKE1)) BACKGROUNDPOS)) (make-posn 413 38))
(check-expect (compute-apple-position 396 1 (compute-available-pos (cons (make-posn 88 13) (snake-position SNAKE1)) BACKGROUNDPOS)) (make-posn 113 13))

; Code
(define (compute-apple-position n acc lop)
  (cond
    [(or (empty? (rest lop)) (= n acc)) (first lop)]    ; case limit where n is equal to the accumulator and there are not other possible free positions
    [else
     (compute-apple-position n (+ acc 1) (rest lop))])) ; create an apple's position


;;;;;;;;;; COMPUTE DIRECTIONS ;;;;;;;;;;
; compute-new-posn: Posn Direction -> Posn
; updates the given posn to follow the right direction
; Header (define (compute-bew-posn pos dir) (make-posn 13 13))

; Examples
(check-expect (compute-new-posn (make-posn 13 38) UP) (make-posn 13 13))
(check-expect (compute-new-posn (make-posn 13 38) RIGHT) (make-posn 38 38))
(check-expect (compute-new-posn (make-posn 13 38) DOWN) (make-posn 13 63))
(check-expect (compute-new-posn (make-posn 38 13) LEFT) (make-posn 13 13))
(check-expect (compute-new-posn (make-posn 13 13) "ciao") (make-posn 13 13))

; Code
(define (compute-new-posn pos dir)
  (cond
    [(equal? dir UP) (make-posn (posn-x pos) (decrement-pos (posn-y pos)))]
    [(equal? dir DOWN) (make-posn (posn-x pos) (increment-pos (posn-y pos)))]
    [(equal? dir RIGHT) (make-posn (increment-pos (posn-x pos)) (posn-y pos))]
    [(equal? dir LEFT) (make-posn (decrement-pos (posn-x pos)) (posn-y pos))]
    [else pos]))


;;;;;;;;;; DIRECTION BY POSITION ;;;;;;;;;;
; direction-by-posn: Posn Posn -> Direction
; computes the direction given two posns
; Header (define (direction-by-posn posb posa) UP)

; Examples
(check-expect (direction-by-posn (make-posn 13 38) (make-posn 13 13)) UP)
(check-expect (direction-by-posn (make-posn 13 38) (make-posn 38 38)) RIGHT)
(check-expect (direction-by-posn (make-posn 13 13) (make-posn 13 38)) DOWN)
(check-expect (direction-by-posn (make-posn 38 38) (make-posn 13 38)) LEFT)
;(check-expect (direction-by-posn (make-posn 13 138) (make-posn 13 113)) UP)

; Code
(define (direction-by-posn posb posa)
  (cond
    [(< (posn-y posa) (posn-y posb)) UP]
    [(> (posn-x posa) (posn-x posb)) RIGHT]
    [(> (posn-y posa) (posn-y posb)) DOWN]
    [(< (posn-x posa) (posn-x posb)) LEFT]))



;;;;;;;;;; POSN AFTER? ;;;;;;;;;;
; posn-after?: Direction Direction Posn Breakpoint -> Boolean
; checks whether the posn is after the breakpoint or not
; Header (define (posn-after? last d pos br) #false)

; Examples
; same direction
; up
(check-expect (posn-after? UP UP (make-posn 13 13) (make-posn 13 13)) #false)
(check-expect (posn-after? UP UP (make-posn 13 13) (make-posn 13 38)) #true)
; right
(check-expect (posn-after? RIGHT RIGHT (make-posn 13 13) (make-posn 13 13)) #false)
(check-expect (posn-after? RIGHT RIGHT (make-posn 38 13) (make-posn 13 13)) #true)
; down
(check-expect (posn-after? DOWN DOWN (make-posn 13 13) (make-posn 13 13)) #false)
(check-expect (posn-after? DOWN DOWN (make-posn 13 38) (make-posn 13 13)) #true)
; left
(check-expect (posn-after? LEFT LEFT (make-posn 13 13) (make-posn 13 13)) #false)
(check-expect (posn-after? LEFT LEFT (make-posn 13 13) (make-posn 38 13)) #true)
; different direction
; up
(check-expect (posn-after? UP RIGHT (make-posn 13 13) (make-posn 13 13)) #false)
(check-expect (posn-after? UP RIGHT (make-posn 13 13) (make-posn 13 38)) #true)
(check-expect (posn-after? UP LEFT (make-posn 13 13) (make-posn 13 38)) #true)
; right
(check-expect (posn-after? RIGHT DOWN (make-posn 13 13) (make-posn 13 13)) #false)
(check-expect (posn-after? RIGHT DOWN (make-posn 38 13) (make-posn 13 13)) #true)
(check-expect (posn-after? RIGHT UP (make-posn 38 13) (make-posn 13 13)) #true)
; down
(check-expect (posn-after? DOWN LEFT (make-posn 13 13) (make-posn 13 13)) #false)
(check-expect (posn-after? DOWN LEFT (make-posn 13 38) (make-posn 13 13)) #true)
(check-expect (posn-after? DOWN RIGHT (make-posn 13 38) (make-posn 13 13)) #true)
; left
(check-expect (posn-after? LEFT UP (make-posn 13 13) (make-posn 13 13)) #false)
(check-expect (posn-after? LEFT UP (make-posn 13 13) (make-posn 38 13)) #true)
(check-expect (posn-after? LEFT DOWN (make-posn 13 13) (make-posn 38 13)) #true)
; any other
(check-expect (posn-after? "I'm" "a string" (make-posn 13 13) (make-posn 38 13)) #false)

; Code
(define (posn-after? last d pos br)
  (cond
    [(equal? d UP)                                                ;
     (or (< (posn-y pos) (posn-y br))                             ;
         (and (equal? last LEFT) (< (posn-x pos) (posn-x br)))    ;
         (and (equal? last RIGHT) (> (posn-x pos) (posn-x br))))] ;
    [(equal? d DOWN)                                              ;
     (or (> (posn-y pos) (posn-y br))                             ;
         (and (equal? last LEFT) (< (posn-x pos) (posn-x br)))    ;
         (and (equal? last RIGHT) (> (posn-x pos) (posn-x br))))] ;
    [(equal? d LEFT)                                              ;
     (or (< (posn-x pos) (posn-x br))                             ;
         (and (equal? last UP) (< (posn-y pos) (posn-y br)))      ;
         (and (equal? last DOWN) (> (posn-y pos) (posn-y br))))]  ;
    [(equal? d RIGHT)                                             ;
     (or (> (posn-x pos) (posn-x br))                             ;
         (and (equal? last UP) (< (posn-y pos) (posn-y br)))      ;
         (and (equal? last DOWN) (> (posn-y pos) (posn-y br))))]  ;
    [else #false]))                                               ;


;;;;;;;;;; COMPUTE POSN DIRECTION ON BREAKPOINTS ;;;;;;;;;;
; posn-dir-breakpoints: Direction Direction Posn List<Breakpoints> -> Posn
; compute the positions based on the breakpoints
; Header (posn-dir-breakpoints last d pos lob) posn)

; Examples
; same directions
(check-expect (posn-dir-breakpoints UP UP (make-posn 13 88) (list (make-posn 13 88) (make-posn 13 63) (make-posn 13 38))) (make-posn 13 63))
(check-expect (posn-dir-breakpoints RIGHT RIGHT (make-posn 113 13) (list (make-posn 88 13) (make-posn 113 13) (make-posn 138 13))) (make-posn 138 13))
(check-expect (posn-dir-breakpoints DOWN DOWN (make-posn 13 63) (list (make-posn 13 38) (make-posn 13 63) (make-posn 13 88))) (make-posn 13 88))
(check-expect (posn-dir-breakpoints LEFT LEFT (make-posn 138 13) (list (make-posn 168 13) (make-posn 138 13) (make-posn 113 13))) (make-posn 113 13))
; different directions
(check-expect (posn-dir-breakpoints UP RIGHT (make-posn 13 88) (list (make-posn 13 88) (make-posn 13 63) (make-posn 13 38))) (make-posn 38 88))
(check-expect (posn-dir-breakpoints UP RIGHT (make-posn 13 88) (list (make-posn 13 88))) (make-posn 38 88))
(check-expect (posn-dir-breakpoints RIGHT DOWN (make-posn 113 13) (list (make-posn 88 13) (make-posn 113 13) (make-posn 138 13))) (make-posn 113 38))
(check-expect (posn-dir-breakpoints DOWN LEFT (make-posn 63 63) (list (make-posn 63 38) (make-posn 63 63) (make-posn 63 88))) (make-posn 38 63))
(check-expect (posn-dir-breakpoints LEFT UP (make-posn 138 88) (list (make-posn 168 88) (make-posn 138 88) (make-posn 113 88))) (make-posn 138 63))
; empty
(check-expect (posn-dir-breakpoints UP RIGHT (make-posn 13 13) '()) (make-posn 38 13))
(check-expect (posn-dir-breakpoints DOWN LEFT (make-posn 38 13) (list (make-posn 13 13))) (make-posn 13 13))

; Code
(define (posn-dir-breakpoints last d pos lob) ;last=last direction d=current direction pos=posn lob=list of breakpoint
  (cond
    [(empty? lob) (compute-new-posn pos d)]                                                      ;
    [(equal? pos (first lob)) (compute-new-posn pos d)]                                          ;
    [(and (equal? (posn-x pos) (posn-x (first lob))) (not (posn-after? last d pos (first lob)))) ;
     (cond
       [(< (posn-y (first lob)) (posn-y pos))                                                    ;
        (compute-new-posn pos UP)]                                                               ;
       [(> (posn-y (first lob)) (posn-y pos))                                                    ;
        (compute-new-posn pos DOWN)]                                                             ;
       [else pos])]                                                                              ;
    [(and (equal? (posn-y pos) (posn-y (first lob))) (not (posn-after? last d pos (first lob)))) ;
     (cond
       [(< (posn-x (first lob)) (posn-x pos))                                                    ;
        (compute-new-posn pos LEFT)]                                                             ;
       [(> (posn-x (first lob)) (posn-x pos))                                                    ;
        (compute-new-posn pos RIGHT)]                                                            ;
       [else pos])]                                                                              ;
    [else (posn-dir-breakpoints last d pos (rest lob))]))                                        ;


;;;;;;;;;; UPDATE-POSITIONS ;;;;;;;;;;
; update-positions: Direction List<Posn> List<Breakpoint> -> List<Posn>
; updates a list of positions based on a list of breakpoints
; Header (define (update-position d lop lob) (list (make-posn 13 63) (make-posn 13 38) (make-posn 13 13)))

; Examples
(check-expect (update-positions UP (list (make-posn 13 88) (make-posn 13 63) (make-posn 13 38)) '()) (list (make-posn 13 63) (make-posn 13 38) (make-posn 13 13)))
(check-expect (update-positions RIGHT (list (make-posn 88 13) (make-posn 113 13) (make-posn 138 13)) '()) (list (make-posn 113 13) (make-posn 138 13) (make-posn 163 13)))
(check-expect (update-positions DOWN (list (make-posn 13 38) (make-posn 13 63) (make-posn 13 88)) '()) (list (make-posn 13 63) (make-posn 13 88) (make-posn 13 113)))
(check-expect (update-positions LEFT (list (make-posn 168 13) (make-posn 138 13) (make-posn 113 13)) '()) (list (make-posn 143 13) (make-posn 113 13) (make-posn 88 13)))
(check-expect (update-positions UP (list (make-posn 13 88) (make-posn 13 63) (make-posn 13 38)) (list (make-posn 13 88))) (list (make-posn 13 63) (make-posn 13 38) (make-posn 13 13)))

; Code
(define (update-positions d lop lob)
  (cond
    [(empty? (rest lop)) (cons (compute-new-posn (first lop) d) '())]                                                                                                               ; create a new head's posn
    [(empty? (rest (rest lop)))                                                                                                                                                     ; 
     (cons (posn-dir-breakpoints (direction-by-posn (first lop) (second lop)) d (first lop) lob) (update-positions d (rest lop) lob))]                                              ; recursion over lop
    [else
     (cons (posn-dir-breakpoints (direction-by-posn (first lop) (second lop)) (direction-by-posn (second lop) (third lop)) (first lop) lob) (update-positions d (rest lop) lob))])) ; 


;;;;;;;;;; CUT BREAKPOINTS ;;;;;;;;;;
; cut-breakpoints: Length Any List<Any> -> List<Any>
; cuts out last elements from a list based on the Length and on any type that matches the type of the list (wich is the tail of the snake)
; (used to cut out breakpoints from a list of posn in this program)
; Header (define (cut-breakpoints 3 (list (make-posn 13 13))))

; Examples
(check-expect (cut-breakpoints 3 2 (list 1 2 3 4 5 6)) (list 1))
(check-expect (cut-breakpoints 10 1 (list 1 2 3 4 5 6)) (list 1 2 3 4 5 6))
(check-expect (cut-breakpoints 7 1 (list 1)) '())
(check-expect (cut-breakpoints 6 5 (list 1 2 3 4 5 6)) (list 1 2 3 4))
(check-expect (cut-breakpoints 5 1 (list 3 2 1)) (list 3 2))


; Code
(define (cut-breakpoints n pos lob)
  (cond
    [(empty? lob) lob]
    [(< (- n 2) (length lob)) (cut-breakpoints n pos (reverse (rest (reverse lob))))]
    [(equal? pos (first (reverse lob))) (cut-breakpoints n pos (reverse (rest (reverse lob))))]
    [(and (empty? (rest lob)) (equal? pos (first lob))) '()]
    [else lob]))