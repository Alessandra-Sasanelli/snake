# snake
Pf1 project 2023: "Snake Game"
by Alessandra Sasanelli and Simone Maccario

MILESTONE
-------------------------------------------
    CHANGES FROM THE PROPOSAL
    - added a new datatype called Breakpoint
    - added new field breakpositions to the snake struct
    - definition of List<Posn> changed
    - AVAILABLEPOSITIONS changed name to BACKGROUNDPOS 
      (now is just the collection of all the positions on the background)
    - Position data type has not been defined
    - We didn't implement the function draw-apple; we have incorporated it
      into the draw-appstate
    - update-snake-position now called update-positions

    FUNCTIONS FOR THE POSITIONS
    - implemented function to compute background positions
    - implemented function to compute available positions
    - implemented function to compute apple position
    - implemented function to increment a posn coordinate
    - implemented function to decrement a posn coordinate
    - implemented function to update a posn based on the direction
    - implemented function to cut useless breakpoints
    - implemented function to check if a posn is before or after a breakpoint 
      (to be fixed)
    - implemented function to update a posn based on direction and List<Breakpoint>
    - implemented function to update a List<Posn> based on direction and List<Breakpoint>
    
    FUNCTIONS ABOUT DIRECTION
    - implemented function to get direction based on two posns
    - implemneted function to update snake's direction

    FUNCTIONS ABOUT MOVEMENT
    - implemented function to move snake
    - implemented function to update the appstate on tick

    DRAW
    - implemented function to draw snake on the background
    - implemented function to draw the appstate
    
    GENERALS FOR THE BIGBANG
    - implemented function to increment games tick
    - implemented function to reset the state
    - implemented function to stop the game
    - implemented function to handle the keyboard
    
    OTHERS
    - discussed on the background for the game
    - discussed on having a the entry for the game when starting the application
    - discussed on the apple style for the game
    - discussed on tracking the points for the game (based on the snake-length)

FINAL
--------------------------------------------------------------------------------
    CHANGES FROM MILESTONE
    - deleted Breakpoint
    - deleted field breakpositions to the snake struct
    - definition of List<Posn> changed
    - rewritten function update-positions
    - deleted functions for breakpoints
    - refactored code to be divided into diffent files
    - deleted function to change speed on tick

    POSITIONS FILE (positions.rkt)
    PROVIDES:
      constants: 
        - BACKGROUNDPOS
      functions:
        - make-positions --> compute background positions
        - compute-available-pos --> compute available positions
        - compute-apple-position --> compute apple position
        - increment-pos --> increment a posn coordinate
        - decrement-pos --> decrement a posn coordinate
        - direction-by-posn --> get direction based on two posns
        - update-positions --> update all elements of a list
          of posn based on last two posns direction
    NOT PROVIDED
      functions:
        - function to update a posn based on the direction
        - function to delete a posn from a list if present

    SNAKE FILE (snake.rkt)
    PROVIDES:
      constants: 
        - TAIL
        - SNAKEUNIT
        - SNAKEHEAD
        - SNAKE1
      functions:
        - move-snake --> calls the update positions from position file on the
          snake-position
        - change-snake-direction --> changes snake's head direction
        - draw-snake --> draws the snake on the background
        - check-eat-snake --> check if the snake hit itself during the game
    NOT PROVIDED
      functions:
        - rotate-el: rotates image based on direction

    GENERALS FILE (generals.rkt)
    PROVIDES:
      constants: 
        - BACKGROUND
        - GAMEBACK
        - GAME-OVER
        - HOME
        - APPLEUNIT
        - EATEN
        - DEATH
        - UP
        - DOWN
        - RIGHT
        - LEFT
        - GAME-T
        - GAME-F
        - QUIT-T
        - QUIT-F
      functions:
        - last --> returns last element of a list
        - number->image --> takes a sting of numbers and returns an image
    NOT PROVIDED
      functions:
        - number->path --> returns the path of the corresponding number

    SNAKE-FINAL FILE (snake-final.rkt)
    CONSTANTS: 
      - APPLE
      - FASTSPEED
    VARIABLES:
      - TICK
      - RATE
    functions:
      - initialize-rate --> reset state variable RATE
      - draw-appstate --> draws the appstate
      - draw-end --> draws the game over screen
      - draw-game --> decide to draw wheather the home or the game
      - eating --> handles the eating of the apple
      - move --> moves the snake using an auxiliary function
      - start --> starts the game, changing the canvas
      - reset --> changes the game in the appstate
      - quit --> changes the quit in the appstate
      - handle-keyboard --> handles the keyboard events
      - end? --> stops the game

