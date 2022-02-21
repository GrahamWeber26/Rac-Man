; 😶 Rac-Man C< -  -  -
; Final Project for CS 208 Program Design
; Professor: Dr. Derrick Tate
; Stephen Taylor and Graham Weber
; December 3, 2021

; Libraries
  (require 2htdp/image)
  (require 2htdp/universe)
  (require 2htdp/iTunes)
  (require 2htdp/abstraction)
  (require 2htdp/batch-io)

; Constants, Images & Structs
  (define scorefile (read-words/line "racman-highscores.txt"))
  (define DIAM 40)
  (define DOTCOUNT 24)
  (define playArea
    (rectangle (* DIAM 20) (* DIAM 20) "solid" "black"))

  (define playButton
    (overlay (text "Play" 40 "black") (rectangle 200 80 "solid" (color 255 223 0 255))))

  (define dotPic
    (circle (/ DIAM 10) "solid" "pink"))

  (define cookiePic
    (circle (/ DIAM 5) "solid" "yellow"))

  (define racman-pic
    (circle (/ DIAM 2.5) "solid" (color 255 223 0 255)))
  
  ; vertical wall
  ; height is a number
  ; Number -> Image
  (define (vWall height)
    (overlay (rectangle (/ DIAM 2.5) height "solid" "blue")
                (overlay/offset (circle (/ DIAM 5) "solid" "blue")
                                0 height
                                (circle (/ DIAM 5) "solid" "blue"))))
  ; horizontal wall
  ; width is a number
  ; Number -> Image
  (define (hWall width)
    (overlay (rectangle width (/ DIAM 2.5) "solid" "blue")
                (overlay/offset (circle (/ DIAM 5) "solid" "blue")
                                width 0
                                (circle (/ DIAM 5) "solid" "blue"))))

  (define maze
    (overlay/offset (hWall 150) 200 100
      (overlay/offset (vWall 200) 50 0
      playArea)))

  ; Maze image from https://smst319.wordpress.com/2012/10/08/maze-game/
  (define maze1 (bitmap "images/maz1.jpg"))
  
  ; list of dots 
  (define dotList
    (list (make-posn 200 100) (make-posn 220 100) (make-posn 240 100) (make-posn 260 100)
          (make-posn 200 120)
          (make-posn 200 140)
          (make-posn 200 160)
          (make-posn 200 180)
          (make-posn 200 200) (make-posn 220 200) (make-posn 240 200) (make-posn 260 200)
          (make-posn 300 300) (make-posn 340 300) (make-posn 380 300) (make-posn 420 300) (make-posn 460 300) (make-posn 500 300) (make-posn 540 300) (make-posn 580 300)
          (make-posn 420 340)
          (make-posn 420 380)
          (make-posn 420 420)
          (make-posn 420 460)))

  ; A highscores is a list-of numbers
  (define-struct menustate [highscores])

  ; x and y are numbers, curdir and nexdir are direction strings
  (define-struct racman [x y curdir nexdir])

  ; racman is a struct, dots is a [list-of posn] speed, lives, and points are numbers, powerup is a boolean
  (define-struct gamestate [racman dots speed powerup lives points])

  ; Default starting state of the menu
  (define menustart (make-gamestate #false dotList 20 #false 3 100))     

  ; Default starting state of game
  (define start  (make-gamestate (make-racman 20 20 "right" "right") dotList 5 #false 3 0))





; Menu Functions
  ; renderMenu
    ; a scores is a highschores
    ; scores -> image
    ; Renders the menu screen with a list of high scores
    (define (renderMenu score)
        (overlay/offset
          (bitmap "images/racman-outlined.png")
          0 250
          (overlay/offset 
            (overlay/offset (text "PRESS SPACE TO BEGIN" 40 (color 255 223 0 255))
              0 100
              (text "HIGH SCORES" 30 "blue"))
                0 0
                (overlay/align "middle" "bottom" (renderScores score) playArea))))
      
  ; renderScores
    ; [list-of Numbers] -> image
    ; Renders the list of high scores
    (define (renderScores score)
      (cond
        [(or (empty? score) (not (> score 0))) (text "---" 20 "white")]
        [else (above (text (first (first scorefile)) 20 "white") (renderScores (rest scorefile)))]))
        
  ; menuKeys
    ; An ms is a menustate, a key is a keyboard input
    ; menustate -> menustate
    ; calls the functions specified by key presses
    (define (menuKeys ms key)
        (cond
            [(string=? key " ") start]
            [(string=? key "escape") (exit)]
            [else ms]))

; Render Functions     
  ; stopScreen
    ; gs is a gamestate
    ; gamestate -> image
    ; displays end screen
    (define (stopScreen gs)
      (overlay/align "middle" "middle" (text (if (empty? (gamestate-dots gs)) "YOU WIN!" "YOU LOSE") 48 "red") (render gs)))
      ; render
        ; a gamestate is a struct
        ; gamestate -> image
        ; renders either the menu or game
        (define (render gs)
          (cond
            [(racman? (gamestate-racman gs)) (renderGame gs)]
            [(not (gamestate-racman gs)) (renderMenu (gamestate-points gs))]
            [(gamestate-racman gs) (place-image (text "PAUSED" 40 "white" (renderGame gs)))]))

  ; renderGame
    ; a gamestate is a struct
    ; gamestate -> image
    ; renders the current state of the game
    (define (renderGame gs)
      (place-image racman-pic (racman-x (gamestate-racman gs)) (racman-y (gamestate-racman gs))
                  (renderDots (gamestate-dots gs) maze)))

  ; renderDots
    ; [list-of dots] image -> image
    ; renders the current dots on background
    (define (renderDots lod im)
      (cond
        [(empty? lod) im]
        [else (place-image (if (and (= (posn-x (first lod)) 200) (= (posn-y (first lod)) 140)) cookiePic dotPic) (posn-x (first lod)) (posn-y (first lod))
                          (renderDots (rest lod) im))]))

; Move Functions
  ; move
    ; gamestate -> gamestate
    ; moves Rac-Man after checking for gameover
    (define (move gs)
      (if (not (racman? (gamestate-racman gs))) gs
        (make-gamestate
          (if (<= (gamestate-lives gs) 0) #false (moveRacman gs))
          (if (empty? (gamestate-dots gs)) dotList (eatDots (gamestate-dots gs) (gamestate-racman gs)))
          (if (empty? (gamestate-dots gs)) (+ 2 (gamestate-speed gs)) (gamestate-speed gs))
          (gamestate-powerup gs) (gamestate-lives gs) (checkPoints (gamestate-dots gs)))))

  ; moveRacman
    ; gamestate -> rman
    ; moves Racman
    (define (moveRacman gs)
      (match (mazeCollision (gamestate-racman gs) (gamestate-dir gs))
        [(or '(#false #false) '(#true #false)) (moveRacmanNext (gamestate-racman gs) (gamestate-speed gs))]
        ['(#true #false) (moveRacmanCurrent (gamestate-racman gs) (gamestate-speed gs))]
        [else (gamestate-racman gs)]))

  ; moveRacmanNext
    ; gamestate -> rman
    ; moves Racman
    (define (moveRacmanNext rman spd)
      (cond
        [(string=? "up" (racman-nexdir rman))
                          (make-racman (racman-x rman) (- (racman-y rman) spd) "up" (racman-nexdir rman))]
        [(string=? "down" (racman-nexdir rman))
                          (make-racman (racman-x rman) (+ (racman-y rman) spd) "down" (racman-nexdir rman))]
        [(string=? "left" (racman-nexdir rman))
                          (make-racman (- (racman-x rman) spd) (racman-y rman) "left" (racman-nexdir rman))]
        [(string=? "right" (racman-nexdir rman))
                          (make-racman (+ (racman-x rman) spd) (racman-y rman) "right" (racman-nexdir rman))]
        [else rman]))

  ; moveRacmanCurrent
    ; gamestate -> rman
    ; moves Racman
    (define (moveRacmanCurrent rman spd)
      (cond
        [(string=? "up" (racman-curdir rman))
                          (make-racman (racman-x rman) (- (racman-y rman) spd) "up" (racman-nexdir rman))]
        [(string=? "down" (racman-curdir rman))
                          (make-racman (racman-x rman) (+ (racman-y rman) spd) "down" (racman-nexdir rman))]
        [(string=? "left" (racman-curdir rman))
                          (make-racman (- (racman-x rman) spd) (racman-y rman) "left" (racman-nexdir rman))]
        [(string=? "right" (racman-curdir rman))
                          (make-racman (+ (racman-x rman) spd) (racman-y rman) "right" (racman-nexdir rman))]
        [else rman]))


  ; mazeCollision
    ; rman is a racman
    ; racman -> boolean
    ; checks if racman hit a wall
    (define (mazeCollision rman dir)
      (list #false #false))
      ;(cond
      ;  [(string=? "up" dir) #true]
        ; [(and (string=? "down" (racman-nexdir rman)) ()) #true]
        ; []
        ; []
        ; [else #false]))  
  
  ; eatDots
    ; We need to abstract this function to work for ghosts too
    ; lod is a list-of Dots, rman is a racman
    ; [list-of Dots] rman -> [list-of Dots]
    ; Creates the list of remaining dots
    (define (eatDots lod rman)
      (cond
        [(empty? lod) '()]
        [else (if (ateDot? (first lod) rman)
            (remove (first lod) lod)
            (cons (first lod) (eatDots (rest lod) rman)))]))

  ; ateDot?
    ; d is a posn, rman is a racman
    ; posn, rman -> boolean
    ; checks if racman collided with the dot
    (define (ateDot? d rman)
      (if (and
            (< (abs (- (racman-x rman) (posn-x d))) 10)
            (< (abs (- (racman-y rman) (posn-y d))) 10))
          #true #false))

  ; checkPoints
    ; gs is a gamestate
    ; gamestate -> points
    ; checks for update of points
    (define (checkPoints lod)
      (- DOTCOUNT (length lod)))

; On-Key Functions
  ; keyhandler
    ; gs is a gamestate, key is a keyboard input
    ; gamestate, key -> gamestate
    ; Handles key inputs
    (define (keyhandler gs key)
      (cond
        [(string=? key "escape")
          (make-gamestate #false (gamestate-dots gs) (gamestate-speed gs) (gamestate-powerup gs) (gamestate-lives gs) (gamestate-points gs))]
        [(racman? (gamestate-racman gs)) (changeDir gs key)]
        [else (menuKeys gs key)]))

  ; changeDir
    ; gs is a gamestate, key is a keyboard input
    ; gamestate, key -> gamestate
    ; changes nexdir to change next direction of racman
    (define (changeDir gs key)
      (make-gamestate 
        (make-racman (racman-x (gamestate-racman gs)) (racman-y (gamestate-racman gs)) (racman-curdir (gamestate-racman gs))
          (cond
            [(string=? key "left") "left"]
            [(string=? key "right") "right"]
            [(string=? key "up") "up"]
            [(string=? key "down") "down"]
            [else (racman-nexdir (gamestate-racman gs))]))
        (gamestate-dots gs) (gamestate-speed gs) (gamestate-powerup gs) (gamestate-lives gs) (gamestate-points gs)))                      

  ; gameOver? No longer relevant
    ; gs is a gamestate
    ; gamestate -> boolean
    ; checks if user or or lost
    ; (define (gameOver? gs)
    ;   (if (or (empty? (gamestate-dots gs)) 
    ;           (<= (gamestate-lives gs) 0))
    ;       #true #false))

; main
  ; ws is one of:
  ;   - gamestate
  ;   - menustate
  ; worldstate -> worldstate
  ; main function
  (define (main ws)
    (big-bang ws
      [on-tick move]
      [to-draw render]
      [on-key keyhandler]))

; Start the program
(main menustart)