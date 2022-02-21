; 😶 Rac-Man C< -  -  -
; Final Project for CS 208 Program Design
; Professor: Dr. Derrick Tate
; Stephen Taylor and Graham Weber
; December 3, 2021

; Libraries
  (require 2htdp/image)
  (require 2htdp/universe)
  ;require 2htdp/iTunes)
  (require 2htdp/abstraction)
  (require 2htdp/batch-io)

; Constants, Images & Structs
    (define scorefile (read-words/line "racman-highscores.txt"))
    (define DIAM 40)
    (define WORLDSIZE (* DIAM 17))      ; 680
    (define PATHWIDTH (round (/ DIAM 2.5)))
    (define DOTCOUNT 24)
    (define E "empty") 
    (define O "dot")   
    (define x "wall")
    (define C "cookie")

    (define playArea
        (rectangle WORLDSIZE WORLDSIZE "solid" "black"))

    (define playButton
        (overlay (text "Play" 40 "black") (rectangle 200 80 "solid" (color 255 223 0 255))))
  
    (define wallDot
        (circle (/ DIAM 10) "solid" "blue"))

    (define wallBox
      (rectangle PATHWIDTH PATHWIDTH "solid" (color 33 33 255)))

    (define dotPic
        (circle (/ DIAM 10) "solid" "pink"))

    (define cookiePic
        (circle (/ DIAM 5) "solid" "yellow"))

  ; Ghost1 image
  (define ghost2
    (bitmap "images/python-d.png"))
  
  ; Ghost2 image
  (define ghost1
    (bitmap "images/kevin-u.jpg"))
  
  ;Ghost3 image
  (define ghost3
    (rectangle 30 40 "solid" "pink"))
  ;Ghost4 image
  (define ghost4
    (rectangle 30 40 "solid" "orange"))


    (define racman-pics
      (list
          (list (bitmap "images/basic-r.png")
            (underlay/offset (bitmap "images/basic-r.png") 6 0 (rotate -27 (wedge 23.5 57 "solid" "black"))))
          (list (rotate 270 (bitmap "images/basic-r.png"))
            (rotate 270 (underlay/offset (bitmap "images/basic-r.png") 6 0 (rotate -27 (wedge 23.5 57 "solid" "black")))))
          (list (flip-horizontal (bitmap "images/basic-r.png"))
            (flip-horizontal (underlay/offset (bitmap "images/basic-r.png") 6 0 (rotate -27 (wedge 23.5 57 "solid" "black")))))
          (list (rotate 90 (bitmap "images/basic-r.png"))
            (rotate 90 (underlay/offset (bitmap "images/basic-r.png") 6 0 (rotate -27 (wedge 23.5 57 "solid" "black")))))))

      
  
  ; boardMap
    ; idea for vector map from junzew on github: https://github.com/junzew/rkt-pacman/blob/master/pacman.rkt
    (define boardMap
      (list (list x x x x x x x x x x x x x x x x x x x x x)
            (list x C O O O O O O O O x O O O O O O O O C x)
            (list x O x x O x x x x O x O x x x x O x x O x)
            (list x O O O O O O O O O O O O O O O O O O O x)
            (list x O x x O x O x x x x x x x O x O x x O x)
            (list x O O O O x O O O O x O O O O x O O O O x)
            (list x x x x O x x x x O x O x x x x O x x x x)
            (list x x x x O x O O O O O O O O O x O x x x x)
            (list x x x x O x O x x x O x x x O x O x x x x)
            (list O O O O O O O x O O O O O x O O O O O O O)
            (list x x x x O x O x x x x x x x O x O x x x x)
            (list x x x x O x O O O O O O O O O x O x x x x)
            (list x x x x O x O x x x x x x x O x O x x x x)
            (list x O O O O O O O O O x O O O O O O O O O x)
            (list x C x x O x x x x O x O x x x x O x x C x)
            (list x O O x O O O O O O O O O O O O O x O O x)
            (list x x O x O x O x x x x x x x O x O x O x x)
            (list x O O O O x O O O O x O O O O x O O O O x)
            (list x O x x x x x x x O x O x x x x x x x O x)
            (list x C O O O O O O O O O O O O O O O O O C x)
            (list x x x x x x x x x x x x x x x x x x x x x)))


  ; STRUCTS
  ; A highscores is a list-of numbers
  (define-struct menustate [highscores])

  ; x and y are numbers, curdir and nexdir are direction strings
  (define-struct racman [x y curdir nexdir])

  ; Ghost is a struct 
  ; x and y are numbers
  ; Interpretation - position of ghost 
  (define-struct ghost [x y])

  ; box is a list of posns (corners of the box)
  (define-struct mazebox [box])

  ; racman is a struct, dots is a [list-of posn] speed, lives, and points are numbers, powerup is a boolean
 (define-struct gamestate [racman ghosts dots speed powerup lives points])


; Maze Creation Functions
  ; box
    ; posn -> list of posns
    ; creates a box equidistant from a posn
    (define (box x y)
        (list (make-posn (- x (/ PATHWIDTH 2)) (- y (/ PATHWIDTH 2)))
            (make-posn (+ x (/ PATHWIDTH 2)) (- y (/ PATHWIDTH 2)))
            (make-posn (- x (/ PATHWIDTH 2)) (+ y (/ PATHWIDTH 2)))
            (make-posn (+ x (/ PATHWIDTH 2)) (+ y (/ PATHWIDTH 2)))))

(define racman-open
      (underlay/offset (bitmap "images/basic-r.png") 6 0 (rotate -27 (wedge 23.5 57 "solid" "black"))))
  

  ; dotListGenerator
    ; pathWidth is a number
    ; number -> list-of dots
    ; generates the first list of dots
    (define (dotListGenerator pathWidth)
      (filter posn? (for*/list ([i (in-range (length boardMap))]
                  [j (in-range (length (first boardMap)))])
              (if (or (string=? (list-ref (list-ref boardMap i) j) "dot")
                      (string=? (list-ref (list-ref boardMap i) j) "cookie"))
                (make-posn (+ (* j 2 pathWidth) pathWidth) (+ (* i 2 pathWidth) pathWidth)) 
                "x"))))
    (define dotList (dotListGenerator PATHWIDTH))

  ; createMaze
  ; pathWidth is a number
  ; number -> list-of mazeboxes
  ; creates the maze
  (define (createMaze pathWidth)
    (filter mazebox? (for*/list ([i (in-range (length boardMap))]
                [j (in-range (length (first boardMap)))])
            (if (string=? (list-ref (list-ref boardMap i) j) "wall") 
                (make-mazebox (box (+ (* j 2 pathWidth) pathWidth) (+ (* i 2 pathWidth) pathWidth))) "x"))))
  (define mehze (createMaze PATHWIDTH))
  
  ; Default starting state of the menu
  (define menustart (make-gamestate #false #false dotList 20 #false 3 100))


  ; Default starting state of game
  (define start
    (make-gamestate (make-racman (/ WORLDSIZE 2) (- WORLDSIZE (* 3.5 PATHWIDTH)) "space" "space")
                    (make-ghost 60 60)
                    dotList 5 #false 3 0))
 


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
            [(racman? (gamestate-racman gs)) (renderGame gs)]                                   ; Render game
            [(not (gamestate-racman gs)) (renderMenu (gamestate-points gs))]                    ; Render menu
            [(gamestate-racman gs) (place-image (text "PAUSED" 40 "white" (renderGame gs)))]))  ; Render pause

  ; renderGame
    ; a gamestate is a struct
    ; gamestate -> image
    ; renders the current state of the game
    (define (renderGame gs)
      (renderRacman (gamestate-racman gs)
        (renderGhosts (gamestate-ghosts gs)
          (renderDots (gamestate-dots gs) 
          (renderMaze mehze playArea)))))
            ;(renderBox (first (gamestate-dots gs)))


(define (renderRacman racman im)
    (place-image (list-ref (list-ref racman-pics
                      (match 
                        (racman-curdir racman) 
                        ["right" (if (= (modulo (racman-x racman) 6) 0) (0 4))]
                        ["down" (if (= (modulo (racman-y racman) 6) 0) 1 5)]
                        ["left" (if (= (modulo (racman-x racman) 6) 0) 2 6)]
                        ["up" (if (= (modulo (racman-y racman) 6) 0) 3 7)]
                        [else 0]))
                    (if))
                  (racman-x racman) 
                  (racman-y racman) 
                  im))


  (define (renderGhosts ghosts im)
    (place-image ghost1 (ghost-x ghosts) (ghost-y ghosts) im))
  
  ; renderDots
    ; [list-of dots] image -> image
    ; renders the current dots on background
    (define (renderDots lod im)
      (cond
        [(empty? lod) im]
        [else (place-image (if (and (= (posn-x (first lod)) 200) (= (posn-y (first lod)) 140)) cookiePic dotPic) 
                            (posn-x (first lod)) (posn-y (first lod))
                          (renderDots (rest lod) im))]))

; Move Functions
  ; tock
    ; gamestate -> gamestate
    ; moves Rac-Man after checking for gameover
    (define (tock gs)
      (if (not (racman? (gamestate-racman gs))) gs
        (make-gamestate
          (if (<= (gamestate-lives gs) 0) #false (moveRacman gs))
          (gamestate-ghosts gs)
          (if (empty? (gamestate-dots gs)) dotList (eatDots (gamestate-dots gs) (gamestate-racman gs)))
          (if (empty? (gamestate-dots gs)) (+ 2 (gamestate-speed gs)) (gamestate-speed gs))
          (gamestate-powerup gs) (gamestate-lives gs) (checkPoints (gamestate-dots gs)))))

  ; moveRacman
    ; gamestate -> rman
    ; moves Racman
    (define (moveRacman gs)
        (cond
            [(not (mazeCollision (moveRacmanNext (gamestate-racman gs) (gamestate-speed gs)))) 
              (moveRacmanNext (gamestate-racman gs) (gamestate-speed gs))]
            [(not (mazeCollision (gamestate-racman gs)))
              (moveRacmanCurrent (gamestate-racman gs) (gamestate-speed gs))]
            [else (make-racman
                    (match (racman-curdir (gamestate-racman gs))
                      ["left" (+ (racman-x (gamestate-racman gs)) (/ DIAM 10))]
                      ["right" (- (racman-x (gamestate-racman gs)) (/ DIAM 10))]
                      [else (racman-x (gamestate-racman gs))])
                    (match (racman-curdir (gamestate-racman gs))
                      ["up" (+ (racman-y (gamestate-racman gs)) (/ DIAM 10))]
                      ["down" (- (racman-y (gamestate-racman gs)) (/ DIAM 10))]
                      [else (racman-y (gamestate-racman gs))]) 
                    "space" (racman-nexdir (gamestate-racman gs)))]))


  ; moveRacmanNext
    ; gamestate -> rman
    ; moves Racman
    (define (moveRacmanNext rman spd)
      (match (racman-nexdir rman)
        ["up" (make-racman (racman-x rman) 
            (if (< (racman-y rman) -5) (+ 10 WORLDSIZE) (- (racman-y rman) spd)) "up" (racman-nexdir rman))]
        ["down" (make-racman (racman-x rman) 
            (if (> (racman-y rman) (+ 5 WORLDSIZE)) -10 (+ (racman-y rman) spd)) "down" (racman-nexdir rman))]
        ["left" (make-racman 
            (if (< (racman-x rman) -5) (+ 10 WORLDSIZE) (- (racman-x rman) spd)) (racman-y rman) "left" (racman-nexdir rman))]
        ["right" (make-racman 
            (if (> (racman-x rman) (+ 5 WORLDSIZE)) -10 (+ (racman-x rman) spd)) (racman-y rman) "right" (racman-nexdir rman))]
        [else rman]))

  ; moveRacmanCurrent
    ; gamestate -> rman
    ; moves Racman
    (define (moveRacmanCurrent rman spd)
      (match (racman-curdir rman)
        ["up" (make-racman (racman-x rman) (- (racman-y rman) spd) "up" (racman-nexdir rman))]
        ["down" (make-racman (racman-x rman) (+ (racman-y rman) spd) "down" (racman-nexdir rman))]
        ["left" (make-racman (- (racman-x rman) spd) (racman-y rman) "left" (racman-nexdir rman))]
        ["right" (make-racman (+ (racman-x rman) spd) (racman-y rman) "right" (racman-nexdir rman))]
        [else rman]))

  ; mazeCollision
    ; rman is a racman
    ; racman -> boolean
    ; checks if racman hit a wall
    (define (mazeCollision rman)
      (member? #t (for/list [(i (in-range (length mehze)))]
            (boxCollision (racman-x rman) (racman-y rman) (mazebox-box (list-ref mehze i))))))

    ; boxCollision
        ; posn, posn -> bool
        ; checks if racman collided with the box
        (define (boxCollision rmanx rmany box)
            (if (and (>= rmanx (- (posn-x (list-ref box 0)) 21))
                    (>= rmany (- (posn-y (list-ref box 0)) 21))
                    (<= rmanx (+ (posn-x (list-ref box 3)) 21))
                    (<= rmany (+ (posn-y (list-ref box 3)) 21))) #t #f))

    ; renderMaze
      ; lomb is a list-of mazeBoxes, im is an image
      ; lomb, im -> image
      ; render the maze
      (define (renderMaze lomb playArea)
        (cond
          [(empty? lomb) playArea]
          [else (renderMaze (rest lomb)                   ; maybe add playArea
                  (renderBox (mazebox-box (first lomb)) playArea))]))

    ; renderBox
    ; conerlist is a list-of posns, im is an image
    ; list of posns, image -> image
    ; renders 1 mazebox
    (define (renderBox cornerlist im)
      (place-image wallBox (+ (posn-x (first cornerlist)) 
                              (/ (- (posn-x (list-ref cornerlist 1)) (posn-x (first cornerlist))) 2))
                          (+ (posn-y (first cornerlist)) 
                              (/ (- (posn-y (list-ref cornerlist 2)) (posn-y (first cornerlist))) 2))
                    im))
  
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
          (make-gamestate #false #false (gamestate-dots gs) (gamestate-speed gs) (gamestate-powerup gs) (gamestate-lives gs) (gamestate-points gs))]
        [(racman? (gamestate-racman gs)) (changeDir gs key)]
        [else (menuKeys gs key)]))

  ; changeDir
    ; gs is a gamestate, key is a keyboard input
    ; gamestate, key -> gamestate
    ; changes nexdir to change next direction of racman
    (define (changeDir gs key)
      (make-gamestate 
        (make-racman (racman-x (gamestate-racman gs)) (racman-y (gamestate-racman gs)) (racman-curdir (gamestate-racman gs))
          (match key
            ["left" "left"]
            ["right" "right"]
            ["up" "up"]
            ["down" "down"]
            [else (racman-nexdir (gamestate-racman gs))]))
        (gamestate-ghosts gs) (gamestate-dots gs) (gamestate-speed gs) (gamestate-powerup gs) (gamestate-lives gs) (gamestate-points gs)))                      

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
      [on-tick tock]
      [to-draw render]
      [on-key keyhandler]))

; Start the program
(main menustart)