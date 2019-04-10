;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname tetris) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A simple tetris clone

; requires
(require 2htdp/universe)
(require 2htdp/image)

; physical constants
(define WIDTH 10) ; # of blocks, horizontally
(define SIZE 10) ; blocks are sqaures
(define SCENE-SIZE (* WIDTH SIZE))

; graphical constants
(define BLOCK ; red squares with black rims
  (overlay
   (square (- SIZE 1) 'solid 'red)
   (square SIZE 'outline 'black)))
(define MT (empty-scene SCENE-SIZE SCENE-SIZE))

(define-struct tetris [block landscape])
(define-struct block [x y])

; A Tetris is a structure:
; (make-tetris Block Landscape)
; A Landscape is one of:
; - '()
; - (cons Block Landscape)
; A Block is a structure:
; (make-block N N)

; interpretations
; (make-block x y) depicts a block whose left
; corner is (* x SIZE) pixels from the left
; (* y SIZE) pixels from the top;
; (make-tetris b0 (list b1 b2 ...)) means b0 is the
; dropping block, while b1, b2, and ... are resting

; instance of data collection

; Tetris:
(define tetris0 (make-tetris (make-block 1 1) '()))

(define tetris1 (make-tetris (make-block 5 5)
                             (cons (make-block 2 2) '())))

(define tetris3 (make-tetris (make-block 3 3)
                             (cons (make-block 4 4)
                                   (cons (make-block 8 8)
                                         (cons (make-block 10 10)
                                               '())))))

; Landscape
(define landscape0 '())

(define landscape1 (cons (make-block 1 1) '()))

; Block
(define block-dropping (make-block 0 3))
(define block0 (make-block 0 0)) ; top left corner of grid
(define block3 (make-block 9 0)) ; top right corner
(define block1 (make-block 0 9)) ; bottom left corner
(define block2 (make-block 9 9)) ; bottom right corner

; Number -> Number
; consumes a relative position and outputs coordinates in pixels

(check-expect (the-grid 0) 5)
(check-expect (the-grid 9) 95)

(define (fn-the-grid position)
  (... ... (... position ...)))

(define (the-grid position)
  (+ 5 (* position SIZE)))

; Tetris -> Image
(check-expect (tetris-render
               (make-tetris (make-block 5 5) '()))
              (place-image BLOCK 55 55 MT))

(check-expect (tetris-render
               (make-tetris (make-block 0 0)
                            (cons (make-block 0 9)
                                  (cons (make-block 1 9)
                                        (cons (make-block 2 9)
                                              '())))))
              (place-images
               (list BLOCK BLOCK BLOCK BLOCK)
               (list (make-posn 5 5) (make-posn 5 95) (make-posn 15 95)
                     (make-posn 25 95))
               MT))
               
(define (fn-tetris-render tetraminos)
  (cond
    [(empty? (tetris-landscape tetraminos))
     (... BLOCK
          (... ... (block-x (tetris-block tetriminos)))
          (... ... (block-y (tetris-block tetriminos)))
          MT)]
    [else (... BLOCK
               (... (block-x (first (tetris-landscape tetriminos))))
               (... (block-y (first (tetris-landscape tetriminos))))
               (fn-tetris-render
                (make-tetris (tetris-block tetriminos)
                             (rest (tetris-landscape tetriminos)))))]))
                             
(define (tetris-render tetriminos)
  (cond
    [(empty? (tetris-landscape tetriminos))
     (place-image BLOCK
                  (the-grid (block-x (tetris-block tetriminos)))
                  (the-grid (block-y (tetris-block tetriminos)))
                  MT)]
    [else (place-image BLOCK
                       (the-grid (block-x (first (tetris-landscape tetriminos))))
                       (the-grid (block-y (first (tetris-landscape tetriminos))))
                       (tetris-render
                        (make-tetris (tetris-block tetriminos)
                                     (rest (tetris-landscape tetriminos)))))]))

; Tetris -> Tetris
; consumes a tetris and creates a new tetris each tick 

; the first block
(check-expect (tock (make-tetris (make-block 0 0) '()))
              (make-tetris (make-block 0 1) '())) 

(check-expect (tock (make-tetris (make-block 0 9) '()))
              (make-tetris (make-block 0 0)
                           (cons (make-block 0 9) '())))

(define (fn-tock tetriminos)
  (cond
    [(landed? tetriminos)
     (... (... (... (... (tetris-block tetriminos)
                         (tetris-landscape tetriminos)))))]
    [else (... (... (... tetriminos))
               (tetris-landscape tetriminos))]))

(define (tock tetriminos)
  (cond
    [(landed? tetriminos)
     (make-tetris
      (make-block 0 0)
      (cons (tetris-block tetriminos)
            (tetris-landscape tetriminos)))]
    [else (update-block tetriminos)]))

; Tetris -> Boolean
; consumes a tetris and returns true if the block is resting on
; the ground or another block

(check-expect (landed? (make-tetris (make-block 0 0) '())) #false)

(check-expect (landed? (make-tetris (make-block 0 9) '())) #true)

(check-expect (landed? (make-tetris (make-block 0 8)
                                    (cons (make-block 0 9) '())))
              #true)

(define (fn-landed? t)
  (cond
    [else (... (... (... (tetris-block (... t) (tetris-landscape t))))
               ...
               ...)]))

(define (landed? t)
  (cond
    [else (if (or (member? (tetris-block (update-block t))
                           (tetris-landscape t))
                  (equal? (block-y (tetris-block t)) 9))
              #true
              #false)]))

; Tetris -> Tetris
; consumes a tetris and returns an new tetris with an updated block
; position

(check-expect (update-block (make-tetris (make-block 0 0) '()))
              (make-tetris (make-block 0 1) '()))

(define (fn-update-block t)
  (... (... (block-x (tetris-block t))
            (... (block-x (tetris-block t))))
       (tetris-landscape t)))

(define (update-block t)
  (make-tetris (make-block (block-x (tetris-block t))
                           (add1 (block-y (tetris-block t))))
               (tetris-landscape t)))

; Tetris Key -> Tetris
; consumes a tetris and immediately updates the position of the
; block depending on user input, left right and down. If the block
; would collide with the wall or another block the state remains
; unchanged

(check-expect (control "" (make-tetris (make-block 0 0) '()))
              (make-tetris (make-block 0 0) '()))

(check-expect (control "right" (make-tetris (make-block 0 0) '()))
              (make-tetris (make-block 1 0) '()))

(check-expect (control "right" (make-tetris (make-block 9 0) '()))
              (make-tetris (make-block 9 0) '()))

(check-expect
 (control "right"
          (make-tetris
           (make-block 8 8)
           (cons (make-block 9 9)
                 (cons (make-block 9 8) '()))))
 (make-tetris (make-block 8 8)
              (cons (make-block 9 9)
                    (cons (make-block 9 8) '()))))

(check-expect (control "left" (make-tetris (make-block 0 0) '()))
              (make-tetris (make-block 0 0) '()))

(check-expect (control "left" (make-tetris (make-block 1 0) '()))
              (make-tetris (make-block 0 0) '()))

(check-expect
 (control "left"
          (make-tetris
           (make-block 8 1)
           (cons (make-block 0 9)
                 (cons (make-block 0 8) '()))))
 (make-tetris (make-block 8 1)
              (cons (make-block 0 9)
                    (cons (make-block 0 8) '()))))

(check-expect (control "down" (make-tetris (make-block 0 0) '()))
              (make-tetris (make-block 0 1) '()))

(check-expect
 (control "down"
          (make-tetris
           (make-block 0 8)
           (cons (make-block 0 9) '())))
 (make-tetris (make-block 0 8)
              (cons (make-block 0 9) '())))

(define (fn-control key t)
  (cond
    [(landed? t) ...]
    [(string=? key "right")
     (cond [else (if (collision? key t)
                     ...
                     (... (... (... (... (block-x (tetris-block t)))
                                    (block-y (tetris-block t)))
                               (tetris-landscape t))))])]
    [(string=? key "left")
     (cond [else (if (collision? key t)
                     ...
                     (... (... (... (... (block-x (tetris-block t)))
                                    (block-y (tetris-block t)))
                               (tetris-landscape t))))])]
    [(string=? key "down")
     (... (... (... (block-x (tetris-block t))
                    (... (block-y (tetris-block t))))
               (tetris-landscape t)))]
    [else ...]))
                                   
(define (control key t)
  (cond
    [(landed? t) t]
    [(string=? key "right")
     (cond
       [else
        (if (collision? t key)
            t
            (tetris-render
             (make-tetris
              (make-block (add1 (block-x (tetris-block t)))
                          (block-y (tetris-block t)))
              (tetris-landscape t))))])]
    [(string=? key "left")
     (cond
       [else
        (if (collision? t key)
            t
            (tetris-render
             (make-tetris
              (make-block (sub1 (block-x (tetris-block t)))
                          (block-y (tetris-block t)))
              (tetris-landscape t))))])]
    [(string=? key "down")
     (tetris-render
      (make-tetris (make-block (block-x (tetris-block t))
                               (add1 (block-y (tetris-block t))))
                   (tetris-landscape t)))]
    [else t]))
                                             

; Tetris Key -> Boolean
; consumes a tetris and a key and returns true if there would be a
; collision after resolving the key press

(check-expect (collision?
               (make-tetris (make-block 9 0) '()) "right")
              #true)

(check-expect (collision?
               (make-tetris (make-block 8 8)
                            (cons (make-block 9 9)
                                  (cons (make-block 9 8) '()))) "right")
              #true)

(check-expect (collision?
               (make-tetris (make-block 5 5) '()) "right")
              #false)

(check-expect (collision?
               (make-tetris (make-block 0 0) '()) "left")
              #true)

(check-expect (collision?
               (make-tetris (make-block 1 8)
                            (cons (make-block 0 9)
                                  (cons (make-block 0 8) '()))) "left")
              #true)

(check-expect (collision?
               (make-tetris (make-block 5 5) '()) "left")
              #false)

;(define (fn-collision? t key)
;  (cond
;    [(or (and (equal? key "right")
;          (equal? (block-x (tetris-block t)) 9))
;         (member? (make-tetris (make-block
;                                (add1 (block-x (tetris-block t)))
;                                (block-y (tetris-block t))))
;                  (tetris-landscape t)))
;     #true]
;    [(or (and (equal? key "left")
;          (equal? (block-x (tetris-block t)) 0))
;         (member? (make-tetris (make-block
;                                (sub1 (block-x (tetris-block t)))
;                                (block-y (tetris-block t))))
;                  (tetris-landscape t)))
;     #true]
;    [else #false]))

(define (collision? t key)
  (cond
    [(or (and (equal? key "right")
              (equal? (block-x (tetris-block t)) 9))
         (member? (make-tetris (make-block
                                (add1 (block-x (tetris-block t)))
                                (block-y (tetris-block t)))
                               (tetris-landscape t))
                  (tetris-landscape t)))
     #true]
    [(or (and (equal? key "left")
              (equal? (block-x (tetris-block t)) 0))
         (member? (make-tetris (make-block
                                (sub1 (block-x (tetris-block t)))
                                (block-y (tetris-block t)))
                               (tetris-landscape t))
                  (tetris-landscape t)))
     #true]
    [else #false]))

; Tetris -> Tetris
; launches the program from some initial state s

(define (tetris-main rate)
  (big-bang (make-tetris (make-block 0 0) '())
    [on-tick tock rate]
    [to-draw tetris-render]
    ;[on-key control]
    ;[stop-when last-world? last-picture]
    [state #t]
    [name "Tetris"]))

; usage
;(tetris-main 0.2)





















