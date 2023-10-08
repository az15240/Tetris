#lang racket

;; This is the only file you turn in, so do not modify the other files as
;; part of your solution.

(require "hw6provided.rkt")
(provide my-tetris%)
(require racket/random)  ; for random-ref
; Uncomment this line if you do the challenge problem
;(provide my-tetris-challenge%)

;; Edit the two classes below to implement your enhancements.

(define my-all-shapes
  (vector
   (vector (vector '(0 . 0) '(1 . 0) '(0 . 1) '(1 . 1)))               ; square (only needs one)
   (rotations (vector '(0 . 0) '(-1 . 0) '(1 . 0) '(0 . -1)))          ; T
   (vector (vector '(0 . 0) '(-1 . 0) '(1 . 0) '(2 . 0))               ; long (only needs two)
           (vector '(0 . 0) '(0 . -1) '(0 . 1) '(0 . 2)))
   (rotations (vector '(0 . 0) '(0 . -1) '(0 . 1) '(1 . 1)))           ; L
   (rotations (vector '(0 . 0) '(0 . -1) '(0 . 1) '(-1 . 1)))          ; inverted L
   (rotations (vector '(0 . 0) '(-1 . 0) '(0 . -1) '(1 . -1)))         ; S
   (rotations (vector '(0 . 0) '(1 . 0) '(0 . -1) '(-1 . -1)))         ; Z
   (rotations (vector '(0 . 0) '(1 . 0) '(0 . 1) '(-1 . 1) '(-1 . 0))) ; first additional shape
   (vector (vector '(0 . 0) '(-1 . 0) '(-2 . 0) '(1 . 0) '(2 . 0))     ; second additional shape (long, only needs two)
           (vector '(0 . 0) '(0 . -1) '(0 . -2) '(0 . 1) '(0 . 2)))
   (rotations (vector '(0 . 0) '(1 . 0) '(0 . 1)))))                   ; third additional shape

(define my-tetris%
  (class tetris%
    (super-new)

    (inherit get-board)
    (inherit set-board!)

    ; #1
    (define/augment (on-char event)
      (define keycode (send event get-key-code))
      (match keycode
        [#\u (begin (send (get-board) rotate-clockwise) (send (get-board) rotate-clockwise))]
        [#\c (send (get-board) to-cheat #t)]
        [_ (inner #f on-char event)]))

    (define/override (reset-board)
      (set-board! (new my-board% [game this])))
    
    ))

(define my-board%
  (class board%

    (define cheating-state #f)
    
    (super-new)
    
    (inherit get-current-piece)
    (inherit get-score)
    (inherit set-score!)
    (inherit get-delay)
    (inherit set-delay!)
    (inherit get-grid)
    (inherit remove-filled)

    ; #3
    (define/public (to-cheat state)
      (if (not cheating-state) ; if cheating state is already #t, then no change, else check the scores
          (if (>= (get-score) 100)
              (begin (set! cheating-state #t) (set-score! (- (get-score) 100)))
              #f)
          #f))

    ; #2
    (define/override (select-shape)
      (if cheating-state
          (begin (set! cheating-state #f) (vector (vector '(0 . 0)))) ; reset the cheating state
          (random-ref my-all-shapes)))
        
    (define/override (store-current)
      (define points (send (get-current-piece) get-points))
      (for ([idx (in-range (vector-length points))])  ; now not all the pieces have 4 blocks. Have to change this otherwise occurs error when placing the third additional piece with only three cubes
        (match-define (cons x y) (vector-ref points idx))
        (when (>= y 0)
          (vector-set! (vector-ref (get-grid) y) x (send (get-current-piece) get-color))))
      (remove-filled)
      (set-delay! (max (- (get-delay) 2) 80)))
    ))