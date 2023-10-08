#lang racket

(require "hw6provided.rkt")
(require "hw6assignment.rkt")

(define mode (make-parameter 'enhanced))

; (define mode (make-parameter 'original))

(define (top-level)
  (command-line
   #:program "hw6runner"
   #:once-any
   ("--original" "Run the provided (base) game."
                 (mode 'original))
   ("--enhanced" "Run your enhanced version of the game game."
                 (mode 'enhanced))
   ; Uncomment this line if you do the challenge problem
   #;("--challenge" "Run your challenge problem version of the game game."
                    (mode 'challenge)))

  (println (mode))

  (match (mode)
    ['original (new tetris%)]
    ['enhanced (new my-tetris%)]
    ; Uncomment this line if you do the challenge problem
    #;['challenge (new my-tetris-challenge%)]))

(module* main #f
  (define the-game (top-level)))
