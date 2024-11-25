#lang racket

(require racket/string)

(define (pre-process str)
  (displayln str)
  (define commands (string-split str #rx"\n+"))
  (map (lambda (comm) (string-split comm)) commands))

(define (interpret str)
  (define commands (pre-process (bytes->string/utf-8 str))))

(provide interpret)

#|
sample input
circle 0 0 5
line 0 0 0 -2
poly 0 0 2 2 0 2

|#
