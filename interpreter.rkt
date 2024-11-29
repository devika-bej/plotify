#lang racket

(require racket/string)

(define (pre-process str)
  (define commands (string-split str #rx"\n+"))
  (map (lambda (comm)
         (define split_comm (string-split comm))
         (map (lambda (word)
                (if (string->number word)
                    (string->number word)
                    word))
              split_comm))
       commands))

(define (interpret str)
  (define commands (pre-process (bytes->string/utf-8 str)))
  (display (list-ref commands 5)))

(provide interpret)

#|
sample input
circle 0 0 5
line 0 0 0 -2
poly 0 0 2 2 0 2

|#
