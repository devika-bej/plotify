#lang racket

(require eopl)
(require rackunit)
(require racket/string)
(require racket/match)
(require rackunit/text-ui)

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

(define-datatype feature feature? [make-feature (op is-feature-op?) (val (or string? number?))])

(define-datatype
 shape
 shape?
 [make-shape (op is-shape-op?) (arg-list (list-of number?)) (feat-list (list-of feature?))])

(define (butlast lst)
  (reverse (cdr (reverse lst))))

(define (group-commands commands shape-set cur-shape)
  (cond
    [(empty? commands) (reverse (cons cur-shape shape-set))]
    [else
     (let ([cur (car commands)])
       (cond
         [(is-shape-op? (car cur))
          (group-commands (cdr commands)
                          (if (null? cur-shape)
                              shape-set
                              (cons cur-shape shape-set))
                          (append cur (list '())))]
         [(is-feature-op? (car cur))
          (group-commands (cdr commands)
                          shape-set
                          ((lambda (lst ele)
                             (let* ([outer (butlast lst)]
                                    [inner (last lst)]
                                    [new-inner (append inner (list ele))])
                               (append outer (list new-inner))))
                           cur-shape
                           cur))]))]))

(define-datatype ast
                 ast?
                 [graph (vals (list-of shape?))]
                 [shape-val (op is-shape-op?) (arg-list ast?) (feat ast?)]
                 [arg-list (vals (list-of number?))]
                 [feat (op is-feature-op?) (val (or string? number?))])

(define (is-shape-op? k)
  (match k
    ["circle" #t]
    ["ellipse" #t]
    ["line" #t]
    ["poly" #t]
    ["point" #t]
    ["sin" #t]
    ["cos" #t]
    [_ #f]))

(define (is-feature-op? k)
  (match k
    ["color" #t]
    ["opacity" #t]
    ["thick" #t]
    [_ #f]))

(define (interpret str)
  (define commands (pre-process (bytes->string/utf-8 str)))
  (define grouped-commands (group-commands commands '() null))
  (display (list-ref grouped-commands 100)))

(provide interpret)

#|
sample input
circle 0 0 5
color #12ac56
line 0 0 0 -2
color #12ac56
thick 1.2
poly 0 0 2 2 0 2
opacity 0.4

|#

#|

shape ops
circle c_x c_y r
ellipse c_x c_y r_x r_y [angle=0]
line x_1 y_1 x_2 y_2
poly x_1 y_1 x_2 y_2 ... x_n y_n
point x y
sin [c_x=0] [c_y=0] [height=1] [width=pi] [angle=0]
cos [c_x=0] [c_y=0] [height=1] [width=pi] [angle=0]

feature ops
color [hex_code] or [rgb_code]
opacity [decimal val]
thick [decimal val]

|#
