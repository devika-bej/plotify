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
    [(empty? commands) (cons "graph" (list (reverse (cons cur-shape shape-set))))]
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
                 [graph (op is-graph-op?) (vals (list-of ast?))]
                 [shape-val (op is-shape-op?) (arg-list ast?) (feat-list ast?)]
                 [arg-list (vals (list-of number?))]
                 [feat-list (vals (list-of ast?))]
                 [feat (op is-feature-op?) (val is-feature-val?)])

(define (commands->ast commands)
  (match commands
    [(list op (list shapes ...))
     #:when (is-graph-op? op)
     (graph op (map commands->ast shapes))]
    [(list op args ... feat-list)
     #:when (is-shape-op? op)
     (shape-val op (commands->ast args) (commands->ast feat-list))]
    [vals
     #:when (and (list? vals) (andmap number? vals))
     (arg-list vals)]
    [vals
     #:when (or (null? vals)
                (and (list? vals)
                     (andmap (lambda (x) (and (list? x) (is-feature-op? (car x)))) vals)))
     (feat-list (map commands->ast vals))]
    [(list op val)
     #:when (is-feature-op? op)
     (feat op val)]))

(define (is-graph-op? k)
  (match k
    ["graph" #t]
    [_ #f]))

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

(define (is-feature-val? k)
  (or (string? k) (number? k)))

(define (interpret str)
  (define commands (pre-process (bytes->string/utf-8 str)))
  (define grouped-commands (group-commands commands '() null))
  (displayln grouped-commands)
  (define graph-ast (commands->ast grouped-commands))
  (displayln graph-ast))

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