#lang racket

(require eopl)
(require json)
(require racket/dict)
(require racket/match)
(require racket/string)
(require rackunit)
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

(define (eval-circle c_x c_y r)
  (hash 'latex (format "\\left(x - ~a\\right)^{2} + \\left(y - ~a\\right)^{2} <= ~a" c_x c_y r)))

(define (eval-line x_1 y_1 x_2 y_2)
  (hash 'latex
        (format
         "\\left(y - ~a\\right)\\left(~a - ~a\\right) = \\left(x - ~a\\right)\\left(~a - ~a\\right)"
         y_1
         x_1
         x_2
         x_1
         y_1
         y_2)))

(define (eval-poly vals)
  (define exp "\\operatorname{polygon}\\left(")
  (define point-list
    (for/list ([(x i) (in-indexed vals)])
      ((lambda (x i)
         (cond
           [(even? i) (format "\\left( ~a" x)]
           [(odd? i) (format "~a \\right)" x)]))
       x
       i)))
  (define point-str (string-join point-list " , "))
  (hash 'latex (string-append (string-append exp point-str) "\\right)")))

(define (eval a)
  (cases ast
         a
         [graph (op vals) (map eval vals)]
         [shape-val
          (op arg-list feat-list)
          (cons (cond
                  [(equal? op "circle")
                   (define vals (eval arg-list))
                   (match vals
                     [(list c_x c_y r) (eval-circle c_x c_y r)]
                     [else (error "Invalid arguments for circle" vals)])]
                  [(equal? op "line")
                   (define vals (eval arg-list))
                   (match vals
                     [(list x_1 y_1 x_2 y_2) (eval-line x_1 y_1 x_2 y_2)]
                     [else (error "Invalid arguments for line" vals)])]
                  [(equal? op "poly")
                   (define vals (eval arg-list))
                   (cond
                     [(even? (length vals)) (eval-poly vals)]
                     [else (error "Invalid arguments for polygon" vals)])])
                (eval feat-list))]
         [arg-list (vals) vals]
         [feat-list (vals) (map eval vals)]
         [feat
          (op val)
          (cond
            [(equal? op "color") (hash 'color val)]
            [(equal? op "opacity") (hash 'fillOpacity: (number->string val))]
            [(equal? op "thick") (hash 'lineWidth: (number->string val))])]
         [else (error "Unknown ast")]))

(define (show-exp expressions)
  (map (lambda (x)
         (displayln "new")
         (map displayln x))
       expressions))

(define (show-json expressions)
  (map displayln expressions))

(define (enumerate lst)
  (map list
       (range (length lst))
       lst))

(define (exp->json expressions)
  (map (lambda (exp-ele)
         (define index (first exp-ele))
         (define exp (second exp-ele))
         (define new-exp
           (foldl (lambda (cur-exp res-exp)
                    (hash-set res-exp (first (hash-keys cur-exp)) (first (hash-values cur-exp))))
                  (hash)
                  exp))
         (hash-set (hash-set new-exp 'type "expression") 'id (number->string (add1 index))))
       (enumerate expressions)))

(define (interpret str)
  (define commands (pre-process (bytes->string/utf-8 str)))
  (define grouped-commands (group-commands commands '() null))
  (define graph-ast (commands->ast grouped-commands))
  (define expressions (eval graph-ast))
  (define graph-json (exp->json expressions))
  graph-json)

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
