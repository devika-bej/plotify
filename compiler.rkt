#lang racket

(require eopl)
(require json)
(require racket/dict)
(require racket/match)
(require racket/string)
(require rackunit)
(require rackunit/text-ui)

;;; General utility functions

(define (butlast lst)
  (reverse (cdr (reverse lst))))

(define (enumerate lst)
  (map list (range (length lst)) lst))

;;; Datatypes and their utilities

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

(define-datatype
 shape
 shape?
 [make-shape (op is-shape-op?) (arg-list (list-of number?)) (feat-list (list-of feature?))])

(define (is-feature-op? k)
  (match k
    ["color" #t]
    ["opacity" #t]
    ["thick" #t]
    [_ #f]))

(define (is-feature-val? k)
  (or (string? k) (number? k)))

(define-datatype feature feature? [make-feature (op is-feature-op?) (val (or string? number?))])

(define-datatype ast
                 ast?
                 [graph (op is-graph-op?) (vals (list-of ast?))]
                 [shape-val (op is-shape-op?) (arg-list ast?) (feat-list ast?)]
                 [arg-list (vals (list-of number?))]
                 [feat-list (vals (list-of ast?))]
                 [feat (op is-feature-op?) (val is-feature-val?)])

;;; Shape evaluators

(define (eval-circle c_x c_y r)
  (hash 'latex (format "\\left(x - ~a\\right)^{2} + \\left(y - ~a\\right)^{2} <= \\left( ~a \\right)^{2}" c_x c_y r)))

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

(define (eval-point x y)
  (hash 'latex (format "\\left( ~a , ~a \\right)" x y)))

(define (eval-ellipse c_x c_y r_x r_y [m 0])
  (hash
   'latex
   (format
    "\\frac{ ~a \\left(y - ~a \\right) + \\left(x - ~a \\right)}{ ~a }^{2}+\\frac{\\left(y - ~a \\right) - ~a \\left(x - ~a \\right)}{ ~a }^{2} <= \\left( ~a \\right)^{2}+1"
    m
    c_y
    c_x
    r_x
    c_y
    m
    c_x
    r_y
    m)))

;;; Main processing functions

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
                     [else (error "Invalid arguments for polygon" vals)])]
                  [(equal? op "point")
                   (define vals (eval arg-list))
                   (match vals
                     [(list x y) (eval-point x y)]
                     [else (error "Invalid arguments for point" vals)])]
                  [(equal? op "ellipse")
                   (define vals (eval arg-list))
                   (match vals
                     [(list c_x c_y r_x r_y) (eval-ellipse c_x c_y r_x r_y)]
                     [(list c_x c_y r_x r_y m) (eval-ellipse c_x c_y r_x r_y m)]
                     [else (error "Invalid arguments for ellipse" vals)])])
                (eval feat-list))]
         [arg-list (vals) vals]
         [feat-list (vals) (map eval vals)]
         [feat
          (op val)
          (cond
            [(equal? op "color") (hash 'color val)]
            [(equal? op "opacity") (hash 'fillOpacity (number->string val))]
            [(equal? op "thick") (hash 'lineWidth (number->string val))])]
         [else (error "Unknown ast")]))

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

(define (compile str)
  (define commands (pre-process (bytes->string/utf-8 str)))
  (define grouped-commands (group-commands commands '() null))
  (define graph-ast (commands->ast grouped-commands))
  (define expressions (eval graph-ast))
  (define graph-json (exp->json expressions))
  graph-json)

(provide compile)