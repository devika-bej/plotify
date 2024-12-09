#lang racket

(require web-server/servlet
         web-server/servlet-env
         racket/serialize
         json
         "compiler.rkt")

(define (handle-post req)
  (define user-input (request-post-data/raw req))
  (displayln "Received user input:")
  (displayln user-input)
  (define output (compile user-input))
  (response/output #:mime-type #"application/json"
                   #:headers (list (header #"Access-Control-Allow-Origin" #"*"))
                   (λ (out)
                     (define json-output (jsexpr->string output))
                     (write-bytes (string->bytes/utf-8 json-output) out))))

(define (start-server)
  (serve/servlet handle-post #:port 8080 #:servlet-path "/process" #:launch-browser? #f))

(start-server)
