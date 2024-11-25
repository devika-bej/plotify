#lang racket

(require web-server/servlet
         web-server/servlet-env)

(define (handle-request req)
  (define user-input
    (request-post-data/raw req)) 
  (displayln "Received user input:")
  (displayln user-input)        
  (response/xexpr              
   `(html
     (body
      (p "Input received! Thank you!")))))

(define (start-server)
  (serve/servlet
   handle-request              
   #:port 8080                 
   #:servlet-path "/process"   
   #:launch-browser? #f))      

(start-server)
