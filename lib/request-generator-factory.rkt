#lang racket

(require net/http-client)
(require net/uri-codec)

(provide create-request-generator)


(define read-response
  (lambda (response [first-iter #t])
    (let ([line (read-line response 'any)])
      (cond
        [(eof-object? line) '()]
        [first-iter
          (string-join
            (cons line
                  (read-response response #f)))]
        [else (cons line (read-response response #f))]))))

(define encode-query
  (lambda (query [print #f])
    (let ([enc-query
            (uri-path-segment-encode
              (string-replace query #px"\\s+" "/**/"))])
      (if print
        (displayln enc-query)
        (void))
      enc-query)))

(define did-query-succeed?
  (lambda (str-response success-regex [print #f])
    (not
      (not
        (regexp-match
          (regexp success-regex)
          str-response)))))

(define create-request-generator
  (lambda (http-method host path data-format-string success-regex [print #f])
     (cond
       [(equal? (string-downcase http-method) "get")
        (lambda (query)
          (let-values ([(status headers response)
                        (http-sendrecv
                          host
                          (string-append
                            path
                            "?"
                            (encode-query
                              (format data-format-string query) 
                              print)))])
            (did-query-succeed?
              (read-response response)
              success-regex
              print)))]
       [(equal? (string-downcase http-method) "post")
        (lambda (query)
          (let-values ([(status headers response)
                        (http-sendrecv
                          host
                          path
                          #:method "POST"
                          #:data (encode-query
                                   (format data-format-string query)
                                   print))])
            (did-query-succeed?
              (read-response response)
              success-regex
              print)))]
       [else
         (begin
           (displayln "HTTP method not supported at this time")
           (exit))])))

