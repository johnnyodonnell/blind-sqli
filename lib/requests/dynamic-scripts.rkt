#lang racket

(provide is-dynamic-script?)
(provide get-dynamic-script)


(define script-identifier "script:")

(define is-dynamic-script?
  (lambda (str)
    (string-prefix? str script-identifier)))

(define get-dynamic-script
  (lambda (str)
    (dynamic-require
      (cadr (string-split str ":"))
      'generate-string)))

