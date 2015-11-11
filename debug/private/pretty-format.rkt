#lang racket

(provide pretty-fprintf
         pretty-printf
         pretty-eprintf)

(require racket/pretty)
(module+ test
  (require rackunit))

(struct formatted (fmt vs)
  #:methods gen:custom-write
  [(define (write-proc this out mode)
     (apply fprintf out (formatted-fmt this) (formatted-vs this)))])

(define (pretty-fprintf out fmt . vs)
  (pretty-print (formatted fmt vs) out))

(define (pretty-printf fmt . vs)
  (pretty-print (formatted fmt vs)))

(define (pretty-eprintf fmt . vs)
  (pretty-print (formatted fmt vs) (current-error-port)))

(module+ test
  (test-case "pretty-fprintf"
    (define out (open-output-string))
    (parameterize ([pretty-print-columns 56])
      (pretty-fprintf out
                      "~a = ~v"
                      'x
                      '(define (f n)
                         (for/list ([i (in-range n)] #:when (odd? i))
                           (* 2 i)))))
    (check-equal? (get-output-string out)
                  #<<"
x = '(define (f n)
       (for/list
        ((i (in-range n)) #:when (odd? i))
        (* 2 i)))

"
                  )))
