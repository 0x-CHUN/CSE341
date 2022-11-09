
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
    (if (> low high)
        null
        (cons low (sequence (+ low stride) high stride))))
        
(sequence 3 11 2)
(sequence 3 8 3)
(sequence 3 2 1)

(define (string-append-map xs suffix)
    (map (lambda (s) (string-append s suffix)) xs))

(string-append-map (list "dan" "dog" "curry" "dog2") ".jpg")

(define (list-nth-mod xs n)
    (define get-nth (lambda (xs n) (if (= n 0) (car xs) (get-nth (cdr xs) (- n 1)))))
    (cond 
        [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (get-nth xs (remainder n (length xs)))]))

(list-nth-mod (list 0 1 2 3 4) 2)


(define (stream-for-n-steps s n)
    (if (= n 0)
        null
        (let ([pr (s)])
            (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))

(define nats ; 自然数
  (letrec ([f (lambda (x)
                (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))
(stream-for-n-steps nats 5)

(define funny-number-stream
    (letrec ([f (lambda (x) (cons (if (= (remainder x 5) 0) (- 0 x) x)
                                (lambda () (f (+ x 1)))))])
        (lambda () (f 1))))

(stream-for-n-steps funny-number-stream 10)

(define dan-then-dog
    (letrec ([next-dan (lambda () (cons "dan.jpg" (lambda () (next-dog))))]
            [next-dog (lambda () (cons "dog.jpg" (lambda () (next-dan))))])
        (lambda () (next-dan))))
(stream-for-n-steps dan-then-dog 4)

(define (stream-add-zero s)
  (lambda ()
    (let ([pr (s)])
      (cons (cons 0 (car pr)) (stream-add-zero (cdr pr))))))
(stream-for-n-steps (stream-add-zero nats) 5)

(define (cycle-lists xs ys)
    (letrec ([f (lambda (n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                                (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

(stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 6)

(define (vector-assoc v vec)
    (letrec ([f (lambda (n)
                    (cond [(= n (vector-length vec)) #f]
                        [(pair? (vector-ref vec n))
                            (if (equal? v (car (vector-ref vec n)))
                                (vector-ref vec n)
                                (f (+ n 1)))]
                        [#t (f (+ n 1))]))])
        (f 0)))

(vector-assoc 4 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1)))

(define (cached-assoc xs n)
    (letrec ([cache (make-vector n)]
            [index 0])
        (lambda (v)
            (let ([cached-value (vector-assoc v cache)])
                (if cached-value 
                    cached-value
                    (let ([value (assoc v xs)])
                        (if value
                            (begin (vector-set! cache (remainder index n) value)
                                (set! index (+ index 1))
                                value)
                            #f)))))))

((cached-assoc (list (cons 1 2) (cons 3 4)) 3) 3)

(define-syntax while-less
    (syntax-rules (do)
        [(while-less e1 do e2)
            (begin (letrec ([e e1]
                            [f (lambda () (if (> e e2) (f) #t))])
                        (f)))]))
    