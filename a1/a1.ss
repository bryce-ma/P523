;; Bryce Ma
;; bryce.seeker@gmail.com
;; May 9, 2016

(load "match.ss")

(case-sensitive #t)

(optimize-level 2)

;; signed 32-bits integer
(define int32?
  (lambda (x)
    (let ([2^31 (expt 2 31)])
      (and (> x (- 1 2^31))
	   (< x (- 2^32 1))))))

;; signed 64-bits integer
(define int64?
  (lambda (x)
    (let ([2^63 (expt 2 63)])
      (and (> x (- 1 2^63))
	   (< x (- 2^63 1))))))

(define verify-scheme
  (lambda (x)
    (define Program
      (lambda (x)
	(match x
	       [(begin ,statem ,statems* ...)
		(Statement statem)
		(if (not (null? statems*))
		    `(begin ,statems* ...))]
	       [,not-prog (error 'verify "invalid program " not-prog)])))
    (define Statement
      (lambda (x)
	(match x
	       [(set! ,var1 ,var2) (guard (symbol? var2)) (Var var1) (Var var2)]  ;;handle a pair of symbol first
	       [(set! ,var1 ,int64) (guard (and (number? int64) (int64? int64))) (Var var1)]
	       [(set! ,var1 (,binop ,var1 ,var2)) (guard (symbol? var2)) (Var var1) (Var var2) (Binop binop)]
	       [(set! ,var1 (,binop ,var1 ,int32)) (guard (int32? int32)) (Var var1) (Binop binop)]
	       [(set! ,var1 (,op ,var2 ,any)) (error 'verify "first variables followed set! and binop should be same")]
	       [,not-statem (error 'verify "invalid statement " not-statem)])))
    (define Var
      (lambda (x)
	x));todo
    (define Binop
      (lambda (x)
	x));todo
    (Program x) x))

(define generate-x86-64
  (lambda (x)
    x))

(define driver
  (lambda (program)
    (with-output-to-file (number->string (random 100000))
      (lambda ()
	(generate-x86-64 
	 (verify-scheme program))))))
