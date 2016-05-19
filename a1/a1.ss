;; Bryce Ma
;; brycemhy@gmail.com
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
		    (Program `(begin ,statems* ...)))]
	       [,not-prog (error 'verify "invalid program ~s ~%" not-prog)])))
    (define Statement
      (lambda (x)
	(match x
	       [(set! ,var1 ,var2) (guard (symbol? var2)) (Var var1) (Var var2)]  ;;handle a pair of symbol first
	       [(set! ,var1 ,int64) (guard (and (number? int64) (int64? int64))) (Var var1)]
	       [(set! ,var1 (,binop ,var1 ,var2)) (guard (symbol? var2)) (Var var1) (Var var2) (Binop binop)]
	       [(set! ,var1 (,binop ,var1 ,int32)) (guard (int32? int32)) (Var var1) (Binop binop)]
	       [(set! ,var1 (,op ,var2 ,any)) (error 'verify "first variables followed set! and binop should be same")]
	       [,not-statem (error 'parser "invalid statement ~s ~%" not-statem)])))
    (define Var
      (lambda (x)
	(match x
	       [rax 'rax]
	       [rcx 'rcx]
	       [rdx 'rdx]
	       [rbx 'rbx]
	       [rbp 'rbp]
	       [rsi 'rsi]
	       [rdi 'rdi]
	       [r8 'r8]
	       [r9 'r9]
	       [r10 'r10]
	       [r11 'r11]
	       [r12 'r12]
	       [r13 'r13]
	       [r14 'r14]
	       [r15 'r15]
	       [,other (error 'parser "illegal registers ~s ~%" other)])))	
    (define Binop
      (lambda (x)
	(match x
	       [+ '+]
	       [- '-]
	       [* '*]
	       [,other (error 'parser "illegal operator ~s ~%" other)])))
    (Program x)  ;; verify
    x)) ;; if verifying passed then return the original program

(define generate-x86-64
  (lambda (x)
    (define Program
      (lambda (x)
	(match x
	       [(begin ,statem ,statems* ...)
		(Statement statem)
		(if (not (null? statems*))
		    (Program `(begin ,statems* ...)))]
	       [,not-prog (error 'verify "invalid program " not-prog)])))
    (define Statement
      (lambda (x)
	(match x
	       [(set! ,var1 ,var2) (guard (symbol? var2)) (printf "movq %~s, %~s ~%" (Var var2) (Var var1))] ;;handle a pair of symbol first
	       [(set! ,var1 ,int64) (guard (and (number? int64) (int64? int64))) (printf "movq $~s, %~s ~%" int64 (Var var1))]
	       [(set! ,var1 (,binop ,var1 ,var2)) (guard (symbol? var2)) (printf "~s %~s, %~s ~%" (Binop binop) (Var var2)  (Var var1))]
	       [(set! ,var1 (,binop ,var1 ,int32)) (guard (int32? int32)) (printf "~s $~s, %~s ~%" (Binop binop) int32  (Var var1))]
	       [(set! ,var1 (,op ,var2 ,any)) (error 'verify "first variables followed set! and binop should be same")]
	       [,not-statem (error 'parser "invalid statement " not-statem)])))
    (define Var
      (lambda (x)
	(match x
	       [rax 'rax]
	       [rcx 'rcx]
	       [rdx 'rdx]
	       [rbx 'rbx]
	       [rbp 'rbp]
	       [rsi 'rsi]
	       [rdi 'rdi]
	       [r8 'r8]
	       [r9 'r9]
	       [r10 'r10]
	       [r11 'r11]
	       [r12 'r12]
	       [r13 'r13]
	       [r14 'r14]
	       [r15 'r15]
	       [,other (error 'parser "illegal registers" other)])))	
    (define Binop
      (lambda (x)
	(match x
	       [+ 'addq]
	       [- 'subq]
	       [* 'imulq]
	       [,other (error 'parser "illegal operator" other)])))
    (printf ".globl _scheme_entry ~%")
    (printf "_scheme_entry: ~%")
    (Program x)
    (printf "ret ~%")))

(define driver
  (lambda (program)
    (with-output-to-file "t.s"
      (lambda ()
	(generate-x86-64 
	 (verify-scheme program))))))

(define test
  (lambda ()
    (driver '(begin
	       (set! rax 5)
	       (set! rbx 6)
	       (set! rcx 0)
	       (set! rcx (+ rcx rax))
	       (set! rcx (+ rcx rbx))
	       (set! rdx 1)
	       (set! rdx (* rdx rax))
	       (set! rdx (* rdx rbx))
	       (set! rax rdx)))))
