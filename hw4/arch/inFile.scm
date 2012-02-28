;; CodeGen.compileSchemeFile("inFile.scm", "genCode.c");
;; (if #t (if (if #t (if #t (if #t #f))) #f) #f) ;; -> (Void)

;; (begin #t 1 #\c "NOT ME" '() #t) ;; -> #t
;; (or #f #f -9000 #t #f '()) ;; -> -9000
;; ((lambda () (or 1 2 3) (or #f #f -9000 #t #f '()) #f)) ;; -> -#f

;; (or #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t) ;; -> #t

;; (let ((x #t))
;;   ((lambda (p1)
;;      (set! p1 #f)
;;      p1)
;;    x)) ;; -> #f

;; ((lambda (p1 p2) p1) ((lambda () #t) '()) #f) ;; -> Error wrong argnum

;; ((lambda (p1 p2) p1) ((lambda () #t)) #f) ;; -> #t

;; ((lambda (p1) p1) #t)

;; (((lambda (x1 x2)
;;      (lambda (x3) x1))
;;     #f #t)
;;  #t)     ;; -> #f

;; (#f #f #t '()) ;; -> not a closure

;; (((lambda (x)
;;    (lambda (y)
;;      x)) #f) #t) ;; -> #f

;; (((lambda (x y)
;;     (lambda (z) x))
;;   '() #f)
;;  #t)  ;; -> '()

;; ((((lambda (x)
;;      (lambda (y)
;;        (lambda (z) x)))
;;    #f)
;;   #t)
;;  #t)  ;; -> #f

;; (((((lambda (x) (x (x x)))
;;     (lambda (x)
;;       (lambda (y)
;;         (x (x y)))))
;;    (lambda (p)
;;      (p (lambda (x)
;;           (lambda (y)
;;             (lambda (z)
;;               ((z y) x)))))))
;;   (lambda (x)
;;     ((x #t) #f)))
;;  (lambda (x)
;;    (lambda (y)
;;      x))) ;; -> #t


;; ((((lambda (x) (x (x x)))
;;    (lambda (x)
;;      (lambda (y)
;;        (x (x y)))))
;;   (lambda (p)
;;     (p (lambda (x)
;;          (lambda (y)
;;            (lambda (z)
;;              ((z y) x)))))))
;;  (lambda (x)
;;    ((x #t) #f))) ;; -> <proc>
;; ((lambda () #\d)) ;; -> #\d
;; 3478 ;; -> 3478
;; 32897439 ;; -> 32897439

;; (((((lambda (x) (x (x x)))
;;     (lambda (x)
;;       (lambda (y)
;;         (x (x y)))))
;;    (lambda (p)
;;      (p (lambda (x)
;;           (lambda (y)
;;             (lambda (z)
;;               ((z y) x)))))))
;;   (lambda (x)
;;     ((x 2329874) 1)))
;;  (lambda (x)
;;    (lambda (y)
;;      x))) ;; -> 2329874

;; 1
;; 2
;; 3
;; 4
;; 5
;; 6
;; 5

;; "ladsfjlkdsjfalk"

;; ((lambda () 'x))

;; #\a

;; #\space

;; (define *X_name_is_very_long* 8)
;; (define x 777)
;; (define x 999)
;; (define x 888)
;; ((lambda () x))
;; ;; ((lambda () y))
;; ;; ;; *x*
;; ;; "lsakdfj"
;; (define + (lambda (p1 p2) (bin+ p1 p2)))
;; (+ 3 4)
;; (+ 3 4 5)
;; x
;; (define *from_the_GE* #t)
;; *from_the_GE*
;; ;; '*from_the_GE*
;; (define - (lambda (a b) (bin- a b)))
;; (- 100000 1)

;; (define (* x y) (bin* x y))
;; (* 9 11)

;; (define (/ a b) (bin/ a b))
;; (/ 1000 4000)

;; ;; ((lambda () (bin+ 1 299)))
;; ;; ((lambda (p1 p2) (bin+ p1 p2)) 3 4)

;; (bin=? 40 4)
;; (null? 4)
;; (bin<? 1 2)
;; (bin<? 100 -1000)
;; (bin<? -1 -100)
;; (define pr '(1 . (2 . (3 . 4))))
;; pr
;; (car pr)
;; (cdr pr)

;; ((lambda (a . b) b) 1) ; -> ()
;; ((lambda (a . b) b) 1 2) ;; -> (2)
;; ((lambda (a . b) b) 1 2 3) ;; -> (2 3)
;; ((lambda (a . b) b) 1 2 3 4) ;; -> (2 3 4)

;; (define f (lambda (d e f .g) g))
;; (f 1 2 3 4) ;; -> (4)
;; (f 10 11 12 13 14 15) ;; -> (13 14 15)
;; ((lambda (a . b) (bin+ a (car b)))
;;  4 5) ;; -> 9

;; ((lambda s s) -800 -900 -20000) ;; -> (-800 -900 -20000)
;; ((lambda s s)) ;; -> ()

;; (car (list 7))


;; (define +
;;   (letrec ((loop
;;             (lambda (s)
;;               (if (null? s)
;;                   0
;;                   (bin+ (car s)
;;                         (loop (cdr s)))))))
;;     (lambda s (loop s))))
;; (+)
;; (+ 9)

;; (define (fact n)
;;   (if (bin=? 0 n)
;;       1
;;       (bin* n (fact (bin- n 1)))))
;; (fact 5) ;; -> 120


;; (boolean? #t) ;; -> #t
;; (define c #\a)
;; (char? c)
;; (char->integer c)
;; (char? c)
;; (number? c)
;; (integer->char 97)
;; (char->integer (integer->char 105))
;; "^^ 105 ^^"
;; #\a
;; (integer? 6)
;; (integer? '())
;; (number? 50000)
;; (number? #f)
;; (pair? '(3 . 4))
;; (pair? #f)
;; (procedure? (lambda (a) a))
;; (procedure? #f)
;; (string? "a string")
;; (string? #f)
;; (symbol? 'a)
;; (symbol? #f)
;; (zero? 0)
;; (zero? 3)
;; ((lambda () (zero? 0)))
;; (symbol->string (string->symbol "bkjdshf"))
;; (string->symbol #f)

;; (((lambda (a bb ccc)
;;     (lambda (dddd eeeee)
;;       a))
;;   -1 -2 -3)
;;  -4 -5) ;;-> -1

;; (((lambda (a bb ccc)
;;     (lambda (dddd eeeee)
;;       bb))
;;   -1 -2 -3)
;;  -4 -5) ;; -> -2

;; ((lambda (p)
;;    ((lambda () 0 1 1 2 3 5 8 p))) 7665667) ;; -> 7665667


;; ((((((lambda ()
;;        (lambda (a b c)
;;          (lambda () 0 1 1 2 3 5 8
;;            (lambda (d)
;;              (lambda (e f) b)))))
;;      )
;;     "a1" "b2" "c3")
;;    )
;;   "d4")
;;  "e5" "f6") ;; -> "b2"

;; (((((lambda ()
;;       (lambda (s . e)
;;         (lambda (t . n)
;;           (lambda v s))))
;;     )
;;    4 5)
;;   6)
;;  'a) ;; -> 4

;; ((((lambda (a b c)
;;      (lambda (g)
;;        (lambda (h) b)))
;;    1 2 3)
;;   4)
;;  7) ;; -> 2


;; ((((lambda ()
;;      (lambda ()
;;        (lambda () 55))))))

;; ((lambda (p)
;;    ((lambda () p))) 7665667)

;; ((lambda (p)
;;    (begin (set! p 10)
;;           ((lambda () ((lambda (a) p) #f))))) #t)

;; (letrec ((g 1)) g)


;; (letrec ((f (lambda (n) n))) f)

;; (letrec ((f
;;            (lambda (n)
;;              (if (bin=? n 0)
;;                  1
;;                  (bin* n (f (bin- n 1)))))))
;;   (f 5)) ;; -> 120

;; (define lst (list 1 2))
;; lst
;; lst
;; ((lambda s (cdr (car s))) lst)
;; lst
;; (+ 1 2 3 4 5 6)
;; (list 1 2 3)
;; (define setted (list 1 2 3 4))
;; (set-car! setted 'dlsfj)
;; setted
;; (string-length "string of length 19")
;; (string-ref "abc" 1)
;; (string-ref "abc" 0)
;; (string-ref "abc" 1)
;; (let ((s "abc"))
;;   (string-set! s 0 #\Z)
;;   s)

;; (remainder 16 4) ;; -> 0
;; (remainder 5 2)  ;; -> 1
;; (remainder -45 7)  ;; ->  -3
;; (remainder 10 -3)  ;; -> 1
;; (remainder -17 -9)  ;; -> -8

;; (eq? 'a 3) ;; -> #f
;; (eq? #t 't) ;; ->  #f
;; (eq? "abc" 'abc)  ;; -> #f
;; (eq? "hi" '(hi))   ;; ->#f
;; (eq? #f '())   ;; -> #f

;; (eq? 3 53344)   ;; -> #f
;; (eq? 3 3)   ;; -> #t

;; (let ((three 3))
;;   (eq? 3 three)) ;; -> #t


;; (eq? #\a #\b) ;; ->  #f
;; (eq? #\a #\a) ;; -> #t

;; (eq? #t #t)  ;; -> #t
;; (eq? #f #f)  ;; -> #t
;; (eq? #t #f)  ;; -> #f
;; (eq? (null? '()) #t)  ;; -> #t
;; (eq? (null? '(a)) #f)  ;; -> #t

;; (eq? (cdr '(a)) '())  ;; -> #t

;; (eq? 'a 'a)  ;; -> #t
;; (eq? 'a 'b)  ;; -> #f
;; (eq? 'a (string->symbol "a"))  ;; -> #t

;; (eq? '(a) '(b))  ;; -> #f
;; (eq? '(a) '(a)) ;; -> unspecified
;; (let ((x '(a . b))) (eq? x x))  ;; -> #t
;; (let ((x (cons 'a 'b)))
;;   (eq? x x)) ;; -> #t

;; (eq? (cons 'a 'b) (cons 'a 'b))  ;; -> #f

;; (eq? "abc" "cba")   ;; -> #f
;; (eq? "abc" "abc")   ;; -> #t
;; (let ((x "hi")) (eq? x x))   ;; ->#t
;; (let ((x (symbol->string 'hi))) (eq? x x))  ;; -> #t
;; (eq?
;;   (symbol->string 'hi)
;;   (symbol->string 'hi))  ;; -> #t



;; (eq? car car)  ;; -> #t
;; (eq? car cdr) ;; ->  #f
;; (let ((f (lambda (x) x)))
;;   (eq? f f))  ;; -> #t


;; (eq? (lambda (x) x) (lambda (y) y)) ;; -> #f

;; (let ((f (lambda (x)
;;            (lambda ()
;;              (set! x (+ x 1))
;;              x))))
;;   (eq? (f 0) (f 0))) ;; -> #f


(define +
  (letrec ((loop
            (lambda (s)
              (if (null? s)
                  0
                  (bin+ (car s)
                        (loop (cdr s)))))))
    (lambda s (loop s))))

;; (+)
;; (+ 1 2 3)

;; ((lambda (x) (x x 10))
;;  (lambda (x n)
;;    (if (zero? n) #t
;;        (x x (- n 1)))))




;; (eq? '#(a) '#(b))  ;; -> #f
;; (eq? '#(a) '#(a))   ;; -> unspecified
;; (let ((x '#(a))) (eq? x x))   ;; ->#t
;; (let ((x (vector 'a)))
;;   (eq? x x))   ;; ->#t
;; (eq? (vector 'a) (vector 'a)) ;; ->  #f

;; (symbol? (gensym))
;; (symbol? "d")
;; (string? "string")
;; (pair? #f)
;; (gensym)

;; (let ((a (gensym))
;;       (b (gensym)))
;;   (cons a b))

;; (let ((a (gensym)))
;;   (eq? a a)) ;; -> #t

;; (symbol->string (gensym))
;; (symbol->string (gensym))
;; (symbol->string (gensym))
;; (symbol->string (gensym))
;; (symbol->string (gensym))
;; (symbol->string (gensym))
;; (symbol->string (gensym))
;; (gensym)
;; (gensym)
;; (gensym)
;; (gensym)
;; (symbol->string (gensym))
;; (symbol->string (gensym))
;; (symbol->string (gensym))

(define -
  (lambda (a . s)
    (if (null? s)
        (bin- 0 a)
        (bin- a (apply + s)))))



(- 1 2 3)


;; ((lambda (a . s) s) 1 2 3)
;; ((lambda (a . s) (apply + s)) 1 2 3)
;; (+ 1 2 3 4 5)
;; (apply + '(1 2 3))
;; (apply + '(2))
;; (apply + '())
;; (bin- 1 2) ;; -> -1
;; (apply bin- '(1 2)) ;; -> -1
;; (apply bin- '(2 90)) ;; -> -88
;; (apply bin- '(1000 3)) ;; -> 997
;; (apply + (list (apply + '(4 5 6 7 100)))) ;; -> 122
