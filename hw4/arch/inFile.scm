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
;;     '() #t)
;;    #t)     ;; -> #f
;; (#f #f #t '()) ;; -> not a closure

;; (((lambda (x)
;;    (lambda (y)
;;      x)) #f) #t) ;; -> #f

;; (((lambda (x y)
;;    (lambda (z) x))
;;  '() #f)
;; #t)  ;; -> '()

;; ((((lambda (x)
;;      (lambda (y)
;;         (lambda (z) x)))
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

(letrec ((g 1)) g)
;; (letrec ((f (lambda (n) n))) f)

;; (letrec ((f
;;            (lambda (n)
;;              (if (bin=? n 0)
;;                  1
;;                  (bin* n (f (bin- n 1)))))))
;;   (f 4))

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
