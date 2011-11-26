(define nl newline)

(define (d sexpr dep)
  (cond ((not (list? sexpr)) dep)
        ((null? sexpr) dep)
        (else (max (d (car sexpr) (+ 1 dep))
                   (d (cdr sexpr) dep)))))

(define (depth el)
  (d el 0))

(define ex1
  '(a
    (b c)
    (d
     (e f))
    (g h)
    (i
     (j k))))

(define maxlist
  (lambda (lst)
    (letrec ((maxl (lambda (l cur)
                   (cond ((null? l) cur)
                         (else (maxl (cdr l) (max (car l) cur)))))))
      (maxl lst (car lst)))))

(define maxlist2
  (lambda (lst) (eval `(max ,@lst))))

(define mx
  (lambda (lst)
    (apply max lst)))

(define done?
  (lambda (sexpr)
    (> 2 (depth sexpr))))

(define put-in-place!
  ;; place the sexpr in the correct list of the target vector,
  ;; according to its depth.
  (lambda (sexpr target-vec offset)
    (let* ((dep (+ offset (depth sexpr)))
           (oldlist (vector-ref target-vec dep))
           (sym (gensym)))
      (if (> dep 0) (begin
                      (vector-set! target-vec dep
                                   (cons (list sym sexpr) oldlist))
                      sym)
          sexpr))))

(define (one-row-up! sexpr vec idx)
  (let* ((sym (gensym))
         (old-target-list (vector-ref vec (- idx 1))))
    (vector-set! vec (- idx 1) (cons (cons sym sexpr) old-target-list))
    sym))

(define (push-up-if-needed! vec el idx)
;  (display "push-up-if-needed!: ") (display vec)
  (let* ((symbol (car el))             ; keep the symbol
         (sexpr (cadr el))            ; without the symbol
         (ret (list symbol)))
    (display `("push-up! symbol=" ,symbol " sexpr=" ,sexpr))
    (if (not (done? sexpr))
        (begin
          (display "PUSH-UP not done: ") (display sexpr) (nl)
          (let ((operator (car sexpr))
                (operand (cdr sexpr)))
            (if (not (= 0  (depth operator)))
                (let ((op-symbol (one-row-up! (list operator) vec idx)))
                  (display `("PUSH-UP looking at operator=" ,operator
                             " op-symbol=" ,op-symbol "\n"))
                  (set! ret (list symbol op-symbol)))
                (set! ret (list symbol operator)))
            (if (not (= 0  (depth operand)))
                (begin (display `("PUSH-UP looking at operand=" ,operand "\n"
                                  "ret (before)=" ,ret))
                       (set! ret (list
                                  (car ret)
                                  (list (cadr ret)
                                        (one-row-up! operand vec idx)))))
                (set! ret (list (car ret) (list (cadr ret) operand)))))
          (display `("PUSH-UP ret=" ,ret)) ret)
        el))) ; return el unchanged - we're done

(define parallelize
  (lambda (lst)
    (let* ((dep (mx (map depth lst)))
           (v (make-vector (1+ dep) '()))
           (newlst (map (lambda (el) (put-in-place! el v 0)) lst))
           (move-up-in-vec!
            (lambda (vec idx)
              (display `("move-up! vec=",vec "\nidx=",idx)) (newline)
              (let ((mapped-list
                     (map (lambda (el) (push-up-if-needed! vec el idx))
                          (vector-ref vec idx))))
                (display `("\n\n" "mapped-list: " ,mapped-list "\n\n"))
                (vector-set! vec idx mapped-list)))))
      (letrec ((do-it (lambda (f i i-max di)
                        (if (< i i-max)
                            (begin (f i)
                                   (do-it f (+ di i) i-max di))
                            (f i)))))

        (do-it (lambda (row-num)
                 (move-up-in-vec! v (+ (- dep row-num) 1)))
               1 (+ dep 1) 1) ;; check iter edges!
        (newline) (display newlst) (newline)
                                        ;        (list v "_" newlst)
        (display v) (nl)
        ))))

(define (last lst)
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))

(define ex1 '(a (b c) (d (e f)) (g h) (i (j k))))
(define ex2 '(a1 (a2 (a3 (a4 (a5 x))))))
(define ex3 '(((a b) (c d)) ((e f) (g h))))
