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

(define parallelize
  (lambda (lst)
    (let* ((dep (mx (map depth lst)))
           (v (make-vector (1+ dep) '()))
           (newlst (map (lambda (el) (put-in-place! el v 0)) lst)))
      (list v "_" newlst))))

(define (last lst)
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))

(define ex1 '(a (b c) (d (e f)) (g h) (i (j k))))
