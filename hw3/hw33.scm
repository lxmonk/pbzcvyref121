(print-graph #f) ; display circular structures
(print-gensym #f) ; print gensym as g1234
(case-sensitive #f) ; ditto
(print-brackets #f) ; do not use brackets when pretty-printing


;; (define fact$
;;   (lambda (x k)
;;     (if (zero? x)
;;         (k 1)
;;         (fact$ (1- x)
;;                (lambda (n-1)
;;                  (k (* x n-1)))))))

(define (cse e)
  (create-freq-dict e '()         ; initially, '() is an empty a-list
    (lambda (freq-dict)
      (purge-dict freq-dict ; here frequencies will be replaced by gensyms
        (lambda (clean-dict)
          (replace-by-dict e clean-dict
            (lambda (new-e)
              `(let* ,(lets-from-dict clean-dict)))
            ;; no replace was made - return original expression
            e))))))

(define (create-freq-dict e freq-dict cont)
  (define (const? e)
    (or (null? e)  (symbol? e) (not (pair? e))  ;(and (pair? e) (null? (cdr e)))
        (and (pair? e) (equal? 'quote (car e)))))
  (trace create-freq-dict)
  (cond ((const? e) (cont freq-dict))
        ((assoc e freq-dict)
         (create-freq-dict
           (car e) (cons `(,e ,(1+ (cadr (assoc e freq-dict)))) freq-dict)
           (lambda (new-dict)
             (create-freq-dict (cdr e) new-dict cont))))
        (else (create-freq-dict (car e) (cons `(,e 1) freq-dict)
                (lambda (car-dict)
                  (create-freq-dict (cdr e) car-dict
                    (lambda (new-dict)
                      (cont new-dict))))))))

(define (replace-by-dict e clean-dict cont))
