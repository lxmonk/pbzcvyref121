(define nl newline)
(define nop (lambda var (if #f (raise exception))))
(define nop2 (lambda var (if #f (raise exception))))
(define not-nil (lambda (el) (not (null? el))))


(define (d sexpr dep)
  (cond ((not (list? sexpr)) dep)
        ((null? sexpr) dep)
        (else (max (d (car sexpr) (+ 1 dep))
                   (d (cdr sexpr) dep)))))

(define (depth el)
  (d el 0))

(define mx
  (lambda (lst)
    (apply max lst)))

(define done?
  (lambda (sexpr)
    (> 2 (depth sexpr))))

(define (replace-el-2d-list new-el idx lst)
  (cond ((< idx 0) lst)
        ((zero? idx) (cons new-el (cdr lst)))
        (else (cons (car lst) (replace-el-2d-list new-el
                                                  (1- idx)
                                                  (cdr lst))))))

(define (cons-2d-list ins idx lst)
  ;;>(cons-2d-list '(a b) 1 '(() () () (c d)))
  ;;(() ((a b)) () (c d)
  ;;>(cons-2d-list '(g1 (C D)) 1 '((g0 (a b)) ((g2 (e f))) () ((g3 (g h)))))
  ;;((g0 (a b)) ((g1 (C D)) (g2 (e f))) () ((g3 (g h))))
  (cond ((< idx 0) lst)
        ((zero? idx) (cons (cons ins (car lst)) (cdr lst)))
        (else (cons (car lst) (cons-2d-list ins (1- idx) (cdr lst))))))

(define (replace-list lst)
  (cond ((null? lst) '())              ; end of ret
         ((= 0 (depth (car lst)) (cons
                                  (list (car lst) '())
                                  (replace-list (cdr lst)))))
         (else (let ((sym (gensym)))
                 (cons (list sym (list sym (car lst)))
                       replace-list (cdr lst))))))

(define (replace sexpr)
  (let* ((operator (car sexpr))
         (operands (cdr sexpr))
         (operator-done (= 0 (depth operator)))
         (operands-done (= 0 (apply max (map depth operands)))))
    (cond ((and operator-done operands-done)
           ;; the whole sexpr needs replacing (i.e '(a b))
           (let ((sym (gensym)))
             (list sym (list sym sexpr))))
          ((and (not operator-done) (not operands-done));TODO:
           )
          (operands-done (let ((sym (gensym)))
                           (list (cons sym operands)
                                 (list sym operator))))
          (operator-done
           (let*
               ((new-operands-and-symbols
                 (replace-list operands))
                (new-operands
                 (map car new-operands-and-symbols))
                (symbols-to-push
                 (filter not-nil (map cadr new-operands-and-symbols))))
             (list (cons operator new-operands)
                   symbols-to-push)))
          (else (raise "replace - else clause called.")))))

(define (replaced-and-pushed-sexpr row)
  ;; (display `("\nREPLACED-AND-PUSHED-SEXPR row=" ,row "\n"))
  (cond ((null? row) row)
        ((= 0 (depth (car row))) (cons (list (car row) '())
                                 (replaced-and-pushed-sexpr (cdr row))))
        (else (cons (replace (car row))
                    (replaced-and-pushed-sexpr (cdr row))))))

(define (push-deeper row)
  ;;return a list whose car is the first row in its final form (with
  ;;symbols), and the cadr is the next row (before needed
  ;;substitutions).
  (let* ((pairs-row (replaced-and-pushed-sexpr row))
         (this-row (map car pairs-row))
         (next-row (map cadr pairs-row)))
    (cons this-row (filter (lambda (el) (not (null? el))) next-row))))

(define (rec-push-down lst)
  (if (null? (cdr lst))                 ; this is the last row
      lst ;; TODO: needs attention??
      (let* ((this-and-next-rows (push-deeper (car lst)))
             (this-row-updated (car this-and-next-rows))
             (next-row (cadr this-and-next-rows)))
        (cons this-row-updated (rec-push-down (cons next-row (cddr lst)))))))


(define parallelize
  (lambda (sexpr-list)
    (let* ((dep (mx (map depth sexpr-list)))
           (init-list (make-list (1+ dep) '()))
           (decon-list (append (list sexpr-list) (cdr init-list)))
           (newlist (rec-push-down decon-list)))
      newlist)))
;;      (move-up-in-vec!
      ;;       (lambda (vec idx)
      ;;         (display `("move-up! vec=",vec "\nidx=",idx)) (newline)
      ;;         (let ((mapped-list
      ;;                (map (lambda (el) (push-up-if-needed! vec el idx))
      ;;                     (vector-ref vec idx))))
      ;;           (display `("\n\n" "mapped-list: " ,mapped-list "\n\n"))
      ;;           (vector-set! vec idx mapped-list)))))
      ;; (letrec ((do-it (lambda (f i i-max di)
      ;;                   (if (< i i-max)
      ;;                       (begin (f i)
      ;;                              (do-it f (+ di i) i-max di))
      ;;                       (f i)))))

      ;;   (do-it (lambda (row-num)
      ;;            (move-up-in-vec! v (+ (- dep row-num) 1)))
      ;;          1 (+ dep 1) 1) ;; check iter edges!
      ;;   (newline) (display newlst) (newline)
      ;;                                   ;        (list v "_" newlst)
      ;;   (display v) (nl)
      ;;   ))))

(define (last lst)
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))

(define ex1 '(a (b c) (d (e f)) (g h) (i (j k))))
(define ex2 '(a1 (a2 (a3 (a4 (a5 x))))))
(define ex3 '(((a b) (c d)) ((e f) (g h))))
