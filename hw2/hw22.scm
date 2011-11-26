(define nl newline)
(define nop (lambda var (if #f (raise exception))))
(define nop2 (lambda var (if #f (raise exception))))
(define not-nil (lambda (el) (not (null? el))))
(define (short-gensym)
  (string->symbol (symbol->string (gensym))))

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
        ((= 0 (depth (car lst))) (cons (list (car lst) '())
                                       (replace-list (cdr lst))))
        (else (let ((sym (short-gensym)))
                (cons (list sym (list sym (car lst)))
                      (replace-list (cdr lst)))))))

(define (replace sexpr first?)
  (let* ((operator (car sexpr))
         (operands (cdr sexpr))
         (operator-done (>= (+ 0 first?) (depth operator)))
         (operands-done (or (null? operands)
                            (>= (+ 0 first?)
                                (apply max (map depth operands)))))
         (sym (short-gensym)))
    (cond ((and operator-done operands-done)
           ;; the whole sexpr needs replacing (i.e '(a b))
           (list sym (list sym sexpr)))
          ((and (not operator-done) (not operands-done))
           ;; (let* ((operator-symbol (short-gensym))
           ;;        (new-operands-and-symbols (replace-list operands))
           ;;        (new-operands
           ;;         (map car new-operands-and-symbols))
           ;;        (operands-symbols-to-push
           ;;         (filter not-nil (map cadr new-operands-and-symbols))))
           (list sym
                 (list sym sexpr))); (list (cons operator-symbol new-operands)

                   ;; (list (cons operator-symbol operator)
                   ;;       operands-symbols-to-push))))
          (operands-done (
                          ;; (list (cons sym operands) (list sym operator))))
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

(define (replaced-and-pushed-sexpr row first?)
  ;; (display `("\nREPLACED-AND-PUSHED-SEXPR row=" ,row "\n"))
  (cond ((null? row) row)
        ((= 0 (depth (car row)))
         (cons (list (car row) '())
               (replaced-and-pushed-sexpr (cdr row) first?)))
        (else (cons (replace (car row) (if first? 0 0))
                    (replaced-and-pushed-sexpr (cdr row) first?)))))

(define (push-deeper row first?)
  ;;return a list whose car is the first row in its final form (with
  ;;symbols), and the cadr is the next row (before needed
  ;;substitutions).
  (let* ((pairs-row (replaced-and-pushed-sexpr row first?))
         (this-row (map car pairs-row))
         (next-row (map cadr pairs-row)))
    (cons this-row (filter not-nil next-row))))

(define (rec-push-down lst first?)
  ;; (if first?
  ;;     (let* ((this-and-next-rows (push-deeper (car lst) first?))
  ;;            (this-row-updated (car this-and-next-rows))
  ;;            (next-rows (cons-2d-list (cadr this-and-next-rows) 0
  ;;                                     (cdr lst)))))
  ;;  (rec-push-down (cons this-row-updated next-rows) #f))
  (if (null? (cdr lst))             ; this is the last row
      lst                           ;; TODO: needs attention??
      (let* ((this-and-next-rows (push-deeper (car lst) first?))
             (this-row-updated (car this-and-next-rows))
             (next-rows (cons-2d-list (cadr this-and-next-rows) 0
                                      (cdr lst))))
        (cons this-row-updated
              (rec-push-down next-rows #f)))))


(define parallelize
  (lambda (sexpr-list)
    (let* ((dep (mx (map depth sexpr-list)))
           (init-list (make-list (1+ dep) '()))
           (decon-list (append (list sexpr-list) (cdr init-list)))
           (newlist (rec-push-down decon-list #t)))
      (nl) (nl) (nl) (nl)
      newlist)))



(define (trace!)
  (trace parallelize)
  (trace rec-push-down)
  (trace push-deeper)
  (trace replaced-and-pushed-sexpr)
  (trace replace)
  (trace replace-list))
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
