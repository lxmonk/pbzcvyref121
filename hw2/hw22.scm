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
          (operands-done '())
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
  (if (null? (cdr lst))             ; this is the last row
      lst                           ;; TODO: needs attention??
      (let* ((this-and-next-rows (push-deeper (car lst) first?))
             (this-row-updated (car this-and-next-rows))
             (next-rows (cons-2d-list (cadr this-and-next-rows) 0
                                      (cdr lst))))
        (cons this-row-updated
              (rec-push-down next-rows #f)))))

(define (push-down-1 first-row)
  ;; this is only for the first run (no symbols like the other
  ;; levels). This line will be the one executed after the let's.
  (letrec ((replace-if-needed
            (lambda (sexpr)
              (if (= 0 (depth sexpr))
                  (list sexpr '())
                  (let ((sym (short-gensym)))
                    (list sym (list sym sexpr)))))))
    (map replace-if-needed first-row)))

(define (push-down-2 let-ribs-list simplified-row next-row)
  (if (null? let-ribs-list)
      (list simplified-row next-row)
      (let* ((rib (car let-ribs-list))
             (let-sym (car rib))
             (sexpr (cadr rib))
             (replaced-sexpr (replace-if-needed sexpr))
             (simplified-sexpr (cons let-sym
                                     (list (car replaced-sexpr))))
             (next-row-sexpr (cadr replaced-sexpr)))
        (push-down-2 (cdr let-ribs-list)
                     (append simplified-row (list simplified-sexpr))
                     (cons next-row-sexpr next-row)))))

(define (go-over-first-line full-list)
  ;; return the full list after 1st row is finalized and second is
  ;; with symbols from first, not yet finalized.
  (let* ((first-row (car full-list))
         (both-new-rows (push-down-1 first-row))
         (new-first-row (map car both-new-rows))
         (new-second-row (filter not-nil (map cadr both-new-rows)))
         (simplified-2-rows (list new-first-row new-second-row)))
    (append simplified-2-rows (cddr full-list))))


(define (go-over-lines let-list)
  (if (null? (cdr let-list))
      let-list
      (let* ((first-in-list (car let-list))
             (both-new-rows (push-down-2 first-in-list '() '()))
             (new-first-row (map car both-new-rows))
             (new-second-row (filter not-nil
                                     (map cadr both-new-rows))))
        (cons new-first-row
              (go-over-lines (cons new-second-row (cddr let-list)))))))


(define parallelize
  (lambda (sexpr-list)
    (let* ((dep (mx (map depth sexpr-list)))
           (init-list (make-list (1+ dep) '()))
           (decon-list (append (list sexpr-list) (cdr init-list)))
           (after-first-go (go-over-first-line decon-list))
           (newlist (cons (car after-first-go)
                          (go-over-lines (cdr after-first-go)))))
                     ;     (scan-lines-in-pairs (cdr after-first-go)))))
      (nl) (nl) (nl) (nl)
      newlist)))



(define (trace!)
  (trace parallelize)
  (trace rec-push-down)
  (trace push-deeper)
  (trace replaced-and-pushed-sexpr)
  (trace replace)
  (trace replace-list)
  (trace push-down-1)
  (trace go-over-first-line)
  (trace go-over-lines)
  (trace push-down-2))

(define (last lst)
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))

(define ex1 '(a (b c) (d (e f)) (g h) (i (j k))))
(define ex2 '(a1 (a2 (a3 (a4 (a5 x))))))
(define ex3 '(((a b) (c d)) ((e f) (g h))))