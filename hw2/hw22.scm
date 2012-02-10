(print-graph #f) ; display circular structures
(print-gensym #f) ; print gensym as g1234
(case-sensitive #f) ; ditto
(print-brackets #f) ; do not use brackets when pretty-printing

(define nl newline)
(define nop (lambda var (if #f (raise exception))))
(define nop2 (lambda var (if #f (raise exception))))
(define not-nil (lambda (el) (not (null? el))))
(define (short-gensym) (gensym))

(define (map>2 f lst)
  (if (>= 2 (depth lst))
      (f lst)
      (map f lst)))

(define (d sexpr dep)
  (cond ((not (list? sexpr)) dep)
        ((null? sexpr) dep)
        (else (max (d (car sexpr) (1+ dep))
                   (d (cdr sexpr) dep)))))

(define (depth el)
  (d el 0))

;; (define (map-depth sexpr)
;;   (letrec ((mapper
;;             (lambda sexpr)))))

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

(define replace-all-d=1
  (lambda (sexpr staying going)
    (cond ((null? sexpr) (list staying going))
          ((= 0 (depth (car sexpr)))
           (replace-all-d=1 (cdr sexpr) staying
                            (append going `(,(car sexpr)))))
          ((= 1 (depth (car sexpr)))
           (let ((sym (short-gensym)))
             (replace-all-d=1 (cdr sexpr)
                              (cons (list sym (car sexpr)) staying)
                              (append going `(,sym)))))
          ((< 1 (depth (car sexpr)))
           (let* ((returned-expr (replace-all-d=1 (car sexpr) '() '()))
                  (stay-part (car returned-expr))
                  (go-part (cadr returned-expr)))
             (replace-all-d=1 (cdr sexpr) (cons stay-part staying)
                              (append going (list go-part))))))))


(define (flatten-so-d=3 lst)
  (letrec ((flatterner
            (lambda (oldlst newlst)
              (if (null? oldlst)
                  newlst
                  (let ((el (car oldlst)))
                    (if (= 2 (depth el))
                        (flatterner (cdr oldlst) (cons el newlst))
                        (flatterner (cdr oldlst) (append newlst el))))))))
    (flatterner lst '())))

(define (push-down-11 sexpr)
  (if (= 1 (depth sexpr))
      (map (lambda (el) (list el '())) sexpr)
      (map
       (lambda (el)
         (cond ((= 0 (depth el)) (list '()  el))
               ((= 1 (depth el))
                (let ((sym (short-gensym)))
                  (list (list sym el) sym)))
               (else (replace-all-d=1 el '() '())))) sexpr)))

(define (push-down-2 let-ribs-list simplified-row next-row)
  (if (null? let-ribs-list)
      (list (flatten-so-d=3 simplified-row) next-row)
      (let* ((rib (car let-ribs-list))
             (let-sym (car rib))
             (sexpr (cadr rib))
             (staying-and-going (push-down-11 sexpr))
             (next-row-sexpr-tmp (or (and (list? (cdr staying-and-going))
                                          (filter not-nil
                                                  (map cadr
                                                       staying-and-going)))
                                     '()))
             (simplified-sexpr-tmp (map car staying-and-going))
             (simplified-sexpr-unboxed (filter not-nil
                                       (if (null? next-row-sexpr-tmp)
                                           (cons let-sym
                                                 (list simplified-sexpr-tmp))
                                           simplified-sexpr-tmp)))
             (simplified-sexpr (if (< 2 (depth simplified-sexpr-unboxed))
                                   simplified-sexpr-unboxed
                                   (list simplified-sexpr-unboxed)))
             (next-row-sexpr (if (null? next-row-sexpr-tmp) '()
                                 (cons let-sym (list next-row-sexpr-tmp)))))
        (push-down-2 (cdr let-ribs-list)
                     (append simplified-row simplified-sexpr)
                     (if (not-nil next-row-sexpr)
                         (cons next-row-sexpr next-row)
                         next-row)))))

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
             (new-first-row (car both-new-rows))
             (new-second-row (filter not-nil
                                     (cadr both-new-rows))))
        (cons new-first-row
              (go-over-lines (cons new-second-row (cddr let-list)))))))

(define (fix-depth rib)
  rib)
  ;; (if (<= 3 (depth rib))
  ;;     rib
  ;;     (cons (fix-depth (car rib) (fix-depth (cdr rib))))))

(define (create-let newlist buttom-line)
  (letrec ((rec-let
            (lambda (ribs)
              (if (null? (cdr ribs))
                  `(let ,(fix-depth ribs) ,buttom-line)
                  `(let ,(fix-depth (car ribs))
                     ,(rec-let (cdr ribs)))))))
    (if (or (null? newlist)
            (null? (car newlist)))
        buttom-line
        (rec-let newlist)
;;         (`(let ,(car newlist))
;; ; `((let ,(cadr newlist) ,buttom-line))
;;                 (rec-let (cdr newlist))
                ))))

(define parallelize
  (lambda (sexpr-list)
;    (trace!)
    (if (>= 1 (depth sexpr-list))
        sexpr-list
        (let* ((dep (mx (map depth sexpr-list)))
               (init-list (make-list (1+ dep) '()))
               (decon-list (append (list sexpr-list) (cdr init-list)))
               (after-first-go (go-over-first-line decon-list))
               (reslist (cons (car after-first-go)
                          (go-over-lines (cdr after-first-go))))
               (buttom-line (car reslist))
               (newlist (cdr reslist)))
          (create-let newlist buttom-line)))))



(define (trace!)
  (trace parallelize)
  (trace rec-push-down)
  (trace push-deeper)
  (trace replaced-and-pushed-sexpr)
  (trace replace)
  (trace replace-list)
  (trace push-down-1)
  (trace push-down-11)
  (trace go-over-first-line)
  (trace go-over-lines)
  (trace push-down-2)
  (trace replace-all-d=1)
  (trace flatten-so-d=3)
  (trace fix-depth)
  (trace create-let))

(define (last lst)
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))

(define ex1 '(a (b c) (d (e f)) (g h) (i (j k))))
(define ex2 '(a1 (a2 (a3 (a4 (a5 x))))))
(define ex3 '(((a b) (c d)) ((e f) (g h))))
(define ex4 '(((a)))) ;(= 3 (depth ex4)) => #t
(define ex5 '(((((a1 a2) a3) a4) a5) a6))
(define ex6 '((((a b) (c d) (e f))
               (g h)
               (i j))
              (k (l m))))
(define ex7 '(a (b c) (d e) (f g)))
(define ex8 '((a b) (a b)))
(define ex9 '(a (b c) (d (e f) (g (h i) (j k)))))
