(print-graph #f) ; display circular structures
(print-gensym #f) ; print gensym as g1234
(case-sensitive #f) ; ditto
(print-brackets #f) ; do not use brackets when pretty-printing

;; These should have been included, I don't know why they weren't
;; so I had to implement them myself.
(define (reduce fn init lst)
  (if (null? lst)
      init
      (reduce fn (fn init (car lst)) (cdr lst))))

(define (del-assoc entry a-list)
  (letrec ((del (lambda (entry alist new-a-list)
             (if (null? alist) new-a-list
                 (let ((next (car alist)))
                   (if (equal? entry (car next))
                       (del entry (cdr alist) new-a-list)
                       (del entry (cdr alist)
                         (cons next new-a-list))))))))
    (del entry a-list '())))

(define (cse e)
  (purge-dict (create-freq-dict e '()) ;; initially, '() is
                                       ;;  an empty a-list.
    ;; here frequencies will be replaced by gensyms:
    (lambda (clean-dict)
      (replace-by-dict e clean-dict
        (lambda (new-e)
          `(let* ,(lets-from-dict clean-dict) ,new-e))
        ;; no replace was made - return original expression
        (lambda () e)))))

(define (lets-from-dict dict)
  (let ((ordered-dict (bring-order-to dict)))
    (reverse (map reverse dict))))

(define (bring-order-to dict)
  (bubble-sort dict (lambda (e1 e2) (is-in? e1 e2))))

(define (is-in? e1 e2)
  (let ((sym-e1 (symbol->string (cadr e1))))
    (letrec ((finder (lambda (sym e)
                       (cond ((null? e) #f)
                             ((not (pair? e)) (equal?
                                                (symbol->string e)
                                                sym))
                             ((ormap (lambda (el) (finder sym el))
                                e))
                             (else #f)))))
      (finder sym-e1 e2))))

(define (replace-by-dict e dict ret-rep ret-not)
  (let* ((keys (map car dict))
         (vals (map cadr dict)))
    (letrec ((worker (lambda (keys vals e)
                       (if (null? keys) e
                           (worker (cdr keys) (cdr vals)
                             (replacer (car keys) (car vals)
                               e)))))
             (y-combinator (lambda (e0)
               (let* ((v1 (worker keys vals e0))
                      (v2 (worker keys vals v1)))
                 (if (equal? v1 v2) v2
                     (y-combinator v2))))))
      (let ((e-stable (y-combinator e)))
        (if (equal? e e-stable) (ret-not)
            (ret-rep e-stable))))))

(define expunge
  (lambda (dirty clean)
    (if (null? dirty) clean
        (let* ((next (car dirty))
               (e (car next))
               (freq (cadr next)))
          (expunge (reverse (del-assoc e dirty))
            (if (> freq 1)   ; add to clean dict, with gensym
                (cons (list e (gensym)) clean)
                clean))))))

(define (replacer e e-sym dict-e)
  (cond ((not (pair? dict-e)) dict-e)
        ((equal? e dict-e) e-sym)
        (else (map (lambda (el) (replacer e e-sym el)) dict-e))))

(define (list-range lst start stop)
  (if (= stop start) '()
      (cons (list-ref lst start)
        (list-range lst (1+ start) stop))))

(define (consN first rest)
    (if (null? first) rest
        (if (null? rest) first
            (cons first rest))))

(define (reference dict n)
  (if (= n (length dict)) dict
      (let* ((key (car (list-ref dict n)))
             (val (cadr (list-ref dict n)))
             (dict-without-nth-line (append (list-range dict 0 n)
                                      (cons '()
                                        (list-range dict (1+ n)
                                          (length dict)))))
             (new-dict (replacer key val dict-without-nth-line))
             (updated-new-dict (map (lambda (entry)
                                      (if (null? entry)
                                          `(,key ,val)
                                          entry))
                                 new-dict)))
        (reference updated-new-dict (1+ n)))))


(define (purge-dict dirty cont)
  (let* ((clean-dict (expunge dirty '())))
    (letrec ((change-detector
               (lambda (dict)
                 (let* ((replaced-dict-v1 (reference dict 0))
                        (replaced-dict-v2 (reference replaced-dict-v1 0)))
                   (if (equal? replaced-dict-v1 replaced-dict-v2)
                       replaced-dict-v2 ;; latest edition is better :)
                       (change-detector replaced-dict-v2))))))
      (cont (change-detector clean-dict)))))

(define (create-freq-dict e freq-dict)
  (define (const? e)
    (or (symbol? e) (not (pair? e))
        (and (pair? e) (equal? 'quote (car e)))))
  (cond ((null? e) freq-dict)
        ((const? e) freq-dict)
        ((assoc e freq-dict)
         (reduce
           (lambda (init-dict el) (create-freq-dict el init-dict)) ; fn
           (cons `(,e ,(1+ (cadr (assoc e freq-dict)))) freq-dict) ; init
           (cdr e)))                                               ; lst
        ;; not found in a-list
        (else
          (reduce
            (lambda (init-dict el)
              (create-freq-dict el init-dict)) ; fn
            (cons `(,e 1) freq-dict)           ; init
            (cdr e)))))                        ; lst

(define (bubble-sort x gt?)
  (letrec
      ((fix (lambda (f i)
              (if (equal? i (f i))
                  i
                  (fix f (f i)))))
       (sort-step (lambda (l)
                    (if (or (null? l) (null? (cdr l)))
                        l
                        (if (gt? (car l) (cadr l))
                            (cons (cadr l)
                              (sort-step (cons (car l) (cddr l))))
                            (cons (car  l)
                              (sort-step (cdr l))))))))
    (fix sort-step x)))


;;; me love them testings..

;; (cse '(+ 2 3))

;; (cse '(f (f (f (f x)))))

;; (cse '(* (+ 2 3 4) (+ 2 3 4)))

;; (cse '(f (g x y) (f (g x y) z)))

;; (cse '(+ (* (- x y) (* x x))
;;         (* x x)
;;         (foo (- x y))
;;         (goo (* (- x y) (* x x)))))

;; (cse '(f (g x)
;;         (g (g x))
;;         (h (g (g x)) (g x))
;;         ((g x) (g x))))

;; (cse '(list (cons 'a 'b)
;;         (cons 'a 'b)
;;         (list (cons 'a 'b)
;;           (cons 'a 'b))
;;         (list (list (cons 'a 'b)
;;                 (cons 'a 'b)))))

;; (cse '(list '(a b)
;;         (list '(a b) '(c d))
;;         (list '(a b) '(c d))))
;; !!!
