(print-graph #f) ; display circular structures
(print-gensym #f) ; print gensym as g1234
(case-sensitive #f) ; ditto
(print-brackets #f) ; do not use brackets when pretty-printing

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
  (purge-dict (create-freq-dict e '()) ; initially, '() is
    ;;   an empty a-list
    ;; here frequencies will be replaced by gensyms
    (lambda (clean-dict)
      (replace-by-dict e clean-dict
        (lambda (new-e)
          `(let* ,(lets-from-dict clean-dict)))
        ;; no replace was made - return original expression
        e))))

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

(define (purge-dict dirty cont)
  (cont (expunge dirty '())))


(define (create-freq-dict e freq-dict)
  (define (const? e)
    (or (symbol? e) (not (pair? e))
        (and (pair? e) (equal? 'quote (car e)))))
       ; (null? e)  (and (pair? e) (null? (cdr e)))
  ;; (trace create-freq-dict)
  (cond ((null? e) freq-dict)
        ((const? e) freq-dict)
        ((assoc e freq-dict)
         (reduce
           (lambda (init-dict el) (create-freq-dict el init-dict)) ; fn
           (cons `(,e ,(1+ (cadr (assoc e freq-dict)))) freq-dict) ; init
           (cdr e)))                                               ; lst)
        ;; not found in a-list
        (else
          (reduce
            (lambda (init-dict el)
              (create-freq-dict el init-dict)) ; fn
            (cons `(,e 1) freq-dict)           ; init
            (cdr e)))))                        ;lst

  ;; (cond ((const? e) (cont freq-dict))
  ;;       ((assoc e freq-dict)
  ;;        (create-freq-dict
  ;;          (car e) (cons `(,e ,(1+ (cadr (assoc e freq-dict))))
  ;;                    freq-dict)
  ;;          (lambda (new-dict)
  ;;            (create-freq-dict (cdr e) new-dict cont))))
  ;;       (else (create-freq-dict (car e) freq-dict
  ;;               (lambda (car-dict)
  ;;                 (create-freq-dict (cdr))))

  ;;         (create-freq-dict (car e) (cons `(,e 1) freq-dict)
  ;;               (lambda (car-dict)
  ;;                 (create-freq-dict (cdr e) car-dict
  ;;                   (lambda (new-dict)
  ;;                     (cont new-dict))))))))

(define (replace-by-dict e clean-dict cont))
