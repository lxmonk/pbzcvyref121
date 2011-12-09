;; >(sched
;;   '(( mov (1) (2))
;;     (mov (3) (2))
;;     (add (4) (5 6))
;;     (add (3) (3 1))))
;; ((( mov (1) (2))
;;   (mov (3) (2))
;;   (add (4) (5 6)))
;;  (( add (3) (3 1))))

(define (flatten x)
  ;; Thank you http://rosettacode.org/wiki/Flatten_a_list#Scheme
  (cond ((null? x) '())
        ((not (pair? x)) (list x))
        (else (append (flatten (car x))
                      (flatten (cdr x))))))

(define (insert-out full-instruction dest-level outlist)
  (let ((line (car outlist))
        (lines (cdr outlist)))
    (cond ((= 0 dest-level) (cons (cons full-instruction line) lines))
          ((and (= 1 dest-level) (null? lines))
           (append outlist (list full-instruction)))
          (else (cons line (insert-out full-instruction
                                       (1- dest-level)
                                       lines))))))
(define (filter-not w dict)
  (cond ((null? dict) dict)
        ((equal? w (caar dict)) (cdr dict))
        (else (cons (car dict) (filter-not w (cdr dict))))))

(define (insert-dict w dest-level dictionary)
  (let ((old-entry (assoc w dictionary)))
    (if (not old-entry) (append dictionary `((,w ,dest-level)))
        (append (filter-not w dictionary) `((,w ,dest-level))))))

(define (sched$ inlist outlist dictionary)
  (if (null? inlist) outlist
      (let* ((full-instruction (car inlist))
             (instruction (car full-instruction))
             (w (caadr full-instruction))
             (rs (caddr full-instruction))
;             (dc (display `("rs: " ,rs "\n")))
             (map-results (filter (lambda (el) el)
                                  (map (lambda (r) (assoc r dictionary)) rs)))
;             (dontcare (display `(map-results: " " ,map-results "\n")))
             (levels-writing-to-rs (if (null? map-results) map-results
                                       (map cadr map-results)))
             ;; (dontcare2 (begin (display `(levels-writing-to-rs: " "
             ;;                                                    ,levels-writing-to-rs "\n")) -1))
             (dest-level
              (if (null? levels-writing-to-rs)
                  0
                  (1+ (apply max levels-writing-to-rs)))))
        (sched$ (cdr inlist)
                (insert-out full-instruction dest-level outlist)
                (insert-dict w dest-level dictionary)))))

(define (sched instructions)
  (sched$ instructions '(()) '()))

(define testcase1
  '((mov (1) (2))
    (mov (3) (2))
    (add (4) (5 6))
    (add (3) (3 1))))
