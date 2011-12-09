;; >(sched
;;   '(( mov (1) (2))
;;     (mov (3) (2))
;;     (add (4) (5 6))
;;     (add (3) (3 1))))
;; ((( mov (1) (2))
;;   (mov (3) (2))
;;   (add (4) (5 6)))
;;  (( add (3) (3 1))))

(define (sched instructions)
  (sched$ instructions '(()) '() '() values))

(define sched$
  (lambda inlist outlist wslist rslist success)
  (if (null? inlist) (success outlist)
      (let* ((instruction (car inlist))
             (ws (cadr instruction))
             (rs (caddr instruction)))

        )))
