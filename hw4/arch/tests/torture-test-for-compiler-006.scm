;;; torture-test-for-compiler-006.scm
;;; Yet another torture test -- a rather simple one...
;;;
;;; Programmer: Mayer Goldberg, 2011

(define test
  (let ((p1 (lambda (x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)
	      (lambda (z)
		(z x2 x3 x4 x5 x6 x7 x8 x9 x10 x1))))
	(s '(a b c d e f g h i j)))
    (lambda ()
      (equal? (((((((((((apply p1 s) p1) p1) p1) p1) p1) p1) p1) p1) p1)
	       list)
	      s))))