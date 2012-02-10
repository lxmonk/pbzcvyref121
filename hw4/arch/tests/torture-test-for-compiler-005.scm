;;; torture-test-for-compiler-005.scm
;;; Should return #t
;;;
;;; Programmer: Mayer Goldberg, 2010

(((((lambda (x) (x (x x)))
    (lambda (x)
      (lambda (y)
        (x (x y)))))
   (lambda (p)
     (p (lambda (x)
          (lambda (y)
            (lambda (z)
              ((z y) x)))))))
  (lambda (x)
    ((x #t) #f)))
 (lambda (x)
   (lambda (y)
     x)))
