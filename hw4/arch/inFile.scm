;; (if #t (if (if #t (if #t (if #t #f))) #f) #f)
;; (begin #t #t #f '() #t)
;; (or #f #f #t #f '())


;; (let ((x #t))
;;   ((lambda (p1)
;;      (set! p1 #f) p1)
;;    x))
;; ((lambda (p1 p2) p1) ((lambda () #t) '()) #f)
;; ((lambda (p1 p2) p1) ((lambda () #t)) #f)
;; ((lambda (p1) p1) #t)
;; (((lambda (x1 x2)
;;      (lambda (x3) x1))
;;     '() #t)
;;    #t)     ;; -> #f
;; (#f #f #t '()) ;; -> not a closure
;; (((lambda (x)
;;    (lambda (y)
;;      x)) #f) #t) ;; -> #f

;; (((lambda (x y)
;;    (lambda (z) x))
;;  '() #f)
;; #t)  ;; -> '()



;; ((((lambda (x)
;;      (lambda (y)
;;         (lambda (z) x)))
;;    #f)
;;   #t)
;;  #t)  ;; -> '()

;; (((((lambda (x) (x (x x)))
;;     (lambda (x)
;;       (lambda (y)
;;         (x (x y)))))
;;    (lambda (p)
;;      (p (lambda (x)
;;           (lambda (y)
;;             (lambda (z)
;;               ((z y) x)))))))
;;   (lambda (x)
;;     ((x #t) #f)))
;;  (lambda (x)
;;    (lambda (y)
;;      x))) ;; -> #t


;; ((((lambda (x) (x (x x)))
;;    (lambda (x)
;;      (lambda (y)
;;        (x (x y)))))
;;   (lambda (p)
;;     (p (lambda (x)
;;          (lambda (y)
;;            (lambda (z)
;;              ((z y) x)))))))
;;  (lambda (x)
;;    ((x #t) #f))) ;; -> <proc>
;; ((lambda () #\d)) ;; -> #\d
;; 3478 ;; -> 3478
;; 32897439 ;; -> 32897439


;;; CODEGEN.compileSchemeFile("inFile.scm", "genCode.c");
