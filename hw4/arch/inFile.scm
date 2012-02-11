;; (if #t (if (if #t (if #t (if #t #f))) #f) #f) ;; -> (Void)

;; (begin #t 1 #\c "NOT ME" '() #t) ;; -> #t
;; (or #f #f -9000 #t #f '()) ;; -> -9000
;; (or #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t) ;; -> #t

;; (let ((x #t))
;;   ((lambda (p1)
;;      (set! p1 #f)
;;      p1)
;;    x)) ;; -> #f

;; ((lambda (p1 p2) p1) ((lambda () #t) '()) #f) ;; -> Error wrong argnum

;; ((lambda (p1 p2) p1) ((lambda () #t)) #f) ;; -> #t

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
;;  #t)  ;; -> #f

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
    ((x 2329874) 1)))
 (lambda (x)
   (lambda (y)
     x))) ;; -> 2329874
;; CODEGEN.compileSchemeFile("inFile.scm", "genCode.c");
1
2
3
4
5
6
5

"ladsfjlkdsjfalk"

((lambda () 'x))

#\a

#\space
