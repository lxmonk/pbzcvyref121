;; (if #t (if #f #f) #f)
;; (begin #t #t #f '() #t)
;; (or #f #f #f)
;; (lambda (p1)
;;   (set! p1 #f))
(#f #f #t '())
;;; CodeGen.compileSchemeFile("inFile.scm", "genCode.c");
