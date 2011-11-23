(define are-parentheses-balanced?
  (lambda (str)
          (are-parentheses-balanced-worker str 0)))

(define are-parentheses-balanced-worker
  ;; this function is tail-revursive!! 
  (lambda (str lp)
    (cond ((= 0 (string-length str)) (= 0 lp))
          ((< lp 0) #f)
          ((equal? #\( (string-ref str 0))
           (are-parentheses-balanced-worker (substring str 1 (string-length str))
                                            (+ lp 1)))
          ((equal? #\) (string-ref str 0))
           (are-parentheses-balanced-worker (substring str 1 (string-length str)) (- lp 1)))
          ;; anything but left or right parens will be ignored, so that (lanbda (x) x) is as legal as (())
          (else (are-parentheses-balanced-worker (substring str 1 (string-length str)) lp )))))


