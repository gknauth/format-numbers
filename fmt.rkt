#lang racket

(provide fmt-i-02d
         fmt-i-02x
         fmt-c-02x
         fmt-1s-02x
         fmt-ymd8-s10
         subspacezero
         real->scientific-string
         format-float)

(define (fmt-i-02d n)
  (let ((s (format "~a" n)))
    (if (= (string-length s) 1)
        (string-append "0" s)
        s)))

(define (fmt-i-02x n)
  (let ((s (format "~x" n)))
    (if (= (string-length s) 1)
        (string-append "0" s)
        s)))

(define (fmt-c-02x c)
  (fmt-i-02x (char->integer c)))

(define (fmt-1s-02x s)
  (fmt-c-02x (string-ref s 0)))

(define (subspacezero s)
  (regexp-replace* #rx" " s "0"))

(define (fmt-ymd8-s10 yyyymmdd)
  (let*-values ([(year mmdd) (quotient/remainder yyyymmdd 10000)]
                [(month day) (quotient/remainder mmdd 100)])
    (string-append (number->string year) "-" (fmt-i-02d month) "-" (fmt-i-02d day))))
         
;; real->scrientific-string
;; 2007-04-15 thanks to jensaxel@soegaard.net 
;; 2009-12-14 modified by gknauth
(define real->scientific-string
  (case-lambda
    [(x)
     (real->scientific-string x 2)]
    [(x digits-after-decimal-k)
     (let* ([sign         (if (negative? x) -1 +1)]
            [x            (* sign (inexact->exact x))]
            [e-safe       (if (= x 0)
                              0
                              (floor (/ (log x) (log 10))))]
            [e-orig       (inexact->exact    e-safe)]
            [e            (inexact->exact (- e-safe))]
            [x-normalized (* (inexact->exact x) (expt 10 e))])
           (format "~a~ae~a"
                   (if (negative? sign) "-" "")
                   (if (zero? digits-after-decimal-k)
                       (round x-normalized)
                       (real->decimal-string
                           (exact->inexact x-normalized)
                            digits-after-decimal-k))
                   e-orig))]))

;; original format-float (most of the meat now in format-numerals)
;; from Joe Marshall <jmarshall@alum.mit.edu>

;;;;; input number is exact

(define (format-exact exact digits)
  (format-numerals (exact-to-intstr-with-order-of-magnitude exact digits) digits))

(define (exact-to-intstr-with-order-of-magnitude exact digits)
  (number->string (float-to-int-with-order-of-magnitude exact digits)))

(define (exact-to-int-with-order-of-magnitude exact order-of-magnitude)
  (round (* exact (expt 10 order-of-magnitude))))

;;;;; input number is float

(define (format-float float digits)
  (format-exact (inexact->exact float) digits))

(define (float-to-intstr-with-order-of-magnitude float digits)
  (exact-to-intstr-with-order-of-magnitude (inexact->exact float) digits))

(define (float-to-int-with-order-of-magnitude float order-of-magnitude)
  (exact-to-int-with-order-of-magnitude (inexact->exact float) order-of-magnitude))

(define (format-numerals numerals digits)
  (let* ((length   (string-length numerals))
         (dot      (- length digits)))
    (string-append
     (if (< dot 0) "0" (substring numerals 0 dot))
     "."
     (if (< dot 0)
         (string-append (make-string (- dot) #\0) numerals)
         (substring numerals dot length)))))




