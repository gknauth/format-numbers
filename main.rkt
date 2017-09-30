#lang racket/base
;; format-numbers
;; Copyright Geoffrey S. Knauth. See file "info.rkt".

;; Thanks for Neil Van Dyke's numberformat-old, which got me started, seeing how he
;; made his racket package.

(provide (all-defined-out))

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

;; real->scientific-string
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

;; original format-float came from from Joe Marshall <jmarshall@alum.mit.edu>
;; most of the meat is now in format-numerals

;;;;; input number is exact

(define (format-exact exact digits)
  (right-insert-decimal-point (exact-to-intstr-with-order-of-magnitude exact digits) digits))

(define (exact-to-intstr-with-order-of-magnitude exact order-of-magnitude)
  (number->string (float-to-int-with-order-of-magnitude exact order-of-magnitude)))

(define (exact-to-int-with-order-of-magnitude exact order-of-magnitude)
  (round (* exact (expt 10 order-of-magnitude))))

;;;;; input number is float

(define (format-float float digits)
  (format-exact (inexact->exact float) digits))

(define (float-to-intstr-with-order-of-magnitude float order-of-magnitude)
  (exact-to-intstr-with-order-of-magnitude (inexact->exact float) order-of-magnitude))

(define (float-to-int-with-order-of-magnitude float order-of-magnitude)
  (exact-to-int-with-order-of-magnitude (inexact->exact float) order-of-magnitude))

(define (right-insert-decimal-point numerals digits)
  (let* ((length   (string-length numerals))
         (dot      (- length digits)))
    (string-append
     (if (< dot 0) "0" (substring numerals 0 dot))
     "."
     (if (< dot 0)
         (string-append (make-string (- dot) #\0) numerals)
         (substring numerals dot length)))))


(module+ test
  ;; Tests to be run with raco test
  (require rackunit)
  (check-equal? (fmt-i-02d 5) "05")
  (check-equal? (fmt-i-02x 11) "0b")
  (check-equal? (fmt-c-02x #\017) "0f")
  (check-equal? (fmt-1s-02x "\016") "0e")
  (check-equal? (format-float 123.4567 2) "123.46")
  (check-equal? (format-float -123.4567 2) "-123.46")
  (check-equal? (format-exact 1234567/10000 2) "123.46")
  (check-equal? (format-exact -1234567/10000 2) "-123.46")
  (check-equal? (float-to-intstr-with-order-of-magnitude 123.4567 4) "1234567")
  (check-equal? (float-to-intstr-with-order-of-magnitude 123.4567 2) "12346")
  (check-equal? (exact-to-intstr-with-order-of-magnitude 1234567/10000 4) "1234567")
  (check-equal? (exact-to-intstr-with-order-of-magnitude 1234567/10000 2) "12346")
  (check-equal? (right-insert-decimal-point "1234567" 2) "12345.67")
  (check-equal? (right-insert-decimal-point "1234567" 4) "123.4567")
  
  )

;(doc (section "Introduction")
;     (para "This is a simple number formatting module.  It will get better over time."))

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.


  )

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included a LICENSE.txt file, which links to
;; the GNU Lesser General Public License.
;; If you would prefer to use a different license, replace LICENSE.txt with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here
