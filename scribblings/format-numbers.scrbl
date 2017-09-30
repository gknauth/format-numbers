#lang scribble/manual
@(require scribble/core scribble/example format-numbers)

@require[@for-label[format-numbers
                    racket/base]]

@title{format-numbers: simple number formatting}
@author{Geoff Knauth}

@defmodule[format-numbers]

The @racketmodname[format-numbers] module provides simple formatting for numbers.

@section{Real to string}

@defproc[(real->scientific-string [n float?]) string?]{}

@;----------------------------------------------------------------------
@section{Making two digit strings}

@(define my-eval (make-base-eval `(require format-numbers)))

@defproc[(fmt-i-02d [n integer?]) string?]{Takes a number in the range
 @code{0..99} and turns it into a two-digit decimal string padded with leading zeroes if necessary.}

@examples[#:eval my-eval
          (fmt-i-02d 5)
          (fmt-i-02d 99)]

@defproc[(fmt-i-02x [n integer?]) string?]{Takes a number in the range
 @code{0..255} and turns it into a two-digit hexidecimal string padded with leading zeroes if necessary.}

@examples[#:eval my-eval
          (fmt-i-02x 0)
          (fmt-i-02x 5)
          (fmt-i-02x 16)
          (fmt-i-02x 64)
          (fmt-i-02x 255)]

@defproc[(fmt-c-02x [c char?]) string?]{Takes a character code and turns it into a two-digit hexidecimal string padded with leading zeroes if necessary.}

@examples[#:eval my-eval
          (fmt-c-02x #\017)
	  (fmt-c-02x #\A)
	  (fmt-c-02x #\0)]

@defproc[(fmt-1s-02x [s string?]) string?]{}

@examples[#:eval my-eval
          (fmt-1s-02x "\016")]

@;----------------------------------------------------------------------
@section{Decimal point control}

@defproc[(format-exact [exact exact?] [digits integer?]) string?]{}

@examples[#:eval my-eval
          (format-exact 1234567/10000 2)
          (format-exact -1234567/10000 2)]

@defproc[(format-float [float float?] [digits integer?]) string?]{}

@examples[#:eval my-eval
          (format-float 123.4567 2)
          (format-float -123.4567 2)]

@defproc[(right-insert-decimal-point [numerals string?] [digits integer?]) string?]{Insert a decimal point @racket[digits] in from the right.}

@examples[#:eval my-eval
          (right-insert-decimal-point "1234567" 2)
          (right-insert-decimal-point "1234567" 4)]

@;----------------------------------------------------------------------
@section{Utility functions}

@defproc[(subspacezero [s string?]) string?]{Turn spaces into zeroes.}

@examples[#:eval my-eval
          (subspacezero " 2")
	  (subspacezero "have you gone mad?")]

@defproc[(exact-to-intstr-with-order-of-magnitude [exact exact?] [order-of-magnitude integer?]) string?]{Multiply @racket[exact] by 10@superscript{@racket[order-of-magnitude]}, round the result to an integer, and turn it into a string.}

@examples[#:eval my-eval
          (exact-to-intstr-with-order-of-magnitude 3/100 2)
          (exact-to-intstr-with-order-of-magnitude 1234567/10000 4)
          (exact-to-intstr-with-order-of-magnitude 1234567/10000 2)]

@defproc[(exact-to-int-with-order-of-magnitude [exact exact?] [order-of-magnitude integer?]) integer?]{Multiply @racket[exact] by 10@superscript{@racket[order-of-magnitude]}, and round the result to an integer.}

@examples[#:eval my-eval
          (exact-to-int-with-order-of-magnitude 3/100 2)
          (exact-to-int-with-order-of-magnitude 1234567/10000 4)
          (exact-to-int-with-order-of-magnitude 1234567/10000 2)]

@defproc[(float-to-intstr-with-order-of-magnitude [float float?] [order-of-magnitude integer?]) string?]{Multiply @racket[float] by 10@superscript{@racket[order-of-magnitude]}, round the result to an integer, and turn it into a string.}

@examples[#:eval my-eval
          (float-to-intstr-with-order-of-magnitude 123.4567 4)
          (float-to-intstr-with-order-of-magnitude 123.4567 2)]

@defproc[(float-to-int-with-order-of-magnitude [float float?] [order-of-magnitude integer?]) string?]{Multiply @racket[float] by 10@superscript{@racket[order-of-magnitude]}, and round the result to an integer.}

@examples[#:eval my-eval
          (float-to-int-with-order-of-magnitude 123.4567 4)
          (float-to-int-with-order-of-magnitude 123.4567 2)]

@;{
;; Local Variables:
;; mode: scribble
;; End:
}
