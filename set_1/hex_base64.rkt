#lang racket

(provide (all-defined-out))

(define (digit->hexchar i)
  ;; range 0 - 16, returns character 0-f
  (cond [(<= i 9) (integer->char (+ (char->integer #\0) (max i 0)))]
	[else (integer->char (+ (char->integer #\a) (min 16 i)))]))

(define (integer->hexchar i)
  ;; range 0 -255, returns string
  (let* ([d1 (quotient i 16)]
	 [d2 (modulo i 16)])
    (list->string (map digit->hexchar (list d1 d2))))
  )

(define (bytes->hex bstr)
  (apply string-append (map integer->hexchar (bytes->list bstr))))

(define (hexchar->integer c)
  (cond [(char<=? #\0 c #\9) (- (char->integer c) (char->integer #\0))]
	[(char<=? c #\f) (+ 10 (- (char->integer c) (char->integer #\a)))]
	[else 0]))

(define (hex->bytes s)
  (let* ([cleaned (string-downcase s)]
	 [valid-hex (regexp-match? #px"^([[:xdigit:]]{2})*$" cleaned)])

    (if (and valid-hex (even? (string-length cleaned)))
	(apply bytes (for/list ([i (range (/ (string-length cleaned) 2 ))])
	  (+ (* 16 (hexchar->integer (string-ref cleaned (* 2 i))))
	     (hexchar->integer (string-ref cleaned (+ (* 2 i) 1))))))
	(raise-argument-error "improper format")
	)))

(define (integer->b64char i)
  (cond [(<= 0 i 25) (integer->char (+ (char->integer #\A) i))]
	[(<= 26 i 51) (integer->char (+ (char->integer #\a) (- i 26)))]
	[(<= 52 i 61) (integer->char (+ (char->integer #\0) (- i 52)))]
	[(= 62 i) #\+]
	[(= 63 i)  #\/]
	[else #\=]))

(define (bytes->six-bit-list bytstr)
  ;;assume we are being passeed <= 3 byte chunks
  ;; let 64 be the padding char
  (let* ([bn (bytes-length bytstr)]
	 [padn (max (- 3 bn) 0)] 
	 [char1 (bytes-ref bytstr 0)]
	 [char2 (if (> bn 1) (bytes-ref bytstr 1) 0)]
	 [char3 (if (> bn 2) (bytes-ref bytstr 2) 0)]
	 [b64-1 (arithmetic-shift char1 -2)]
	 [b64-2 (+ (arithmetic-shift (bitwise-and char1 3) 4)
		   (arithmetic-shift char2 -4))]
	 [b64-3 (+ (arithmetic-shift (bitwise-and char2 15) 2)
		   (arithmetic-shift char3 -6))]
	 [b64-4 (+ (bitwise-and char3 63))])
    (cond [(= padn 0) (list b64-1 b64-2 b64-3 b64-4)]
	  [(= padn 1) (list b64-1 b64-2 b64-3 64)]
	  [else (list b64-1 b64-2 64 64)])
    ))
	 
(define (bytes->b64 bstr)
  (apply string-append (for/list ([i (range (floor (/ (bytes-length bstr) 3)))])
    (let ([cur-sub (subbytes bstr (* 3 i) (* 3 (+ i 1)))])
       (list->string (map integer->b64char (bytes->six-bit-list cur-sub)))
      ))))
