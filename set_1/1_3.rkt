#lang racket
(require (file "hex_base64.rkt"))

(define (list-intertwine lst-a lst-b)
  (flatten (map cons lst-a lst-b)))

(define my-scores
  (let ([most-freq (string->list "EARIOTNSLCUDPMHGBFYWKVXZJQ")]
	[scores (range 26 0 -1)])
    (apply hash (list-intertwine most-freq scores)))
  )

(define (intlist-upcase intlist)
  (map (lambda (n)
	 (if (>= n 97) (- n 32)
	     n))
       intlist))

(define (score-intlist intlist)
  (let ([filtered (filter (lambda (n) (<= 65 n 90)) (intlist-upcase intlist))])
    (apply + (map (lambda (n) (hash-ref my-scores (integer->char n))) filtered))
    ))

(let* ([decoded (bytes->list (hex->bytes "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"))])
  (for/fold ([best-score 0]
	     [best-str #"unscored"]
	     #:result (bytes->string/utf-8 best-str))
      ([i (range 256)])
    (let* ([xored (map (lambda (n) (bitwise-xor n i)) decoded)]
	   [cur-str (list->bytes xored)]
	   [cur-score (score-intlist xored)])
      (values (max best-score cur-score)
	      (if (> cur-score best-score)
		  cur-str
		  best-str))
      ))
  )
