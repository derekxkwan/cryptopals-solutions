#lang racket
(require (file "hex_base64.rkt"))

(provide (all-defined-out))

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


(define (hexstr->intlist hexstr)
  ((compose bytes->list hex->bytes) hexstr))

(define (score-hexstr hexstr)
  (score-intlist (hexstr->intlist hexstr)))

(define (score-xors-intlist intlist)
  (for/fold ([best-score 0]
	     [best-str #"unscored"]
	     #:result (cons best-score best-str))
      ([i (range 256)])
    (let* ([xored (map (lambda (n) (bitwise-xor n i)) intlist)]
	   [cur-str (list->bytes xored)]
	   [cur-score (score-intlist xored)])
      (values (max best-score cur-score)
	      (if (or (> cur-score best-score)
		    (and (= cur-score best-score) (<= 65 (bytes-ref cur-str 0) 90)))
		  cur-str
		    best-str))
      ))
  )

(let* ([cmdln (current-command-line-arguments)]
       [run? (with-handlers ([exn:fail? (lambda (exn) #f)])
	       (equal? "run" (vector-ref cmdln 0)))]
       )
  (when run?
    (let* ([hex-input "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"]
	   [score-xors ((compose score-xors-intlist hexstr->intlist) hex-input)])
      (println (bytes->string/utf-8 (cdr score-xors))))
    ))
