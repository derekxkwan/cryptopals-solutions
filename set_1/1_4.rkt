#lang racket

(require (prefix-in hsc: (file "1_3.rkt")))

(call-with-input-file "4.txt"
  (letrec ([i 0]
	   [xored "none"]
	   [xored-idx -1]
	   [best-score 0]
	   [file-iterator
	    (lambda (in)
	      (let ([line (read-line in 'any)])
		
		(if (not (eof-object? line))
		    (begin
		      (let* ([cur-intlist (hsc:hexstr->intlist (string-trim line))]
			     [best-stuff (hsc:score-xors-intlist cur-intlist)]
			     [cur-score (car best-stuff)]
			     [best-str (cdr best-stuff)])
			(when (> cur-score best-score)
			  (set! best-score cur-score)
			  (set! xored-idx i)
			  (set! xored  best-str))
			(set! i (+ i 1))
			(file-iterator in)
			)
		      )
		    (fprintf (current-output-port) "(~a:~a) ~a~%" xored-idx best-score xored)
		    )
		))
	    ])
    file-iterator
  )
  )
