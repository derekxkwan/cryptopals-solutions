#lang racket
(require (file "hex_base64.rkt"))

(let* ([my-lamb (lambda (n) (bytes->list (hex->bytes n)))]
	[h1 (my-lamb "1c0111001f010100061a024b53535009181c")]
       [h2 (my-lamb "686974207468652062756c6c277320657965")])
  (apply (compose bytes->hex bytes) (map (lambda (x y) (bitwise-xor x y)) h1 h2)))
