#lang racket
(require (file "hex_base64.rkt"))


(let* ([hex-input "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"]
       [result (bytes->b64 (hex->bytes hex-input))])
  (println result)
  )
