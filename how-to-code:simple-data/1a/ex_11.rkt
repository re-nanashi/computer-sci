(define (bobble s)
  (if (<= (string-length s) 6)
      (string append s "ible")
      s))

(bobble (substring "fungus" 0 4))

(bobble "fung")

(if (<= (string-length "fung") 6)
    (string-append "fung" "ible")
    "fung")

(if (<= 4 6)
    (string-append "fung" "ible")
    "fung")

(if true 
    (string-append "fung" "ible")
    "fung")

(string-append "fung" "ible")

"fungible"
