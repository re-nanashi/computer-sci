(define-struct mail (from date message))
;; Mail is is (make-mail String Number String)
;; interp. (mail-from) is a name
;;         (mail-date) is the date sent
;;         (mail-message) is the message sent

;; ListOfMail -> ListOfMail(sorted)
;; sort the list by name.

(define (sort_mail lom)
  (cond [(empty? lom) empty]
        [else
          (insert (first lom)
                  (sort_mail (rest lom)))]))

;; Mail ListOfMail(sorted) -> ListOfMail(sorted)
;; insert the mail in sorted list alphabetically.

(define (insert m lom)
  (cond [(empty? lom) (cons m empty)]
        [else
          (cond [(string<? m (first lom))
                 (cons m lom)]
                [else
                  (cons (first lom)
                        (insert m (rest lom)))])]))
