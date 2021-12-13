(require 2htdp/image)
(require 2htdp/universe)

;; Simple editor

;; ==========
;; Constants:

(define WIDTH 300)
(define HEIGHT 20)
(define MTS (empty-scene WIDTH HEIGHT))

(define CURSOR (rectangle 2 14 "solid" "red"))

(define TEXT_SIZE 14)
(define TEXT_COLOR "black")

;; =================
;; Data definitions:

(define-struct editor (pre post))
;; Editor is (make-editor String String)
;; interp. pre is the text before the cursor
;;         post is the text after the cursor            
(define editor1 (make-editor "" ""))
(define editor2 (make-editor "a" ""))
(define editor3 (make-editor "" "b"))

#;
(define (editorFunc e)
  (... (editor-pre e)               ; String
       (editor-post e)))            ; String

;; Template rules used:
;; - Compound: 2 fields

;; =================
;; Functions:

;; Editor -> Editor
;; a simple 1 line editor, start with (main (make-editor "" ""))
(define (main e)
  (big-bang e                         ; Editor
            (to-draw render)          ; Editor -> Editor
            (on-key input)))          ; Editor -> Image

;; Editor -> Editor
;; renders the (editor-pre) and (editor-post) between the cursor
(check-expect (render (make-editor "" "")) (overlay/align "left" "middle" CURSOR MTS))
(check-expect (render (make-editor "a" "")) (overlay/align "left" "middle" (beside (text "a" TEXT_SIZE TEXT_COLOR) CURSOR) MTS))
(check-expect (render (make-editor "" "b")) (overlay/align "left" "middle" (beside CURSOR (text "b" TEXT_SIZE TEXT_COLOR)) MTS))
(check-expect (render (make-editor "a" "b")) (overlay/align "left" "middle" (beside 
                                                                                 (text "a" TEXT_SIZE TEXT_COLOR) 
                                                                                 CURSOR 
                                                                                 (text "b" TEXT_SIZE TEXT_COLOR)) MTS))

;(define (render e) MTS)               ; stub
#;
(define (render e)
  (... e))

(define (render e)
  (overlay/align "left" "middle" 
                 (beside (text (editor-pre e) TEXT_SIZE TEXT_COLOR)
                         CURSOR
                         (text (editor-post e) TEXT_SIZE TEXT_COLOR))
                 MTS))

;; Editor -> Editor
;; appends the keyboard input to (editor-pre)
(check-expect (input editor1 "a") (make-editor "a" ""))
(check-expect (input editor3 "c") (make-editor "c" "b"))
(check-expect (input (make-editor "aa" "bb") "\b") (make-editor "a" "bb"))
(check-expect (input (make-editor "aa" "bb") "left") (make-editor "a" "abb"))
(check-expect (input (make-editor "aa" "bb") "right") (make-editor "aab" "b"))

; (define (input e key) input)        ; stub
; <template from Editor>

(define (input e key)
  (cond [(key=? key "\b") (make-editor 
                            (withoutLast (editor-pre e)) 
                            (editor-post e))]
        [(key=? key "left") (make-editor 
                              (withoutLast (editor-pre e)) 
                              (string-append (getLastChar (editor-pre e)) (editor-post e)))]
        [(key=? key "right") (make-editor 
                               (string-append (editor-pre e) (getFirstChar (editor-post e))) 
                               (withoutFirst (editor-post e)))]
        [(= 1 (string-length key)) (make-editor (string-append (editor-pre e) key) (editor-post e))]))

;; String -> String
;; given a string, this produces the string without its first char
(check-expect (withoutFirst "hey") "ey")
(check-expect (withoutFirst "") "")

; (define (withoutFirst str) "a")        ; stub
#;
(define (withoutFirst str)               ; template
  (... str))

(define (withoutFirst str)
  (cond [(string=? "" str) ""]
        [else (substring str 1)]))

;; String -> String
;; produces the first char of the given string
(check-expect (getFirstChar "ab") "a") 
(check-expect (getFirstChar "") "") 

;(define (getFirstChar str) "a")          ; stub
#;
(define (getFirstChar str)
  (... str))

(define (getFirstChar str)
  (cond [(string=? "" str) ""]
        [else (string-ith str 0)]))

;; String -> String
;; given a string, this produces the string without its last char
(check-expect (withoutLast "hey") "he")
(check-expect (withoutLast "") "")

; (define (withoutLast str) "a")        ; stub
#;
(define (withoutLast str)               ; template
  (... str))

(define (withoutLast str)
  (cond [(string=? "" str) ""]
        [else (substring 
                str 
                0 
                (sub1 (string-length str)))]))

;; String -> String
;; produces the last char of the given string
(check-expect (getLastChar "ab") "b") 
(check-expect (getLastChar "") "") 

;(define (getLastChar str) "a")          ; stub
#;
(define (getLastChar str)
  (... str))

(define (getLastChar str)
  (cond [(string=? "" str) ""]
        [else (string-ith str (sub1 (string-length str)))]))

(main (make-editor "" ""))
