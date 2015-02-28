
;;; Variable Declarations

(define look #f) ; Lookahead Character

;;; Read New Character From Input Stream

(define (get-char!)
  (set! look (read-char)))

;;; Report an Error

(define (error* s)
  (newline)
  (display (string-append "Error: " s ".")))

;;; Report Error and Halt

(define (abort s)
  (error* s)
  ;; TODO: This is not R5RS
  (exit))

;;; Report What Was Expected

(define (expected s)
  (abort (string-append s " Expected")))

;;; Match a Specific Input Character

(define (match x)
  (if (char=? look x)
      (get-char!)
      (expected (string-append "''" (string x) "''"))))

;;; Recognize an Alpha Character

(define (is-alpha? c)
  (char-alphabetic? c))

;;; Recognize a Decimal Digit

(define (is-digit? c)
  (char-numeric? c))

;;; Recognize an Addop

(define (is-addop? c)
  (or (char=? c #\+)
      (char=? c #\-)))

;;; Get an Identifier

(define (get-name)
  (if (not (is-alpha? look))
      (expected "Name"))
  (let ((result (char-upcase look)))
    (get-char!)
    result))

;;; Get a Number

(define (get-num)
  (let ((value 0))
    (if (not (is-digit? look))
        (expected "Integer"))
    (do ()
        ((not (is-digit? look)))
      (set! value (+ (* 10 value)
                     (- (char->integer look)
                        (char->integer #\0))))
      (get-char!))
    value))

;;; Output a String with Tab

(define (emit s)
  (display (string-append "\t" s)))

;;; Output a String with Tab and CRLF

(define (emit-ln s)
  (emit s)
  (newline))

;;; Parse and Translate a Math Factor

(define (factor)
  (let ((result))
    (if (char=? look #\()
        (begin
          (match #\()
          (set! result (expression))
          (match #\))
          result)
        (get-num))))

;;; Parse and Translate a Math Term

(define (term)
  (let ((value (get-num)))
    (do ()
        ((not (or (char=? look #\*)
                  (char=? look #\/))))
      (case look
        ((#\*)
         (match #\*)
         (set! value (* value (factor))))
        ((#\/)
         (match #\/)
         (set! value (quotient value (factor))))))
    value))

;;; Parse and Translate an Expression

(define (expression)
  (let ((value))
   (if (is-addop? look)
       (set! value 0)
       (set! value (term)))
   (do ()
       ((not (is-addop? look)))
     (case look
       ((#\+)
        (match #\+)
        (set! value (+ value (term))))
       ((#\-)
        (match #\-)
        (set! value (- value (term))))))
   value))

;;; Initialize

(define (init)
  (get-char!))

;;; Main Program

(define (cradle)
  (init)
  (display (expression)))

