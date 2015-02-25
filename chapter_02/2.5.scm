
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

;;; Get an Identifier

(define (get-name)
  (if (not (is-alpha? look))
      (expected "Name"))
  (let ((result (char-upcase look)))
    (get-char!)
    result))

;;; Get a Number

(define (get-num)
  (if (not (is-digit? look))
      (expected "Integer"))
  (let ((result look))
    (get-char!)
    result))

;;; Output a String with Tab

(define (emit s)
  (display (string-append "\t" s)))

;;; Output a String with Tab and CRLF

(define (emit-ln s)
  (emit s)
  (newline))

;;; Recognize and Translate an Add

(define (add)
  (match #\+)
  (term)
  (emit-ln "ADD (SP)+, D0"))

;;; Recognize and Translate a Subtract

(define (substract)
  (match #\-)
  (term)
  (emit-ln "SUB (SP)+, D0")
  (emit-ln "NEG D0"))

;;; Parse and Translate a Math Expression

(define (term)
  (emit-ln (string-append "MOVE #" (string (get-num)) ", D0")))

;;; Parse and Translate an Expression

(define (expression)
  (term)
  (do ()
      ((not (or (char=? look #\+)
                (char=? look #\-))))
    (emit-ln "MOVE D0, -(SP)")
    (case look
      ((#\+) (add))
      ((#\-) (substract))
      (else (expected "Addop")))))

;;; Initialize

(define (init)
  (get-char!))

;;; Main Program

(define (cradle)
  (init)
  (expression))

