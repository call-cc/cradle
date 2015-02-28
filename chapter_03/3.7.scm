
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

;;; Recognize an Alpha Character

(define (is-alpha? c)
  (char-alphabetic? c))

;;; Recognize a Decimal Digit

(define (is-digit? c)
  (char-numeric? c))

;;; Recognize an Alphanumeric

(define (is-alnum? c)
  (or (is-alpha? c)
      (is-digit? c)))

;;; Recognize an Addop

(define (is-addop? c)
  (or (char=? c #\+)
      (char=? c #\-)))

;;; Recognize White Space

(define (is-white? c)
  (or (char=? c #\space)
      (char=? c #\tab)))

;;; Skip Over Leading White Space

(define (skip-white!)
  (do ()
      ((not (is-white? look)))
    (get-char!)))

;;; Match a Specific Input Character

(define (match x)
  (if (not (char=? look x))
      (expected (string-append "''" (string x) "''"))
      (begin
        (get-char!)
        (skip-white!))))

;;; Get an Identifier

(define (get-name)
  (let ((token ""))
    (if (not (is-alpha? look))
        (expected "Name"))
    (do ()
        ((not (is-alnum? look)))
      (set! token (string-append token (string (char-upcase look))))
      (get-char!))
    (skip-white!)
    token))

;;; Get a Number

(define (get-num)
  (let ((value ""))
    (if (not (is-digit? look))
        (expected "Integer"))
    (do ()
        ((not (is-digit? look)))
      (set! value (string-append value (string look)))
      (get-char!))
    (skip-white!)
    value))

;;; Output a String with Tab

(define (emit s)
  (display (string-append "\t" s)))

;;; Output a String with Tab and CRLF

(define (emit-ln s)
  (emit s)
  (newline))

;;; Parse and Translate an Identifier

(define (ident)
  (let ((name (string (get-name))))
    (if (char=? look #\()
        (begin
          (match #\()
          (match #\))
          (emit-ln (string-append "BSR " name)))
        (emit-ln (string-append "MOVE " name "(PC), D0")))))

;;; Parse and Translate a Math Factor

(define (factor)
  (cond
   ((char=? look #\()
    (match #\()
    (expression)
    (match #\)))
   ((is-alpha? look)
    (ident))
   (else
    (emit-ln (string-append "MOVE #" (string (get-num)) ", D0")))))

;;; Recognize and Translate a Multiply

(define (multiply)
  (match #\*)
  (factor)
  (emit-ln "MULS (SP)+, D0"))

;; Recognize and Translate a Divide

(define (divide)
  (match #\/)
  (factor)
  (emit-ln "MOVE (SP)+, D1")
  (emit-ln "DIVS D1, D0"))

;;; Parse and Translate a Math Term

(define (term)
  (factor)
  (do ()
      ((not (or (char=? look #\*)
                (char=? look #\/))))
    (emit-ln "MOVE D0, -(SP)")
    (case look
      ((#\*) (multiply))
      ((#\/) (divide)))))

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

;;; Parse and Translate an Expression

(define (expression)
  (if (is-addop? look)
      (emit-ln "CLR D0")
      (term))
  (do ()
      ((not (is-addop? look)))
    (emit-ln "MOVE D0, -(SP)")
    (case look
      ((#\+) (add))
      ((#\-) (substract)))))

;;; Parse and Translate an Assignment Statement

(define (assignment)
  (let ((name (get-name)))
    (match #\=)
    (expression)
    (emit-ln (string-append "LEA " (string name) "(PC), A0"))
    (emit-ln "MOVE D0, (A0)")))

;;; Initialize

(define (init)
  (get-char!)
  (skip-white!))

;;; Main Program

(define (cradle)
  (init)
  (assignment)
  (if (not (char=? look #\newline))
      (expected "Newline")))

