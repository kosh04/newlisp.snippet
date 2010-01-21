;;; cl.lsp --- Common Lisp like functions

;; (constant (global 't) true)
(define (null x) (not (true? x)))
(define car first)
(define cdr rest)
(define defconstant
  (lambda-macro ()
    (constant (args 0) (eval (args 1)))))
(define export global)
(define progn begin)
(define (funcall f) (apply f (args)))
(define let* letn)
(define intern sym)                     ; or make-symbol
(define symbol-name name)
(define char-code char)                 ; (char "A") => 65
(define code-char char)                 ; (char 65)  => "A"
(define rplaca                          ; (rplaca x y)
  (lambda-macro ()
    (setf (first (eval (args 0))) (eval (args 1)))
    (eval (args 0))))
(define rotatef swap)
(define complement
  (lambda-macro ()
    (letex ((f (args 0)))
      (lambda ()
        (not (apply f (args)))))))
(define identity
  ;; なんでマクロにしたんだっけ？
  (lambda-macro ()
    (eval (args 0))))

(define (find-symbol str (ctx (context)))
  (sym str ctx nil))

(define read-from-string read-expr)

;;; @@number
(constant 'most-positive-fixnum 0x7fffffffffffffff)
(constant 'most-negative-fixnum 0x8000000000000000)
(defconstant pi (mul (atan 1) 4))       ; 3.141592654 (mul (acos 0) 2)
(define equal =)
(define incf inc)
(define decf dec)
(define (plusp number) (< 0 number)) ; or (> number) , (sgn number nil nil true)
(define (minusp number) (< number 0)) ; or (< number) , (sgn number true nil nil)
(define (ash i cnt) (sgn cnt (>> i (abs cnt)) i (<< i cnt)))
(define logand &)
(define logxor ^)
(define logior |)
(define lognot ~)
(define expt pow)
(define (/= number)
  "true if NUMBER and rest numbers are different all. otherwise nil."
  (for-all (lambda (x) (not (= x number))) (args)))
;; (/= 1 2 3 1)                            ; nil
;; (!= 1 2 3 1)                            ; true ?

;;; @@list
(define intersection intersect)
(define set-difference difference)
(define butlast chop)
(define (nthcdr n lst) (slice lst n))
(define (common-lisp:last lst (n 1))
  ((- n) lst))
(define every for-all)
(define (some f lst)
  (dolist (obj lst (f obj))))
(define position find)
(define find-if exists)
(define remove-duplicates unique)
;(define (remove item seq) (clean (fn (x) (= x item)) seq))
(define (remove item seq)
  (if (string? seq)
      (replace item seq "")
      (replace item seq)))
(define remove-if clean)
(define remove-if-not filter)
(define common-lisp:delete              ; 破壊的 (destructive)
  (lambda-macro ()
    (if (string? (eval (args 1)))
        (replace (eval (args 0)) (eval (args 1)) "")
        (replace (eval (args 0)) (eval (args 1))))))
(define (count-if f seq)
  (length (filter f seq)))
(define (mapcar f lst)
  "syntax: (mapcar function list &rest more-lists)"
  (letn ((lists (cons lst (args)))
         (minlength (apply min (map length lists))))
    (apply map (cons f (map (lambda (x)
                              (slice x 0 minlength))
                            lists)))))
;; (mapcar list '(1 2 3 4) '(10 nil 30) '(100 200 300 400 500 600))
;; => ((1 10 100) (2 nil 200) (3 30 300))
;; (map list '(1 2 3 4) '(10 nil 30) '(100 200 300 400 500 600))
;; => ((1 10 100) (2 nil 200) (3 30 300) (4 nil 400))

;;; @@sequence
;(define concat string)
(define (concat) (join (args)))
(define copy-seq copy)
(define string-upcase upper-case)
(define string-downcase lower-case)
(define string-capitalize title-case)

(define (subseq seq start end)
  (cond (end (slice seq start (- end start)))
        (true (slice seq start))))

(define (string-equal string1 string2)
  "Compare two strings ignore case."
  (let ((PCRE_CASELESS 1))
    (list? (regex (string "^" (regex-quote string1) "$")
                  string2
                  PCRE_CASELESS))))

(define (string-left-trim char-bag str)
  (if (string? char-bag)
      (setq char-bag (map char (explode char-bag))))
  (catch
      (dostring (c str)
        (unless (member c char-bag)
          (throw (slice str $idx))))))

(define (string-right-trim char-bag str)
  (if (string? char-bag)
      (setq char-bag (map char (explode char-bag))))
  (catch
      (dostring (c (reverse (copy str)))
        (unless (member c char-bag)
          (throw (slice str 0 (- (length str) $idx)))))))

(define (string-trim char-bag str)
  (string-right-trim char-bag (string-left-trim char-bag str)))

;; (define (string-trim char-bag str) (trim str char-bag char-bag))
;; (define (string-left-trim char-bag str) (trim str char-bag ""))
;; (define (string-right-trim char-bag str) (trim str "" char-bag))

(define-macro (ignore-errors form)
  (eval-string (prin1-to-string form) (context) nil))

;; @syntax (unwind-protect protected-form cleanup-form*) => result
;; (context 'unwind-protect)
(letex ((result (sym (uuid))))
(define-macro (unwind-protect )
  (local (result)
    (if (catch (eval (args 0)) 'result)
        (begin (map eval (1 (args))) result)
        (begin (map eval (1 (args))) (throw-error (5 result))))))
)

(define (prin1-to-string obj)
  (cond ((string? obj) (format"\"%s\"" (replace "\\" obj "\\\\")))
        ("else" (string obj))))

(context MAIN)
;;; EOF
