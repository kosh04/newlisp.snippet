;;; test-builtin.lsp

(load "unittest.lsp")

;; Refer from ~/Downloads/lang/newlisp-git/newlisp/qa-dot

(define-test "$"
  (begin
    (find "a|b" "xtzabc" 0)
    (= ($ 0) $0))
  (let ($0 123)
    (= ($ 0) 123))
  )

(define-test "!"
  (integer? (! "")))

(define-test "!=#integer"
  (!= 0 1)
  (!= 1 2 3 1)                          ; ?
  (!= 1)                                ; ?
  )

(define-test "!=#string"
  (!= "abc" "ABC")
  (!= "abc")                            ; ?
  (!= "a" (char 0x3fffe4))
  (!= (char 0x3fffe4) "a")
  )

(define-test "!=#list"
  (!= '(1 2 3))                          ; ?
  )

(define-test "%"
  (= (%  10 3)  1)
  (= (% -10 3) -1))

(define-test "%#error"
  (local (noerr)
    (nil? (catch (% 0 0) 'noerr))))

(define-test "&"
  (= 0x8000000000000000
     (& 0x8000000000000000
        0xffffffffffffffff)))

(define-test "*"
  (= (* (* 123456789 123456789)) 15241578750190521)
  (= (*) 1))

(define-test "+"
  (= (+ 999999999999999999 1) 1000000000000000000)
  (= (+ 9223372036854775807 -9223372036854775808) -1)
  (= (+ -9223372036854775808 -1) 9223372036854775807) ; wraps around
  (= (+) 0))

(define-test "-"
  (= (- 100000000 1) 99999999))

(define-test "/"
  (= (/ 15241578750190521 123456789) 123456789)
  (= (/ -10 5) -2))

(define-test "<"
  (< 1 "a")                             ; ?
  (< "a" 'a))

(define-test "<#number"
  (< -9223372036854775808 9223372036854775807)
  (< 1 1.000000001)
  (< -1)
  (< -1 0)
  (< -1.23)
  (< -1.23 0)
  (nil? (< 1))
  (nil? (< 1 0)))

(define-test "<#string"
  (< "abcdefg" "abcdefgh")
  (true? (> "1"))                      ; ?
  (nil?  (< "1"))                      ; ?
  )

(define-test "<#list"
  (< '(a b) '(b c) '(c d))
  (< '(((a b))) '(((b c))))
  (< '(a (b c)) '(a (b d)) '(a (b (d))))
  (not (< '(a b) '(b d) '(b c)))
  (not (< '()))
  )

(define-test "<<"
  (= (<< 1  0) 0x0000000000000001)
  (= (<< 1  1) 0x0000000000000002)
  (= (<< 1  2) 0x0000000000000004)
  (= (<< 1  4) 0x0000000000000010)
  (= (<< 1  8) 0x0000000000000100)
  (= (<< 1 63) 0x8000000000000000))

(define-test "<="
  (<= -9223372036854775808
      -9223372036854775808)
  (<= 1 1.00000001))

(define-test "=#number"
  (= 1.23456789 1.23456789) 
  (= 123456789 123456789) 
  (= 0xFFFFFFFFFFFFFFFF -1)
  (= 0b1111111111111111111111111111111111111111111111111111111111111111 -1)
  (= 0)
  )

(define-test "=#string"
  (= "\xe9\xe2\xe4\xe1\xed\xf3\xfa\xf1\xd1\xf6\xf2"
     "\xe9\xe2\xe4\xe1\xed\xf3\xfa\xf1\xd1\xf6\xf2")
  (= "")                                ; ?
  )

(define-test "=#list"
  (= '(1 2 3 (4 5) (a b ("CDEFG" "HIJK") 'u 'v 'w)) 
     '(1 2 3 (4 5) (a b ("CDEFG" "HIJK") 'u 'v 'w)))
  (= '())                               ; ?
  )


(define-test "append#list"
  (= (append '(1 2 3) '(4 5 6) '(a b))
     '(1 2 3 4 5 6 a b)))

(define-test "append#array"
  (= (let ((A (array 3 2 (sequence 1 6)))
           (B (array 2 2 (sequence 7 10))))
       (append A B))
     ;; (array-list *) => '((1 2) (3 4) (5 6) (7 8) (9 10))
     (array 5 2 (sequence 1 10))))

(define-test "append#string"
  (= (let (more " how are you")
       (append "Hello " "world," more))
     "Hello world, how are you"))

(Test:run)
