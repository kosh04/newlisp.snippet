;;; test-builtin.lsp

(load "unittest.lsp")

;; ~/Downloads/lang/newlisp-git/newlisp/qa-dot

(define-test "$"
  (begin
    (find "a|b" "xtzabc" 0)
    (= ($ 0) $0)))

(define-test "!"
  (integer? (! "")))

(define-test "!=#integer"
  (!= 0 1)
  (!= 1 2 3 1)                          ; ?
  )

(define-test "!=#string"
  (!= "abc" "ABC")
  (!= "a" (char 0x3fffe4))
  (!= (char 0x3fffe4) "a"))

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
