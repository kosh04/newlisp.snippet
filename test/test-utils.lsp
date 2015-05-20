;;; test-utils.lsp

(load "utils.lsp")
(load "unittest.lsp")

(define-test "compose"
  (define (double x) (* x 2))
  (= ((compose) 10) 10)
  (= ((compose ++ ++) 1) 3)
  (= ((compose ++ double) 5) 11)
  (= ((compose ++ double ++) 6) 15)
)

(define-test "mkassoc"
  (= (mkassoc) '())
  (= (let ((HELLO "world")
           (n1 (+ 1 2 3)))
       (mkassoc HELLO n1 (n2 (+ 4 5 6))))
     '(("HELLO" "world")
       ("n1" 6)
       ("n2" 15)))
   )

(Test:run)
