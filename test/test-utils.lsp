;;; test-utils.lsp

(load "utils.lsp")
(load "unittest.lsp")

(define-test "compose"
  (define (double x) (* x 2))
  (= ((compose) 10) 10)
  (= ((compose ++ ++) 1) 3)
  (= ((compose ++ double) 5) 11)
  (= ((compose ++ double ++) 6) 15))

(Test:run)
