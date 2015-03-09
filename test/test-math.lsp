;;; test-math.lsp

(load "math.lsp")
(load "unittest.lsp")

(define-test "log10"
  (= (log10    1) 0.0)
  (= (log10   10) 1.0)
  (= (log10  100) 2.0)
  (= (log10 1000) 3.0))
