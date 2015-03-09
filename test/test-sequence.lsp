;;; test-sequence.lsp

(load "sequence.lsp")
(load "unittest.lsp")

(define-test "sample"
  (let ((lst '(a b c)))
    (member (sample lst) lst)))

(define-test "range"
  (= (range) '())
  (= (range 0) '(0))
  (= (range 9) '(0 1 2 3 4 5 6 7 8 9))
  (= (range -9) '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9)) ; ?
  (= (range 1 5) '(1 2 3 4 5))
  (= (range 1 5 0.5) '(1 1.5 2 2.5 3 3.5 4 4.5 5)))
