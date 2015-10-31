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

(define-test "fold"
  (= (fold cons '() '(1 2 3 4)) '(4 3 2 1))
  (= (fold list 'foo '(1 2 3 4)) '(4 (3 (2 (1 foo))))))

(define-test "foldl"
  (= (foldl cons '() '(1 2 3 4)) '((((() 1) 2) 3) 4))
  (= (foldl list 'foo '(1 2 3 4)) '((((foo 1) 2) 3) 4)))

(define-test "foldr"
  (= (foldr cons '() '(1 2 3 4)) '(1 2 3 4))
  (= (foldr list 'foo '(1 2 3 4)) '(1 (2 (3 (4 foo))))))

(define-test "reduce"
  (= (reduce * '(1 2 3 4 5)) 120)
  (= (reduce - '(1 2 3 4 5)) -13)       ;== (- (- (- (- 1 2) 3) 4) 5)
  (= (reduce + '()) 0)
  (= (reduce + '(3)) 3)
  (= (reduce list '(1 2 3 4)) '(((1 2) 3) 4))
;;(= (reduce + '(foo)) 'foo)
  )

