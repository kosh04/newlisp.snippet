;;; test-keyword.lsp

(load "keyword.lsp")
(load "unittest.lsp")

(define-test "keyword#string"
  (keyword? (keyword "foo"))
  (keyword? (eval (keyword "foo")))     ; :self-evaluate
  (= (term (keyword "foo")) ":foo"))

(define-test "keyword#symbol"
  (keyword? (keyword 'foo))
  (keyword? (eval (keyword 'foo)))
  (= (term (keyword 'foo)) ":foo")
  (= (term (keyword 'Class:foo)) ":foo")
  (= (keyword 'foo) (keyword 'Class:foo)))

(define-test "keyword#other-type"
  (nil? (keyword nil))
  (nil? (keyword 1)))
