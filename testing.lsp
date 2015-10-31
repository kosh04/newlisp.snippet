;; @module testing.lsp
;; @description simple testing log tool
;; @version 0.1-preview

;;; Example:

;; (ok (nil? nil))
;; (ok (= (apply + (sequence 1 10)) 55) "1+2+...+10 = 55")

;; (is (fact 10) 3628800 "fact(10) is 3628800")

;; @syntax (ok Expr [Description])
(define ok
  (define-macro (expr (text ""))
    ()))

;; @syntax (is Expr Expect [Description])
(define is
  (define-macro (is expr expect (text ""))
    (ok (= (eval expr) (eval expect)) text)))

;; eof
