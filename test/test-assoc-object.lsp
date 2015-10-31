;;; test-utils.lsp

(load "assoc-object.lsp")
(load "unittest.lsp")

(define-test "mkassoc"
  (= (mkassoc) (@) '(@))
  (= (let ((HELLO "world")
           (n1 (+ 1 2 3)))
       (mkassoc HELLO n1 (n2 (+ 4 5 6))))
     (@ (HELLO "world")
        (n1 (+ 1 2 3))
        (n2 (+ 4 5 6)))
     (quote
      (@ (HELLO "world")
         (n1 6)
         (n2 15))))
  (= (@ ostype)
     (list '@ (list 'ostype ostype)))
  )

(Test:run)
