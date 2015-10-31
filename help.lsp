;;; help.lsp

(define (apropos str (do-print true))
  "Return symbols that matches the regexp."
  (let ((acc (find-all str (symbols) $it
                       (lambda (x y)
                         (regex x (term y))))))
    (when (and acc do-print)
      (dolist (item acc)
        (cond
         ((primitive? (eval item))
          (println item "\t" "<primitive>"))
         ((lambda? (eval item))
          (println item "\t" "<lambda>"))
         ((macro? (eval item))
          (println item "\t" "<macro>"))
         ("else"
          (println item)))))
    acc))
