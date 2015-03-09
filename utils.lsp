;;; utils.lsp

(define (compose)
  "Compose function right-associatively."
  (letex ((_fns (reverse (args))))
    (lambda (x)
      (dolist (f '_fns)
        (setf x (f x)))
      x)))
