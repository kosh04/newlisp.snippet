;;; utils.lsp

(define (compose)
  "Compose function right-associatively."
  (letex ((_fns (reverse (args))))
    (lambda (x)
      (dolist (f '_fns)
        (setf x (f x)))
      x)))

(define (get-unix-time) (date-value))
(define (get-universal-time)
  (+ (get-unix-time) 2208988800)) ; (encode-universal-time 0 0 0 1 1 1970 0)

(define-macro (mkassoc)
  "make assoc list by symbols/lists."
  (map (lambda (x)
         (cond ((symbol? x)
                (list (term x) (eval x)))
               ((list? x)
                (list (string (first x)) (eval (last x))))))
       (args)))
