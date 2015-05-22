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

;; @example
;;  (assert (= 0 (fib 0)))
;;  (assert (= 1 (fib 1)))
;;  (assert (= 55 (fib 10)))
(define-macro (assert)
  (or (not (nil? (eval (args 0))))
      (throw-error (list "assert fail" (args 0)))))
