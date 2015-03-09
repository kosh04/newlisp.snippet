;;; sequence.lsp

(define (sample seq)
  (unless (or (list? seq)
              (string? seq))
    (throw-error "Error:LIST_OR_STRING_EXPECTED"))
  ;;(first (randomize seq true))
  (seq (rand (length seq))))

;; syntax: (range [from] to [step])
(define (range)
  (case (length (args))
    (1 (sequence 0 (args 0)))
    (2 (sequence (args 0) (args 1)))
    (3 (sequence (args 0) (args 1) (args 2)))
    (true '())))
