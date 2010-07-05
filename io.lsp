;; io.lsp --- Input/Output functions for newLISP

;; http://en.wikipedia.org/wiki/Standard_streams
(setq stdin 0 stdout 1 stderr 2)

;; call-with-{input,output}-file @scheme
(define (with-file-handler filename proc (mode "r"))
  (let ((fd (or (open (namestring filename) mode)
                (throw-error (list filename (sys-error))))))
    (unwind-protect
        (proc fd)
      (close fd))))

(define (with-output-file filename proc) (with-file-handler filename proc "w"))
(define (with-input-file filename proc) (with-file-handler filename proc "r"))


(define (echo in (out stdout))
  (cond ((socket? in)
         (let ((len (net-peek in)) buf)
           (when (!= len 0)
             (net-receive in buf len)
             (write-line out buf))))
        ("else"
         (local (buf)
           (while (read in buf 0x800)
             (write out buf)))
         ;; (while (read-line in) (write-line out))
         ))
  (if (string? out) out))
;; (echo stdin "HELLO ") => "HELLO newLISP!\n"

;; == (define cat (lambda (file) (print (read-file file))))
(define (cat filename)
  (with-input-file filename echo))

(define (tee filename buffer)
  "Write BUFFER contents to standard-output and FILENAME."
  (append-file filename buffer)
  (print buffer))

(unless peek
;; for Win32
(define (peek fd)
  (or (net-peek fd)
      (let ((ptr (seek fd)))
        (when ptr
          (- (seek fd -1) (seek fd ptr))))))
)

(context MAIN)
;;; EOF
