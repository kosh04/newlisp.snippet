;; io.lsp --- Input/Output functions for newLISP

;; http://en.wikipedia.org/wiki/Standard_streams
(setq stdin 0 stdout 1 stderr 2)
#include <stdio.h>
(define	STDIN_FILENO	0)
(define	STDOUT_FILENO	1)
(define	STDERR_FILENO	2)

(define (open! )
  (or (apply open (args))
      (throw-error (list (args) (sys-error)))))

;; call-with-{input,output}-file @scheme
(define (with-file-handler filename proc (mode "r"))
  (let ((fd (open! (namestring filename) mode)))
    (unwind-protect
        (proc fd)
      (close fd))))

(define (with-output-file filename proc) (with-file-handler filename proc "w"))
(define (with-input-file filename proc) (with-file-handler filename proc "r"))

(define (echo (in stdin) (out stdout))
  (let ((buf "")
        (len 0))
    (cond
      ;; socket?
      ((and (integer? in) (net-local in))
       (setq len (net-peek in))
       (when (!= len 0)
         (net-receive in buf len)
         (write-line out buf)))
      (true
       ;; (while (read-line in) (write-line out))
       (while (read in buf 0x1000)      ; or (peek in)
         (write out buf)
         (++ len (length buf)))))
    (if (string? out)
        out
        len)))
;; (echo stdin "") => [make strings from input]

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
