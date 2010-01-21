;; io.lsp --- Input/Output functions for newLISP

;; http://en.wikipedia.org/wiki/Standard_streams
(setq stdin 0 stdout 1 stderr 2)

;; call-with-{input,output}-file @scheme
(define (with-file-handler filename proc (mode "r"))
  (let ((fd (or (open filename mode)
                (throw-error (list filename (sys-error))))))
    (unwind-protect
        (proc fd)
      (close fd))))

(define (with-output-file filename proc) (with-file-handler filename proc "w"))
(define (with-input-file filename proc) (with-file-handler filename proc "r"))

;; 出力を文字列に切り替えることも出来る
;; (echo stdin "HELLO ") => "HELLO newLISP!\n"
(define (echo in (out stdout))
  (cond ((socket? in)
         (let ((len (net-peek in)) buf)
           (when (!= len 0)
             (net-receive in buf len)
             (write-line out buf))))
        ("else"
         (while (read-line in)
           (write-line out))))
  (if (string? out) out))

;== (define cat read-file)
(define (cat filename)
  (with-input-file filename echo))

;; ファイルと標準出力へ書き出し
(define (tee filename buffer)
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
