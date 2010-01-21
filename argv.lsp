;; argv.lsp --- provide cleanup arguments

;; newLISP の起動引数以外の引数を提供

;(context 'argv)

(define invocation-name (first $main-args)) ; "newlisp" or "newlisp.exe"
(define $argv (rest $main-args))

(define (argv i)
  (cond (i (when (< i (length $argv))
             ($argv i)))
        ("else" $argv)))

(define-macro (pop-args)
  (let ((n (find (eval (args 0)) $argv
                 (lambda (x y)
                   (starts-with y x 0)))))
    (when n
      (cond
        ((= $1 "")                      ; "--arg" "Value"
         (pop $argv (+ n 1))
         (pop $argv n))
        (true                           ; "--arg[Value]"
         (pop $argv n)))
      true)))

;;
(pop-args "-n")
(pop-args "-h")
(pop-args "-c")
(pop-args "-C")
(pop-args "-http")
;;; FIXME: valid regex?
(pop-args "-s(\\d*)")
(pop-args "-m(\\d*)")
(pop-args "-e(.*)")
(pop-args "-l(.*)")
(pop-args "-L(.*)")
(pop-args "-p(\\d*)")
(pop-args "-d(\\d*)")
(pop-args "-w(.*)")


(define (getopt optstring (has-value nil))
  "オプション引数の解析."
  (let ((pos (find optstring $main-args
                   (lambda (x y) (starts-with y x)))))
    (if (and pos has-value)
        (if (!= (main-args pos) optstring)
            (slice (main-args pos) (length optstring))
            (main-args (+ pos 1)))
        (integer? pos))))

;;; Example:
;; (main-args)                  ;=> ("newlisp" "-C" "-w" "/home" "-s10000")
;; (getopt "-w")                ;=> true
;; (getopt "-w" true)           ;=> "/home"
;; (getopt "-s" true)           ;=> "10000"
;; (getopt "-n")                ;=> nil


(context MAIN)

;;; EOF
