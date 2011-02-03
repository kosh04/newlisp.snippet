;; argv.lsp --- provide cleanup arguments

;; newLISP の起動引数以外の引数を提供

;;; ChangeLog:
;;
;; - 2010-01-21 初版作成
;; - 2010-10-04
;;   なるべくnewlisp起動時の流れに沿うように修正
;;   ("-start" のような失敗する引数も許すようになった)
;;   オプション-t,-6の追加
;; - 2011-01-29
;;   オプション-vの追加

;;; TODO
;;
;; - ファイル名の扱いはどうする？

;(context 'argv)

(define invocation-name (first $main-args)) ; "newlisp" or "newlisp.exe"
(define $argv (rest $main-args))

;; @syntax (argv index)
(define (argv i)
  (cond (i (if (< i (length $argv)) ($argv i) nil))
        (true $argv)))

;; @syntax (pop-args str value?)
(define (pop-args str (has-value nil))
  (let ((n (find str $argv
                 (lambda (x y)
                   (starts-with y x)))))
    (when n
      (cond
        ((and has-value (= 2 (length (argv n)))) ; "-arg" "value"
         (if (empty? ((+ n 1) $argv))
             (write 2 (string "missing parameter for " (argv n) "\n")) ; XXX
             (pop $argv (+ n 1)))         
         (pop $argv n))
        (true                           ; "-arg[value]"
         (pop $argv n))))
    nil))

;; "-n" option must be first.
(if (= (argv 0) "-n")
    (pop $argv))
;;
(pop-args "-h")
(pop-args "-c")
(pop-args "-C")
(pop-args "-http")
(pop-args "-s" true)
(pop-args "-m" true)
(pop-args "-e" true)
(pop-args "-l" true)
(pop-args "-L" true)
(pop-args "-p" true)
(pop-args "-d" true)
(pop-args "-t" true)
(pop-args "-v")
(pop-args "-w" true)
(pop-args "-6")


(define (getopt optstring (has-value nil))
  "オプション引数の解析."
  (let ((pos (find optstring $main-args
                   (lambda (x y) (starts-with y x 0)))))
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
