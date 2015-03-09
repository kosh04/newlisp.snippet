;;; regex.lsp --- Regular Expression (pcre) functions for newLISP

;; IPアドレスの正規表現の例はこちらから
;; http://www.regular-expressions.info/regexbuddy/ipaccurate.html

(define re-mac-addr
  "(?i)((?:[0-9a-f]{2}[:-]){5}[0-9a-f]{2})")

(define re-ip-addr
  {\b(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\b})

(define (regex-quote str)
  (replace "[][$*+.?\\^]" str (string "\\" $0) 0))

(define (regex? str)
  (if (string? str)
      (if (starts-with str "ERCP")
          true                          ; compiled-regex
          'maybe)                       ; XXX: possible to compile?
      nil))

(setq
  PCRE_CASELESS        1
  PCRE_MULTILINE       2
  PCRE_DOTALL          4
  PCRE_EXTENDED        8
  PCRE_ANCHORED       16
  PCRE_DOLLAR_ENDONLY 32
  PCRE_EXTRA          64
  PCRE_NOTBOL        128
  PCRE_NOTEOL        256
  PCRE_UNGREEDY      512
  PCRE_NOTEMPTY     1024
  PCRE_UTF8         2048
  REPLACE_ONCE    0x8000
  PRECOMPILED    0x10000
 )

;; 正規表現のオプション(//six)を数値に変換するヘルパー関数
;; v.10.6.2 より直接指定が利用可能
(define (re kwd)
  (letn ((opt 0)
         (|| (lambda (i) (setq opt (| opt i)))))
    (dostring (c kwd)
      (case (char c)
        ("i" (|| 1))        ; PCRE_CASELESS
        ("m" (|| 2))        ; PCRE_MULTILINE
        ("s" (|| 4))        ; PCRE_DOTALL
        ("x" (|| 8))        ; PCRE_EXTENDED
        (true (throw-error "unknown keyword"))))
    opt))


(context MAIN)
;;; EOF
