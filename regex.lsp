;;; regex.lsp --- Regular Expression (pcre) functions for newLISP

(define split-string parse)
(define compile-regexp regex-comp)

(define (string-match regexp str (start 0) end)
  (regex regexp (subseq str start end)))

(define (substitute-string str pattern replacement)
  (replace pattern str replacement))

;; CLISP: clisp/regexp/regexp.lisp
;;   ($ ^ . * [ ] \ + ?) :extended
;;   ($ ^ . * [ ] \ )
;; PHP: preg_quote (. \ + * ? [ ^ ] $ ( ) { } = ! < > | : -) -> "[]!$(-+.:<-?[\\{|}^-]"
;; Ruby: Regexp.quote
(define (regex-quote str)
  (replace "[][$*+.?\\^]" str (string "\\" $0) 0))

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

(context MAIN)
;;; EOF
