;;; test-unicode.lsp	-*- coding: utf-8 -*-

(load "unicode.lsp")
(load "unittest.lsp")

(define-test "utf8"
  (primitive? utf8))

(define-test "utf8qa"
  (= (length (char 937)) 2))

(define-test "unicode-escape"
  (= (unicode-escape "") "")
  (= (unicode-escape "hello") "hello")
  (= (unicode-escape "\u3053\u3093\u306b\u3061\u306f")
     "\\u3053\\u3093\\u306b\\u3061\\u306f")
  (= (unicode-escape "\\u3053\\u3093\\u306b\\u3061\\u306f")
     "\\u3053\\u3093\\u306b\\u3061\\u306f"))

(define-test "unicode-unescape"
  (= (unicode-unescape "") "")
  (= (unicode-unescape "hello") "hello")
  (= (unicode-unescape "\u3053\u3093\u306b\u3061\u306f")
     "\u3053\u3093\u306b\u3061\u306f")
  (= (unicode-unescape "\\u3053\\u3093\\u306b\\u3061\\u306f")
     "\u3053\u3093\u306b\u3061\u306f"))

(Test:run)
