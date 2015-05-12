;;; test-mlang.lsp -*- coding: utf-8 -*-

(unless (member ostype '("Win32" "Windows" "Cygwin"))
  (println "Nothing to do on unix platform")
  (exit))

(load "mlang.lsp")
(load "unittest.lsp")

(define (MLangTest:lossless= str codepage)
  (= str (MLang:decode (MLang:encode str codepage) codepage)))

(define-test "lossless="
  (MLangTest:lossless= "" "sjis")
  (MLangTest:lossless= "" "euc-jp")
  (MLangTest:lossless= "" "utf-8")
  (MLangTest:lossless= "\u3053\u3093\u306b\u3061\u306f" "sjis")
  (MLangTest:lossless= "\u3053\u3093\u306b\u3061\u306f" "euc-jp")
  (MLangTest:lossless= "\u3053\u3093\u306b\u3061\u306f" "utf-8")
  (MLangTest:lossless= "私はガラスを食べられます。それは私を傷つけません。" "sjis")
  (MLangTest:lossless= "私はガラスを食べられます。それは私を傷つけません。" "euc-jp")
  (MLangTest:lossless= "私はガラスを食べられます。それは私を傷つけません。" "utf-8"))

(Test:run)
