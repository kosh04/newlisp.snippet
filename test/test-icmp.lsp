;; test-icmp.lsp

(unless (member ostype '("Win32" "Windows" "Cygwin"))
  (println "Nothing to do on unix platform")
  (exit))

(load "icmp.lsp")
;;(load "unittest.lsp")

(println (Icmp:ping "localhost"))
(println (Icmp:ping "www.newlisp.org"))
(println (Icmp:ping "google.com"))
(println (Icmp:ping "yahoo.com"))
(println (Icmp:ping "github.com"))
(println (Icmp:ping "8.8.8.8"))

(exit)
