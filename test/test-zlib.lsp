;; test-zlib.lsp

(load "zlib.lsp")
(load "unittest.lsp")

(setf _data_text "Hello Hello Hello Hello Hello Hello!")
(setf _data_file "zlib.lsp")
(setf _data_binary
      (if (find ostype '("Windows" "Win32"))
          (env "COMSPEC")
          (env "SHELL")))

(define (lossless= data)
  (= data (zlib:inf (zlib:def data))))

(define-test "simple"
  (lossless= _data_text)
  (lossless= (read-file _data_file))
  (lossless= (read-file _data_binary)))
