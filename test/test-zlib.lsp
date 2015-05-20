;; test-zlib.lsp

(load "zlib.lsp")

;(define (printf) (print (apply format (args))))

(import "msvcrt" "printf")
(import "msvcrt" "strlen")
;(import "cygwin1" "printf")
;(import "cygwin1" "strlen")

(setf a "Hello Hello Hello Hello Hello Hello!" ;(dup "\000" 50)
      b (dup "\000" 50)
      c (dup "\000" 50))


(let ((str "Hello Hello Hello Hello Hello Hello!"))
  (cpymem str a (length str)))

(printf "Uncompressed size is: %lu\n" (length a))
(printf "Uncompressed data is: '%.*s'\n" (length a) a)

(setf z (z_stream))

(:slot z "zalloc" 0)
(:slot z "zfree"  0)
(:slot z "opaque" 0)
(:slot z "avail_in" (length a))
(:slot z "next_in" (address a))
(:slot z "avail_out" (length b))
(:slot z "next_out" (address b))

(printf "deflate: %d,%d,%d\n"
 (zlib:deflateInit_ (z 1) zlib:Z_DEFAULT_COMPRESSION (zlib:version) (length (z 1)))
 (zlib:deflate (z 1) zlib:Z_FINISH)
 (zlib:deflateEnd (z 1))
 )

(:pp z)
(printf "Compressed size is: %lu\n" (:slot z "total_out"))
(printf "Compressed data is: '%.*s'\n" (:slot z "total_out") b)
;;(slice b 0 (strlen b))

;; inflate b into c
(setf zin (z_stream))
(:slot zin "zalloc" 0)
(:slot zin "zfree"  0)
(:slot zin "opaque" 0)
(:slot zin "avail_in" (- (:slot z "next_out") (address b)))
(:slot zin "next_in" (address b))
(:slot zin "avail_out" (length c))
(:slot zin "next_out" (address c))

(printf "inflate: %d,%d,%d\n"
 (zlib:inflateInit_ (zin 1) (zlib:version) (length (zin 1)))
 (zlib:inflate (zin 1) zlib:Z_NO_FLUSH)
 (zlib:inflateEnd (zin 1))
 )

(:pp zin)
(printf "Uncompressed size is: %lu\n" (:slot zin "total_out"))
(printf "Uncompressed data is: '%.*s'\n" (:slot zin "total_out") c)

(exit)
