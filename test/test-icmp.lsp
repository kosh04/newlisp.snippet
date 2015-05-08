;; test-icmp.lsp

(unless (member ostype '("Win32" "Windows" "Cygwin"))
  (exit))

(load "icmp.lsp")
(load "unittest.lsp")

(import "msvcrt.dll" "printf")
(import "kernel32.dll" "GetLastError")
(import "ws2_32.dll" "inet_addr")
(import "ws2_32.dll" "inet_ntoa")

(define _timeout 1000)
(define _send_data " !\"#$%&'()*+,-./0123456789:;<=>?@")
(define _sizeof_ICMP_ECHO_REPLY 28)

(define INVALID_HANDLE_VALUE -1) ; (HANDLE)(-1)

(define (parse-echo-reply ReplyBuffer)
  (apply (lambda (ipaddr status rtt size reserved data opt)
           (list "From" (unpack "bbbb" (pack ">lu" ipaddr))
                 "Status" status
                 "RTT" rtt
                 "Size" size
                 "Data" (get-string data)))
         (unpack ">lu< lu lu u u lu lu" ReplyBuffer)))

(define (icmp-ping host (timeout _timeout) (data _send_data))
  (let ((icmp (IcmpCreateFile))
        (reply_buffer (dup "\000" (+ _sizeof_ICMP_ECHO_REPLY (length data) 1)))
        (host_addr (inet_addr (or (net-lookup host true)
                                  (throw-error (list 'net-lookup (last (net-error)))))))
        (status -1))
    (if (= icmp INVALID_HANDLE_VALUE)
        (throw-error (list 'IcmpCreateFile icmp)))
    (setf status (IcmpSendEcho icmp
                               host_addr
                               data
                               (length data)
                               0
                               reply_buffer
                               (length reply_buffer)
                               timeout))
    (unless (!= status 0)
      (throw-error (list 'IcmpSendEcho status "GetLastError" (GetLastError))))
    (IcmpCloseHandle icmp)
    (parse-echo-reply reply_buffer)))

(println (icmp-ping "localhost"))
(println (icmp-ping "newlisp.org"))
(println (icmp-ping "google.com"))
(println (icmp-ping "yahoo.com"))
(println (icmp-ping "github.com"))

(exit)
