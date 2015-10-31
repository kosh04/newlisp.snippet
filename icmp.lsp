;; @module icmp.lsp
;; @description Enable win32 ICMP echo request (IPv4 only)
;; @version 0.2
;; @lisence MIT

;; @link
;; - INFO: Implementing Incternet Pings Using Icmp.dll
;;   https://support.microsoft.com/ja-jp/kb/170591
;; - Microsoft's ICMP API
;;   http://www.sockets.com/ms_icmp.htm
;; - Winsock Programmer's FAQ: Ping: ICMP.DLL Method
;;   http://www.kt.rim.or.jp/~ksk/wskfaq-ja/examples/dllping.html

;;; NOTE:

;; Win64 version does not work yet.

;;; Code:

(context 'Icmp)

(import "ws2_32.dll" "inet_addr")
(import "ws2_32.dll" "inet_ntoa")

(define _icmp_dll "Iphlpapi.dll")

;; HANDLE IcmpCreateFile(void);
(import _icmp_dll "IcmpCreateFile")

;; BOOL IcmpCloseHandle(_In_ HANDLE IcmpHandle);
(import _icmp_dll "IcmpCloseHandle")

;; DWORD IcmpParseReplies(
;;   _In_ LPVOID ReplyBuffer,
;;   _In_ DWORD  ReplySize
;; );
(import _icmp_dll "IcmpParseReplies")

;; DWORD IcmpSendEcho(
;;   __in     HANDLE IcmpHandle,
;;   __in     IPAddr DestinationAddress,
;;   __in     LPVOID RequestData,
;;   __in     WORD RequestSize,
;;   __in     PIP_OPTION_INFORMATION RequestOptions,
;;   __inout  LPVOID ReplyBuffer,
;;   __in     DWORD ReplySize,
;;   __in     DWORD Timeout
;; );
(import _icmp_dll "IcmpSendEcho")

(import _icmp_dll "IcmpSendEcho2")
(import _icmp_dll "do_echo_rep")
(import _icmp_dll "do_echo_req")
(import _icmp_dll "register_icmp")

(define INVALID_HANDLE_VALUE -1) ; (HANDLE)(-1)

;;#include <ipexport.h>
(define IP_STATUS_BASE 11000)

(define IP_SUCCESS 0)
(define IP_BUF_TOO_SMALL (+ IP_STATUS_BASE 1))
(define IP_DEST_NET_UNREACHABLE (+ IP_STATUS_BASE 2))
(define IP_DEST_HOST_UNREACHABLE (+ IP_STATUS_BASE 3))
(define IP_DEST_PROT_UNREACHABLE (+ IP_STATUS_BASE 4))
(define IP_DEST_PORT_UNREACHABLE (+ IP_STATUS_BASE 5))
(define IP_NO_RESOURCES (+ IP_STATUS_BASE 6))
(define IP_BAD_OPTION (+ IP_STATUS_BASE 7))
(define IP_HW_ERROR (+ IP_STATUS_BASE 8))
(define IP_PACKET_TOO_BIG (+ IP_STATUS_BASE 9))
(define IP_REQ_TIMED_OUT (+ IP_STATUS_BASE 10))
(define IP_BAD_REQ (+ IP_STATUS_BASE 11))
(define IP_BAD_ROUTE (+ IP_STATUS_BASE 12))
(define IP_TTL_EXPIRED_TRANSIT (+ IP_STATUS_BASE 13))
(define IP_TTL_EXPIRED_REASSEM (+ IP_STATUS_BASE 14))
(define IP_PARAM_PROBLEM (+ IP_STATUS_BASE 15))
(define IP_SOURCE_QUENCH (+ IP_STATUS_BASE 16))
(define IP_OPTION_TOO_BIG (+ IP_STATUS_BASE 17))
(define IP_BAD_DESTINATION (+ IP_STATUS_BASE 18))

(define IP_DEST_NO_ROUTE (+ IP_STATUS_BASE 2))
(define IP_DEST_ADDR_UNREACHABLE (+ IP_STATUS_BASE 3))
(define IP_DEST_PROHIBITED (+ IP_STATUS_BASE 4))
(define IP_DEST_PORT_UNREACHABLE (+ IP_STATUS_BASE 5))
(define IP_HOP_LIMIT_EXCEEDED (+ IP_STATUS_BASE 13))
(define IP_REASSEMBLY_TIME_EXCEEDED (+ IP_STATUS_BASE 14))
(define IP_PARAMETER_PROBLEM (+ IP_STATUS_BASE 15))

(define IP_DEST_UNREACHABLE (+ IP_STATUS_BASE 40))
(define IP_TIME_EXCEEDED (+ IP_STATUS_BASE 41))
(define IP_BAD_HEADER (+ IP_STATUS_BASE 42))
(define IP_UNRECOGNIZED_NEXT_HEADER (+ IP_STATUS_BASE 43))
(define IP_ICMP_ERROR (+ IP_STATUS_BASE 44))
(define IP_DEST_SCOPE_MISMATCH (+ IP_STATUS_BASE 45))

(define IP_ADDR_DELETED (+ IP_STATUS_BASE 19))
(define IP_SPEC_MTU_CHANGE (+ IP_STATUS_BASE 20))
(define IP_MTU_CHANGE (+ IP_STATUS_BASE 21))
(define IP_UNLOAD (+ IP_STATUS_BASE 22))
(define IP_ADDR_ADDED (+ IP_STATUS_BASE 23))
(define IP_MEDIA_CONNECT (+ IP_STATUS_BASE 24))
(define IP_MEDIA_DISCONNECT (+ IP_STATUS_BASE 25))
(define IP_BIND_ADAPTER (+ IP_STATUS_BASE 26))
(define IP_UNBIND_ADAPTER (+ IP_STATUS_BASE 27))
(define IP_DEVICE_DOES_NOT_EXIST (+ IP_STATUS_BASE 28))
(define IP_DUPLICATE_ADDRESS (+ IP_STATUS_BASE 29))
(define IP_INTERFACE_METRIC_CHANGE (+ IP_STATUS_BASE 30))
(define IP_RECONFIG_SECFLTR (+ IP_STATUS_BASE 31))
(define IP_NEGOTIATING_IPSEC (+ IP_STATUS_BASE 32))
(define IP_INTERFACE_WOL_CAPABILITY_CHANGE (+ IP_STATUS_BASE 33))
(define IP_DUPLICATE_IPADD (+ IP_STATUS_BASE 34))
(define IP_NO_FURTHER_SENDS (+ IP_STATUS_BASE 35))

(define IP_GENERAL_FAILURE (+ IP_STATUS_BASE 50))
(define MAX_IP_STATUS IP_GENERAL_FAILURE)
(define IP_PENDING (+ IP_STATUS_BASE 255))


(define _timeout 1000)
(define _send_data "ICMP SEND DATA")
(define _sizeof_ICMP_ECHO_REPLY 28)

(define-macro (mkassoc)
  "make assoc list by symbols/lists."
  (map (lambda (x)
         (cond ((symbol? x)
                (list (term x) (eval x)))
               ((list? x)
                (list (string (first x)) (eval (last x))))))
       (args)))

(define (error)
  (let ((msg (apply format (args))))
    (throw (mkassoc ("error" msg)))))

(define (parse-echo-reply ReplyBuffer)
  ;;(IcmpParseReplies ReplyBuffer (length ReplyBuffer)) ; should == 0
  (apply (lambda (ipaddr status rtt size reserved data* opt)
           (if (= status 0)
               (let ((from (get-string (inet_ntoa ipaddr)))
                     (data (get-string data*)))
                 (mkassoc from status rtt size data))
               (mkassoc status)))
         (unpack "lu lu lu u u lu lu" ReplyBuffer)))

;; @syntax (Icmp:ping <target> [<timeout>])
;; @param <target> String: Target hostname or IP-address
;; @param <timeout> Integer: The time to wait for replies (in milliseconds)
;; @return Assoc: Echo reply data.
;; @esample
;; (Icmp:ping "newlisp.org")
;; => (("from" "208.94.116.204") ("status" 0) ("rtt" 146) ("size" 14) ("data" "ICMP SEND DATA"))
(define (Icmp:ping target (timeout _timeout))
  (catch
    (let ((icmp (IcmpCreateFile))
          (reply_buffer (dup "\000" (+ _sizeof_ICMP_ECHO_REPLY (length _send_data))))
          (host_addr (inet_addr (or (net-lookup target true)
                                    (error "net-lookup: %s" (last (net-error))))))
          (ret -1))
      (if (= icmp INVALID_HANDLE_VALUE)
          (error "IcmpCreateFile: %d" icmp))
      (setf ret (IcmpSendEcho2 icmp 0 0 0
                               host_addr
                               _send_data
                               (length _send_data)
                               0
                               reply_buffer
                               (length reply_buffer)
                               timeout))
      (IcmpCloseHandle icmp)
      (letn ((rep (parse-echo-reply reply_buffer))
             (status (lookup "status" rep)))
        (cond ((= status 0) rep)
              ((= status IP_REQ_TIMED_OUT) nil)
              (true
               (error "IcmpSendEcho: %d" status)))))))

(context MAIN)
;;; icmp.lsp ends here
