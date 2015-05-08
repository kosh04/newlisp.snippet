;;; icmp.lsp

;; XXX: NOT WORKING YET

;; About Icmp.dll

;; INFO: Implementing Incternet Pings Using Icmp.dll
;; https://support.microsoft.com/ja-jp/kb/170591

;; Microsoft's ICMP API
;; http://www.sockets.com/ms_icmp.htm

;; Winsock Programmer's FAQ: Ping: ICMP.DLL Method
;; http://www.kt.rim.or.jp/~ksk/wskfaq-ja/examples/dllping.html

(load "c-typedef.lsp")
(load "winapi-typedef.lsp")

(define _icmp_dll "icmp.dll")

;; HANDLE IcmpCreateFile(void);
(import _icmp_dll "IcmpCreateFile" _HANDLE _void)

;; BOOL IcmpCloseHandle(_In_ HANDLE IcmpHandle);
(import _icmp_dll "IcmpCloseHandle" _BOOL _HANDLE)

;; DWORD IcmpParseReplies(
;;   _In_ LPVOID ReplyBuffer,
;;   _In_ DWORD  ReplySize
;; );
(import _icmp_dll "IcmpParseReplies" _DWORD _LPVOID _DWORD)

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
;;(import _icmp_dll "IcmpSendEcho" _DWORD _HANDLE "IPAddr" _LPVOID _WORD "void*" _LPVOID _DWORD _DWORD)
(import _icmp_dll "IcmpSendEcho")

(import _icmp_dll "IcmpSendEcho2")
(import _icmp_dll "do_echo_rep")
(import _icmp_dll "do_echo_req")
(import _icmp_dll "register_icmp")

;;; icmp.lsp ends here
