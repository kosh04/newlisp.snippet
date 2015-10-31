;; memo/winsock.lsp

;; How to use network-byte-order in libffi?

;; typedef struct {
;;   union {
;;     struct {
;;       u_char s_b1,s_b2,s_b3,s_b4;
;;     } S_un_b;
;;     struct {
;;       u_short s_w1,s_w2;
;;     } S_un_w;
;;     u_long S_addr;
;;   } S_un;
;; } IPAddr;
(struct 'S_un_b _u_char _u_char _u_char _u_char)
(struct 'S_un_w _u_short _u_short)
(struct 'IPAddr _u_long)                ; FIXME: how to use C type union {...};

;; typedef struct ip_option_information {
;;   UCHAR  Ttl;
;;   UCHAR  Tos;
;;   UCHAR  Flags;
;;   UCHAR  OptionsSize;
;;   PUCHAR OptionsData;
;; } IP_OPTION_INFORMATION, *PIP_OPTION_INFORMATION;
(struct 'ip_option_information _UCHAR _UCHAR _UCHAR _UCHAR _PUCHAR)

;; typedef struct icmp_echo_reply {
;;   IPAddr                       Address;
;;   ULONG                        Status;
;;   ULONG                        RoundTripTime;
;;   USHORT                       DataSize;
;;   USHORT                       Reserved;
;;   PVOID                        Data;
;;   struct ip_option_information  Options;
;; } ICMP_ECHO_REPLY, *PICMP_ECHO_REPLY;
(struct 'icmp_echo_reply _u_long _ULONG _ULONG _USHORT _USHORT _PVOID "ip_option_information")
