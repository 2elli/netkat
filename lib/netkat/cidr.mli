type octets = int * int * int * int

(** Which address fields a network matches on: @srcK or @dstK. *)
type direction = SRC | DST

type t = { octets : octets;  (** [192; 168; 1; 1] *) prefix : int  (** 24 *) }
(** An IPv4 network in CIDR notation, e.g. 192.168.1.1/24 *)

val of_string : string -> t
(** Parses "a.b.c.d/p". Fails on octets > 255 or prefix > 32. *)

val to_string : t -> string

val to_exp : direction -> t -> Nkexp.t
(** One @srcK=octet (or @dstK=octet for [DST]) filter per octet covered by
    the prefix, e.g. 192.168.1.1/24 = @src0=192;@src1=168;@src2=1. A prefix
    that is not a multiple of 8 becomes a union over the matching values of
    the partially-covered octet. *)
