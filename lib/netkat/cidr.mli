type octets = int * int * int * int

(** An IPv4 network in CIDR notation, e.g. 192.168.1.1/24 *)
type t = {
  octets : octets;  (** [192; 168; 1; 1] *)
  prefix : int;       (** 24 *)
}

(** Parses "a.b.c.d/p". Fails on octets > 255 or prefix > 32. *)
val of_string : string -> t

val to_string : t -> string

(** One @srcK=octet filter per octet covered by the prefix, e.g.
    192.168.1.1/24 = @src0=192;@src1=168;@src2=1. A prefix that is not a
    multiple of 8 becomes a union over the matching values of the
    partially-covered octet. *)
val to_exp : t -> Nkexp.t
