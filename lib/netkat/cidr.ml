type octets = int * int * int * int
type t = { octets : octets; prefix : int }

let to_list (ip : octets) : int list =
  match ip with a, b, c, d -> [ a; b; c; d ]

let of_string (s : string) : t =
  let invalid () = failwith (Printf.sprintf "invalid network address: %s" s) in
  match String.split_on_char '/' s with
  | [ addr; p ] ->
      let octets : octets =
        match List.map int_of_string (String.split_on_char '.' addr) with
        | [ a; b; c; d ] -> (a, b, c, d)
        | _ -> invalid ()
      in
      let prefix = int_of_string p in
      if List.exists (fun o -> o > 255) (to_list octets) || prefix > 32 then
        invalid ()
      else { octets; prefix }
  | _ -> invalid ()

let to_string ({ octets; prefix } : t) : string =
  Printf.sprintf "%s/%d"
    (String.concat "." (List.map string_of_int (to_list octets)))
    prefix

let to_exp ({ octets; prefix } : t) : Nkexp.t =
  let test k v =
    Nkexp.filter true
      (Field.get_or_assign_fid (Printf.sprintf "@src%d" k))
      (Value.of_int v)
  in
  let full = prefix / 8 and rest = prefix mod 8 in
  let fulls =
    List.filteri (fun k _ -> k < full) (to_list octets) |> List.mapi test
  in
  let partial =
    if rest = 0 then []
    else
      let size = 1 lsl (8 - rest) in
      let base = List.nth (to_list octets) full land lnot (size - 1) in
      [ Nkexp.union (List.init size (fun i -> test full (base + i))) ]
  in
  match fulls @ partial with [] -> Nkexp.skip | es -> Nkexp.seq es
