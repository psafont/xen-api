type bus_type = Xen | Scsi | Floppy | Ide [@@deriving rpcty]

type t = bus_type * int * int [@@deriving rpcty]

let bus_type_to_string = function
  | Xen ->
      "Xen"
  | Scsi ->
      "Scsi"
  | Floppy ->
      "Floppy"
  | Ide ->
      "Ide"

let to_debug_string (bus, disk, partition) =
  Printf.sprintf "%s(%d, %d)" (bus_type_to_string bus) disk partition

(* ocamlp4-friendly operators *)
let ( <| ) = ( lsl )

let ( >| ) = ( lsr )

let int_of_string x =
  try int_of_string x
  with _ -> failwith (Printf.sprintf "int_of_string [%s]" x)

(* If this is true then we will use the deprecated (linux-specific) IDE
   encodings for disks > 3 *)
let use_deprecated_ide_encoding = true

let make bus ~disk ~partition =
  let max_xen = ((1 <| 20) - 1, 15) in
  let max_scsi = (15, 15) in
  let max_ide = if use_deprecated_ide_encoding then (19, 63) else (3, 63) in
  let max_floppy = (2, 0) in
  let assert_in_range description (disk_limit, partition_limit) (disk, partition)
      =
    if disk < 0 || disk > disk_limit then
      failwith
        (Printf.sprintf "%s disk number out of range 0 <= %d <= %d" description
           disk disk_limit
        ) ;
    if partition < 0 || partition > partition_limit then
      failwith
        (Printf.sprintf "%s partition number out of range 0 <= %d <= %d"
           description partition partition_limit
        )
  in
  ( match bus with
  | Xen ->
      assert_in_range "xen" max_xen (disk, partition)
  | Scsi ->
      assert_in_range "scsi" max_scsi (disk, partition)
  | Floppy ->
      assert_in_range "floppy" max_floppy (disk, partition)
  | Ide ->
      assert_in_range "ide" max_ide (disk, partition)
  ) ;
  (bus, disk, partition)

let ( || ) = ( lor )

let standard_ide_table = [3; 22]

let deprecated_ide_table = standard_ide_table @ [33; 34; 56; 57; 88; 89; 90; 91]

let to_xenstore_int = function
  | Xen, disk, partition when disk < 16 ->
      202 <| 8 || disk <| 4 || partition
  | Xen, disk, partition ->
      1 <| 28 || disk <| 8 || partition
  | Scsi, disk, partition ->
      8 <| 8 || disk <| 4 || partition
  | Floppy, disk, partition ->
      203 <| 8 || disk <| 4 || partition
  | Ide, disk, partition ->
      let m = List.nth deprecated_ide_table (disk / 2) in
      let n = disk - (disk / 2 * 2) in
      (* NB integers behave differently to reals *)
      m <| 8 || n <| 6 || partition

let of_xenstore_int x =
  let ( && ) = ( land ) in
  if (x && 1 <| 28) <> 0 then
    (Xen, x >| 8 && ((1 <| 20) - 1), x && ((1 <| 8) - 1))
  else
    match x >| 8 with
    | 202 ->
        (Xen, x >| 4 && ((1 <| 4) - 1), x && ((1 <| 4) - 1))
    | 8 ->
        (Scsi, x >| 4 && ((1 <| 4) - 1), x && ((1 <| 4) - 1))
    | 203 ->
        (Floppy, x >| 4 && ((1 <| 4) - 1), x && ((1 <| 4) - 1))
    | n ->
        let idx =
          snd
            (List.fold_left
               (fun (i, res) e -> (i + 1, if e = n then i else res))
               (0, -1) deprecated_ide_table
            )
        in
        if idx < 0 then failwith (Printf.sprintf "Unknown device number: %d" x) ;
        (Ide, (x >| 6 && ((1 <| 2) - 1)) + (idx * 2), x && ((1 <| 6) - 1))

let to_xenstore_key x = to_xenstore_int x

let of_xenstore_key x = of_xenstore_int x

(* NB the device encoding is base 26 starting from 1 rather than 0 eg 0 -> a 25
   -> z 26 -> aa *)

(** Return an integer encoded as a linux device suffix *)
let rec string_of_int26 x =
  let high, low = ((x / 26) - 1, (x mod 26) + 1) in
  let high' = if high = -1 then "" else string_of_int26 high in
  let low' = String.make 1 (char_of_int (low + int_of_char 'a' - 1)) in
  high' ^ low'

let to_linux_device (bus, disk, part) =
  let p x = if x = 0 then "" else string_of_int x in
  let bus =
    match bus with Xen -> "xvd" | Scsi -> "sd" | Floppy -> "fd" | Ide -> "xvd"
  in
  Printf.sprintf "%s%s%s" bus (string_of_int26 disk) (p part)

let of_linux_device x =
  let fail () = failwith (Printf.sprintf "Failed to parse device name: %s" x) in
  let open Astring in
  let b26_to_int x =
    (* Convert a linux device string back into an integer *)
    (* Assumes all characters are in range *)
    let b26 =
      String.Sub.to_string x
      |> Stdlib.String.to_seq
      |> Seq.map (fun c -> int_of_char c - int_of_char 'a' + 1)
      |> Seq.fold_left (fun acc x -> (acc * 26) + x) 0
    in
    b26 - 1
  in

  let parse_int x =
    match String.Sub.span ~min:1 ~sat:Char.Ascii.is_digit x with
    | i, s -> (
      match String.Sub.to_int i with None -> fail () | Some i -> (i, s)
    )
  in
  let parse_b26 x =
    match String.Sub.span ~min:1 ~sat:Char.Ascii.is_lower x with
    | b, s ->
        (b26_to_int b, s)
  in
  (* Parse a string "abc123" into x, y where x is "abc" interpreted as base-26
     and y is 123 *)
  let parse_b26_int x =
    let pre, x = parse_b26 x in
    if String.Sub.is_empty x then
      (pre, 0)
    else
      let post, x = parse_int x in
      if not (String.Sub.is_empty x) then fail () ;
      (pre, post)
  in
  (* Parse a string "123p456" into x, y where x = 123 and y = 456 *)
  let parse_int_p_int x =
    let parse_p x =
      match String.Sub.head x with
      | Some 'p' ->
          String.Sub.tail x
      | Some _ | None ->
          fail ()
    in

    let pre, x = parse_int x in

    if String.Sub.is_empty x then
      (pre, 0)
    else
      let post, x = parse_int (parse_p x) in
      if not (String.Sub.is_empty x) then fail () ;
      (* previous parser also allowed garbade at the end *)
      (pre, post)
  in
  if String.is_prefix ~affix:"xvd" x then
    let rest = String.sub_with_range ~first:3 x in
    let disk, partition = parse_b26_int rest in
    (Xen, disk, partition)
  else if String.is_prefix ~affix:"sd" x then
    let rest = String.sub_with_range ~first:2 x in
    let disk, partition = parse_b26_int rest in
    (Scsi, disk, partition)
  else if String.is_prefix ~affix:"fd" x then
    let rest = String.sub_with_range ~first:2 x in
    let disk, partition = parse_b26_int rest in
    (Floppy, disk, partition)
  else if String.is_prefix ~affix:"hd" x then
    let rest = String.sub_with_range ~first:2 x in
    let disk, partition = parse_b26_int rest in
    (Ide, disk, partition)
  else if String.is_prefix ~affix:"d" x then
    let rest = String.sub_with_range ~first:1 x in
    let disk, partition = parse_int_p_int rest in
    (Xen, disk, partition)
  else
    fail ()

let upgrade_linux_device x =
  if Astring.String.is_prefix ~affix:"hd" x then
    let rest = Astring.String.with_range ~first:2 x in
    "xvd" ^ rest
  else
    x

let disk (_, disk, _) = disk

let of_disk_number hvm n = if hvm && n < 4 then (Ide, n, 0) else (Xen, n, 0)

let of_string ~hvm name =
  try of_disk_number hvm (int_of_string name) with _ -> of_linux_device name
