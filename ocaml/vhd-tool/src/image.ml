let get_device_numbers path =
  let rdev = (Unix.LargeFile.stat path).Unix.LargeFile.st_rdev in
  let major = rdev / 256 and minor = rdev mod 256 in
  (major, minor)

let is_nbd_device path =
  let nbd_device_num = 43 in
  let major, _ = get_device_numbers path in
  major = nbd_device_num

type t = [`Vhd of string | `Raw of string | `Nbd of string * string]

let to_string = function
  | `Vhd x ->
      "vhd:" ^ x
  | `Raw x ->
      "raw:" ^ x
  | `Nbd (x, y) ->
      Printf.sprintf "nbd:(%s,%s)" x y

type nbd_connect_info = {path: string; exportname: string} [@@deriving rpc]

let get_nbd_device path =
  let nbd_device_prefix = "/dev/nbd" in
  if String.starts_with ~prefix:nbd_device_prefix path && is_nbd_device path
  then
    let nbd_number =
      String.sub path
        (String.length nbd_device_prefix)
        (String.length path - String.length nbd_device_prefix)
    in
    let {path; exportname} =
      let persistent_nbd_info_dir = "/var/run/nonpersistent/nbd" in
      let filename = persistent_nbd_info_dir ^ "/" ^ nbd_number in
      Xapi_stdext_unix.Unixext.string_of_file filename
      |> Jsonrpc.of_string
      |> nbd_connect_info_of_rpc
    in
    Some (`Nbd (path, exportname))
  else
    None

let image_behind_nbd_device image =
  match image with
  | Some (`Nbd (path, _)) ->
      (* The nbd server path exposed by tapdisk can lead us to the actual image
         file below. Following the symlink gives a path like
            `/run/blktap-control/nbd<pid>.<minor>`,
         containing the tapdisk pid and minor number. Using this information,
         we can get the file path from tap-ctl.
      *)
      let default _ _ = image in
      let filename = Unix.realpath path |> Filename.basename in
      Scanf.ksscanf filename default "nbd%d.%d" (fun pid minor ->
          match Tapctl.find (Tapctl.create ()) ~pid ~minor with
          | _, _, Some ("vhd", vhd) ->
              Some (`Vhd vhd)
          | _, _, Some ("aio", vhd) ->
              Some (`Raw vhd)
          | _, _, _ | (exception _) ->
              image
      )
  | _ ->
      image

let of_device path =
  match Tapctl.of_device (Tapctl.create ()) path with
  | _, _, Some ("vhd", vhd) ->
      Some (`Vhd vhd)
  | _, _, Some ("aio", vhd) ->
      Some (`Raw vhd)
  | _, _, _ ->
      None
  | exception Tapctl.Not_blktap ->
      get_nbd_device path |> image_behind_nbd_device
  | exception Tapctl.Not_a_device ->
      None
  | exception _ ->
      None
