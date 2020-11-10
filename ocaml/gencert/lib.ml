(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *)

module Net = Network_client.Client

module D = Debug.Make (struct let name = "gencert_lib" end)

exception Unexpected_address_type of string

(* Try to get all FQDNs, use the hostname if none are available *)
let hostnames () =
  let hostname = Unix.gethostname () in
  let fqdns =
    Unix.getaddrinfo hostname "" [Unix.AI_CANONNAME]
    |> List.filter_map (fun addrinfo ->
           match addrinfo.Unix.ai_canonname with
           | "" ->
               None
           | name ->
               Some name)
  in
  match fqdns with [] -> [hostname] | fqdns -> fqdns

let get_management_ip_addr ~dbg =
  let iface = Inventory.lookup Inventory._management_interface in
  try
    if iface = "" || (not @@ Net.Interface.exists dbg iface) then
      None
    else
      let addrs =
        match
          String.lowercase_ascii
            (Inventory.lookup Inventory._management_address_type
               ~default:"ipv4")
        with
        | "ipv4" ->
            Net.Interface.get_ipv4_addr dbg iface
        | "ipv6" ->
            Net.Interface.get_ipv6_addr dbg iface
        | s ->
            raise
              (Unexpected_address_type
                 (Printf.sprintf "Expected 'ipv4' or 'ipv6', got %s" s))
      in
      let addrs =
        List.map (fun (addr, _) -> Unix.string_of_inet_addr addr) addrs
      in
      (* Filter out link-local addresses *)
      let addrs =
        List.filter (fun addr -> String.sub addr 0 4 <> "fe80") addrs
      in
      Some (List.hd addrs)
  with _ -> None

open Api_errors
open Rresult

let supported_cert_hashes = [`SHA256]

let validate_private_key pkcs8_private_key =
  let ensure_key_length = function
    | `RSA priv ->
        let length = Mirage_crypto_pk.Rsa.priv_bits priv in
        if length < 2048 || length > 4096 then
          Error
            (`Msg
              ( server_certificate_key_rsa_length_not_supported
              , [Int.to_string length] ))
        else
          Ok (`RSA priv)
  in
  let raw_pem = Cstruct.of_string pkcs8_private_key in
  X509.Private_key.decode_pem raw_pem
  |> R.reword_error (fun (`Msg err_msg) ->
         let unknown_algorithm = "Unknown algorithm " in
         if Astring.String.is_prefix ~affix:"multi-prime RSA" err_msg then
           `Msg (server_certificate_key_rsa_multi_not_supported, [])
         else if Astring.String.is_prefix ~affix:unknown_algorithm err_msg then
           `Msg
             ( server_certificate_key_algorithm_not_supported
             , [
                 Astring.String.with_range
                   ~first:(String.length unknown_algorithm)
                   err_msg
               ] )
         else (
           D.info {|Failed to validate private key because "%s"|} err_msg ;
           `Msg (server_certificate_key_invalid, [])
         ))
  >>= ensure_key_length

let has_valid_time time certificate =
  let not_before, not_after = X509.Certificate.validity certificate in
  Ptime.(is_later ~than:not_before time && is_earlier ~than:not_after time)

let load_server_certificate pem =
  let raw_pem = Cstruct.of_string pem in
  X509.Certificate.decode_pem raw_pem
  |> R.reword_error (fun (`Msg err_msg) ->
         D.info {|Failed to validate certificate because "%s"|} err_msg ;
         `Msg (server_certificate_invalid, []))

let validate_certificate server time too_early_error expired_error =
  let ensure_validity ~time certificate =
    let to_string = Ptime.to_rfc3339 ~tz_offset_s:0 in
    let not_before, not_after = X509.Certificate.validity certificate in
    if Ptime.is_earlier ~than:not_before time then
      Error (`Msg (too_early_error, [to_string time; to_string not_before]))
    else if Ptime.is_later ~than:not_after time then
      Error (`Msg (expired_error, [to_string time; to_string not_after]))
    else
      Ok certificate
  in
  let ensure_sha256_signature_algorithm certificate =
    match X509.Certificate.signature_algorithm certificate with
    | Some (_, hash) when List.mem hash supported_cert_hashes ->
        Ok certificate
    | _ ->
        Error (`Msg (server_certificate_signature_not_supported, []))
  in
  ensure_validity ~time server >>= ensure_sha256_signature_algorithm

let validate_server_certificate cert time private_key =
  let ensure_keys_match private_key certificate =
    let public_key = X509.Certificate.public_key certificate in
    match (public_key, private_key) with
    | `RSA pub, `RSA priv when pub = Mirage_crypto_pk.Rsa.pub_of_priv priv ->
        Ok ()
    | _ ->
        Error (`Msg (server_certificate_key_mismatch, []))
  in
  ensure_keys_match private_key cert >>= fun () ->
  validate_certificate cert time server_certificate_not_valid_yet
    server_certificate_expired

let validate_intermediate_certificate cert time =
  validate_certificate cert time server_certificate_intermediate_not_valid_yet
    server_certificate_intermediate_expired

let issuer_matches_subject parent cert =
  X509.(
    Distinguished_name.equal
      (Certificate.subject parent)
      (Certificate.issuer cert))

let ext_authority_matches_subject trusted cert =
  match
    X509.(
      Extension.
        ( find Authority_key_id (Certificate.extensions cert)
        , find Subject_key_id (Certificate.extensions trusted) ))
  with
  | _, None | None, _ ->
      (* not mandatory *)
      true
  | Some (_, (Some auth, _, _)), Some (_, au) ->
      Cstruct.equal auth au
  (* TODO: check exact rules in RFC5280 *)
  | Some (_, (None, _, _)), _ ->
      (* not mandatory *)
      true

let signs pathlen trusted cert =
  match
    ( issuer_matches_subject trusted cert
    , ext_authority_matches_subject trusted cert
    , X509.Validation.validate_signature supported_cert_hashes trusted cert
    , X509.Validation.validate_path_len pathlen trusted )
  with
  | true, true, Ok (), true ->
      Ok ()
  | _ ->
      Error (`Msg (server_certificate_chain_invalid, []))

let rec validate_anchors pathlen cert = function
  | [] ->
      Error (`Msg (server_certificate_chain_invalid, []))
  | x :: xs -> (
    match signs pathlen x cert with
    | Ok _ ->
        Ok x
    | Error _ ->
        validate_anchors pathlen cert xs
  )

let issuers_of trusted cert =
  List.filter (fun p -> issuer_matches_subject p cert) trusted

let verify_single_chain time anchors chain =
  let open X509.Validation in
  let rec climb pathlen = function
    | cert :: issuer :: certs ->
        validate_intermediate_certificate issuer time >>= fun _ ->
        signs pathlen issuer cert >>= fun () ->
        climb (succ pathlen) (issuer :: certs)
    | [c] ->
        let anchors = issuers_of anchors c in
        validate_anchors pathlen c anchors
    | [] ->
        Error (`Msg (server_certificate_chain_invalid, []))
  in
  climb 0 chain

let valid_chains time anchors chains =
  let rec loop acc chains =
    match (acc, chains) with
    | [], [] ->
        Error (`Msg (server_certificate_chain_invalid, []))
    | _, [] ->
        Ok acc
    | _, chain :: chains -> (
      match verify_single_chain time anchors chain with
      | Ok root ->
          loop (root :: acc) chains
      | Error _ ->
          loop acc chains
    )
  in
  loop [] chains

let verify_chain_of_trust ~server ~private_key ~intermediates ~time ~anchors =
  validate_server_certificate server time private_key >>= fun server ->
  (* build all paths *)
  let paths = X509.Validation.build_paths server intermediates in
  let anchors = List.filter (has_valid_time time) anchors in
  (* return valid chains *)
  valid_chains time anchors paths

let load_ca_cert_bundle ca_cert_bundle_path =
  Xapi_stdext_unix.Unixext.string_of_file ca_cert_bundle_path
  |> Cstruct.of_string
  |> X509.Certificate.decode_pem_multiple
  |> function
  | Ok (_ :: _ as trust_anchors) ->
      Ok trust_anchors
  | _ ->
      Error (`Msg (server_certificate_chain_invalid, []))

let validate_certificate_chain pem_chain =
  Cstruct.of_string pem_chain |> X509.Certificate.decode_pem_multiple
  |> function
  | Ok (_ :: _ as intermediates) ->
      Ok ([pem_chain], intermediates)
  | Ok [] ->
      D.info "Rejected certificate chain because it's empty." ;
      Error (`Msg (server_certificate_chain_invalid, []))
  | Error (`Msg err_msg) ->
      D.info {|Failed to validate certificate chain because "%s"|} err_msg ;
      Error (`Msg (server_certificate_chain_invalid, []))

let install_server_certificate ?(pem_chain = None) ~pem_leaf ~pkcs8_private_key
    ~server_cert_path ~ca_cert_bundle_path =
  let now = Ptime_clock.now () in
  let ( let* ) = ( >>= ) in
  let* private_key = validate_private_key pkcs8_private_key in
  let* anchors = load_ca_cert_bundle ca_cert_bundle_path in
  let* pem_chain, intermediates =
    (function
      | None ->
          Ok ([], [])
      | Some pem_chain ->
          validate_certificate_chain pem_chain)
      pem_chain
  in
  let* server = load_server_certificate pem_leaf in
  let* trust_roots =
    verify_chain_of_trust ~server ~private_key ~intermediates ~time:now ~anchors
  in

  let root_fingerprints =
    List.map (X509.Certificate.fingerprint Mirage_crypto.Hash.(`SHA256)) trust_roots
  in

  let server_cert_components = pkcs8_private_key :: pem_leaf :: pem_chain in
  let* () =
    server_cert_components
    |> String.concat "\n\n"
    |> Selfcert.write_certs server_cert_path
    |> R.reword_error (function `Msg msg -> `Msg (internal_error, [msg]))
  in
  Ok (server, root_fingerprints)
