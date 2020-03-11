module XenAPI = Client.Client
module Date = Xapi_stdext_date.Date

let seconds_per_day = 3600. *. 24.
let seconds_per_30_days = 30. *. seconds_per_day

let days_until_expiry epoch expiry =
  int_of_float (expiry /. seconds_per_day -. epoch /. seconds_per_day)

let get_certificate_attributes rpc session =
  XenAPI.Certificate.get_all_records rpc session
  |> List.map (fun (_, certificate) ->
      let host = certificate.API.certificate_host in
      let hostname = XenAPI.Host.get_name_label rpc session host in
      (host, hostname), certificate.API.certificate_not_after
  )

let generate_alert epoch ((host, hostname), expiry) =
  let days = days_until_expiry epoch (Date.to_float expiry) in
  if days > 30 then
    host, None
  else
    let expiring_message () =
      Printf.sprintf "The TLS server certificate of host %s expires in %d days \
        (expires on %s)"
        hostname days (Date.to_string expiry)
    in
    let expired_message () =
      Printf.sprintf "The TLS server certificate of host %s expired on %s"
        hostname (Date.to_string expiry)
    in
    let message, alert =
      if days < 0 then
        expired_message (), Api_messages.host_server_certificate_expiring_07
      else if days < 8 then
        expiring_message (), Api_messages.host_server_certificate_expiring_07
      else if days < 15 then
        expiring_message (), Api_messages.host_server_certificate_expiring_14
      else
        expiring_message (), Api_messages.host_server_certificate_expiring_30
    in
    host, Some (message, alert)

let execute rpc session (host, alert) =
  let host_uuid = XenAPI.Host.get_uuid rpc session host in
  (* we need to remove any host_server_certificate_expiring messages affecting
     this host as it needs to be refreshed *)
  let obsolete_messages = XenAPI.Message.get_all_records rpc session
  |> List.filter_map (fun (ref, record) ->
      if record.API.message_name = (fst Api_messages.host_server_certificate_expiring_07)
           && record.API.message_obj_uuid = host_uuid then
        Some ref
      else
        None
      )
  in
  List.iter (fun self -> XenAPI.Message.destroy rpc session self) obsolete_messages;

  match alert with
  | None ->
      ()
  | Some (message, (alert, priority)) ->
    ignore (XenAPI.Message.create rpc session alert priority `Host host_uuid message)
