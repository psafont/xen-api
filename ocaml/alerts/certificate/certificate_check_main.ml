module XenAPI = Client.Client

let rpc xml =
  let open Xmlrpc_client in
  XMLRPC_protocol.rpc
    ~srcstr:"certificate-check"
    ~dststr:"xapi"
    ~transport:(Unix "/var/xapi/xapi")
    ~http:(xmlrpc ~version:"1.0" "/")
    xml

let _ =
  let session = XenAPI.Session.login_with_password
      ~rpc:rpc
      ~uname:""
      ~pwd:""
      ~version:"1.0"
      ~originator:"certificate-check"
  in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
       let now = Unix.time () in
       let send_alert_maybe attributes =
         attributes
         |> Certificate_check.generate_alert now
         |> Certificate_check.execute rpc session
       in
       Certificate_check.get_certificate_attributes rpc session
       |> List.iter send_alert_maybe
    )
    (fun () -> XenAPI.Session.logout rpc session)
