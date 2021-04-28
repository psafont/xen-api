type existing_cert_strategy = Erase_old | Merge

(** [existing_cert_strategy] is used to determine how to treat existing
 *  certs in /etc/stunnel/certs-pool
 *  Erase_old => existing certs in the trusted certs dir will be removed
 *  Merge     => merge incoming certs with certs in the trusted certs dir,
 *               resolving conflicts by taking the incoming cert *)

val local_exec : __context:Context.t -> command:string -> string
(** execute a string encoded job, returning a string encoded result *)

val go :
     __context:Context.t
  -> from_hosts:API.ref_host list
  -> to_hosts:API.ref_host list
  -> existing_cert_strategy:existing_cert_strategy
  -> unit
(** Certificates are collected from [from_hosts] and installed on [to_hosts].
 *  On success, new bundles will have been generated on all [to_hosts] *)

type host_cert

val make_joiner_cert :
  __context:Context.t -> uuid:string -> certificate:string -> host_cert

val exchange_certificates_with_joiner :
  __context:Context.t -> joiner_certificate:host_cert -> host_cert list

val host_cert_to_string_to_string : host_cert -> string * string

val import_joining_pool_certs :
  __context:Context.t -> pool_certs:(string * string) list -> unit
