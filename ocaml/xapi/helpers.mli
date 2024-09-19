module Delay = Xapi_stdext_threads.Threadext.Delay

val get_process_output : ?handler:(string -> int -> string) -> string -> string

val log_exn_continue : string -> ('a -> unit) -> 'a -> unit

type log_output = Always | Never | On_failure

val filter_patterns : (Re.re * string) list

val filter_args : string list -> string list

val call_script :
     ?log_output:log_output
  -> ?env:string array
  -> ?stdin:string
  -> ?timeout:float
  -> string
  -> string list
  -> string

val choose_network_name_for_pif : string -> string

val checknull : (unit -> string) -> string

val ignore_invalid_ref : ('a Ref.t -> 'b Ref.t) -> 'a Ref.t -> 'b Ref.t option

val get_pool : __context:Context.t -> [`pool] Ref.t

val get_master : __context:Context.t -> [`host] Ref.t

val get_bridge_is_connected : __context:Context.t -> string -> bool

val get_management_iface_is_connected : __context:Context.t -> bool

val get_management_ip_addr : __context:Context.t -> string option

val get_localhost_uuid : unit -> string

val get_localhost_uncached : __context:Context.t -> [`host] Ref.t

val get_localhost : __context:Context.t -> [`host] Ref.t

val determine_gateway_and_dns_ifs :
     __context:Context.t
  -> ?management_interface:API.ref_PIF
  -> unit
  -> ([`PIF] Ref.t * string) option * ([`PIF] Ref.t * string) option

val update_pif_address : __context:Context.t -> self:[`PIF] Ref.t -> unit

val update_getty : unit -> unit

val set_gateway :
  __context:Context.t -> pif:[`PIF] Ref.t -> bridge:string -> unit

val set_DNS : __context:Context.t -> pif:[`PIF] Ref.t -> bridge:string -> unit

val update_pif_addresses : __context:Context.t -> unit

val make_rpc : __context:Context.t -> Rpc.call -> Rpc.response

val make_timeboxed_rpc :
  __context:Context.t -> float -> Rpc.call -> Rpc.response

val secret_string_of_request : Http.Request.t -> SecretString.t option

val with_cookie : SecretString.t -> Http.Request.t -> Http.Request.t

val make_remote_rpc_of_url :
     verify_cert:Stunnel.verification_config option
  -> srcstr:string
  -> dststr:string
  -> Http.Url.t * SecretString.t option
  -> Rpc.call
  -> Rpc.response

val make_remote_rpc :
     ?verify_cert:Stunnel.verification_config option
  -> __context:Context.t
  -> string
  -> Rpc.call
  -> Rpc.response

type 'a api_object =
  | LocalObject of 'a Ref.t
  | RemoteObject of ((Rpc.call -> Rpc.response) * API.ref_session * 'a Ref.t)

val call_api_functions_internal :
     __context:Context.t
  -> ((Rpc.call -> Rpc.response) -> API.ref_session -> 'a)
  -> 'a

val call_api_functions :
     __context:Context.t
  -> ((Rpc.call -> Rpc.response) -> API.ref_session -> 'a)
  -> 'a

val call_emergency_mode_functions :
  string -> ((Rpc.call -> Rpc.response) -> [`session] Ref.t -> 'a) -> 'a

val progress : __context:Context.t -> float -> unit

val is_domain_zero_with_record :
  __context:Context.t -> [`VM] Ref.t -> API.vM_t -> bool

val is_domain_zero : __context:Context.t -> [`VM] Ref.t -> bool

exception No_domain_zero of string

val domain_zero_ref_cache : [`VM] Ref.t option ref

val get_domain_zero : __context:Context.t -> API.ref_VM

val update_domain_zero_name :
  __context:Context.t -> [`host] Ref.t -> string -> unit

type hvmloader_boot_t = {timeoffset: string}

type direct_boot_t = {
    kernel: string
  ; kernel_args: string
  ; ramdisk: string option
}

type indirect_boot_t = {
    bootloader: string
  ; extra_args: string
  ; legacy_args: string
  ; pv_bootloader_args: string
  ; vdis: API.ref_VDI list
}

type boot_method =
  | Hvmloader of hvmloader_boot_t
  | Direct of direct_boot_t
  | Indirect of indirect_boot_t

val rolling_upgrade_in_progress_of_oc : (string * 'a) list -> bool

val rolling_upgrade_in_progress : __context:Context.t -> bool

val check_domain_type : API.domain_type -> [`hvm | `pv | `pv_in_pvh | `pvh]

val domain_type :
  __context:Context.t -> self:[`VM] Ref.t -> [`hvm | `pv | `pv_in_pvh | `pvh]

val boot_method_of_vm : __context:Context.t -> vm:API.vM_t -> boot_method

val needs_qemu_from_domain_type :
  [< `hvm | `pv | `pv_in_pvh | `pvh | `unspecified] -> bool

val will_have_qemu_from_record : API.vM_t -> bool

val will_have_qemu : __context:Context.t -> self:[`VM] Ref.t -> bool

val has_qemu_currently : __context:Context.t -> self:[`VM] Ref.t -> bool

val has_qemu : __context:Context.t -> self:[`VM] Ref.t -> bool

val is_running : __context:Context.t -> self:[`VM] Ref.t -> bool

val get_guest_installer_network : __context:Context.t -> [`network] Ref.t

val get_host_internal_management_network :
  __context:Context.t -> [`network] Ref.t

val get_my_pbds : Context.t -> ('a Ref.t * API.pBD_t) list

val is_sr_shared : __context:Context.t -> self:[`SR] Ref.t -> bool

val get_main_ip_address : __context:'a -> string

val is_pool_master : __context:Context.t -> host:[`host] Ref.t -> bool

val assert_we_are_master : __context:Context.t -> unit

val compare_int_lists : int list -> int list -> int

val group_by :
     ordering:[< `ascending | `descending]
  -> ('a -> 'b)
  -> 'a list
  -> ('a * 'b) list list

val sort_by_schwarzian : ?descending:bool -> ('a -> 'b) -> 'a list -> 'a list

val platform_version_inverness : int list

val version_string_of : __context:Context.t -> [`host] api_object -> string

val version_of : __context:Context.t -> [`host] api_object -> int list

val compare_host_platform_versions :
  __context:Context.t -> [`host] api_object -> [`host] api_object -> int

val max_version_in_pool : __context:Context.t -> int list

val host_has_highest_version_in_pool :
  __context:Context.t -> host:[`host] api_object -> bool

val host_versions_not_decreasing :
     __context:Context.t
  -> host_from:[`host] api_object
  -> host_to:[`host] api_object
  -> bool

val is_platform_version_same_on_master :
  __context:Context.t -> host:[`host] Ref.t -> bool

val maybe_raise_vtpm_unimplemented : string -> string -> unit

val assert_platform_version_is_same_on_master :
  __context:Context.t -> host:[`host] Ref.t -> self:'a Ref.t -> unit

val assert_rolling_upgrade_not_in_progress : __context:Context.t -> unit

val assert_host_has_highest_version_in_pool :
  __context:Context.t -> host:API.ref_host -> unit

val pool_has_different_host_platform_versions : __context:Context.t -> bool

val host_has_pbd_for_sr :
  __context:Context.t -> host:[`host] Ref.t -> sr:[`SR] Ref.t -> bool

val check_sr_exists :
  __context:Context.t -> self:[`SR] Ref.t -> [`SR] Ref.t option

val check_sr_exists_for_host :
     __context:Context.t
  -> self:[`SR] Ref.t
  -> host:[`host] Ref.t
  -> [`SR] Ref.t option

val choose_suspend_sr : __context:Context.t -> vm:[`VM] Ref.t -> [`SR] Ref.t

val cancel_tasks :
     __context:'a
  -> ops:(string * 'b) list
  -> all_tasks_in_db:'c Ref.t list
  -> task_ids:string list
  -> set:((string * 'b) list -> unit)
  -> unit

val is_removable : __context:Context.t -> vbd:[`VBD] Ref.t -> bool

val assert_is_valid_tcp_udp_port : port:int -> name:string -> unit

val assert_is_valid_tcp_udp_port_range :
     first_port:int
  -> first_name:string
  -> last_port:int
  -> last_name:string
  -> unit

val assert_is_valid_ip :
  [> `ipv4 | `ipv4or6 | `ipv6] -> string -> string -> unit

val parse_cidr : [< `ipv4 | `ipv6] -> string -> (string * int) option

val valid_cidr_aux : [< `ipv4 | `ipv4or6 | `ipv6] -> string -> bool

val assert_is_valid_cidr :
  [< `ipv4 | `ipv4or6 | `ipv6] -> string -> string -> unit

val assert_is_valid_ip_addr :
  [`ipv4 | `ipv4or6 | `ipv6] -> string -> string -> unit

val is_valid_MAC : string -> bool

val this_is_my_address : __context:Context.t -> string -> bool

val get_live_hosts : __context:Context.t -> [`host] Ref.t list

val gethostbyname : string -> string

val clone_suspended_vm_enabled : __context:Context.t -> bool

val guest_agent_run_script_enabled : __context:Context.t -> bool

val on_oem : __context:Context.t -> bool

exception File_doesnt_exist of string

val bisect : (int64 -> bool) -> int64 -> int64 -> int64

val vm_should_always_run : bool -> string -> bool

val is_xha_protected : __context:Context.t -> self:[`VM] Ref.t -> bool

val is_xha_protected_r : API.vM_t -> bool

val local_storage_exists : unit -> bool

val touch_file : string -> unit

val vm_to_string : Context.t -> 'a Ref.t -> string

val vm_string_to_assoc : string -> (string * string) list

val i_am_srmaster : __context:Context.t -> sr:[`SR] Ref.t -> bool

val get_all_plugged_srs : __context:Context.t -> [`SR] Ref.t list

val get_local_plugged_srs : __context:Context.t -> [`SR] Ref.t list

val find_health_check_task : __context:Context.t -> sr:'a Ref.t -> 'b Ref.t list

val update_vswitch_controller :
  __context:Context.t -> host:[`host] Ref.t -> unit

val assert_vswitch_controller_not_active : __context:Context.t -> unit

val assert_using_vswitch : __context:Context.t -> unit

exception No_pvs_server_available

val assert_pvs_servers_available :
  __context:Context.t -> pvs_site:[`PVS_site] Ref.t -> unit

val assert_is_valid_ref :
  __context:Context.t -> name:string -> ref:'a Ref.t -> unit

val force_loopback_vbd : __context:Context.t -> bool

val compute_hash : unit -> string

val resolve_uri_path : root:string -> uri_path:string -> string

val run_in_parallel : funs:(unit -> 'a) list -> capacity:int -> 'a list

val with_global_lock : (unit -> 'a) -> 'a

module type POLICY = sig
  type t

  val standard : t

  val fail_quickly : t

  val fail_immediately : t

  val wait : __context:Context.t -> t -> exn -> t
end

module Early_wakeup : sig
  val table : (string * string, Delay.t) Hashtbl.t

  val table_m : Mutex.t

  val wait : string * string -> float -> unit

  val broadcast : 'a -> unit

  val signal : string * string -> unit
end

module Policy : POLICY

val retry_with_global_lock :
  __context:Context.t -> doc:string -> ?policy:Policy.t -> (unit -> 'a) -> 'a

val retry_until_timeout :
  ?interval:float -> ?timeout:float -> string -> (unit -> bool) -> unit

val get_first_pusb : __context:Context.t -> [`USB_group] Ref.t -> [`PUSB] Ref.t

val get_first_vusb : __context:Context.t -> [`USB_group] Ref.t -> [`VUSB] Ref.t

val host_supports_hvm : __context:Context.t -> [`host] Ref.t -> bool

val env_with_path : (string * string) list -> string array

module Task : sig
  val wait_for : __context:Context.t -> tasks:API.ref_task list -> unit
end

val try_internal_async :
     __context:Context.t
  -> (Rpc.t -> 'b)
  -> (unit -> API.ref_task)
  -> (unit -> 'b)
  -> 'b

module PoolSecret : sig
  val make : unit -> SecretString.t

  val is_authorized : SecretString.t -> bool

  val refresh_cache_or_create_new : unit -> unit
end

val with_temp_out_ch_of_temp_file :
     ?mode:open_flag list
  -> string
  -> string
  -> (string * ((out_channel -> 'a) -> 'a) -> 'b)
  -> 'b

module FileSys : sig
  type path = string

  val realpathm : path -> path

  val mv : src:path -> dest:path -> unit

  val cpr : src:path -> dest:path -> unit

  val redirect : string -> fname:path -> unit
end

val update_ca_bundle : unit -> unit

val external_certificate_thumbprint_of_master :
  hash_type:[`Sha1 | `Sha256] -> string option

val get_active_uefi_certificates :
  __context:Context.t -> self:[`pool] Ref.t -> string

val uefi_mode_to_string : [< `setup | `user] -> string

module BoundedPsq : sig
  module type Ordered = sig
    type t

    val compare : t -> t -> int
  end

  module type S = sig
    type t

    type k

    type v

    val create : capacity:int -> t

    val add : t -> k -> v -> unit

    val remove : t -> k -> unit

    val clear : t -> unit

    val min : t -> (k * v) option

    val find_opt : k -> t -> v option

    val contains : t -> k -> bool

    val iter : (k -> v -> unit) -> t -> unit

    val size : t -> int
  end

  module Make : functor (K : Ordered) (V : Ordered) -> sig
    type t

    type k = K.t

    type v = V.t

    val create : capacity:int -> t

    val add : t -> k -> v -> unit

    val remove : t -> k -> unit

    val clear : t -> unit

    val min : t -> (k * v) option

    val find_opt : k -> t -> v option

    val contains : t -> k -> bool

    val iter : (k -> v -> unit) -> t -> unit

    val size : t -> int
  end
end

module AuthenticationCache : sig
  module Expires : functor
    (Data : sig
       type t
     end)
    -> sig
    type t = Data.t with_expiration

    and 'a with_expiration = {data: 'a; expires: Mtime.span}

    val compare : 'a with_expiration -> 'b with_expiration -> int
  end

  module type Ordered = sig
    type t

    val compare : t -> t -> int
  end

  module type Secret = sig
    type key

    type digest

    type salt

    type secret

    type t

    val create : digest -> salt -> secret -> t

    val read : t -> digest * salt * secret

    val hash : key -> salt -> digest

    val create_salt : unit -> salt

    val equal_digest : digest -> digest -> bool
  end

  module type S = sig
    type t

    type user

    type password

    type session

    val create : size:int -> t

    val cache : t -> user -> password -> session -> unit

    val cached : t -> user -> password -> session option
  end

  module Make : functor (User : Ordered) (Secret : Secret) -> sig
    type t

    type user = User.t

    type password = Secret.key

    type session = Secret.secret

    val create : size:int -> t

    val cache : t -> user -> password -> session -> unit

    val cached : t -> user -> password -> session option
  end
end
