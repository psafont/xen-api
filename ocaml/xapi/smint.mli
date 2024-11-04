type vdi_info = {vdi_info_uuid: string option; vdi_info_location: string}

module Feature : sig
  type capability =
    | Sr_create
    | Sr_delete
    | Sr_attach
    | Sr_detach
    | Sr_scan
    | Sr_probe
    | Sr_update
    | Sr_supports_local_caching
    | Sr_stats
    | Sr_metadata
    | Sr_trim
    | Sr_multipath
    | Sr_caching
    | Vdi_create
    | Vdi_delete
    | Vdi_attach
    | Vdi_detach
    | Vdi_mirror
    | Vdi_mirror_in
    | Vdi_clone
    | Vdi_snapshot
    | Vdi_resize
    | Vdi_activate
    | Vdi_activate_readonly
    | Vdi_deactivate
    | Vdi_update
    | Vdi_introduce
    | Vdi_resize_online
    | Vdi_generate_config
    | Vdi_attach_offline
    | Vdi_reset_on_boot
    | Vdi_configure_cbt
    | Vdi_compose
    | Large_vdi
    | Thin_provisioning
    | Vdi_read_caching
    | Import
    | Probe_ext

  type t = capability * int64

  val string_to_capability : string -> capability option

  val capability_to_string : capability -> string

  val to_string : capability * int64 -> string

  val capability_of : t -> capability

  val unparse : string * int64 -> string

  val compat_features :
    ('a * Int64.t) list -> ('a * Int64.t) list -> ('a * Int64.t) list

  val of_string_int64_opt : string * 'a -> (capability * 'a) option

  val has_capability : capability -> t list -> bool

  val parse_string_int64 : string list -> (string * Int64.t) list

  val parse_capability_int64 : string list -> (capability * Int64.t) list
end

type sr_driver_info = {
    sr_driver_filename: string
  ; sr_driver_name: string
  ; sr_driver_description: string
  ; sr_driver_vendor: string
  ; sr_driver_copyright: string
  ; sr_driver_version: string
  ; sr_driver_required_api_version: string
  ; sr_driver_features: Feature.t list
  ; sr_driver_text_features: string list
  ; sr_driver_configuration: (string * string) list
  ; sr_driver_required_cluster_stack: string list
  ; sr_smapi_version: Storage_interface.smapi_version
}

val query_result_of_sr_driver_info :
  sr_driver_info -> Storage_interface.query_result

type attach_info = {
    params: string option
  ; params_nbd: string
  ; o_direct: bool
  ; o_direct_reason: string
  ; xenstore_data: (string * string) list
}

exception Backend_missing_field of string

exception Not_implemented_in_backend
