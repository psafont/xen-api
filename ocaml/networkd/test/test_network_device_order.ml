open Network_device_order
open Network_interface

let pci_addr0 = Pciaddr.of_string "0000:01:0f.0" |> Result.get_ok

let pci_addr1 = Pciaddr.of_string "0000:01:0f.1" |> Result.get_ok

let pci_addr2 = Pciaddr.of_string "0000:01:0f.2" |> Result.get_ok

let pci_addr3 = Pciaddr.of_string "0000:01:0f.3" |> Result.get_ok

let pci_addr4 = Pciaddr.of_string "0000:05:0f.0" |> Result.get_ok

let pci_addr5 = pci_addr4

let pci_addr6 = Pciaddr.of_string "0000:06:0f.0" |> Result.get_ok

let pci_addr7 = Pciaddr.of_string "0000:06:0f.1" |> Result.get_ok

let mac_addr0 = Macaddr.of_string "ec:f4:bb:e6:d7:b8" |> Result.get_ok

let mac_addr1 = Macaddr.of_string "ec:f4:bb:e6:d7:b9" |> Result.get_ok

let mac_addr2 = Macaddr.of_string "ec:f4:bb:e6:d7:ba" |> Result.get_ok

let mac_addr3 = Macaddr.of_string "ec:f4:bb:e6:d7:bb" |> Result.get_ok

let mac_addr4 = Macaddr.of_string "00:02:c9:ed:fd:f0" |> Result.get_ok

let mac_addr5 = Macaddr.of_string "00:02:c9:ed:fd:f1" |> Result.get_ok

let mac_addr6 = Macaddr.of_string "fc:f4:bb:e6:d7:b8" |> Result.get_ok

let mac_addr7 = Macaddr.of_string "fc:f4:bb:e6:d7:b9" |> Result.get_ok

let name0 = "eno1"

let name1 = "eno2"

let name2 = "eno3"

let name3 = "eno4"

let name4 = "enp5s0"

let name5 = "enp5s0d1"

let last_order =
  [
    {name= name0; pci= pci_addr0; mac= mac_addr0; position= 0; present= true}
  ; {name= name1; pci= pci_addr1; mac= mac_addr1; position= 1; present= true}
  ; {name= name2; pci= pci_addr2; mac= mac_addr2; position= 2; present= true}
  ; {name= name3; pci= pci_addr3; mac= mac_addr3; position= 3; present= true}
  ; {name= name4; pci= pci_addr4; mac= mac_addr4; position= 5; present= true}
  ; {name= name5; pci= pci_addr5; mac= mac_addr5; position= 4; present= true}
  ]

let dev0 =
  {
    Dev.name= name0
  ; pci= pci_addr0
  ; mac= mac_addr0
  ; bios_eth_order= 0
  ; multi_nic= false
  }

let dev1 =
  {
    Dev.name= name1
  ; pci= pci_addr1
  ; mac= mac_addr1
  ; bios_eth_order= 1
  ; multi_nic= false
  }

let dev2 =
  {
    Dev.name= name2
  ; pci= pci_addr2
  ; mac= mac_addr2
  ; bios_eth_order= 2
  ; multi_nic= false
  }

let dev3 =
  {
    Dev.name= name3
  ; pci= pci_addr3
  ; mac= mac_addr3
  ; bios_eth_order= 3
  ; multi_nic= false
  }

let dev4 =
  {
    Dev.name= name4
  ; pci= pci_addr4
  ; mac= mac_addr4
  ; bios_eth_order= 4 (* XXX Inconsistent with last_order !!*)
  ; multi_nic= false
  }

let dev5 =
  {
    Dev.name= name5
  ; pci= pci_addr5
  ; mac= mac_addr5
  ; bios_eth_order= 5 (* XXX Inconsistent with last_order !!*)
  ; multi_nic= false
  }

let currents = [dev0; dev1; dev2; dev3; dev4; dev5]

let dev6 =
  {
    Dev.name= "eno6"
  ; pci= pci_addr6
  ; mac= mac_addr6
  ; bios_eth_order= 1 (* XXX incosistent with currents *)
  ; multi_nic= false
  }

let dev7 =
  {
    Dev.name= "eno7"
  ; pci= pci_addr7
  ; mac= mac_addr7
  ; bios_eth_order= 0 (* XXX incosistent with currents *)
  ; multi_nic= false
  }

let plug dev devices = List.cons dev devices

let unplug Dev.{mac; _} devices =
  List.filter (fun dev -> dev.Dev.mac <> mac) devices

let pos_of_mac mac order =
  match List.find_opt (fun dev -> dev.mac = mac) order with
  | Some {position; _} ->
      Some position
  | _ ->
      None

let present_of_mac mac order =
  match List.find_opt (fun dev -> dev.mac = mac) order with
  | Some {present; _} ->
      present
  | _ ->
      failwith "Can't find the device!"

let expected_of_dev_mac current mac_addr =
  List.find_map
    (fun Dev.{mac; bios_eth_order; _} ->
      if Macaddr.compare mac mac_addr = 0 then
        Some bios_eth_order
      else
        None
    )
    current

let expected_of_seen_mac (current : Seen.t list) mac_addr =
  List.find_map
    (function
      | Seen.{index= Mac_addr mac; position; _}
        when Macaddr.compare mac mac_addr = 0 ->
          Some position
      | _ ->
          None
      )
    current

let expected_of_seen_label (current : Seen.t list) name =
  List.find_map
    (function
      | Seen.{index= Label label; position; _} when String.equal name label ->
          Some position
      | _ ->
          None
      )
    current

let test_position_and_presence ?(plugged = true) expected new_order mac_addr =
  let name = Format.asprintf "Position assigned for %a" Macaddr.pp mac_addr in
  Alcotest.(check @@ option int) name expected (pos_of_mac mac_addr new_order) ;
  Alcotest.(check bool) "Present" plugged (present_of_mac mac_addr new_order)

let test_default () =
  let order = sort' ~currents ~rules:[] ~last_order:[] |> Result.get_ok in
  let test_position_and_presence mac =
    test_position_and_presence (expected_of_dev_mac currents mac) order mac
  in

  Alcotest.(check int) "6 devices in the order" 6 (List.length order) ;
  test_position_and_presence mac_addr0 ;
  test_position_and_presence mac_addr1 ;
  test_position_and_presence mac_addr2 ;
  test_position_and_presence mac_addr3 ;
  test_position_and_presence mac_addr4 ;
  test_position_and_presence mac_addr5

let test_initial_rules_via_mac () =
  let rules =
    Seen.
      [
        {position= 0; index= Mac_addr mac_addr5}
      ; {position= 1; index= Mac_addr mac_addr4}
      ; {position= 2; index= Mac_addr mac_addr3}
      ; {position= 3; index= Mac_addr mac_addr2}
      ; {position= 5; index= Mac_addr mac_addr1}
      ; {position= 4; index= Mac_addr mac_addr0}
      ]
  in
  let order = sort' ~currents ~rules ~last_order:[] |> Result.get_ok in

  let test_position_and_presence mac =
    test_position_and_presence (expected_of_seen_mac rules mac) order mac
  in

  Alcotest.(check int) "6 devices in the order" 6 (List.length order) ;
  test_position_and_presence mac_addr0 ;
  test_position_and_presence mac_addr1 ;
  test_position_and_presence mac_addr2 ;
  test_position_and_presence mac_addr3 ;
  test_position_and_presence mac_addr4 ;
  test_position_and_presence mac_addr5

let test_initial_rules_via_label () =
  let rules =
    Seen.
      [
        {position= 0; index= Label name5}
      ; {position= 1; index= Label name4}
      ; {position= 2; index= Label name3}
      ; {position= 3; index= Label name2}
      ; {position= 5; index= Label name1}
      ; {position= 4; index= Label name0}
      ]
  in

  let order = sort' ~currents ~rules ~last_order:[] |> Result.get_ok in

  let test_position_and_presence label mac =
    test_position_and_presence (expected_of_seen_label rules label) order mac
  in

  Alcotest.(check int) "6 devices in the order" 6 (List.length order) ;
  test_position_and_presence name0 mac_addr0 ;
  test_position_and_presence name1 mac_addr1 ;
  test_position_and_presence name2 mac_addr2 ;
  test_position_and_presence name3 mac_addr3 ;
  test_position_and_presence name4 mac_addr4 ;
  test_position_and_presence name5 mac_addr5

let test_replacement () =
  let mac_addr0' = Macaddr.of_string "fc:f4:bb:e6:d7:b8" |> Result.get_ok in
  let mac_addr1' = Macaddr.of_string "fc:f4:bb:e6:d7:b9" |> Result.get_ok in
  let dev0' =
    {
      Dev.name= "eno10"
    ; pci= pci_addr0
    ; mac= mac_addr0'
    ; bios_eth_order= 1
    ; multi_nic= false
    }
  in
  let dev1' =
    {
      Dev.name= "eno11"
    ; pci= pci_addr1
    ; mac= mac_addr1'
    ; bios_eth_order= 0
    ; multi_nic= false
    }
  in
  let currents =
    currents |> unplug dev0 |> unplug dev1 |> plug dev0' |> plug dev1'
  in
  let order = sort' ~currents ~rules:[] ~last_order |> Result.get_ok in

  let test_replaced_position_and_presence position mac =
    test_position_and_presence (Some position) order mac
  in

  let test_position_and_presence mac =
    test_position_and_presence (expected_of_dev_mac currents mac) order mac
  in

  Alcotest.(check int) "6 devices in the order" 6 (List.length order) ;
  (* XXX hardcoded because it doesn't match the bios order in currents *)
  test_replaced_position_and_presence 0 mac_addr0' ;
  test_replaced_position_and_presence 1 mac_addr1' ;
  test_position_and_presence mac_addr2 ;
  test_position_and_presence mac_addr3 ;
  (* XXX hardcoded because it doesn't match the bios order in currents *)
  test_replaced_position_and_presence 5 mac_addr4 ;
  test_replaced_position_and_presence 4 mac_addr5

let test_adding () =
  let currents = currents |> plug dev6 |> plug dev7 in

  let order = sort' ~currents ~rules:[] ~last_order |> Result.get_ok in

  let test_replaced_position_and_presence position mac =
    test_position_and_presence (Some position) order mac
  in

  let test_position_and_presence mac =
    test_position_and_presence (expected_of_dev_mac currents mac) order mac
  in

  Alcotest.(check int) "8 devices in the order" 8 (List.length order) ;
  test_position_and_presence mac_addr0 ;
  test_position_and_presence mac_addr1 ;
  test_position_and_presence mac_addr2 ;
  test_position_and_presence mac_addr3 ;
  (* XXX hardcoded because it doesn't match the bios order in currents *)
  test_replaced_position_and_presence 5 mac_addr4 ;
  test_replaced_position_and_presence 4 mac_addr5 ;
  (* XXX bios_eth_order is ignored here *)
  test_replaced_position_and_presence 7 mac_addr6 ;
  test_replaced_position_and_presence 6 mac_addr7

let test_removing () =
  let currents = currents |> unplug dev0 |> unplug dev1 in

  let order = sort' ~currents ~rules:[] ~last_order |> Result.get_ok in

  let test_replaced_position_and_presence position mac =
    test_position_and_presence (Some position) order mac
  in

  let test_deleted_position_and_presence position mac =
    test_position_and_presence ~plugged:false (Some position) order mac
  in

  let test_position_and_presence mac =
    test_position_and_presence (expected_of_dev_mac currents mac) order mac
  in

  Alcotest.(check int) "6 devices in the order" 6 (List.length order) ;
  test_deleted_position_and_presence 0 mac_addr0 ;
  test_deleted_position_and_presence 1 mac_addr1 ;
  test_position_and_presence mac_addr2 ;
  test_position_and_presence mac_addr3 ;
  (* XXX hardcoded because it doesn't match the bios order in currents *)
  test_replaced_position_and_presence 5 mac_addr4 ;
  test_replaced_position_and_presence 4 mac_addr5

let test_plug_remembered () =
  (* Simulate that device 0 and 1 were previously unplugged *)
  let last_order =
    last_order
    |> List.map (fun dev ->
           if dev.mac = dev0.mac || dev.mac = dev1.mac then
             {dev with present= false}
           else
             dev
       )
  in

  let order = sort' ~currents ~rules:[] ~last_order |> Result.get_ok in

  let test_replaced_position_and_presence position mac =
    test_position_and_presence (Some position) order mac
  in

  let test_position_and_presence mac =
    test_position_and_presence (expected_of_dev_mac currents mac) order mac
  in

  Alcotest.(check int) "6 devices in the order" 6 (List.length order) ;

  test_position_and_presence mac_addr0 ;
  test_position_and_presence mac_addr1 ;
  test_position_and_presence mac_addr2 ;
  test_position_and_presence mac_addr3 ;
  (* XXX hardcoded because it doesn't match the bios order in currents *)
  test_replaced_position_and_presence 5 mac_addr4 ;
  test_replaced_position_and_presence 4 mac_addr5

let test_multi_nic_inplace_reorder () =
  let mac_addr4' = Macaddr.of_string "01:02:c9:ed:fd:f0" |> Result.get_ok in
  let mac_addr5' = Macaddr.of_string "01:02:c9:ed:fd:f1" |> Result.get_ok in
  let dev4' =
    {
      Dev.name= "eno14"
    ; pci= pci_addr4
    ; mac= mac_addr4'
    ; bios_eth_order= 1 (* XXX inconsistent with currents *)
    ; multi_nic= true
    }
  in
  let dev5' =
    {
      Dev.name= "eno15"
    ; pci= pci_addr5
    ; mac= mac_addr5'
    ; bios_eth_order= 0 (* XXX inconsistent with currents *)
    ; multi_nic= true
    }
  in
  (* The MAC addresses of multi_nic functions change *)
  let currents =
    currents |> unplug dev4 |> unplug dev5 |> plug dev4' |> plug dev5'
  in
  let order = sort' ~currents ~rules:[] ~last_order |> Result.get_ok in

  let test_replaced_position_and_presence position mac =
    test_position_and_presence (Some position) order mac
  in

  let test_position_and_presence mac =
    test_position_and_presence (expected_of_dev_mac currents mac) order mac
  in

  Alcotest.(check int) "6 devices in the order" 6 (List.length order) ;

  test_position_and_presence mac_addr0 ;
  test_position_and_presence mac_addr1 ;
  test_position_and_presence mac_addr2 ;
  test_position_and_presence mac_addr3 ;
  (* XXX both replaced, they don't match bios_eth_order *)
  test_replaced_position_and_presence 5 mac_addr4' ;
  test_replaced_position_and_presence 4 mac_addr5'

let test_multi_nic_new_devices () =
  let mac_addr6 = Macaddr.of_string "01:02:c9:ed:fd:f0" |> Result.get_ok in
  let mac_addr7 = Macaddr.of_string "01:02:c9:ed:fd:f1" |> Result.get_ok in

  (* New devices are on the same PCI address as 2 existing ones *)
  let dev6' =
    {
      Dev.name= "enp5s0d2"
    ; pci= pci_addr4
    ; mac= mac_addr6
    ; bios_eth_order= 1 (* XXX inconsistent with currents !*)
    ; multi_nic= true
    }
  in
  let dev7' =
    {
      Dev.name= "enp5s0d3"
    ; pci= pci_addr5
    ; mac= mac_addr7
    ; bios_eth_order= 0 (* XXX inconsistent with currents !*)
    ; multi_nic= true
    }
  in
  let currents = currents |> plug dev6' |> plug dev7' in

  let order = sort' ~currents ~rules:[] ~last_order |> Result.get_ok in

  let test_replaced_position_and_presence position mac =
    test_position_and_presence (Some position) order mac
  in

  let test_position_and_presence mac =
    test_position_and_presence (expected_of_dev_mac currents mac) order mac
  in

  Alcotest.(check int) "8 devices in the order" 8 (List.length order) ;

  test_position_and_presence mac_addr0 ;
  test_position_and_presence mac_addr1 ;
  test_position_and_presence mac_addr2 ;
  test_position_and_presence mac_addr3 ;
  (* XXX hardcoded because it doesn't match the bios order in currents *)
  test_replaced_position_and_presence 5 mac_addr4 ;
  test_replaced_position_and_presence 4 mac_addr5 ;
  (* XXX Both replaced, they don't match bios_eth_order *)
  test_replaced_position_and_presence 6 mac_addr6 ;
  test_replaced_position_and_presence 7 mac_addr7

let test_pci_changes () =
  let move_bus_by_1 pci_addr = Xcp_pci.{pci_addr with bus= pci_addr.bus + 1} in
  let currents =
    currents |> List.map (fun dev -> Dev.{dev with pci= move_bus_by_1 dev.pci})
  in
  let order = sort' ~currents ~rules:[] ~last_order |> Result.get_ok in

  let test_replaced_position_and_presence position mac =
    test_position_and_presence (Some position) order mac
  in

  let test_position_and_presence mac =
    test_position_and_presence (expected_of_dev_mac currents mac) order mac
  in

  Alcotest.(check int) "6 devices in the order" 6 (List.length order) ;

  test_position_and_presence mac_addr0 ;
  test_position_and_presence mac_addr1 ;
  test_position_and_presence mac_addr2 ;
  test_position_and_presence mac_addr3 ;
  (* XXX hardcoded because it doesn't match the bios order in currents *)
  test_replaced_position_and_presence 5 mac_addr4 ;
  test_replaced_position_and_presence 4 mac_addr5

let tests =
  [
    ( "test_known_cases"
    , [
        ("test_default", `Quick, test_default)
      ; ("test_initial_mapping_via_mac", `Quick, test_initial_rules_via_mac)
      ; ("test_initial_mapping_via_name", `Quick, test_initial_rules_via_label)
      ; ("test_replacement", `Quick, test_replacement)
      ; ("test_adding", `Quick, test_adding)
      ; ("test_removing", `Quick, test_removing)
      ; ("test_replug", `Quick, test_plug_remembered)
      ; ( "test_multi_nic_inplace_reorder"
        , `Quick
        , test_multi_nic_inplace_reorder
        )
      ; ("test_multi_nic_new_devices", `Quick, test_multi_nic_new_devices)
      ; ("test_pci_changes", `Quick, test_pci_changes)
      ]
    )
  ]
