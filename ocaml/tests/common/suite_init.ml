let harness_init () =
  Debug.log_to_stdout () ;
  Printexc.record_backtrace true ;
  Inventory.inventory_filename :=
    Filename.concat Test_common.working_area "xapi-inventory" ;
  Xcp_client.use_switch := false ;
  Pool_role.set_pool_role_for_test () ;
  Xapi.register_callback_fns ()

let run_with_init name test_name test =
  harness_init () ;
  (* Alcotest hides the standard output of successful tests,
     so we will probably not exceed the 4MB limit in Travis *)
  Alcotest.run name [test_name, test]
