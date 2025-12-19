open! Core
open! Hardcaml
open! Aof2025

let generate_rtl () =
  let module C = Circuit.With_interface (Design_top.I) (Design_top.O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true () in
  let circuit = C.create_exn ~name:"design_top" (Design_top.hierarchical scope) in
  let rtl_circuits =
    Rtl.create ~database:(Scope.circuit_database scope) Verilog [ circuit ]
  in
  let rtl = Rtl.full_hierarchy rtl_circuits |> Rope.to_string in
  print_endline rtl;
;;

let generate_rtl_command =
  Command.basic
    ~summary:""
    [%map_open.Command
      let () = return () in
      fun () -> generate_rtl ()]
;;

let () =
  Command_unix.run
    (Command.group ~summary:"" [ "aof2025", generate_rtl_command ])
;;
