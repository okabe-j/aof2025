open! Core
open! Hardcaml
open! Signal

let shift_in ~clock ~clear ~n ?(ready = vdd) (signal : _ With_valid.t) =
  let spec = Reg_spec.create ~clock ~clear () in
  let valid_in = ready &: signal.valid in
  let counter =
    reg_fb spec ~width:(num_bits_to_represent n) ~enable:valid_in ~f:(fun x ->
      mux2 (x ==:. n - 1) (zero (width x)) (x +:. 1))
  in
  let shreg =
    reg_fb
      spec
      ~width:(n * width signal.value)
      ~enable:valid_in
      ~f:(fun x -> drop_top ~width:(width signal.value) (x @: signal.value))
  in
  let valid = reg spec (valid_in &: (counter ==:. n - 1)) in
  { With_valid.valid; value = shreg }
;;

let shift_out ~clock ~clear ?(ready = vdd) (signal : _ With_valid.t) =
  assert((width signal.value) > 0 && (width signal.value) % 8 = 0);
  let spec = Reg_spec.create ~clock ~clear () in
  let valid_in = ready &: signal.valid in
  let n = (width signal.value) / 8 in
  let counter = reg_fb spec ~width:(num_bits_to_represent n) 
        ~f:(fun x -> mux2 valid_in (of_unsigned_int ~width:(width x) n) @@ 
                     mux2 (x >:. 0) (x -:. 1) (zero @@ width x))
  in
  let shreg = reg_fb spec ~width:(width signal.value)
        ~f:(fun x -> mux2 valid_in signal.value @@ mux2 (counter >:. 0) (sll x ~by:8) x)
  in
  let valid = (counter >:. 0) in
  { With_valid.valid; value = (sel_top ~width:8 shreg) }
;;

let mul_10 = fun x -> (sll x ~by:3) +: (sll x ~by:1)

let bcd_to_binary ~clock ~clear ~output_width (signal : _ With_valid.t) =
  assert((width signal.value) > 0 && (width signal.value) % 4 = 0);
  let spec = Reg_spec.create ~clock ~clear () in
  let digits = (width signal.value) / 4 in
  let counter = reg_fb spec ~width:(num_bits_to_represent digits)
      ~f:(fun x -> mux2 signal.valid (of_unsigned_int ~width:(width x) digits) @@
                   mux2 (x >:. 0) (x -:. 1) (zero @@ width x)) in
  let buf = reg_fb spec ~width:(width signal.value) 
      ~f:(fun x -> mux2 signal.valid signal.value @@
                   mux2 (counter >:. 0) (sll x ~by:4) (zero @@ width x)) in

  let acc = reg_fb spec ~width:output_width 
      ~f:(fun x -> mux2 (counter >:. 0) 
                  ((mul_10 x) +: (uresize ~width:output_width @@ sel_top ~width:4 buf)) 
                  (zero @@ width x)) in
  let valid = reg spec (counter ==:. 1) in
  { With_valid.valid; value = acc }
;;
