module List = struct
  include List
  let make size thing = 
    let rec _make i lst = 
      if i = size then lst
      else _make (succ i) (thing :: lst)
    in
    _make 0 [];;

  let repeat_fn fn times =
    let rec _make i lst = 
      if i = times then lst
      else _make (succ i) (fn () :: lst)
    in
    List.rev @@ _make 0 [];;
end;;

module Pervasives = struct
  include Pervasives

  let read_byte = input_byte ;;

  let input_byte_array nbytes in_ch = 
    let arr = Array.make nbytes 0 in
    for i=0 to (nbytes-1) do
      arr.(i) <- (input_byte in_ch)
    done;
    arr ;;

  let read_bool in_ch =
    (input_byte in_ch) > 0 ;;

  let read_pascal_string in_ch = 
    let strlen = input_byte in_ch in
    really_input_string in_ch strlen ;;

  (* TODO: Remove duplicate code in read_int *)

  let read_int16 in_ch = 
    Array.fold_right
      (fun init elem -> init + elem)
      (Array.mapi 
        (fun offset elem -> elem lsl (offset*8)) 
        (input_byte_array 2 in_ch)) 
      0 ;;

  let read_int32 in_ch = 
    let open Int32 in
    Array.fold_right
      (fun init elem -> add init elem)
      (Array.mapi 
        (fun offset elem -> shift_left (of_int elem) (offset*8)) 
        (input_byte_array 4 in_ch)) 
      zero ;;

  let read_int64 in_ch = 
    let open Int64 in
    Array.fold_right
      (fun init elem -> add init elem)
      (Array.mapi 
        (fun offset elem -> shift_left (of_int elem) (offset*8)) 
        (input_byte_array 8 in_ch)) 
      zero ;;

  let read_single in_ch = 
    Int32.to_float @@ read_int32 in_ch ;;

  let read_double in_ch = 
    Int64.to_float @@ read_int64 in_ch ;;

  let read_int32_array in_ch n = 
    let arr = Array.make n Int32.zero in
    for i=0 to (n-1) do
      arr.(i) <- (read_int32 in_ch)
    done;
    arr ;;

  let read_bool_array in_ch n = 
    let arr = Array.make n false in
    for i=0 to (n-1) do
      arr.(i) <- (read_bool in_ch)
    done;
    arr ;;

  let read_string_array in_ch n = 
    let arr = Array.make n "" in
    for i=0 to (n-1) do
      arr.(i) <- (read_pascal_string in_ch)
    done;
    arr ;;

end;;

