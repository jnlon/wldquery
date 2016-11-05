let byte_at b at = int_of_char (Bytes.get b at) ;;
let byte_of_bytes b = Bytes.get b 0;;
let bool_of_bytes b = 
  if (byte_at b 0) > 0 then true else false
;;

let int16_of_bytes b = 
  (byte_at b 0) lor ((byte_at b 1) lsl 8)
;;

let int32_of_bytes b = 
  let open Int32 in
  let shl n bits = shift_left n bits in
  let ival i = shl (of_int (byte_at b i)) (i*8) in
  add (ival 0) @@ add (ival 1) @@ add (ival 2) @@ (ival 3)
;;

let int64_of_bytes b = 
  let open Int64 in
  let shl n bits = shift_left n bits in
  let ival i = shl (of_int (byte_at b i)) (i*8) in
  add (ival 0) @@ add (ival 1) @@ add (ival 2) @@
  add (ival 3) @@ add (ival 4) @@ add (ival 5) @@
  add (ival 6) @@ ival 7
;;

let single_of_bytes b = (Int32.float_of_bits (int32_of_bytes b)) ;;
let double_of_bytes b = (int64_of_bytes b) ;;

let int_of_bytes b =
  let rec int_of_bytes i acc = 
    if i = Bytes.length b then
      acc
    else
    int_of_bytes (succ i) (acc + ((byte_at b i) lsl (i*8)))
  in
  int_of_bytes 0 0
;;

let string_of_bytes b = Bytes.to_string b ;;

let int32_array_of_bytes b = 
  let blen = (Bytes.length b) in
  let arr = Array.make (blen/4) Int32.zero in
  let rec set_arr at = 
    if at = (blen) then ()
    else begin
      let tmp_bytes = Bytes.create 4 in
      Bytes.blit b at tmp_bytes 0 4;
      arr.(at / 4) <- int32_of_bytes tmp_bytes;
      set_arr (at + 4)
    end
  in
  set_arr 0;
  arr
;;

let bool_array_of_bits b = 
  let blen = (Bytes.length b) in
  let arr = Array.make (8 * blen) false in
  let bool_of_int i = if i = 0 then false else true in
  let bit_values = [1;2;4;8;16;32;64;128] in
  let rec set_arr at value = 
    List.iteri 
      begin 
        fun i v -> 
          arr.(at+i) <- (bool_of_int (value land v))
      end
      bit_values
  in
  for i=0 to (blen-1) do
    set_arr (i*8) (int_of_char @@ Bytes.get b i)
  done;
  arr
;;
