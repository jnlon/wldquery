(* Log levels *)
type log_level_t = 
  | Debug
  | Info
  | Notice
  | Warning
  | Error
  | Fatal
;;

(* Places we can log to *)
type log_out_t = 
  | Stdout
  | Stderr
  | File of Unix.file_descr
;;

(* A Log level and where it goes *)
type log_conf_t = log_level_t * log_out_t list ;;

(* Default log configuration *)
let _log_config = ref [
  (Debug, []);
  (Info, []);
  (Notice, [Stdout]);
  (Warning, [Stderr]);
  (Error, [Stderr]);
  (Fatal, [Stderr]) 
] ;;

(* Updates _log_config *)
let rec really_set_config = function
  (level, out_lst) :: rest -> begin
    _log_config := List.remove_assoc level !_log_config ;
    _log_config := (level, out_lst) :: !_log_config ;
    really_set_config rest
  end
  | [] -> ()
;;

(* Write str to all log locations *)
let rec write_to str = function
    Stdout :: rest -> (print_string str ; write_to str rest)
  | Stderr :: rest -> (prerr_string str ; write_to str rest)
  | File fd :: rest -> 
    begin
      ignore @@ Unix.single_write fd (Bytes.of_string str) 0 (String.length str) ;
      write_to str rest; 
    end
  | [] -> ()
;;

(* Log a string 's' to outputs specified by 'lvl' *)
let str lvl s = 
  let out_ports = (List.assoc lvl !_log_config) in
  if out_ports != [] then
    write_to s out_ports
  else ()
;;

(* Like Log.str, but with formatting *)
let printf lvl = 
  Printf.ksprintf @@ str lvl
;;

(* Log string 's' with a newline appended *)
let line lvl s = str lvl (s ^ "\n") ;;

(* Log the file offset of 'fd' *)
let file_offset lvl fd =
  let open Unix in
  try
    printf lvl "File offset: 0x%x\n" (lseek fd 0 SEEK_CUR)
  with unix_error -> ()
;;

(* Turn a bytes object into a human readable string *)
let int_string_of_bytes b = 
  let buf = Buffer.create 500 in
  Bytes.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%d " (int_of_char c))) b;
  Buffer.contents buf
;;
  
(* Log a bytes object *)
let bytes lvl b = 
  str lvl @@ int_string_of_bytes b
;;

(* Log a bytes object with a label attached *)
let label_bytes lvl label b =
  ignore @@ printf lvl "%s: [%s]" label (int_string_of_bytes b)
;;

(* Convenience functions *)
let set_debug out_lst = really_set_config [(Debug, out_lst)] ;;
let set_info out_lst = really_set_config [(Info, out_lst)] ;;
let set_notice out_lst = really_set_config [(Notice, out_lst)] ;;
let set_warning out_lst = really_set_config [(Warning, out_lst)] ;;
let set_error out_lst = really_set_config [(Error, out_lst)] ;;
let set_fatal out_lst = really_set_config [(Fatal, out_lst)] ;;

let debugln = line Debug ;;
let infoln = line Info ;;
let noticeln = line Notice ;;
let warningln = line Warning ;;
let errorln = line Error ;;
let fatalln = line Fatal ;;

let debugstr = str Debug ;;
let infostr = str Info ;;
let noticestr = str Notice ;;
let warningstr = str Warning ;;
let errorstr = str Error ;;
let fatalstr = str Fatal ;;

let debugf s = printf Debug s ;;
let infof s = printf Info s ;;
let noticef s = printf Notice s ;;
let warningf s = printf Warning s ;;
let errorf s = printf Error s ;;
let fatalf s = printf Fatal s ;;
