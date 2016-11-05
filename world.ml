type array_size_t =
  | Static of int (* fixed number of elements *)
  | Variable
;;

type read_type_t = 
  | Bool
  | Byte 
  | Int16
  | Int32
  | Int64
  | Array of (read_type_t * array_size_t)
  | String
  | Single
  | Double
;;

let rec byte_size_of_type = function
  | Bool | Byte -> 1
  | Int16 -> 2
  | Int32 | Single -> 4
  | Int64 | Double -> 8
  | String -> 0
  | Array (read_type, array_size) -> 
      (byte_size_of_type read_type) * 
      begin 
        match array_size with
          Static i -> i
          | _ -> 0
      end
;;

exception Read_too_few_bytes of bytes ;;
exception Wld_version_unsupported of string ;;
let supported_wld_version = 156;;

let single_read fd len =
  let buf = Bytes.create len in
  let rec _single_read read = 
    let r = Unix.read fd buf read (len - read) in
    match r with
      | _ when (r + read) = len -> buf
      | 0 -> raise (Read_too_few_bytes buf)
      | _ -> (_single_read (r + read))
  in
  _single_read 0
;;

let read_int16 fd = Util.int16_of_bytes @@ single_read fd 2 ;;
let read_int32 fd = Util.int32_of_bytes @@ single_read fd 4 ;; 
let read_single fd = Util.single_of_bytes @@ single_read fd 4 ;; 
let read_int64 fd = Util.int64_of_bytes @@ single_read fd 8 ;;
let read_byte fd = Util.byte_of_bytes @@ single_read fd 1 ;;
let read_bool fd = Util.bool_of_bytes @@ single_read fd 1 ;;
let read_string fd = 
  let len = Util.byte_at (single_read fd 1) 0 in
  single_read fd len
;;

(* Load header *)
let load_wld_header fd = 
  let read num_bytes = single_read fd num_bytes in
  let read_n_strings n =
    let byte_strings = Extensions.List.make_fn (fun () -> read_string fd) n in
    let buffer = Buffer.create 300 in
    List.iter (Buffer.add_bytes buffer) byte_strings ; (* Should we null-terminate these? *)
    Buffer.to_bytes buffer;
  in
  let rec read_header read_seq bytelst = 
    match read_seq with
      | [] -> bytelst
      | (s, Bool) :: tl -> read_header tl ((s,(read 1)) :: bytelst)
      | (s, Byte) :: tl -> read_header tl ((s,(read 1)) :: bytelst)
      | (s, Int16) :: tl -> read_header tl ((s,(read 2)) :: bytelst)
      | (s, Int32) :: tl -> read_header tl ((s,(read 4)) :: bytelst)
      | (s, Single) :: tl -> read_header tl ((s,(read 4)) :: bytelst)
      | (s, Int64) :: tl -> read_header tl ((s,(read 8)) :: bytelst)
      | (s, Double) :: tl -> read_header tl ((s,(read 8)) :: bytelst)
      | (s, String) :: tl -> begin
            let strsz = int_of_char @@ Bytes.get (read 1) 0 in
            read_header tl ((s,(read strsz)) :: bytelst)
          end
      | (s, Array (t, array_sz)) :: tl -> begin
        let elements =     (* How many elements has this array? *)
          match array_sz with 
            | Variable -> Util.int_of_bytes @@ snd (List.hd bytelst)  (* size of last read variable *)
            | Static sz -> sz 
        in
        match t with
          | String ->  (* Array of strings? *)
              read_header tl ((s,(read_n_strings elements)) :: bytelst)
          | t -> begin (* Array of other types? *)
              let readsize = (elements * (byte_size_of_type t)) in
              read_header tl ((s,(read readsize)) :: bytelst)
          end
        end
  in 

  let wld_header_read_seq = [
     ("version", Int32); 
     ("relogic", Int64);
     ("revision", Int32);
     ("favorite", Int64);
     ("_num_position", Int16);
     ("positions", Array (Int32, Static 10));
     ("_num_importance", Int16);
     ("importance", Array (Bool, Static 53));

     ("world_name", String);
     ("world_id", Int32);
     ("left_world_boundary", Int32);
     ("right_world_boundary", Int32);
     ("top_world_boundary", Int32);
     ("bottom_world_boundary", Int32);
     ("max_tiles_y", Int32);
     ("max_tiles_x", Int32);
     ("expert_mode", Bool);
     ("creation_time", Double);
     ("moon_type", Byte);
     ("tree_x", Array (Int32, Static 3)); (* size = 3 *)
     ("tree_style", Array (Int32, Static 4)); (* size = 4 *)
     ("cave_back_x", Array (Int32, Static 3)); (* size = 3 *)
     ("cave_back_style", Array (Int32, Static 4)); (* size = 4 *)
     ("ice_back_style", Int32);
     ("jungle_back_style", Int32);
     ("hell_back_style", Int32);
     ("spawn_tile_x", Int32);
     ("spawn_tile_y", Int32);
     ("world_surface", Double);
     ("rock_layer", Double);
     ("temp_time", Double);
     ("temp_day_time", Bool);
     ("temp_moon_phase", Int32);
     ("temp_blood_moon", Bool);
     ("temp_eclipse", Bool);
     ("dungeon_x", Int32);
     ("dungeon_y", Int32);
     ("crimson", Bool);

     ("downed_boss_1", Bool);
     ("downed_boss_2", Bool);
     ("downed_boss_3", Bool);
     ("downed_queen_bee", Bool);
     ("downed_mech_boss_1", Bool);
     ("downed_mech_boss_2", Bool);
     ("downed_mech_boss_3", Bool);
     ("downed_mech_boss_any", Bool);
     ("downed_plant_boss", Bool);
     ("downed_golem_boss", Bool);
     ("downed_slime_king", Bool);
     ("saved_goblin", Bool);
     ("saved_wizard", Bool);
     ("saved_mech", Bool);
     ("downed_goblins", Bool);
     ("downed_clown", Bool);
     ("downed_frost", Bool);
     ("downed_pirates", Bool);
     ("shadow_orb_smashed", Bool);
     ("spawn_meteor", Bool);
     ("shadow_orb_count", Byte);
     ("altar_count", Int32);
     ("hard_mode", Bool);
     ("invasion_delay", Int32);
     ("invasion_size", Int32);
     ("invasion_type", Int32);
     ("invasion_x", Double);

     ("slime_rain_time", Double);
     ("sundial_cooldown", Byte);
     ("temp_raining", Bool);
     ("temp_rain_time", Int32);
     ("temp_max_rain", Single);
     ("ore_tier1", Int32);
     ("ore_tier2", Int32);
     ("ore_tier3", Int32);
     ("tree_bg", Byte);
     ("corrupt_bg", Byte);
     ("jungle_bg", Byte);
     ("snow_bg", Byte);
     ("hallow_bg", Byte);
     ("crimson_bg", Byte);
     ("desert_bg", Byte);
     ("ocean_bg", Byte);
     ("cloud_bg_active", Int32);
     ("num_clouds", Int16);
     ("wind_speed", Single);
     ("_num_angler_finished", Int32);
     ("angler_who_finished_today", Array (String, Variable));
     ("saved_angler", Bool);
     ("angler_quest", Int32);
     ("saved_stylist", Bool);
     ("saved_tax_collector", Bool);
     ("invasion_size_start", Int32);
     ("temp_cultist_delay", Int32);
     ("_num_npc_killed", Int16);
     ("npc_kill_count", Array (Int32, Variable));
     ("fast_forward_time", Bool);
     ("downed_fishron", Bool);
     ("downed_martians", Bool);
     ("downed_ancient_cultist", Bool);
     ("downed_moonlord", Bool);
     ("downed_halloween_king", Bool);
     ("downed_halloween_tree", Bool);
     ("downed_christmas_ice_queen", Bool);
     ("downed_christmas_santank", Bool);
     ("downed_christmas_tree", Bool);

     ("downed_tower_solar", Bool);
     ("downed_tower_vortex", Bool);
     ("downed_tower_nebula", Bool);
     ("downed_tower_stardust", Bool);

     ("tower_active_solar", Bool);
     ("tower_active_vortex", Bool);
     ("tower_active_nebula", Bool);
     ("tower_active_stardust", Bool);
     ("lunar_apocalypse_is_up", Bool) ]
  in

  let header = read_header wld_header_read_seq [] in

  List.iter (fun x -> Log.label_bytes Log.Debug (fst x) (snd x)) (List.rev header);
  Log.infoln "Loaded header";
  Log.file_offset Log.Debug fd;

  header;
;;

let wld_header_of_fd fd = 
  let header = load_wld_header fd in
  let version = Util.int_of_bytes @@ List.assoc "version" header in
  let _ = 
    if (supported_wld_version = version) then ()
    else 
    begin
      let errstr = 
        Printf.sprintf "World version must be %d, this wld has version %d" 
                        supported_wld_version version 
      in
      raise (Wld_version_unsupported errstr)
    end
  in
  header
;;

let wld_header_of_path pathstr = 
  Log.noticef "Reading world file '%s'\n" pathstr;
  let open Unix in
  let fd = openfile pathstr [O_RDONLY] 0 in
  let header = wld_header_of_fd fd in
  close fd;
  header
;;
