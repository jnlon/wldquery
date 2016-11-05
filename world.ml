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

(* Load tile data *)
let load_wld_tiles fd wld_header = 
  Log.infoln "Loading tiles...";
  let max_x = Util.int_of_bytes (List.assoc "max_tiles_x" wld_header) in
  let max_y = Util.int_of_bytes (List.assoc "max_tiles_y" wld_header) in
  let importance = Util.bool_array_of_bits (List.assoc "importance" wld_header) in

  let read_wld_tile () =
    let tilebuf = Buffer.create 0 in
    let bit_on bts n = ((int_of_char (Bytes.get bts 0)) land n) = n in
    let add_n_bytes n = let b = (single_read fd n) in Buffer.add_bytes tilebuf b ; b in
    let add_byte () = add_n_bytes 1 in
    let add_int16 () = add_n_bytes 2 in
    let add_int32 () = add_n_bytes 4 in

    let b3 = add_byte () in
    let b2 = if (bit_on b3 1) then add_byte () else Bytes.make 1 '\000' in
    let b = if (bit_on b2 1) then add_byte () else Bytes.make 1 '\000' in

    let tile_id = begin
      if (bit_on b3 2) then begin (* If active tile *)
        let num2 = (* Tile ID *)
          (Util.int_of_bytes (if (bit_on b3 32) then add_int16 () else add_byte ()))
        in

        if importance.(num2) then ignore(add_int32 ()) (* texture coordinates *)
        else ();

        if bit_on b 8 then ignore(add_byte ()) else (); (* Tile color *)
        num2 (* <-- type of tile *)
      end 
        else -1 (* No tile here *)
    end
    in

    if bit_on b3 4 then begin
      ignore(add_byte ()); (* wall type *)
      if bit_on b 16 then ignore(add_byte ()) else (); (* wall color *)
    end else ();

    let b4 = ((Util.byte_at b3 0) land 24) lsr 3 in
    if b4 != 0 then ignore(add_byte ()) else ();  (* Liquid type *)

    let tile_id = match b4 with  (* treat liquid like a tile *)
      | 1 -> 600      (* Water *)
      | 2 -> 700      (* Lava *)
      | 3 -> 800      (* Honey *)
      | _ -> tile_id  (* no liquid *)
    in

    let b4 = ((Util.byte_at b3 0) land 192) lsr 6 in

    (* RLE count *)
    let k =  
      match b4 with
        | 0 -> Bytes.make 1 '\000'
        | 1 -> add_byte ()
        | _ -> add_int16 () 
    in

    ( (Buffer.to_bytes tilebuf) , 
      [ ("rle_length", (Util.int_of_bytes k)) ; ("tile_id", tile_id) ] )
  in

  (*let tiles = Array.make_matrix max_x max_y (Bytes.create 0) in*)
  let tiles = Array.make_matrix max_y max_x '\000' in
  for x=0 to (max_x-1) do
    Log.printf Log.Info "Loading tile row %d/%d\n" (x+1) max_x;
    (*Printf.printf "--> New X: %d (max_x = %d, max_y = %d)\n" x max_x max_y;*)
    let rec for_every_y y =
      if y >= max_y then ()
      else begin
        let tile_data = read_wld_tile () in 
        let tile_bytes = fst tile_data in
        let rle_count = List.assoc "rle_length" (snd tile_data) in
        let tile_id = List.assoc "tile_id" (snd tile_data) in
        let tile_char = Tiles.char_of_tile @@ Tiles.tile_of_id tile_id in

        (* Log everything here *)
        Log.printf Log.Debug 
          "TID = %3d ; TCHR = %c ; K = %3d ; (y=%d,x=%d) ; BYTES = [%s\b]\n" 
          tile_id tile_char rle_count y x (Log.int_string_of_bytes tile_bytes);

        (* Set tile char at y,x *)
        tiles.(y).(x) <- tile_char;

        (* Copy tile data RLE times down *)
        for y_with_rle=(y+1) to (y+rle_count) do 
          tiles.(y_with_rle).(x) <- tile_char
        done;

        (* Continue down column *)
        for_every_y (rle_count + y + 1)
      end 
    in
    for_every_y 0;
  done;
  Log.infof " %d tiles\n" (max_y*max_x);
  tiles
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

let wld_tiles_of_fd fd = 
  let header = load_wld_header fd in
  let version = Util.int_of_bytes @@ List.assoc "version" header in

  let _ = 
    if (supported_wld_version = version) 
    then ()
    else 
    begin
      let errstr = 
        Printf.sprintf "World version must be %d, this wld has version %d" 
                        supported_wld_version version 
      in
      raise (Wld_version_unsupported errstr)
    end
  in

  load_wld_tiles fd header 
;;

let wld_tiles_of_path pathstr = 
  Log.noticef "Reading world file '%s'\n" pathstr;
  let open Unix in
  let fd = openfile pathstr [O_RDONLY] 0 in
  let tiles = wld_tiles_of_fd fd in
  close fd;
  tiles
;;
