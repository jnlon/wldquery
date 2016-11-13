open Extensions.Pervasives ;; 

exception Wld_version_unsupported of string ;;
exception Read_type_unsupported of string ;;
exception Unknown_array_size_type of string ;;
let supported_wld_version = 177;;

type array_size =
  | Static of int (* fixed number of elements *)
  | Variable of string (* Use string as key to the number of elements *) ;;

type read_type = 
  | Bool | Byte 
  | Int16 | Int32 | Int64
  | Single | Double
  | Array of (read_type * array_size)
  | String ;;

type header_value = 
  | HBool of bool 
  | HByte of int
  | HInt of int64
  | HFloat of float
  | HArray of header_value array
  | HString of string ;;

type header_pair = (string * header_value) ;;
type header = header_pair list ;;

type tiles = int array array ;;

type position = 
  { x : int ;
    y : int } ;;

type item = 
 { id : int ;
   prefix : int } ;;

type chest = 
  { pos : position ;
    name : string ;
    items : item array } ;;

type sign = 
  { pos : position ;
    text : string } ;;

type entity = 
  { pos: position ;
    id: int; } ;;

type npc = 
  { active : bool ;
    name : string ;
    displayName : string ;
    positionX : string ;
    positionY : string ;
    homeless : bool ;
    homeTileX : int ;
    homeTileY : int } ;;

type pressure_plate = { pos: position } ;;

type world = 
  { header: header ;
    tiles: tiles ;
    chests: chest array ;
    signs: sign array ;
    npcs : npc array ;
    entities: entity array ;
    pressure_plates: pressure_plate array } ;;

let rec byte_size_of_type = function
  | Bool | Byte -> 1
  | Int16 -> 2
  | Int32 | Single -> 4
  | Int64 | Double -> 8
  | Array (t, Static array_size) -> 
      (byte_size_of_type t) * array_size 
  | String | _ -> 0 ;;

type header_read_pairs_t = (string * read_type ) list ;;
let header_pairs = [
   ("version", Int32); 
   ("relogic", Int64);
   ("revision", Int32);
   ("favorite", Int64);
   ("_num_position", Int16);
   ("positions", Array (Int32, Static 10));
   ("_num_importance", Int16);
   ("importance", Array (Bool, Static 58));

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
   ("angler_who_finished_today", Array (String, Variable "_num_angler_finished"));
   ("saved_angler", Bool);
   ("angler_quest", Int32);
   ("saved_stylist", Bool);
   ("saved_tax_collector", Bool);
   ("invasion_size_start", Int32);
   ("temp_cultist_delay", Int32);
   ("_num_npc_killed", Int16);
   ("npc_kill_count", Array (Int32, Variable "_num_npc_killed"));
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
   ("lunar_apocalypse_is_up", Bool);
   (* v 170 *)
   ("temp_party_manual", Bool);
   ("temp_party_genuine", Bool);
   ("temp_party_cooldown", Int32);
   ("_num_celebrating", Int32);
   ("temp_party_celebrating_npcs", Array (Int32, Variable "_num_celebrating"));
   (* v 174*)
   ("temp_sandstorm_happening", Bool);
   ("temp_sandstorm_time_left", Int32);
   ("temp_sandstorm_severity", Single);
   ("temp_sandstorm_intended_severity", Single) ] ;;

let sprintf = Printf.sprintf ;;
let read_header_from_pair in_chan (previously_read : header) pair : header =
  let rec data_of_pair = function
    | ("version", Int32) -> 
        let v = (read_int32 in_chan) in
        if (Int32.to_int v) != supported_wld_version
          then 
            raise @@ Wld_version_unsupported 
              (sprintf "Wld version is %ld, I only support %d" v supported_wld_version)
          else ("version", HInt (Int64.of_int32 v))
    | (key, Int64) -> (key, HInt (read_int64 in_chan))
    | (key, Int32) -> (key, HInt (Int64.of_int32 (read_int32 in_chan)))
    | (key, Int16) -> (key, HInt (Int64.of_int (read_int16 in_chan)))
    | (key, Bool)  -> (key, HBool (read_bool in_chan))
    | (key, Byte)  -> (key, HByte (read_byte in_chan))
    | (key, String) -> (key, HString (read_pascal_string in_chan))
    | (key, Single) -> (key, HFloat (read_single in_chan))
    | (key, Double) -> (key, HFloat (read_double in_chan))
    | (key, Array (Int32, Static n)) -> begin
       let int32s = read_int32_array in_chan n in
       (key, HArray (Array.map (fun i -> HInt (Int64.of_int32 i)) int32s))
     end
    | (key, Array (Bool, Static n)) -> begin
       let bools = read_bool_array in_chan n in
       (key, HArray (Array.map (fun b -> HBool b) bools))
     end
    | (key, Array (String, Static n)) -> begin
       let strings = read_string_array in_chan n in
       (key, HArray (Array.map (fun b -> HString b) strings))
     end
    | (key, Array (t, Variable prior_key)) ->  begin  (* Convert Variable types to Static*)
       let amount = 
         match (List.assoc prior_key previously_read) with
           | HInt d -> Int64.to_int d
           | _ -> raise @@ Unknown_array_size_type key
       in
       (data_of_pair (key, Array (t, Static amount)))
     end
    | (key, _) -> raise @@ Read_type_unsupported key 
  in
  Log.printf Log.Debug "Reading '%s'\n" (fst pair);
  ((data_of_pair pair) :: previously_read) ;;

let read_header inch : header = 
  List.fold_left 
    (read_header_from_pair inch)
    []
    header_pairs ;;

let rec string_of_header_data hd = 
  let open Printf in
  let open Array in
  match hd with
    | HBool b -> sprintf "%b" b
    | HByte i -> sprintf "%d" i
    | HInt i64 -> sprintf "%Ld" i64
    | HFloat f -> sprintf "%f" f
    | HArray ar -> "[" ^ (String.concat "," (List.map string_of_header_data (to_list ar))) ^ "]"
    | HString str -> "\"" ^ str ^ "\"" ;;

let print_header_pair p =
  Printf.printf "%s\n" (string_of_header_data (snd p))
;;

let read_tiles in_ch = () ;;
let read_chests in_ch = () ;;
let read_signs in_ch = () ;;
let read_npcs in_ch = () ;;
let read_entities in_ch = () ;;
let read_pressure_plates in_ch = () ;;
let read_world in_ch = ()
let world_of_file path = () ;;
let world_of_buffer bytes = () ;;
let save_world_to_file world = () ;;
