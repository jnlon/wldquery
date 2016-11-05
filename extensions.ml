module List = struct
  include List
  let make size thing = 
    let rec _make i lst = 
      if i = size then lst
      else _make (succ i) (thing :: lst)
    in
    _make 0 [];;

  let make_fn fn times =
    let rec _make i lst = 
      if i = times then lst
      else _make (succ i) (fn () :: lst)
    in
    List.rev @@ _make 0 [];;
end;;
