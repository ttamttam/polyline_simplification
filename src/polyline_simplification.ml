open Batteries

let erase_bits_between first last indexes () =
  for i = succ first to pred last
  do BitSet.unset indexes i done

let sqrd first last = Gg.V3.(last - first |> norm2)

let find_middle sqr_epsilon first last a =
  if last < first + 2
  then None
  else
    (* Square distance between a point and (first,last) segment *)
    let sqdist =
      let seg = Gg.V3.of_v2 ~z:0. Gg.V2.(a.(last) - a.(first)) in
      if
        Gg.Float.is_zero ~eps:1e-6 (Gg.V3.norm2 seg)
      then
        (fun p ->
           Gg.V2.(p - a.(first) |> norm2))
      else
        let u = Gg.V3.unit seg in
        (fun p ->
           let vec = Gg.V3.of_v2 ~z:0. Gg.V2.(a.(first) - p) in
           Gg.V3.(cross vec u |> norm2)) in

    Array.fold_lefti
      (fun (idx, high) curr_idx p ->
         let curr_dist = sqdist p in
         if curr_dist > high then (Some curr_idx, curr_dist) else (idx, high))
      (None, sqr_epsilon)
      (Array.sub a (succ first) (last - first - 1))

    (* Getting rid of high: we are only interested in the index in the sub-array*)
    |> fst

    (* And convert it to be meaningful as an index in [a] *)
    |> Option.map ((+) (succ first))

let simplify a epsilon =
  let sqr_epsilon = epsilon *. epsilon in
  let sz = Array.length a in
  let indexes = BitSet.create_full sz in

  (* Ramer-Douglas-Peucker algorithm *)
  let rec simp = function
    | [] -> ()
    | (first, last) :: tl ->
      begin
        match find_middle sqr_epsilon first last a with
        | None ->
          erase_bits_between first last indexes ();
          simp tl
        | Some middle ->
          simp ((first, middle) :: (middle, last) :: tl)
      end in

  let () = simp [(0, pred sz)] in

  (* After the simplification, we just have to collect the indexes of the bits
     remaining set in indexes *)

  (* Turn indexes into an enum *)
  BitSet.enum indexes

  (* Use it to index the array elements we want to keep *)
  |> Enum.map (Array.get a)
  |> Array.of_enum

(*
let a = Array.map Gg.V2.of_tuple [| (0.,0.);(1.,0.1);(2.,-.0.1);
                                    (3.,5.);(4.,6.);(5.,7.);
                                    (6.,8.1);(7.,9.);(8.,9.);(9.,9.) |]

simplify a 1. -> [|(0 0); (2 -0.1); (3 5); (7 9); (9 9); (5 7); (9 9)|]
*)
