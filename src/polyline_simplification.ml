open Batteries

(* int -> int -> Batteries.BitSet.t -> unit -> unit *)
let erase_bits_between first last indexes () =
  for i = succ first to pred last
  do BitSet.unset indexes i done
let is_zero ~eps v = abs_float v <= eps

module type VECTOR = sig
  type t
  val sub : t -> t -> t
  val unit : t -> t
  val norm2 : t -> float
  val cross_norm2 : t -> t -> float
end

type 'a find_middle_func = float -> int -> int -> 'a array -> int option

let find_middle
    (type t) (module V:VECTOR with type t = t)
    sqr_epsilon first last a =

  if last < first + 2
  then None
  else
    (* Square distance between a point and (first,last) segment *)
    let sqdist =
      let seg = V.sub a.(last) a.(first) in
      if
        is_zero ~eps:1e-6 (V.norm2 seg)
      then
        (fun p -> V.(sub p a.(first) |> norm2))
      else
        let u = V.unit seg in
        (fun p -> V.cross_norm2 u (V.sub a.(first) p)) in

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


let simplified_indexes find_middle ~polyline ~epsilon =
  let sqr_epsilon = epsilon *. epsilon in
  let sz = Array.length polyline in
  let indexes = BitSet.create_full sz in

  (* Ramer-Douglas-Peucker algorithm *)
  let rec simp = function
    | [] -> ()
    | (first, last) :: tl ->
      begin
        match find_middle sqr_epsilon first last polyline with
        | None ->
          erase_bits_between first last indexes ();
          simp tl
        | Some middle ->
          simp ((first, middle) :: (middle, last) :: tl)
      end in

  simp [(0, pred sz)];
  indexes

let simplify find_middle ~polyline ~epsilon =
  simplified_indexes find_middle polyline epsilon

 (* Turn indexes into an enum *)
  |> BitSet.enum

  (* Use it to index the array elements we want to keep *)
  |> Enum.map (Array.get polyline)
  |> Array.of_enum

(*
let a = Array.map Gg.V2.of_tuple [| (0.,0.);(1.,0.1);(2.,-.0.1);
                                    (3.,5.);(4.,6.);(5.,7.);
                                    (6.,8.1);(7.,9.);(8.,9.);(9.,9.) |]

simplify a 1. -> [|(0 0); (2 -0.1); (3 5); (7 9); (9 9); (5 7); (9 9)|]
*)
