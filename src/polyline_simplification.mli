(** Ramer-Douglas-Peucker polyline simplification in OCaml. *)

(** {2 Step 1: write a module} *)

(** In order to simplify a point array, you will have to write a [(module V)] of
    type {! VECTOR}, {! VECTOR.t} being the type of the points. *)

module type VECTOR =
sig
  type t

  val sub : t -> t -> t
  (** Substraction of two vectors *)

  val unit : t -> t
  (** [unit v] is the unit vector: [ v / |v| ]  *)

  val norm2 : t -> float
  (** [norm2 v] is the squared norm: [ |v|² ] *)

  val cross_norm2 : t -> t -> float
  (** [cross_norm2 a b] is the squared norm of the cross product: [ |a ∧ b|² ]  *)
end

(** {2 Step 2: retrieve a {! find_middle_func}} *)

type 'a find_middle_func
val find_middle : (module VECTOR with type t = 'a) -> 'a find_middle_func
(** Then, retrive a {! find_middle_func} with [find_middle (module V)]) *)

(** {2 Step 3: simplify} *)

val simplified_indexes : 'a find_middle_func -> polyline:'a array -> epsilon:float -> Batteries.BitSet.t
(** [simplified_indexes fm ~polyline ~epsilon]

    @return a {! Batteries.BitSet.t} of same size than [(polyline:'a array)]: the
   set bits correspond to the elements of [pl] to keep, and the unset bits
   correspond to the elements you can get rid of. *)

val simplify : 'a find_middle_func -> polyline:'a array -> epsilon:float -> 'a array
(** [simplify fm ~polyline ~epsilon]

    @return a simplified ['a array] (it makes use of {! simplified_indexes}
   internally) *)

(** {2 Simple example}

    Suppose you want to simplify the following polyline:

    {[let pl = [| (0.,0.);(1.,0.1);(2.,-.0.1); (3.,5.); (4.,6.);
            (5.,7.); (6.,8.1);(7.,9.);(8.,9.);(9.,9.) |]]}

     First, you need a {! VECTOR} module:

     {[module V2D = struct
   type t = float * float
   let sub (ax, ay) (bx, by) = (ax -. bx, ay -. by)
   let norm2 (x, y) = x *. x +. y *. y
   let unit ((x, y) as v) =
     let n = sqrt (norm2 v) in
     (x /. n, y /. n)
   let cross_norm2 (ax, ay) (bx, by) =
     let v = ax *. by -. ay *. bx in
     v *. v
 end]}

     Second, you retrieve the find_middle function:

     [let fm = find_middle (module V2D)]

     And simplify:

     [simplify fm pl 1.]

     will return the simplified polyline:

     [[|(0., 0.); (2., -0.1); (3., 5.); (7., 9.); (9., 9.)|]]

    {2 Simplify a France map}

    Tested with a France map. The original map is a [(float * float) array] of
    size 306392.

    The first [float] is the latitude, and the second one is the longitude (both
    are in degrees).

    As with the first example, I need a {! VECTOR} module:

     {[module V : Polyline_simplification.VECTOR with type t = Gg.v3 =
struct
  include Gg.V3
  let cross_norm2 a b = cross a b |> norm2
end]}

    Here is a small module to convert the tuples into 3D points with [Array.map T2.to_v3 france_map]:
    {[module T2 : sig
  type t = float * float
  val to_v3 : t -> V.t
  val of_v3 : V.t -> t
end = struct
  type t = float * float
  let r = 6378137.0
  let deg = atan2 1. 1. /. 45.
  let to_v3 (la,lo) =
    let lard = la *. deg and lord = lo *. deg in
    let sla = sin lard and cla = cos lard
    and slo = sin lord and clo = cos lord in
    (r *. sla *. clo, r *. sla *. slo, r *. cla)
    |> Gg.V3.of_tuple
  let of_v3 vv =
    let la = acos (Gg.V3.z vv /. r) in
    let lo = atan2 (Gg.V3.y vv) (Gg.V3.x vv) in
    la /. deg, lo /. deg
end]}

    I retrieved the find_middle function:

    [let fm = find_middle (module V)]

    Instead of using {!simplify}, I used {!simplified_indexes}, turned the
    resulting {!BitSet.t} into a {!Batteries.Enum.t}, and used it to index the
    original array and create the new one:

     {[Polyline_simplification.simplified_indexes fm ~polyline:france ~epsilon
   |> Batteries.BitSet.enum (* Use the indexes to index the array elements we
   want to keep *)
   |> Batteries.Enum.map (Array.get frorig)
   |> Batteries.Array.of_enum]}

   On my machine, it takes less than 1 s to perform (and output to SVG) 6
   simplifications with precisions 1000 m, 500 m, 100 m, 50 m, 10 m and 1 m.

   Even the 1 m precision is interesting, because it contains only 87158
   points (to be compared to the 306392 of the original map).

    *)
