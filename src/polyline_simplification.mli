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

 (** {2 Example}

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
*)
