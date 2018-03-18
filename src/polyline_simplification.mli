(** Ramer-Douglas-Peucker algorithm
    for polyline simplification *)
val simplify: Gg.v2 array -> float -> Gg.v2 array
