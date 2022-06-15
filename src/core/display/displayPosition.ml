open Globals

let encloses_position p_target p =
	p_target.pmin <> -1 && p_target.pmax <> -1 && p.pmin <= p_target.pmin && p.pmax >= p_target.pmax

let encloses_position_gt p_target p =
	p_target.pmin <> -1 && p_target.pmax <> -1 && p.pmin <= p_target.pmin && p.pmax > p_target.pmax

class display_position_container =
	object (self)
		(** Current display position *)
		val mutable pos = nul