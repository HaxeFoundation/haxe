open Globals
open Type

class virtual hxb_reader_api = object(self)
	method virtual make_module : path -> string -> module_def
	method virtual add_module : module_def -> unit
	method virtual resolve_type : string list -> string -> string -> HxbData.typing_mode -> module_type
	method virtual resolve_module : path -> HxbData.typing_mode -> module_def
	method virtual basic_types : basic_types
	method virtual get_var_id : int -> int
	method virtual read_expression_eagerly : tclass_field -> bool
	method virtual make_lazy_type : Type.t -> (unit -> Type.t) -> Type.t
	(* Shared forwarding-class registry (lazy inheritance restore). See Common.context.hxb_forward_classes.
	   Returns an empty table when forwarding is not supported by this api (full restore). *)
	method forward_classes : (path,tclass) Hashtbl.t = Hashtbl.create 0
	(* Whether lazy-inheritance forwarding is enabled for this context (gated, default off). *)
	method forwarding_enabled : bool = false
	(* Forwarding support: resolve a class ref to the REAL tclass when its module is already available
	   without forcing a decode (request lut, resident tier, cached typed module). A stub minted for a
	   module that later gets SERVED as an already-typed module is never merged by read_mtf (no decode
	   happens), so its TInst identity splits from the real class and unification against it fails --
	   in display mode that silently degrades e.g. call results to TMono. *)
	method peek_class (_ : path) (_ : string) : tclass option = None
end

class hxb_reader_api_null = object(self)
	inherit hxb_reader_api

	method make_module _ = assert false
	method add_module _ = assert false
	method resolve_type _ _ _ _ = assert false
	method resolve_module _ _ = assert false
	method basic_types = assert false
	method get_var_id _ = assert false
	method read_expression_eagerly _ = assert false
	method make_lazy_type _ _ = assert false
end