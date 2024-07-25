open Globals
open Type

class virtual hxb_reader_api = object(self)
	method virtual make_module : path -> string -> module_def
	method virtual add_module : module_def -> unit
	method virtual resolve_type : string list -> string -> string -> module_type
	method virtual resolve_module : path -> module_def
	method virtual basic_types : basic_types
	method virtual get_var_id : int -> int
	method virtual read_expression_eagerly : tclass_field -> bool
end

class hxb_reader_api_null = object(self)
	inherit hxb_reader_api

	method make_module _ = assert false
	method add_module _ = assert false
	method resolve_type _ _ _ = assert false
	method resolve_module _ = assert false
	method basic_types = assert false
	method get_var_id _ = assert false
	method read_expression_eagerly _ = assert false
end