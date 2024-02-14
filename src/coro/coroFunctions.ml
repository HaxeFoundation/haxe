open Globals
open Type
open CoroTypes

let make_block typepos = {
	cb_el = DynArray.create ();
	cb_typepos = typepos;
	cb_next = {next_kind = NextUnknown; next_type = t_dynamic; next_pos = null_pos};
}
