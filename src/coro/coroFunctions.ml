open Globals
open Type
open CoroTypes

let make_block ctx typepos =
	let id = ctx.next_block_id in
	ctx.next_block_id <- ctx.next_block_id + 1;
	{
		cb_id = id;
		cb_el = DynArray.create ();
		cb_typepos = typepos;
		cb_next = {next_kind = NextUnknown; next_type = t_dynamic; next_pos = null_pos};
	}
