
open Type

(* This record holds transient information about an (attempted) call on a field. It is created when resolving
   field calls and is passed to overload filters. *)
   type 'a field_call_candidate = {
	(* The argument expressions for this call and whether or not the argument is optional on the
	   target function. *)
	fc_args  : texpr list;
	(* The applied return type. *)
	fc_ret   : Type.t;
	(* The applied function type. *)
	fc_type  : Type.t;
	(* The class field being called. *)
	fc_field : tclass_field;
	(* The field monomorphs that were created for this call. *)
	fc_monos : Type.t list;
	(* The custom data associated with this call. *)
	fc_data  : 'a;
}

let make_field_call_candidate args ret monos t cf data = {
	fc_args  = args;
	fc_type  = t;
	fc_field = cf;
	fc_data  = data;
	fc_ret   = ret;
	fc_monos = monos;
}

let s_field_call_candidate fcc =
	let pctx = print_context() in
	let se = s_expr_pretty false "" false (s_type pctx) in
	let sl_args = List.map se fcc.fc_args in
	Printer.s_record_fields "" [
		"fc_args",String.concat ", " sl_args;
		"fc_type",s_type pctx fcc.fc_type;
		"fc_field",Printf.sprintf "%s: %s" fcc.fc_field.cf_name (s_type pctx fcc.fc_field.cf_type)
	]
