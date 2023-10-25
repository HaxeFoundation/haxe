open Globals
open Type
open Common
open Ast

type deprecation_context = {
	com        : Common.context;
	class_meta : metadata_entry list;
	field_meta : metadata_entry list;
}

let create_context com = {
	com = com;
	class_meta = [];
	field_meta = [];
}

let warned_positions = Hashtbl.create 0

let warn_deprecation dctx s p_usage =
	let pkey p = (p.pfile,p.pmin) in
	if not (Hashtbl.mem warned_positions (pkey p_usage)) then begin
		Hashtbl.add warned_positions (pkey p_usage) (s,p_usage);
		if not (is_diagnostics dctx.com) then begin
			let options = Warning.from_meta (dctx.class_meta @ dctx.field_meta) in
			dctx.com.warning WDeprecated options s p_usage;
		end
	end

let print_deprecation_message dctx meta s p_usage =
	let s = match meta with
		| _,[EConst(String(s,_)),_],_ -> s
		| _ -> Printf.sprintf "Usage of this %s is deprecated" s
	in
	warn_deprecation dctx s p_usage

let check_meta dctx meta s p_usage =
	try
		print_deprecation_message dctx (Meta.get Meta.Deprecated meta) s p_usage;
	with Not_found ->
		()

let check_cf com cf p = check_meta com cf.cf_meta "field" p

let check_class dctx c p = check_meta dctx c.cl_meta "class" p

let check_enum com en p = check_meta com en.e_meta "enum" p

let check_ef com ef p = check_meta com ef.ef_meta "enum field" p

let check_typedef com t p = check_meta com t.t_meta "typedef" p

let check_module_type com mt p = match mt with
	| TClassDecl c -> check_class com c p
	| TEnumDecl en -> check_enum com en p
	| _ -> ()

let run_on_expr com e =
	let rec expr e = match e.eexpr with
		| TField(e1,fa) ->
			expr e1;
			begin match fa with
				| FStatic(c,cf) | FInstance(c,_,cf) ->
					check_class com c e.epos;
					check_cf com cf e.epos
				| FAnon cf ->
					check_cf com cf e.epos
				| FClosure(co,cf) ->
					(match co with None -> () | Some (c,_) -> check_class com c e.epos);
					check_cf com cf e.epos
				| FEnum(en,ef) ->
					check_enum com en e.epos;
					check_ef com ef e.epos;
				| _ ->
					()
			end
		| TNew(c,_,el) ->
			List.iter expr el;
			check_class com c e.epos;
			begin match c.cl_constructor with
				(* The AST doesn't carry the correct overload for TNew, so let's ignore this case... (#8557). *)
				| Some cf when cf.cf_overloads = [] -> check_cf com cf e.epos
				| _ -> ()
			end
		| TTypeExpr(mt) | TCast(_,Some mt) ->
			check_module_type com mt e.epos
		| TMeta((Meta.Deprecated,_,_) as meta,e1) ->
			print_deprecation_message com meta "field" e1.epos;
			expr e1;
		| _ ->
			Type.iter expr e
	in
	expr e

let run_on_field dctx cf =
	match cf.cf_expr with
	| Some e when not (Meta.has Meta.Deprecated cf.cf_meta) ->
		run_on_expr {dctx with field_meta = cf.cf_meta} e
	| _ ->
		()

let run com =
	let dctx = create_context com in
	List.iter (fun t -> match t with
		| TClassDecl c when not (Meta.has Meta.Deprecated c.cl_meta) ->
			let dctx = {dctx with class_meta = c.cl_meta} in
			(match c.cl_constructor with None -> () | Some cf -> run_on_field dctx cf);
			(match c.cl_init with None -> () | Some e -> run_on_expr dctx e);
			List.iter (run_on_field dctx) c.cl_ordered_statics;
			List.iter (run_on_field dctx) c.cl_ordered_fields;
		| _ ->
			()
	) com.types

let check_is com cl_meta cf_meta name meta p =
	let dctx = {
		com = com;
		class_meta = cl_meta;
		field_meta = cf_meta;
	} in
	if is_next dctx.com && name = "is" && not (Meta.has Meta.Deprecated meta) then
		warn_deprecation dctx "Using \"is\" as an identifier is deprecated" p