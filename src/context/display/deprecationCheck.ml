open Globals
open Type
open Common
open Ast

let curclass = ref null_class
let curfield = ref null_field

let warned_positions = Hashtbl.create 0

let warn_deprecation com s p_usage =
	let pkey p = (p.pfile,p.pmin) in
	if not (Hashtbl.mem warned_positions (pkey p_usage)) then begin
		Hashtbl.add warned_positions (pkey p_usage) (s,p_usage);
		if com.diagnostics = None then begin
			let options = Warning.from_meta (!curclass.cl_meta @ !curfield.cf_meta) in
			com.warning WDeprecated options s p_usage;
		end
	end

let print_deprecation_message com meta s p_usage =
	let s = match meta with
		| _,[EConst(String(s,_)),_],_ -> s
		| _ -> Printf.sprintf "Usage of this %s is deprecated" s
	in
	warn_deprecation com s p_usage

let check_meta com meta s p_usage =
	try
		print_deprecation_message com (Meta.get Meta.Deprecated meta) s p_usage;
	with Not_found ->
		()

let check_cf com cf p = check_meta com cf.cf_meta "field" p

let check_class com c p = if c != !curclass then check_meta com c.cl_meta "class" p

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

let run_on_field com cf =
	match cf.cf_expr with
	| None ->
		()
	| Some e ->
		curfield := cf;
		run_on_expr com e;
		curfield := null_field

let run com =
	List.iter (fun t -> match t with
		| TClassDecl c ->
			curclass := c;
			(match c.cl_constructor with None -> () | Some cf -> run_on_field com cf);
			(match c.cl_init with None -> () | Some e -> run_on_expr com e);
			List.iter (run_on_field com) c.cl_ordered_statics;
			List.iter (run_on_field com) c.cl_ordered_fields;
			curclass := null_class;
		| _ ->
			()
	) com.types

let if_enabled ?(force=false) com fn =
	if force || not (defined com Define.NoDeprecationWarnings) then fn()

let warn_deprecation ?(force=false) com s p_usage = if_enabled ~force com (fun() -> warn_deprecation com s p_usage)

let print_deprecation_message ?(force=false) com meta s p_usage = if_enabled ~force com (fun() -> print_deprecation_message com meta s p_usage)

let check_meta ?(force=false) com meta s p_usage = if_enabled ~force com (fun() -> check_meta com meta s p_usage)

let check_cf ?(force=false) com cf p = if_enabled ~force com (fun() -> check_cf com cf p)

let check_class ?(force=false) com c p = if_enabled ~force com (fun() -> check_class com c p)

let check_enum ?(force=false) com en p = if_enabled ~force com (fun() -> check_enum com en p)

let check_ef ?(force=false) com ef p = if_enabled ~force com (fun() -> check_ef com ef p)

let check_typedef ?(force=false) com t p = if_enabled ~force com (fun() -> check_typedef com t p)

let check_module_type ?(force=false) com mt p = if_enabled ~force com (fun() -> check_module_type com mt p)

let run_on_expr ?(force=false) com e = if_enabled ~force com (fun() -> run_on_expr com e)

let run_on_field ?(force=false) com cf = if_enabled ~force com (fun() -> run_on_field com cf)

let run ?(force=false) com = if_enabled ~force com (fun() -> run com)

let check_is com name meta p =
	()
	(* if name = "is" && not (Meta.has Meta.Deprecated meta) then
		warn_deprecation com "Using \"is\" as an identifier is deprecated" p *)