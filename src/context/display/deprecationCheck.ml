open Globals
open Type
open Common
open Ast

type deprecation_context = {
	com        : Common.context;
	class_meta : metadata_entry list;
	field_meta : metadata_entry list;
	curmod     : module_def;
}

let create_context com = {
	com = com;
	class_meta = [];
	field_meta = [];
	curmod = null_module;
}

let warn_deprecation dctx s p_usage =
	let pkey p = (p.pfile,p.pmin) in
	if not (Hashtbl.mem dctx.com.part_scope.warned_positions (pkey p_usage)) then begin
		let options = Warning.from_meta (dctx.class_meta @ dctx.field_meta) in
		Hashtbl.add dctx.com.part_scope.warned_positions (pkey p_usage) (s,p_usage,options);
		module_warning dctx.com dctx.curmod WDeprecated options s p_usage;
	end

let print_deprecation_message dctx meta s p_usage =
	let s = match meta with
		| _,(EConst(String(s,_)),_) :: _,_ -> s
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

let rec is_atomic e = match e.eexpr with
	| TConst _ | TLocal _ | TField _ | TTypeExpr _ | TCall _ | TArray _ | TParenthesis _ | TArrayDecl _ | TObjectDecl _ | TNew _ -> true
	| TMeta(_,e1) | TCast(e1,None) -> is_atomic e1
	| _ -> false

exception Not_fixable

let replacement_template cf = match Meta.get Meta.Deprecated cf.cf_meta with
	| _,[_;template],_ -> Some (template,None)
	| _,[_;template;(EConst(String(since,_)),_)],_ -> Some (template,Some since)
	| _ | exception Not_found -> None

let add_fix_from_template dctx cf (template,since) e_this args p =
	let in_range ep = ep <> p && ep.pfile = p.pfile && ep.pmin >= p.pmin && ep.pmax <= p.pmax in
	let text_of e () =
		if not (in_range e.epos) then raise Not_fixable;
		match SourceText.read e.epos with
		| Some s -> s,is_atomic e
		| None -> raise Not_fixable
	in
	let this_text () = match e_this with
		| Some { eexpr = TConst TThis } -> "this",true
		| Some e -> text_of e ()
		| None -> raise Not_fixable
	in
	let omitted = List.filter_map (fun (n,eo) -> match eo with
		| Some e when e.epos <> p -> None
		| _ -> Some n
	) args in
	let bindings = ("this",this_text) :: List.map (fun (n,eo) -> n,match eo with Some e when e.epos <> p -> text_of e | _ -> (fun () -> raise Not_fixable)) args in
	let rec drop_omitted el = match List.rev el with
		| (EConst (Ident n),_) :: el when List.mem n omitted -> drop_omitted (List.rev el)
		| _ -> el
	in
	let rec subst wrap e = match fst e with
		| EConst (Ident n) when List.mem_assoc n bindings ->
			let s,atomic = (List.assoc n bindings) () in
			(EConst (Ident (if wrap && not atomic then "(" ^ s ^ ")" else s)),snd e)
		| ECall(e1,el) ->
			(ECall(subst true e1,List.map (subst false) (drop_omitted el)),snd e)
		| EArrayDecl el ->
			(EArrayDecl (List.map (subst false) el),snd e)
		| EParenthesis e1 ->
			(EParenthesis (subst false e1),snd e)
		| _ ->
			Ast.map_expr (subst true) e
	in
	try
		let text = Ast.Printer.s_expr (subst false template) in
		let options = Warning.from_meta (dctx.class_meta @ dctx.field_meta) in
		add_fix_since dctx.com WDeprecated options ("Replace deprecated " ^ cf.cf_name) p text since
	with Not_fixable ->
		()

let add_replacement_fix dctx cf e_this el p_call =
	match replacement_template cf,follow cf.cf_type with
	| Some template,TFun(args,_) ->
		let rec bind names el = match names,el with
			| (n,_,_) :: names,e :: el -> (n,Some e) :: bind names el
			| (n,_,_) :: names,[] -> (n,None) :: bind names []
			| [],_ -> []
		in
		add_fix_from_template dctx cf template e_this (bind args el) p_call
	| _ ->
		()

let add_access_fix dctx cf e_this p =
	match replacement_template cf,cf.cf_kind with
	| Some template,Var _ -> add_fix_from_template dctx cf template e_this [] p
	| _ -> ()

let run_on_expr com e =
	let rec expr e = match e.eexpr with
		| TCall({eexpr = TField(e1,fa)} as ef,el) ->
			expr ef;
			List.iter expr el;
			begin match fa with
				| FStatic(_,cf) -> add_replacement_fix com cf None el e.epos
				| FInstance(_,_,cf) -> add_replacement_fix com cf (Some e1) el e.epos
				| _ -> ()
			end
		| TField(e1,fa) ->
			expr e1;
			begin match fa with
				| FStatic(c,cf) ->
					check_class com c e.epos;
					check_cf com cf e.epos;
					add_access_fix com cf None e.epos
				| FInstance(c,_,cf) ->
					check_class com c e.epos;
					check_cf com cf e.epos;
					add_access_fix com cf (Some e1) e.epos
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

let run com types =
	let dctx = create_context com in
	List.iter (fun t -> match t with
		| TClassDecl c when not (Meta.has Meta.Deprecated c.cl_meta) ->
			let dctx = {dctx with class_meta = c.cl_meta; curmod = c.cl_module} in
			(match c.cl_constructor with None -> () | Some cf -> run_on_field dctx cf);
			(match TClass.get_cl_init c with None -> () | Some e -> run_on_expr dctx e);
			List.iter (run_on_field dctx) c.cl_ordered_statics;
			List.iter (run_on_field dctx) c.cl_ordered_fields;
		| _ ->
			()
	) types

let check_is com m cl_meta cf_meta name meta p =
	let dctx = {
		com = com;
		class_meta = cl_meta;
		field_meta = cf_meta;
		curmod = m;
	} in
	if is_next dctx.com && name = "is" && not (Meta.has Meta.Deprecated meta) then
		warn_deprecation dctx "Using \"is\" as an identifier is deprecated" p