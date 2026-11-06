open Globals
open Type
open Common
open Ast

type deprecation_context = {
	com        : Common.context;
	class_meta : metadata_entry list;
	field_meta : metadata_entry list;
	curmod     : module_def;
	is_renames : (string * int,string * string list) Hashtbl.t;
}

let create_context com = {
	com = com;
	class_meta = [];
	field_meta = [];
	curmod = null_module;
	is_renames = Hashtbl.create 0;
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

let rec is_atomic e = match e.eexpr with
	| TConst _ | TLocal _ | TField _ | TTypeExpr _ | TCall _ | TArray _ | TParenthesis _ | TArrayDecl _ | TObjectDecl _ | TNew _ -> true
	| TMeta(_,e1) | TCast(e1,None) -> is_atomic e1
	| _ -> false

exception Not_fixable

let replacement_templates cf =
	if not (Meta.has Meta.Deprecated cf.cf_meta) then []
	else List.filter_map (function
		| Meta.DeprecatedReplace,[template],_ -> Some (template,None)
		| Meta.DeprecatedReplace,[template;(EConst(String(since,_)),_)],_ -> Some (template,Some since)
		| _ -> None
	) cf.cf_meta

let rec is_pure e = match e.eexpr with
	| TConst _ | TLocal _ | TTypeExpr _ | TFunction _ -> true
	| TParenthesis e1 | TMeta(_,e1) | TCast(e1,None) -> is_pure e1
	| TField(e1,(FStatic _ | FInstance _ | FAnon _ | FClosure _ | FEnum _)) -> is_pure e1
	| _ -> false

let choice_of_template (template,since) e_this args p =
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
	let given = List.filter_map (fun (n,eo) -> match eo with
		| Some e when e.epos <> p -> Some (n,e)
		| _ -> None
	) args in
	let given = match e_this with Some e -> ("this",e) :: given | None -> given in
	let bindings = ("this",this_text) :: List.map (fun (n,eo) -> n,match eo with Some e when e.epos <> p -> text_of e | _ -> (fun () -> raise Not_fixable)) args in
	let uses = Hashtbl.create 0 in
	let rec drop_omitted el = match List.rev el with
		| (EConst (Ident n),_) :: el when List.mem n omitted -> drop_omitted (List.rev el)
		| _ -> el
	in
	let rec subst wrap e = match fst e with
		| EConst (Ident n) when List.mem_assoc n bindings ->
			Hashtbl.replace uses n (1 + try Hashtbl.find uses n with Not_found -> 0);
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
		(* Duplicating or dropping an argument changes how often its side effects run. *)
		let safe = List.for_all (fun (n,e) -> is_pure e || (try Hashtbl.find uses n with Not_found -> 0) = 1) given in
		Some (choice ~safe ?since text)
	with Not_fixable ->
		None

let add_fix_from_templates dctx cf templates e_this args p =
	match List.filter_map (fun template -> choice_of_template template e_this args p) templates with
	| [] ->
		()
	| choices ->
		let options = Warning.from_meta (dctx.class_meta @ dctx.field_meta) in
		add_fix_choices dctx.com dctx.curmod WDeprecated options ("Replace deprecated " ^ cf.cf_name) p choices

let add_replacement_fix dctx cf e_this el p_call =
	match replacement_templates cf,follow cf.cf_type with
	| (_ :: _) as templates,TFun(args,_) ->
		let rec bind names el = match names,el with
			| (n,_,_) :: names,e :: el -> (n,Some e) :: bind names el
			| (n,_,_) :: names,[] -> (n,None) :: bind names []
			| [],_ -> []
		in
		add_fix_from_templates dctx cf templates e_this (bind args el) p_call
	| _ ->
		()

let add_access_fix dctx cf e_this p =
	match replacement_templates cf,cf.cf_kind with
	| (_ :: _) as templates,Var _ -> add_fix_from_templates dctx cf templates e_this [] p
	| _ -> ()

let name_key cf = (cf.cf_name_pos.pfile,cf.cf_name_pos.pmin)

let add_is_access_fix dctx cf p =
	match Hashtbl.find_opt dctx.is_renames (name_key cf) with
	| Some (name,requires) when p.pmax - p.pmin >= 2 ->
		let p = { p with pmin = p.pmax - 2 } in
		let before = { p with pmin = p.pmin - 1; pmax = p.pmin } in
		let separated = match SourceText.read before with
			| Some c -> not (SourceText.is_ident_char c.[0])
			| None -> true
		in
		if separated && SourceText.read p = Some "is" then begin
			let options = Warning.from_meta (dctx.class_meta @ dctx.field_meta) in
			add_fix dctx.com dctx.curmod ~safe:false ~requires WDeprecated options ("Rename to " ^ name) p name
		end
	| _ ->
		()

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
				| FStatic(_,cf) | FInstance(_,_,cf) | FClosure(_,cf) when cf.cf_name = "is" ->
					add_is_access_fix com cf e.epos
				| _ ->
					()
			end;
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

let rename_is_locals dctx c e =
	let vars = Hashtbl.create 0 in
	let names = Hashtbl.create 0 in
	let add_var v =
		Hashtbl.replace names v.v_name ();
		match v.v_kind with
		| VUser _ when v.v_name = "is" -> Hashtbl.replace vars v.v_id v
		| _ -> ()
	in
	let rec collect e =
		begin match e.eexpr with
			| TVar(v,_) -> add_var v
			| TFunction tf -> List.iter (fun (v,_) -> add_var v) tf.tf_args
			| _ -> ()
		end;
		Type.iter collect e
	in
	collect e;
	if Hashtbl.length vars > 0 then begin
		let taken s = Hashtbl.mem names s || PMap.mem s c.cl_fields || PMap.mem s c.cl_statics in
		let rec fresh n =
			let s = if n = 1 then "isValue" else "isValue" ^ string_of_int n in
			if taken s then fresh (n + 1) else s
		in
		let name = fresh 1 in
		let options = Warning.from_meta (dctx.class_meta @ dctx.field_meta) in
		let fix p =
			if SourceText.read p = Some "is" then add_fix dctx.com dctx.curmod WDeprecated options ("Rename to " ^ name) p name
		in
		Hashtbl.iter (fun _ v -> fix v.v_pos) vars;
		let rec uses e =
			begin match e.eexpr with
				| TLocal v when Hashtbl.mem vars v.v_id -> fix e.epos
				| _ -> ()
			end;
			Type.iter uses e
		in
		uses e
	end

let run_on_field dctx c cf =
	match cf.cf_expr with
	| Some e when not (Meta.has Meta.Deprecated cf.cf_meta) ->
		let dctx = {dctx with field_meta = cf.cf_meta} in
		run_on_expr dctx e;
		if is_next dctx.com then rename_is_locals dctx c e
	| _ ->
		()

let rename_is_field dctx cf =
	match Hashtbl.find_opt dctx.is_renames (name_key cf) with
	| Some (name,requires) ->
		let options = Warning.from_meta (dctx.class_meta @ cf.cf_meta) in
		add_fix dctx.com dctx.curmod ~safe:false ~requires WDeprecated options ("Rename to " ^ name) cf.cf_name_pos name
	| None ->
		()

(* Overriding and implementing fields are renamed together, and only if all of their files are in the fix scope. *)
let collect_is_renames com renames =
	let fields = Hashtbl.create 0 in
	let names = Hashtbl.create 0 in
	let parent = Hashtbl.create 0 in
	let linked = Hashtbl.create 0 in
	let rec find k = match Hashtbl.find_opt parent k with
		| Some k' when k' <> k -> find k'
		| _ -> k
	in
	let union k1 k2 = let r1 = find k1 and r2 = find k2 in if r1 <> r2 then Hashtbl.replace parent r1 r2 in
	let rec inherited c = match c.cl_super with
		| Some (cs,_) -> (try [PMap.find "is" cs.cl_fields] with Not_found -> inherited cs)
		| None -> []
	and implemented c = List.concat_map (fun (ci,_) ->
		(try [PMap.find "is" ci.cl_fields] with Not_found -> []) @ implemented ci
	) c.cl_implements in
	List.iter (function
		| TClassDecl c ->
			let add stat cf =
				Hashtbl.replace names cf.cf_name ();
				if cf.cf_name = "is" then begin
					let k = name_key cf in
					let renamable = not (has_class_flag c CExtern) && not (has_class_field_flag cf CfExtern)
						&& not (Meta.has Meta.Native cf.cf_meta) && SourceText.read cf.cf_name_pos = Some "is" in
					Hashtbl.replace fields k (cf.cf_name_pos.pfile,renamable);
					if not stat then List.iter (fun cf' ->
						let k' = name_key cf' in
						Hashtbl.replace linked k' ();
						union k k'
					) (inherited c @ implemented c)
				end
			in
			List.iter (add false) c.cl_ordered_fields;
			List.iter (add true) c.cl_ordered_statics
		| _ ->
			()
	) com.types;
	let rec fresh n =
		let s = if n = 1 then "isValue" else "isValue" ^ string_of_int n in
		if Hashtbl.mem names s then fresh (n + 1) else s
	in
	let name = fresh 1 in
	Hashtbl.iter (fun k () -> if not (Hashtbl.mem fields k) then Hashtbl.replace fields k ("",false)) linked;
	let groups = Hashtbl.create 0 in
	Hashtbl.iter (fun k info ->
		let r = find k in
		Hashtbl.replace groups r ((k,info) :: (try Hashtbl.find groups r with Not_found -> []))
	) fields;
	Hashtbl.iter (fun _ members ->
		if List.for_all (fun (_,(_,renamable)) -> renamable) members then begin
			let files = List.sort_uniq compare (List.map (fun (_,(file,_)) -> file) members) in
			List.iter (fun (k,_) -> Hashtbl.replace renames k (name,files)) members
		end
	) groups

let run com types =
	let dctx = create_context com in
	if is_next com then collect_is_renames com dctx.is_renames;
	List.iter (fun t -> match t with
		| TClassDecl c when not (Meta.has Meta.Deprecated c.cl_meta) ->
			let dctx = {dctx with class_meta = c.cl_meta; curmod = c.cl_module} in
			(match c.cl_constructor with None -> () | Some cf -> run_on_field dctx c cf);
			(match TClass.get_cl_init c with None -> () | Some e -> run_on_expr dctx e);
			List.iter (run_on_field dctx c) c.cl_ordered_statics;
			List.iter (run_on_field dctx c) c.cl_ordered_fields;
			List.iter (rename_is_field dctx) c.cl_ordered_statics;
			List.iter (rename_is_field dctx) c.cl_ordered_fields;
		| _ ->
			()
	) types

let check_is com m cl_meta cf_meta name meta p =
	let dctx = {
		com = com;
		class_meta = cl_meta;
		field_meta = cf_meta;
		curmod = m;
		is_renames = Hashtbl.create 0;
	} in
	if is_next dctx.com && name = "is" && not (Meta.has Meta.Deprecated meta) then
		warn_deprecation dctx "Using \"is\" as an identifier is deprecated" p