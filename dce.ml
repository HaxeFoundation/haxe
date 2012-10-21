(*
 * Haxe DCE:
 * With this new approach the typer is almost not aware of DCE at all. It instead types what
 * it needs to type (and usually some more) and DCE then takes care of cleaning up. It does
 * so by following the typed AST expressions and mark accessed classes and fields as used.
 *
 * The algorithm works as follows:
 * 1. Find all entry point class fields:
 *	- the main method if exists
 *	- methods marked as @:keep
 *	- methods of classes marked as @:keep
 *
 * 2. Mark implementing/overriding fields of these entry points as @:?used.
 *
 * 3. Mark entry points as @:used.
 *
 * 4. Follow the field expressions (if exists) and see what other classes/fields are added,
 *    e.g. by a TField or TNew AST node.
 *
 * 5. If new fields were added, go back to 2 with the new fields as entry points.
 *
 * 6. Filter the types by keeping those that are used explicitly or have a used field.
 *
 * Notes:
 *  - the only influence of the typer is @:?used marking on structural subtyping
 *  - properties are currently tricky to handle on some targets
 *  - cpp target does not like removing unused overridden fields
 *  - most targets seem to require keeping a property field even if it is used only through its accessor methods
 *  - I did not consider inlining at all because I'm pretty sure I don't have to at this compilation stage
 * 
 *)

open Ast
open Common
open Type

type dce = {
	com : context;
	debug : bool;
	follow_expr : dce -> texpr -> unit;
	mutable added_fields : (tclass * tclass_field * bool) list;
	mutable marked_fields : tclass_field list;
	mutable marked_maybe_fields : tclass_field list;
}

(* checking *)

(* check for @:keepSub metadata, which forces @:keep on child classes *)
let rec super_forces_keep c =
	has_meta ":keepSub" c.cl_meta || match c.cl_super with
	| Some (csup,_) -> super_forces_keep csup
	| _ -> false

(* check if a class is kept entirely *)
let keep_whole_class dce c =
	has_meta ":keep" c.cl_meta
	|| super_forces_keep c
	|| (match c with
		| { cl_extern = true; cl_path = ([],"Math")} when dce.com.platform = Js -> false
		| { cl_extern = true }
		| { cl_path = ["flash";"_Boot"],"RealBoot" } -> true
		| { cl_path = [],"String" }
		| { cl_path = [],"Array" } -> not (dce.com.platform = Js)
		| _ -> false)

(* check if a field is kept *)
let keep_field dce cf =
	has_meta ":keep" cf.cf_meta
	|| has_meta ":used" cf.cf_meta
	|| cf.cf_name = "__init__"
	|| dce.com.platform = Js && (try (match get_meta ":feature" cf.cf_meta with
			| (_,[EConst(String s),_],_) -> Common.has_feature dce.com s
			| _ -> raise Not_found)
		with Not_found -> false)

(* marking *)

(* mark a field as kept *)
let mark_field dce c cf stat = if not (has_meta ":used" cf.cf_meta) then begin
	cf.cf_meta <- (":used",[],cf.cf_pos) :: cf.cf_meta;
	dce.added_fields <- (c,cf,stat) :: dce.added_fields;
	dce.marked_fields <- cf :: dce.marked_fields
end

let rec update_marked_class_fields dce c =
	(* mark all :?used fields as surely :used now *)
	List.iter (fun cf ->
		if has_meta ":?used" cf.cf_meta then mark_field dce c cf true
	) c.cl_ordered_statics;
	List.iter (fun cf ->
		if has_meta ":?used" cf.cf_meta then mark_field dce c cf false
	) c.cl_ordered_fields;
	(* we always have to keep super classes and implemented interfaces *)
	(match c.cl_init with None -> () | Some init -> dce.follow_expr dce init);
	List.iter (fun (c,_) -> mark_class dce c) c.cl_implements;
	(match c.cl_super with None -> () | Some (csup,pl) -> mark_class dce csup)

(* mark a class as kept. If the class has fields marked as @:?keep, make sure to keep them *)
and mark_class dce c = if not (has_meta ":used" c.cl_meta) then begin
	c.cl_meta <- (":used",[],c.cl_pos) :: c.cl_meta;
	update_marked_class_fields dce c;
end

(* mark a type as kept *)
let rec mark_t dce t = match follow t with
	| TInst({cl_kind = KTypeParameter tl},pl) -> List.iter (mark_t dce) tl; List.iter (mark_t dce) pl
	| TInst(c,pl) -> mark_class dce c; List.iter (mark_t dce) pl
	| TFun(args,ret) -> List.iter (fun (_,_,t) -> mark_t dce t) args; mark_t dce ret
	| TEnum(e,pl) -> if not (has_meta ":used" e.e_meta) then e.e_meta <- (":used",[],e.e_pos) :: e.e_meta; List.iter (mark_t dce) pl
	| TAbstract(a,pl) -> if not (has_meta ":used" a.a_meta) then a.a_meta <- (":used",[],a.a_pos) :: a.a_meta; List.iter (mark_t dce) pl
	| TLazy _ | TDynamic _ | TAnon _ | TType _ | TMono _ -> ()

(* find all dependent fields by checking implementing/subclassing types *)
let rec mark_dependent_fields dce csup n stat =
	List.iter (fun mt -> match mt with
		| TClassDecl c when is_parent csup c ->
			let rec loop c =
				(try
					let cf = PMap.find n (if stat then c.cl_statics else c.cl_fields) in
					(* if it's clear that the class is kept, the field has to be kept as well. This is also true for
					   extern interfaces because we cannot remove fields from them *)
					if has_meta ":used" c.cl_meta || (csup.cl_interface && csup.cl_extern) then mark_field dce c cf stat
					(* otherwise it might be kept if the class is kept later, so mark it as :?used *)
					else if not (has_meta ":?used" cf.cf_meta) then begin
						cf.cf_meta <- (":?used",[],cf.cf_pos) :: cf.cf_meta;
						dce.marked_maybe_fields <- cf :: dce.marked_maybe_fields;
					end
				with Not_found ->
					(* if the field is not present on current class, it might come from a base class *)
					(match c.cl_super with None -> () | Some (csup,_) -> loop csup))
			in
			loop c
		| _ -> ()
	) dce.com.types

(* expr and field evaluation *)

let opt f e = match e with None -> () | Some e -> f e

let rec field dce c n stat =
	let find_field n =
		if n = "new" then match c.cl_constructor with
			| None -> raise Not_found
			| Some cf -> cf
		else PMap.find n (if stat then c.cl_statics else c.cl_fields)
	in
	(try
		let cf = find_field n in
		mark_field dce c cf stat;
	with Not_found -> try
		(* me might have a property access on an interface *)
		let l = String.length n - 4 in
		if l < 0 then raise Not_found;
		let prefix = String.sub n 0 4 in
		let pn = String.sub n 4 l in
		let cf = find_field pn in
		if not (has_meta ":used" cf.cf_meta) then begin
			let keep () =
				mark_dependent_fields dce c n stat;
				field dce c pn stat
			in
			(match prefix,cf.cf_kind with
				| "get_",Var {v_read = AccCall s} when s = n -> keep()
				| "set_",Var {v_write = AccCall s} when s = n -> keep()	
				| _ -> raise Not_found
			);
		end;
		raise Not_found
	with Not_found ->
		match c.cl_super with Some (csup,_) -> field dce csup n stat | None -> ());

and expr dce e =
	match e.eexpr with
	| TNew(c,pl,el) ->
		mark_class dce c;
		let rec loop c =
			field dce c "new" false;
			match c.cl_super with None -> () | Some (csup,_) -> loop csup
		in
		loop c;
		List.iter (expr dce) el;
		List.iter (mark_t dce) pl;
	| TVars vl ->
		List.iter (fun (v,e) ->
			opt (expr dce) e;
			mark_t dce v.v_type;
		) vl;
	| TCast(e, Some (TClassDecl c)) ->
		mark_class dce c;
		expr dce e;
	| TTry(e, vl) ->
		expr dce e;
		List.iter (fun (v,e) ->
			expr dce e;
			mark_t dce v.v_type;
		) vl;
	| TTypeExpr (TClassDecl c) ->
		mark_class dce c;
	| TTypeExpr (TAbstractDecl a) ->
		mark_t dce (TAbstract (a,[]))
	| TCast(e, Some (TEnumDecl en)) ->
		mark_t dce (TEnum(en,[]));
		expr dce e;
	| TTypeExpr (TEnumDecl e)
	| TEnumField(e,_) ->
		mark_t dce (TEnum(e,[]));
	| TCall ({eexpr = TLocal ({v_name = "__define_feature__"})},[{eexpr = TConst (TString ft)};e]) ->
		Common.add_feature dce.com ft;
		expr dce e
	(* keep toString method when the class is argument to Std.string or haxe.Log.trace *)
	| TCall ({eexpr = TField({eexpr = TTypeExpr (TClassDecl ({cl_path = (["haxe"],"Log")} as c))},"trace")} as ef, ([e2;_] as args))
	| TCall ({eexpr = TField({eexpr = TTypeExpr (TClassDecl ({cl_path = ([],"Std")} as c))},"string")} as ef, ([e2] as args)) ->
		mark_class dce c;
		(match follow e2.etype with
			| TInst(c,_) ->	field dce c "toString" false
			| _ -> ());
		expr dce ef;
		List.iter (expr dce) args;
	| TCall ({eexpr = TConst TSuper} as e,el) ->
		mark_t dce e.etype;
		List.iter (expr dce) el;
	| TClosure(e,n)
	| TField(e,n) ->
		(match follow e.etype with
		| TInst(c,_) ->
			mark_class dce c;
			field dce c n false;
		| TAnon a ->
			(match !(a.a_status) with
			| Statics c ->
				mark_class dce c;
				field dce c n true;
			| _ -> ())
		| _ -> ());
		expr dce e;
	| _ -> Type.iter (expr dce) e

let run com main =
	let dce = {
		com = com;
		debug = Common.defined com "dce_debug";
		added_fields = [];
		follow_expr = expr;
		marked_fields = [];
		marked_maybe_fields = [];
	} in
	(* first step: get all entry points, which is the main method and all class methods which are marked with @:keep *)
	let rec loop acc types = match types with
		| (TClassDecl c) :: l ->
			let keep_class = keep_whole_class dce c in
			if keep_class then (if c.cl_extern then update_marked_class_fields dce c else mark_class dce c);
			(* extern classes should never serve as entry point *)
			let keep_class = keep_class && (not c.cl_extern || c.cl_interface) in
			let rec loop2 acc cfl stat = match cfl with
				| cf :: l when keep_class || keep_field dce cf ->
					loop2 ((c,cf,stat) :: acc) l stat
				| cf :: l ->
					loop2 acc l stat
				| [] ->
					acc
			in
			let acc = loop2 acc c.cl_ordered_statics true in
			let acc = loop2 acc c.cl_ordered_fields false in
			loop acc l
		| _ :: l ->
			loop acc l
		| [] ->
			acc
	in
	let entry_points = match main with
		| Some {eexpr = TCall({eexpr = TField(e,_)},_)} ->
			(match follow e.etype with
			| TAnon a ->
				(match !(a.a_status) with
				| Statics c ->
					let cf = PMap.find "main" c.cl_statics in
					loop [c,cf,true] com.types
				| _ -> assert false)
			| _ -> assert false)
		| _ -> loop [] com.types
	in	
	if dce.debug then begin
		List.iter (fun (c,cf,_) -> match cf.cf_expr with
			| None -> ()
			| Some _ -> print_endline ("[DCE] Entry point: " ^ (s_type_path c.cl_path) ^ "." ^ cf.cf_name)
		) entry_points;
	end;

	(* second step: initiate DCE passes and keep going until no new fields were added *)
	let rec loop cfl =
		(* extend to dependent (= overriding/implementing) class fields *)	
		List.iter (fun (c,cf,stat) -> mark_dependent_fields dce c cf.cf_name stat) cfl;
		(* mark fields as used *)
		List.iter (fun (c,cf,stat) -> mark_field dce c cf stat; mark_t dce cf.cf_type) cfl;
		(* follow expressions to new types/fields *)
		List.iter (fun (_,cf,_) -> opt (expr dce) cf.cf_expr) cfl;		
		match dce.added_fields with
		| [] -> ()
		| cfl ->
			dce.added_fields <- [];
			loop cfl
	in
	loop entry_points;

	(* third step: filter types *)
	let rec loop acc types =
		match types with
		| (TClassDecl c) as mt :: l when keep_whole_class dce c ->
			loop (mt :: acc) l
		| (TClassDecl c) as mt :: l ->
			(* add :keep so subsequent filter calls do not process class fields again *)
			c.cl_meta <- (":keep",[],c.cl_pos) :: c.cl_meta;
 			c.cl_ordered_statics <- List.filter (fun cf ->
				let b = keep_field dce cf in
				if not b then begin
					if dce.debug then print_endline ("[DCE] Removed field " ^ (s_type_path c.cl_path) ^ "." ^ (cf.cf_name));
					c.cl_statics <- PMap.remove cf.cf_name c.cl_statics;
				end;
				b
			) c.cl_ordered_statics;
			c.cl_ordered_fields <- List.filter (fun cf ->
				let b = keep_field dce cf in
				if not b then begin
					if dce.debug then print_endline ("[DCE] Removed field " ^ (s_type_path c.cl_path) ^ "." ^ (cf.cf_name));
					c.cl_fields <- PMap.remove cf.cf_name c.cl_fields;
				end;
				b
			) c.cl_ordered_fields;
			if c.cl_path <> ([],"EReg") then
				(match c.cl_constructor with Some cf when not (keep_field dce cf) -> c.cl_constructor <- None | _ -> ());
			(* we keep a class if it was used or has a used field *)
			if has_meta ":used" c.cl_meta || c.cl_ordered_statics <> [] || c.cl_ordered_fields <> [] then loop (mt :: acc) l else begin
				if dce.debug then print_endline ("[DCE] Removed class " ^ (s_type_path c.cl_path));
				loop acc l
			end
 		| (TEnumDecl e) as mt :: l when has_meta ":used" e.e_meta || has_meta ":keep" e.e_meta || e.e_extern ->
			loop (mt :: acc) l
		| TEnumDecl _ :: l ->
			loop acc l
		| mt :: l ->
			loop (mt :: acc) l
		| [] ->
			acc
	in
	com.types <- loop [] (List.rev com.types);

	(* extra step to adjust properties that had accessors removed (required for Php and Cpp) *)
	List.iter (fun mt -> match mt with
		| (TClassDecl c) ->
			let rec has_accessor c n stat =
				PMap.mem n (if stat then c.cl_statics else c.cl_fields)
				|| match c.cl_super with Some (csup,_) -> has_accessor csup n stat | None -> false
			in
			let check_prop stat cf =
				(match cf.cf_kind with
				| Var {v_read = AccCall s; v_write = a} ->
					cf.cf_kind <- Var {v_read = if has_accessor c s stat then AccCall s else AccNever; v_write = a}
				| _ -> ());
				(match cf.cf_kind with
				| Var {v_write = AccCall s; v_read = a} ->
					cf.cf_kind <- Var {v_write = if has_accessor c s stat then AccCall s else AccNever; v_read = a}
				| _ -> ())
			in		
			List.iter (check_prop true) c.cl_ordered_statics;
			List.iter (check_prop false) c.cl_ordered_fields;
		| _ -> ()
	) com.types;

	(* remove "override" from fields that do not override anything anymore *)
	List.iter (fun mt -> match mt with
		| TClassDecl c ->
			c.cl_overrides <- List.filter (fun s ->
				let rec loop c =
					match c.cl_super with
					| Some (csup,_) when PMap.mem s csup.cl_fields -> true
					| Some (csup,_) -> loop csup
					| None -> false
				in
				loop c
			) c.cl_overrides;
		| _ -> ()
	) com.types;

	(* cleanup added fields metadata - compatibility with compilation server *)
	let rec remove_meta m = function
		| [] -> []
		| (m2,_,_) :: l when m = m2 -> l
		| x :: l -> x :: remove_meta m l
	in
	List.iter (fun cf -> cf.cf_meta <- remove_meta ":used" cf.cf_meta) dce.marked_fields;
	List.iter (fun cf -> cf.cf_meta <- remove_meta ":?used" cf.cf_meta) dce.marked_maybe_fields;


