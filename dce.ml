(*
 * Copyright (C)2005-2013 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *)

open Ast
open Common
open Type

type dce = {
	com : context;
	full : bool;
	std_dirs : string list;
	debug : bool;
	follow_expr : dce -> texpr -> unit;
	mutable added_fields : (tclass * tclass_field * bool) list;
	mutable marked_fields : tclass_field list;
	mutable marked_maybe_fields : tclass_field list;
	mutable t_stack : t list;
}

(* checking *)

(* check for @:keepSub metadata, which forces @:keep on child classes *)
let rec super_forces_keep c =
	Meta.has Meta.KeepSub c.cl_meta || match c.cl_super with
	| Some (csup,_) -> super_forces_keep csup
	| _ -> false

let is_std_file dce file =
	List.exists (ExtString.String.starts_with file) dce.std_dirs

(* check if a class is kept entirely *)
let keep_whole_class dce c =
	Meta.has Meta.Keep c.cl_meta
	|| not (dce.full || is_std_file dce c.cl_module.m_extra.m_file)
	|| super_forces_keep c
	|| (match c with
		| { cl_extern = true; cl_path = ([],("Math"|"Array"))} when dce.com.platform = Js -> false
		| { cl_extern = true }
		| { cl_path = ["flash";"_Boot"],"RealBoot" } -> true
		| { cl_path = [],"String" }
		| { cl_path = [],"Array" } -> not (dce.com.platform = Js)
		| _ -> false)

(* check if a metadata contains @:ifFeature with a used feature argument *)
let has_used_feature com meta =
	try
		let _,el,_ = Meta.get Meta.IfFeature meta in
		List.exists (fun e -> match fst e with
			| EConst(String s) when Common.has_feature com s -> true
			| _ -> false
		) el
	with Not_found ->
		false

(* check if a field is kept *)
let keep_field dce cf =
	Meta.has Meta.Keep cf.cf_meta
	|| Meta.has Meta.Used cf.cf_meta
	|| cf.cf_name = "__init__"
	|| has_used_feature dce.com cf.cf_meta

(* marking *)

(* mark a field as kept *)
let rec mark_field dce c cf stat =
	let add () =
		if not (Meta.has Meta.Used cf.cf_meta) then begin
			cf.cf_meta <- (Meta.Used,[],cf.cf_pos) :: cf.cf_meta;
			dce.added_fields <- (c,cf,stat) :: dce.added_fields;
			dce.marked_fields <- cf :: dce.marked_fields
		end
	in
	if not (PMap.mem cf.cf_name (if stat then c.cl_statics else c.cl_fields)) then begin
		match c.cl_super with
		| None -> add()
		| Some (c,_) -> mark_field dce c cf stat
	end else
		add()

let rec update_marked_class_fields dce c =
	(* mark all :?used fields as surely :used now *)
	List.iter (fun cf ->
		if Meta.has Meta.MaybeUsed cf.cf_meta then mark_field dce c cf true
	) c.cl_ordered_statics;
	List.iter (fun cf ->
		if Meta.has Meta.MaybeUsed cf.cf_meta then mark_field dce c cf false
	) c.cl_ordered_fields;
	(* we always have to keep super classes and implemented interfaces *)
	(match c.cl_init with None -> () | Some init -> dce.follow_expr dce init);
	List.iter (fun (c,_) -> mark_class dce c) c.cl_implements;
	(match c.cl_super with None -> () | Some (csup,pl) -> mark_class dce csup)

(* mark a class as kept. If the class has fields marked as @:?keep, make sure to keep them *)
and mark_class dce c = if not (Meta.has Meta.Used c.cl_meta) then begin
	c.cl_meta <- (Meta.Used,[],c.cl_pos) :: c.cl_meta;
	update_marked_class_fields dce c;
end

let rec mark_enum dce e = if not (Meta.has Meta.Used e.e_meta) then begin
	e.e_meta <- (Meta.Used,[],e.e_pos) :: e.e_meta;
	PMap.iter (fun _ ef -> mark_t dce ef.ef_type) e.e_constrs;
end

and mark_abstract dce a = if not (Meta.has Meta.Used a.a_meta) then
	a.a_meta <- (Meta.Used,[],a.a_pos) :: a.a_meta

(* mark a type as kept *)
and mark_t dce t = match follow t with
	| TInst({cl_kind = KTypeParameter tl} as c,pl) ->
		if not (Meta.has Meta.Used c.cl_meta) then begin
			c.cl_meta <- (Meta.Used,[],c.cl_pos) :: c.cl_meta;
			List.iter (mark_t dce) tl;
		end;
		List.iter (mark_t dce) pl
	| TInst(c,pl) ->
		mark_class dce c;
		List.iter (mark_t dce) pl
	| TFun(args,ret) ->
		List.iter (fun (_,_,t) -> mark_t dce t) args;
		mark_t dce ret
	| TEnum(e,pl) ->
		mark_enum dce e;
		List.iter (mark_t dce) pl
	| TAbstract(a,pl) ->
		mark_abstract dce a;
		List.iter (mark_t dce) pl
	| TLazy _ | TDynamic _ | TAnon _ | TType _ | TMono _ -> ()

let mark_mt dce mt = match mt with
	| TClassDecl c ->
		mark_class dce c;
	| TEnumDecl e ->
		mark_enum dce e
	| TAbstractDecl a ->
		(* abstract 'feature' is defined as the abstract type beeing used as a value, not as a type *)
		if not (Meta.has Meta.ValueUsed a.a_meta) then a.a_meta <- (Meta.ValueUsed,[],a.a_pos) :: a.a_meta;
		mark_abstract dce a
	| TTypeDecl _ ->
		()

(* find all dependent fields by checking implementing/subclassing types *)
let rec mark_dependent_fields dce csup n stat =
	List.iter (fun mt -> match mt with
		| TClassDecl c when is_parent csup c ->
			let rec loop c =
				(try
					let cf = PMap.find n (if stat then c.cl_statics else c.cl_fields) in
					(* if it's clear that the class is kept, the field has to be kept as well. This is also true for
					   extern interfaces because we cannot remove fields from them *)
					if Meta.has Meta.Used c.cl_meta || (csup.cl_interface && csup.cl_extern) then mark_field dce c cf stat
					(* otherwise it might be kept if the class is kept later, so mark it as :?used *)
					else if not (Meta.has Meta.MaybeUsed cf.cf_meta) then begin
						cf.cf_meta <- (Meta.MaybeUsed,[],cf.cf_pos) :: cf.cf_meta;
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

let rec to_string dce t =
	let push t =
		dce.t_stack <- t :: dce.t_stack;
		fun () -> dce.t_stack <- List.tl dce.t_stack
	in
	if not (List.mem t dce.t_stack) then match follow t with
	| TInst(c,pl) as t ->
		let pop = push t in
		field dce c "toString" false;
		List.iter (to_string dce) pl;
		pop();
	| TEnum(en,pl) as t ->
		let pop = push t in
		PMap.iter (fun _ ef -> to_string dce ef.ef_type) en.e_constrs;
		List.iter (to_string dce) pl;
		pop();
	| TAnon a as t ->
		let pop = push t in
		PMap.iter (fun _ cf -> to_string dce cf.cf_type) a.a_fields;
		pop();
	| TFun(args,r) ->
		List.iter (fun (_,_,t) -> to_string dce t) args;
		to_string dce r;
	| _ -> ()

and field dce c n stat =
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
		if not (Meta.has Meta.Used cf.cf_meta) then begin
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
	with Not_found -> try
		if c.cl_interface then begin
			let rec loop cl = match cl with
				| [] -> raise Not_found
				| (c,_) :: cl ->
					try field dce c n stat with Not_found -> loop cl
			in
			loop c.cl_implements
		end else match c.cl_super with Some (csup,_) -> field dce csup n stat | None -> raise Not_found
	with Not_found -> try
		match c.cl_kind with
		| KTypeParameter tl ->
			let rec loop tl = match tl with
				| [] -> raise Not_found
				| TInst(c,_) :: cl ->
					(try field dce c n stat with Not_found -> loop cl)
				| t :: tl ->
					loop tl
			in
			loop tl
		| _ -> raise Not_found
	with Not_found ->
		if dce.debug then prerr_endline ("[DCE] Field " ^ n ^ " not found on " ^ (s_type_path c.cl_path)) else ())

and expr dce e =
	mark_t dce e.etype;
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
	| TCast(e, Some mt) ->
		mark_mt dce mt;
		expr dce e;
	| TTypeExpr mt ->
		mark_mt dce mt
	| TTry(e, vl) ->
		expr dce e;
		List.iter (fun (v,e) ->
			expr dce e;
			mark_t dce v.v_type;
		) vl;
	| TCall ({eexpr = TLocal ({v_name = "__define_feature__"})},[{eexpr = TConst (TString ft)};e]) ->
		Common.add_feature dce.com ft;
		expr dce e
	(* keep toString method when the class is argument to Std.string or haxe.Log.trace *)
	| TCall ({eexpr = TField({eexpr = TTypeExpr (TClassDecl ({cl_path = (["haxe"],"Log")} as c))},FStatic (_,{cf_name="trace"}))} as ef, ([e2;_] as args))
	| TCall ({eexpr = TField({eexpr = TTypeExpr (TClassDecl ({cl_path = ([],"Std")} as c))},FStatic (_,{cf_name="string"}))} as ef, ([e2] as args)) ->
		mark_class dce c;
		to_string dce e2.etype;
		expr dce ef;
		List.iter (expr dce) args;
	| TCall ({eexpr = TConst TSuper} as e,el) ->
		mark_t dce e.etype;
		List.iter (expr dce) el;
	| TField(e,fa) ->
		begin match fa with
			| FStatic(c,cf) ->
				mark_class dce c;
				mark_field dce c cf true;
			| FInstance(c,cf) ->
				mark_class dce c;
				mark_field dce c cf false;
			| _ ->
				let n = field_name fa in
				begin match follow e.etype with
					| TInst(c,_) ->
						mark_class dce c;
						field dce c n false;
					| TAnon a ->
						(match !(a.a_status) with
						| Statics c ->
							mark_class dce c;
							field dce c n true;
						| _ -> ())
					| _ -> ()
				end;
		end;
		expr dce e;
	| TThrow e ->
		to_string dce e.etype;
		expr dce e
	| _ ->
		Type.iter (expr dce) e

let run com main full =
	let dce = {
		com = com;
		full = full;
		std_dirs = if full then [] else List.map Common.unique_full_path com.std_path;
		debug = Common.defined com Define.DceDebug;
		added_fields = [];
		follow_expr = expr;
		marked_fields = [];
		marked_maybe_fields = [];
		t_stack = [];
	} in
	begin match main with
		| Some {eexpr = TCall({eexpr = TField(e,(FStatic(c,cf)))},_)} ->
			cf.cf_meta <- (Meta.Keep,[],cf.cf_pos) :: cf.cf_meta
		| _ ->
			()
	end;
	(* first step: get all entry points, which is the main method and all class methods which are marked with @:keep *)
	List.iter (fun t -> match t with
		| TClassDecl c ->
			let keep_class = keep_whole_class dce c && (not c.cl_extern || c.cl_interface) in
			let loop stat cf =
				if keep_class || keep_field dce cf then mark_field dce c cf stat
			in
			List.iter (loop true) c.cl_ordered_statics;
			List.iter (loop false) c.cl_ordered_fields;
			begin match c.cl_constructor with
				| Some cf -> loop false cf
				| None -> ()
			end
		| _ ->
			()
	) com.types;
	if dce.debug then begin
		List.iter (fun (c,cf,_) -> match cf.cf_expr with
			| None -> ()
			| Some _ -> print_endline ("[DCE] Entry point: " ^ (s_type_path c.cl_path) ^ "." ^ cf.cf_name)
		) dce.added_fields;
	end;
	(* second step: initiate DCE passes and keep going until no new fields were added *)
	let rec loop () =
		match dce.added_fields with
		| [] -> ()
		| cfl ->
			dce.added_fields <- [];
			(* extend to dependent (= overriding/implementing) class fields *)
			List.iter (fun (c,cf,stat) -> mark_dependent_fields dce c cf.cf_name stat) cfl;
			(* mark fields as used *)
			List.iter (fun (c,cf,stat) ->
				mark_class dce c;
				mark_field dce c cf stat;
				mark_t dce cf.cf_type
			) cfl;
			(* follow expressions to new types/fields *)
			List.iter (fun (_,cf,_) -> opt (expr dce) cf.cf_expr) cfl;
			loop ()
	in
	loop ();
	(* third step: filter types *)
	let rec loop acc types =
		match types with
		| (TClassDecl c) as mt :: l when keep_whole_class dce c ->
			loop (mt :: acc) l
		| (TClassDecl c) as mt :: l ->
			(* add :keep so subsequent filter calls do not process class fields again *)
			c.cl_meta <- (Meta.Keep,[],c.cl_pos) :: c.cl_meta;
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
			(match c.cl_constructor with Some cf when not (keep_field dce cf) -> c.cl_constructor <- None | _ -> ());
			(* we keep a class if it was used or has a used field *)
			if Meta.has Meta.Used c.cl_meta || c.cl_ordered_statics <> [] || c.cl_ordered_fields <> [] then loop (mt :: acc) l else begin
				(match c.cl_init with
				| Some f when Meta.has Meta.KeepInit c.cl_meta ->
					(* it means that we only need the __init__ block *)
					c.cl_extern <- true;
					loop (mt :: acc) l
				| _ ->
					if dce.debug then print_endline ("[DCE] Removed class " ^ (s_type_path c.cl_path));
					loop acc l)
			end
 		| (TEnumDecl e) as mt :: l when Meta.has Meta.Used e.e_meta || Meta.has Meta.Keep e.e_meta || e.e_extern || not (dce.full || is_std_file dce e.e_module.m_extra.m_file) ->
			loop (mt :: acc) l
		| TEnumDecl e :: l ->
			if dce.debug then print_endline ("[DCE] Removed enum " ^ (s_type_path e.e_path));
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
	List.iter (fun cf -> cf.cf_meta <- remove_meta Meta.Used cf.cf_meta) dce.marked_fields;
	List.iter (fun cf -> cf.cf_meta <- remove_meta Meta.MaybeUsed cf.cf_meta) dce.marked_maybe_fields;


