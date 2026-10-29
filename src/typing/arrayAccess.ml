open Ast
open Type
open Typecore
open Error

(* How values of a type can be accessed by index: the element type, how to read the element at an index and how
   to read the length. For loops and array patterns use this (#8012). Abstracts follow how the typer types `e[i]` and
   `e.length` on them: an @:arrayAccess getter, @:arrayAccess on the abstract or a conversion to an array, and a
   get_length or a length forwarded by @:forward. *)
type array_access = {
	aa_type : Type.t;
	aa_get : array_get;
	aa_length : array_length;
}

and array_get =
	| AGIndex (* e[i] *)
	| AGDynamic (* e[i] of a Dynamic value, typed as a monomorph of its own *)
	| AGCall of tclass * tclass_field * tparams (* a call of the abstract's @:arrayAccess getter *)

and array_length =
	| ALField (* the class's length field *)
	| ALCall of tclass * tclass_field * tparams (* a call of the abstract's get_length *)
	| ALForward of Type.t * array_length (* the length of the abstract's underlying type, which @:forward forwards *)

let get_array_length ctx arr p =
	let fa = try quick_field arr.etype "length" with Not_found -> Error.raise_msg (Printf.sprintf "Could not find field length on %s" (s_type_kind arr.etype)) arr.epos in
	let t = match fa with
		| FDynamic _ -> t_dynamic
		| _ -> ctx.com.basic.tint
	in
	mk (TField (arr,fa)) t p

(* The element of e[i], for a type which isn't an abstract with @:arrayAccess functions, the way the typer finds it:
   through @:arrayAccess on an abstract to its underlying type, or by unifying with an array. *)
let rec get_array_element ctx t p =
	match follow t with
	| TInst({cl_path=[],("Array" | "ArrayAccess")},[pt]) ->
		(* This case is not technically needed because the default case would handle it, but it avoids the monomorph
		   creation and unification there. *)
		AGIndex,pt
	| TDynamic None ->
		AGDynamic,t_dynamic
	| TInst({ cl_array_access = Some pt; cl_params = pl },tl) ->
		AGIndex,apply_params pl tl pt
	| TInst({ cl_super = Some (c,stl); cl_params = pl },tl) ->
		let get,t = get_array_element ctx (TInst (c,stl)) p in
		get,apply_params pl tl t
	| TAbstract(a,tl) when Meta.has Meta.ArrayAccess a.a_meta && not (Meta.has Meta.CoreType a.a_meta) ->
		get_array_element ctx (apply_params a.a_params tl a.a_this) p
	| _ ->
		let pt = spawn_monomorph ctx p in
		(try unify_raise t (ctx.t.tarray pt) p with Error { err_message = Unify _ } -> raise Not_found);
		AGIndex,pt

(* The length of an abstract: its get_length, or the length of its underlying type if @:forward forwards it. *)
let rec get_abstract_length a c tl =
	match (try Some (PMap.find "get_length" c.cl_statics) with Not_found -> None) with
	| Some cf_length ->
		(match follow cf_length.cf_type with
			| TFun(_,tr) ->
				(match follow tr with
					| TAbstract({a_path = [],"Int"},_) -> ()
					| _ -> raise Not_found
				)
			| _ ->
				raise Not_found
		);
		ALCall(c,cf_length,tl)
	| None ->
		let forwards = try
			let _,el,_ = Meta.get Meta.Forward a.a_meta in
			el = [] || List.exists (fun e -> match fst e with
				| EConst (Ident "length" | String ("length",_)) -> true
				| _ -> false
			) el
		with Not_found ->
			false
		in
		if not forwards then raise Not_found;
		let t = Abstract.get_underlying_type ~return_first:true a tl in
		ALForward(t,get_forwarded_length t)

and get_forwarded_length t =
	match follow t with
	| TInst(c,tl) as t ->
		(match quick_field t "length" with
			| FInstance(c,tl,cf) ->
				(match follow (apply_params c.cl_params tl cf.cf_type) with
					| TAbstract({a_path = [],"Int"},[]) -> ALField
					| _ -> raise Not_found
				)
			| _ ->
				raise Not_found
		)
	| TAbstract({a_impl = Some c} as a,tl) ->
		get_abstract_length a c tl
	| _ ->
		raise Not_found

let get_array_access ctx t p =
	match follow t with
	| TInst({cl_path=[],"Array"},[pt]) ->
		{ aa_type = pt; aa_get = AGIndex; aa_length = ALField }
	| TDynamic None ->
		{ aa_type = t_dynamic; aa_get = AGDynamic; aa_length = ALField }
	| TInst({ cl_array_access = Some pt } as c,pl) when (try match follow (PMap.find "length" c.cl_fields).cf_type with TAbstract ({ a_path = [],"Int" },[]) -> true | _ -> false with Not_found -> false) ->
		{ aa_type = apply_params c.cl_params pl pt; aa_get = AGIndex; aa_length = ALField }
	| TAbstract({a_impl = Some c} as a,tl) ->
		let aa_length = get_abstract_length a c tl in
		(try
			(* first try: do we have an @:arrayAccess getter field? *)
			let todo = mk (TConst TNull) ctx.t.tint p in
			let cf,_,r,_ = AbstractCast.find_array_read_access_raise ctx a tl todo p in
			{ aa_type = r; aa_get = AGCall(c,cf,tl); aa_length }
		with Not_found ->
			(* second try: do we have @:arrayAccess on a core-type abstract? *)
			if Meta.has Meta.ArrayAccess a.a_meta && Meta.has Meta.CoreType a.a_meta then begin
				(* in which case we assume that a singular type parameter is the element type *)
				let t = match tl with [t] -> t | _ -> raise Not_found in
				{ aa_type = t; aa_get = AGIndex; aa_length }
			end else begin
				(* otherwise, as the typer does when the abstract has no @:arrayAccess function *)
				let aa_get,aa_type = get_array_element ctx t p in
				{ aa_type; aa_get; aa_length }
			end
		)
	| _ -> raise Not_found

let make_array_get ctx aa e_base e_index p = match aa.aa_get with
	| AGIndex -> Texpr.Builder.make_array_access e_base e_index aa.aa_type p
	| AGDynamic -> Texpr.Builder.make_array_access e_base e_index (spawn_monomorph ctx p) p
	| AGCall(c,cf,tl) -> CallUnification.make_static_call_better ctx c cf tl [e_base;e_index] aa.aa_type p

let make_array_length ctx aa e p =
	let rec make length e = match length with
		| ALField -> get_array_length ctx e p
		| ALCall(c,cf,tl) -> CallUnification.make_static_call_better ctx c cf tl [e] ctx.com.basic.tint p
		| ALForward(t,length) -> make length (mk (TCast(e,None)) t e.epos)
	in
	make aa.aa_length e
