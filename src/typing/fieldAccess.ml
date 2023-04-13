open Typecore
open Type
open Error

type field_host =
	(* Get the plain expression with applied field type parameters. *)
	| FGet
	(* Does not apply field type parameters. *)
	| FCall
	(* Actual reading, for FClosure and such. *)
	| FRead
	(* Used as lhs, no semantic difference to FGet. *)
	| FWrite

type 'a accessor_resolution =
	(* Accessor was found. *)
	| AccessorFound of 'a
	(* Accessor was not found, but access was made on anonymous structure. *)
	| AccessorAnon
	(* Accessor was not found. *)
	| AccessorNotFound
	(* Accessor resolution was attempted on a non-property. *)
	| AccessorInvalid

let create e cf fh inline p = {
	fa_on     = e;
	fa_field  = cf;
	fa_host   = fh;
	fa_inline = inline;
	fa_pos    = p;
}

(* Creates the `tfield_access` corresponding to this field access, using the provided field. *)
let apply_fa cf = function
	| FHStatic c -> FStatic(c,cf)
	| FHInstance(c,tl) -> FInstance(c,tl,cf)
	| FHAbstract(a,tl,c) -> FStatic(c,cf)
	| FHAnon -> FAnon cf

let get_host c cf =
	if has_class_field_flag cf CfStatic then
		FHStatic c
	else match c.cl_kind with
		| KAbstractImpl a ->
			FHAbstract(a,extract_param_types a.a_params,c)
		| _ ->
			FHInstance(c,extract_param_types c.cl_params)

let get_host_class_raise = function
	| FHStatic c -> c
	| FHInstance(c,_) -> c
	| FHAbstract(_,_,c) -> c
	| FHAnon -> raise Not_found

(* Returns the mapping function to apply type parameters. *)
let get_map_function fa = match fa.fa_host with
	| FHStatic _ | FHAnon -> (fun t -> t)
	| FHInstance(c,tl) -> TClass.get_map_function c tl
	| FHAbstract(a,tl,_) -> apply_params a.a_params tl

(* Converts the field access to a `TField` node, using the provided `mode`. *)
let get_field_expr fa mode =
	let cf = fa.fa_field in
	let t = match mode with
		| FCall -> cf.cf_type
		| FGet | FRead | FWrite -> Type.field_type cf
	in
	let fa',t = match fa.fa_host with
		| FHStatic c ->
			FStatic(c,cf),t
		| FHInstance(c,tl) ->
			let fa = match cf.cf_kind with
			| Method _ when mode = FRead ->
				FClosure(Some(c,tl),cf)
			| _ ->
				FInstance(c,tl,cf)
			in
			let t = TClass.get_map_function c tl t in
			fa,t
		| FHAbstract(a,tl,c) ->
			FStatic(c,cf),apply_params a.a_params tl t
		| FHAnon ->
			let fa = match cf.cf_kind with
			| Method _ when mode = FRead ->
				FClosure(None,cf)
			| _ ->
				FAnon cf
			in
			fa,t
	in
	mk (TField(fa.fa_on,fa')) t fa.fa_pos

let find_accessor_for_field host cf t mode = match cf.cf_kind with
	| Var v ->
		begin match (match mode with MSet _ -> v.v_write | _ -> v.v_read) with
			| AccCall ->
				let name = (match mode with MSet _ -> "set_" | _ -> "get_") ^ cf.cf_name in
				let forward cf_acc new_host =
					(cf_acc,new_host)
				in
				begin match host with
				| FHStatic c ->
					begin try
						AccessorFound (forward (PMap.find name c.cl_statics) host)
					with Not_found ->
						(* TODO: Check if this is correct, there's a case in hxcpp's VirtualArray *)
						AccessorAnon
					end
				| FHInstance(c,tl) ->
					begin try
						(* Accessors can be overridden, so we have to check the actual type. *)
						let c,tl = match follow t with
							| TInst(c,tl) -> c,tl
							| _ -> c,tl
						in
						let (c2,_,cf_acc) = raw_class_field (fun f -> f.cf_type) c tl name in
						let new_host = match c2 with
							| None -> FHAnon
							| Some(c,tl) -> FHInstance(c,tl)
						in
						AccessorFound (forward cf_acc new_host)
					with Not_found ->
						if has_class_flag c CExtern then AccessorAnon else AccessorNotFound
					end
				| FHAbstract(a,tl,c) ->
					begin try
						AccessorFound (forward (PMap.find name c.cl_statics) host)
					with Not_found ->
						AccessorAnon
					end
				| FHAnon ->
					AccessorAnon
				end
			| _ ->
				AccessorInvalid
		end
	| _ ->
		AccessorInvalid

(* Resolves the accessor on the field access, using the provided `mode`. *)
let resolve_accessor fa mode =
	let forward cf_acc new_host =
		create fa.fa_on cf_acc new_host fa.fa_inline fa.fa_pos
	in
	match find_accessor_for_field fa.fa_host fa.fa_field fa.fa_on.etype mode with
	| AccessorFound(cf_acc,new_host) ->
		AccessorFound (forward cf_acc new_host)
	| AccessorInvalid ->
		AccessorInvalid
	| AccessorAnon ->
		AccessorAnon
	| AccessorNotFound ->
		AccessorNotFound

let get_constructor_access c tl p =
	try
		let e_static = Builder.make_static_this c p in
		let c, tl = match c.cl_kind with
			| KAbstractImpl a -> (match Abstract.follow_with_forward_ctor (TAbstract(a,tl)) with
				| TInst(c,tl) -> c, tl
				| TAbstract({a_impl = Some c},tl) -> c, tl
				| _ -> c, tl)
			| _ -> c, tl
		in
		let cf, fh = match c.cl_kind with
			| KAbstractImpl a -> PMap.find "_new" c.cl_statics, FHAbstract(a,tl,c)
			| _ -> Type.get_constructor c, FHInstance(c,tl)
		in
		create e_static cf fh false p
	with Not_found ->
		raise_typing_error (No_constructor (match c.cl_kind with
			| KAbstractImpl a -> TAbstractDecl a
			| _ -> TClassDecl c
		)) p

let make_static_extension_access c cf e_this inline p =
	let e_static = Texpr.Builder.make_static_this c p in
	{
		se_this = e_this;
		se_access = create e_static cf (FHStatic c) inline p
	}

let make_abstract_static_extension_access a tl c cf e_this inline p =
	let e_static = Texpr.Builder.make_static_this c p in
	{
		se_this = e_this;
		se_access = create e_static cf (FHAbstract(a,tl,c)) inline p
	}
