open Globals
open Type

let rec replace_mono t =
	match t with
	| TMono t ->
		(match t.tm_type with
		| None -> Monomorph.bind t t_dynamic
		| Some _ -> ())
	| TEnum (_,p) | TInst (_,p) | TType (_,p) | TAbstract (_,p) ->
		List.iter replace_mono p
	| TFun (args,ret) ->
		List.iter (fun (_,_,t) -> replace_mono t) args;
		replace_mono ret
	| TAnon _
	| TDynamic _ -> ()
	| TLazy f ->
		replace_mono (lazy_type f)

type 'a path_field_mapping = {
	pfm_path : path;
	pfm_params : type_params;
	pfm_fields : (string,tclass_field) PMap.t;
	mutable pfm_converted : (string * 'a) list option;
	pfm_arity : int;
}

let count_fields pm =
	PMap.fold (fun _ i -> i + 1) pm 0

let pfm_of_typedef td = match follow td.t_type with
	| TAnon an -> {
		pfm_path = td.t_path;
		pfm_params = td.t_params;
		pfm_fields = an.a_fields;
		pfm_converted = None;
		pfm_arity = count_fields an.a_fields;
	}
	| _ ->
		die "" __LOC__

class ['a] tanon_identification (empty_path : string list * string) =
	let is_normal_anon an = match !(an.a_status) with
		| Closed | Const -> true
		| _ -> false
	in
object(self)

	val pfms = Hashtbl.create 0
	val pfm_by_arity = DynArray.create ()
	val mutable num = 0

	method get_pfms = pfms

	method add_pfm (path : path) (pfm : 'a path_field_mapping) =
		while DynArray.length pfm_by_arity <= pfm.pfm_arity do
			DynArray.add pfm_by_arity (DynArray.create ())
		done;
		DynArray.add (DynArray.get pfm_by_arity pfm.pfm_arity) pfm;
		Hashtbl.replace pfms path pfm

	method unify (tc : Type.t) (pfm : 'a path_field_mapping) =
		let check () =
			let pair_up fields =
				PMap.fold (fun cf acc ->
					let cf' = PMap.find cf.cf_name fields in
					(cf,cf') :: acc
				) pfm.pfm_fields []
			in
			let monos = match follow tc with
				| TInst(c,tl) ->
					let pairs = pair_up c.cl_fields in
					let monos = List.map (fun _ -> mk_mono()) pfm.pfm_params in
					let map = apply_params pfm.pfm_params monos in
					List.iter (fun (cf,cf') ->
						if not (unify_kind cf'.cf_kind cf.cf_kind) then raise (Unify_error [Unify_custom "kind mismatch"]);
						Type.unify (apply_params c.cl_params tl (monomorphs cf'.cf_params cf'.cf_type)) (map (monomorphs cf.cf_params cf.cf_type))
					) pairs;
					monos
				| TAnon an1 ->
					let fields = ref an1.a_fields in
					let pairs = pair_up an1.a_fields in
					let monos = List.map (fun _ -> mk_mono()) pfm.pfm_params in
					let map = apply_params pfm.pfm_params monos in
					List.iter (fun (cf,cf') ->
						if not (unify_kind cf'.cf_kind cf.cf_kind) then raise (Unify_error [Unify_custom "kind mismatch"]);
						fields := PMap.remove cf.cf_name !fields;
						Type.type_eq EqDoNotFollowNull cf'.cf_type (map (monomorphs cf.cf_params cf.cf_type))
					) pairs;
					if not (PMap.is_empty !fields) then raise (Unify_error [Unify_custom "not enough fields"]);
					monos
				| _ ->
					raise (Unify_error [Unify_custom "bad type"])
			in
			(* Check if we applied Void to a return type parameter... (#3463) *)
			List.iter (fun t -> match follow t with
				| TMono r ->
					Monomorph.bind r t_dynamic
				| t ->
					if Type.ExtType.is_void t then raise(Unify_error [Unify_custom "return mono"])
			) monos
		in
		try
			check()
		with Not_found ->
			raise (Unify_error [])

	method find_compatible (arity : int) (tc : Type.t) =
		if arity >= DynArray.length pfm_by_arity then
			raise Not_found;
		let d = DynArray.get pfm_by_arity arity in
		let l = DynArray.length d in
		let rec loop i =
			if i >= l then
				raise Not_found;
			let pfm = DynArray.unsafe_get d i in
			try
				self#unify tc pfm;
				pfm
			with Unify_error _ ->
				loop (i + 1)
		in
		loop 0

	method identify_typedef (td : tdef) =
		let rec loop t = match t with
			| TAnon an when is_normal_anon an && not (PMap.is_empty an.a_fields) ->
				self#add_pfm td.t_path (pfm_of_typedef td)
			| TMono {tm_type = Some t} ->
				loop t
			| TLazy f ->
				loop (lazy_type f)
			| t ->
				()
		in
		loop td.t_type

	method identity_anon (an : tanon) =
		let make_pfm path = {
			pfm_path = path;
			pfm_params = [];
			pfm_fields = an.a_fields;
			pfm_converted = None;
			pfm_arity = count_fields an.a_fields;
		} in
		match !(an.a_status) with
		| ClassStatics {cl_path = path} | EnumStatics {e_path = path} | AbstractStatics {a_path = path} ->
			let pfm = make_pfm path in
			self#add_pfm path pfm;
			Some pfm
		| _ ->
			let arity = PMap.fold (fun cf i ->
				replace_mono cf.cf_type;
				i + 1
			) an.a_fields 0 in
			begin try
				Some (self#find_compatible arity (TAnon an))
			with Not_found ->
				let id = num in
				num <- num + 1;
				let path = (["haxe";"generated"],Printf.sprintf "Anon%i" id) in
				let pfm = {
					pfm_path = path;
					pfm_params = [];
					pfm_fields = an.a_fields;
					pfm_converted = None;
					pfm_arity = count_fields an.a_fields;
				} in
				self#add_pfm path pfm;
				Some pfm
			end

	method identify (accept_anons : bool) (t : Type.t) =
		match t with
		| TType(td,tl) ->
			begin try
				Some (Hashtbl.find pfms td.t_path)
			with Not_found ->
				self#identify accept_anons (apply_typedef td tl)
			end
		| TMono {tm_type = Some t} ->
			self#identify accept_anons t
		| TAbstract(a,tl) when not (Meta.has Meta.CoreType a.a_meta) ->
			self#identify accept_anons (Abstract.get_underlying_type a tl)
		| TAbstract({a_path=([],"Null")},[t]) ->
			self#identify accept_anons t
		| TLazy f ->
			self#identify accept_anons (lazy_type f)
		| TAnon an when accept_anons && not (PMap.is_empty an.a_fields) ->
			self#identity_anon an
		| _ ->
			None
end