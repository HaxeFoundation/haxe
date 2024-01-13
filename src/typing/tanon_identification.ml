open Globals
open Type

let replace_mono t =
	let visited_anons = ref [] in
	let rec loop t =
		match t with
		| TMono ({ tm_type = None }) ->
			t_dynamic
		| TAnon an ->
			if not (List.memq an !visited_anons) then begin
				visited_anons := an :: !visited_anons;
				TFunctions.map loop t
			end else
				t
		| _ ->
			TFunctions.map loop t
	in
	loop t

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

class ['a] tanon_identification =
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

	method unify ~(strict:bool) (tc : Type.t) (pfm : 'a path_field_mapping) =
		let uctx = if strict then {
			allow_transitive_cast = false;
			allow_abstract_cast = false;
			allow_dynamic_to_cast = false;
			allow_arg_name_mismatch = false;
			equality_kind = EqStricter;
			equality_underlying = false;
			strict_field_kind = true;
		} else {default_unification_context with equality_kind = EqDoNotFollowNull} in

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
						if not (unify_kind ~strict:uctx.strict_field_kind cf'.cf_kind cf.cf_kind) then raise (Unify_error [Unify_custom "kind mismatch"]);
						Type.unify (apply_params c.cl_params tl (monomorphs cf'.cf_params cf'.cf_type)) (map (monomorphs cf.cf_params cf.cf_type))
					) pairs;
					monos
				| TAnon an1 ->
					let fields = ref an1.a_fields in
					let pairs = pair_up an1.a_fields in
					let monos = List.map (fun _ -> mk_mono()) pfm.pfm_params in
					let map = apply_params pfm.pfm_params monos in
					List.iter (fun (cf,cf') ->
						if strict && (Meta.has Meta.Optional cf.cf_meta) != (Meta.has Meta.Optional cf'.cf_meta) then raise (Unify_error [Unify_custom "optional mismatch"]);
						if not (unify_kind ~strict:uctx.strict_field_kind cf'.cf_kind cf.cf_kind) then raise (Unify_error [Unify_custom "kind mismatch"]);
						fields := PMap.remove cf.cf_name !fields;
						type_eq_custom uctx cf'.cf_type (map (monomorphs cf.cf_params cf.cf_type))
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

	method find_compatible ~(strict : bool) (arity : int) (tc : Type.t) =
		if arity >= DynArray.length pfm_by_arity then
			raise Not_found;
		let d = DynArray.get pfm_by_arity arity in
		let l = DynArray.length d in

		let rec loop i =
			if i >= l then
				raise Not_found;
			let pfm = DynArray.unsafe_get d i in
			try
				self#unify ~strict tc pfm;
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

	method identify_anon ?(strict:bool = false) (an : tanon) =
		let make_pfm path = {
			pfm_path = path;
			pfm_params = [];
			pfm_fields = an.a_fields;
			pfm_converted = None;
			pfm_arity = count_fields an.a_fields;
		} in
		match !(an.a_status) with
		| ClassStatics {cl_path = path} | EnumStatics {e_path = path} | AbstractStatics {a_path = path} ->
			begin try
				Some (Hashtbl.find pfms path)
			with Not_found ->
				let pfm = make_pfm path in
				self#add_pfm path pfm;
				Some pfm
			end
		| _ ->
			let arity,fields = PMap.fold (fun cf (i,acc) ->
				let t = replace_mono cf.cf_type in
				(i + 1),(PMap.add cf.cf_name {cf with cf_type = t} acc)
			) an.a_fields (0,PMap.empty) in
			let an = { a_fields = fields; a_status = an.a_status; } in
			try
				Some (self#find_compatible ~strict arity (TAnon an))
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

	method identify ?(strict:bool = false) (accept_anons : bool) (t : Type.t) =
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
			self#identify_anon ~strict an
		| _ ->
			None
end
