open Globals
open Type
open Typecore

let same_overload_args ?(get_vmtype) t1 t2 f1 f2 =
	let f_transform = match get_vmtype with
		| Some f -> f
		| None -> (fun t -> t)
	in
	let f_eq t1 t2 = type_iseq (f_transform t1) (f_transform t2) in
	let compare_type_params () =
		let rec loop params1 params2 = match params1,params2 with
			| [],[] ->
				true
			| tp1 :: params1,tp2 :: params2 ->
				let constraints_equal t1 t2 = match follow t1,follow t2 with
					| TInst({cl_kind = KTypeParameter tl1},_),TInst({cl_kind = KTypeParameter tl2},_) ->
						Ast.safe_for_all2 f_eq tl1 tl2
					| _ ->
						false
				in
				tp1.ttp_name = tp2.ttp_name && constraints_equal tp1.ttp_type tp2.ttp_type && loop params1 params2
			| [],_
			| _,[] ->
				false
		in
		loop f1.cf_params f2.cf_params
	in
	let compare_arguments tl1 tl2 =
		let rec loop tl1 tl2 = match tl1,tl2 with
			| [],[] ->
				true
			| (n1,o1,t1) :: tl1,(n2,o2,t2) :: tl2 ->
				(* TODO: do we want to compare n and o here? *)
				f_eq t1 t2 && loop tl1 tl2
			| _ ->
				false
		in
		loop tl1 tl2
	in
	let compare_types () =
		let t1 = follow (apply_params f1.cf_params (extract_param_types f2.cf_params) t1) in
		match t1,follow t2 with
		| TFun(tl1,_),TFun(tl2,_) ->
			compare_arguments tl1 tl2
		| _ ->
			false
	in
	compare_type_params () && compare_types ()

let collect_overloads map c i =
	let acc = ref [] in
	let rec loop map c =
		let maybe_add cf =
			let t = map cf.cf_type in
			if not (List.exists (fun (t2,cf2) -> same_overload_args t t2 cf cf2) !acc) then acc := (t,cf) :: !acc
		in
		begin try
			let cf = PMap.find i c.cl_fields in
			begin match cf.cf_kind with
				| Var _ ->
					()
				| Method _ ->
					maybe_add cf;
					List.iter maybe_add cf.cf_overloads
			end;
		with Not_found ->
			()
		end;
		match c.cl_super with
			| None when (has_class_flag c CInterface) ->
				List.iter (fun (c,tl) ->
					let tl = List.map map tl in
					loop (fun t -> apply_params c.cl_params tl (map t)) c
				) c.cl_implements
			| None ->
				()
			| Some (c,tl) ->
				let tl = List.map map tl in
				loop (fun t -> apply_params c.cl_params tl (map t)) c
	in
	loop map c;
	List.rev !acc

let get_overloads (com : Common.context) c i =
	try
		com.overload_cache#find (c.cl_path,i)
	with Not_found ->
		let l = collect_overloads (fun t -> t) c i in
		com.overload_cache#add (c.cl_path,i) l;
		l

(** Overload resolution **)
module Resolution =
struct
	let rec simplify_t t = match t with
		| TInst _ | TEnum _ ->
			t
		| TAbstract(({ a_path = [],"Null" } as t), [t2]) -> (match simplify_t t2 with
			| (TAbstract(a,_) as t2) when Meta.has Meta.CoreType a.a_meta ->
				TAbstract(t, [simplify_t t2])
			| (TEnum _ as t2) ->
				TAbstract(t, [simplify_t t2])
			| t2 -> t2)
		| TAbstract(a,_) when Meta.has Meta.CoreType a.a_meta ->
			t
		| TAbstract(a,tl) -> simplify_t (Abstract.get_underlying_type a tl)
		| TType(t, tl) ->
			simplify_t (apply_typedef t tl)
		| TMono r -> (match r.tm_type with
			| Some t -> simplify_t t
			| None -> t_dynamic)
		| TAnon _ -> t_dynamic
		| TDynamic _ -> t
		| TLazy f -> simplify_t (lazy_type f)
		| TFun _ -> t

	(* rate type parameters *)
	let rate_tp tlfun tlarg =
		let acc = ref 0 in
		List.iter2 (fun f a -> if not (type_iseq f a) then incr acc) tlfun tlarg;
		!acc

	(**
		The rate function returns an ( int * int ) type.
		The smaller the int, the best rated the caller argument is in comparison with the callee.

		The first int refers to how many "conversions" would be necessary to convert from the callee to the caller type, and
		the second refers to the type parameters.
	**)
	let rec rate_conv cacc tfun targ =
		match simplify_t tfun, simplify_t targ with
		| TInst(cf, tlf), TInst(ca, tla) when (has_class_flag cf CInterface) ->
			(* breadth-first *)
			let stack = ref [0,ca,tla] in
			let cur = ref (0, ca,tla) in
			let rec loop () =
				match !stack with
				| [] -> (let acc, ca, tla = !cur in match ca.cl_super with
					| None -> raise Not_found
					| Some (sup,tls) ->
						cur := (acc+1,sup,List.map (apply_params ca.cl_params tla) tls);
						stack := [!cur];
						loop())
				| (acc,ca,tla) :: _ when ca == cf ->
					acc,tla
				| (acc,ca,tla) :: s ->
					stack := s @ List.map (fun (c,tl) -> (acc+1,c,List.map (apply_params ca.cl_params tla) tl)) ca.cl_implements;
					loop()
			in
			let acc, tla = loop() in
			(cacc + acc, rate_tp tlf tla)
		| TInst(cf,tlf), TInst(ca,tla) ->
			let rec loop acc ca tla =
				if cf == ca then
					acc, tla
				else match ca.cl_super with
				| None -> raise Not_found
				| Some(sup,stl) ->
					loop (acc+1) sup (List.map (apply_params ca.cl_params tla) stl)
			in
			let acc, tla = loop 0 ca tla in
			(cacc + acc, rate_tp tlf tla)
		| TEnum(ef,tlf), TEnum(ea, tla) ->
			if ef != ea then raise Not_found;
			(cacc, rate_tp tlf tla)
		| TDynamic _, TDynamic _ ->
			(cacc, 0)
		| TDynamic _, _ ->
			(max_int, 0) (* a function with dynamic will always be worst of all *)
		| TAbstract(a, _), TDynamic _ when Meta.has Meta.CoreType a.a_meta && a.a_path <> ([],"Null") ->
			(cacc + 2, 0) (* a dynamic to a basic type will have an "unboxing" penalty *)
		| _, TDynamic _ ->
			(cacc + 1, 0)
		| TAbstract({ a_path = [], "Null" }, [tf]), TAbstract({ a_path = [], "Null" }, [ta]) ->
			rate_conv (cacc+0) tf ta
		| TAbstract({ a_path = [], "Null" }, [tf]), ta ->
			rate_conv (cacc+1) tf ta
		| tf, TAbstract({ a_path = [], "Null" }, [ta]) ->
			rate_conv (cacc+1) tf ta
		| TAbstract(af,tlf), TAbstract(aa,tla) ->
			(if af == aa then
				(cacc, rate_tp tlf tla)
			else
				let ret = ref None in
				if List.exists (fun t -> try
					ret := Some (rate_conv (cacc+1) (apply_params af.a_params tlf t) targ);
					true
				with | Not_found ->
					false
				) af.a_from then
					Option.get !ret
			else
				if List.exists (fun t -> try
					ret := Some (rate_conv (cacc+1) tfun (apply_params aa.a_params tla t));
					true
				with | Not_found ->
					false
				) aa.a_to then
					Option.get !ret
			else
				raise Not_found)
		| TFun _, TFun _ -> (* unify will make sure they are compatible *)
			cacc,0
		| tfun,targ ->
			raise Not_found

	let is_best arg1 arg2 =
		(Ast.safe_for_all2 (fun v1 v2 ->
			v1 <= v2)
		arg1 arg2) && (List.exists2 (fun v1 v2 ->
			v1 < v2)
		arg1 arg2)

	let rec rm_duplicates acc ret = match ret with
		| [] -> acc
		| fcc :: ret when List.exists (fun fcc2 -> type_iseq fcc.fc_type fcc2.fc_type) acc ->
			rm_duplicates acc ret
		| r :: ret ->
			rm_duplicates (r :: acc) ret

	let s_options rated =
		String.concat ",\n" (List.map (fun ((elist,t,_),rate) ->
			"( " ^ (String.concat "," (List.map (fun(e,_) -> s_expr (s_type (print_context())) e) elist)) ^ " ) => " ^
			"( " ^ (String.concat "," (List.map (fun (i,i2) -> string_of_int i ^ ":" ^ string_of_int i2) rate)) ^ " ) => " ^ (s_type (print_context()) t)
		) rated)

	let count_optionals t =
		match follow t with
		| TFun(args,_) ->
			List.fold_left (fun acc (_,is_optional,_) -> if is_optional then acc + 1 else acc) 0 args
		| _ ->
			0

	let rec fewer_optionals acc compatible = match acc, compatible with
		| _, [] -> acc
		| [], c :: comp -> fewer_optionals [c] comp
		| fcc_acc :: _, fcc :: comp ->
			let acc_opt = count_optionals fcc_acc.fc_type in
			let comp_opt = count_optionals fcc.fc_type in
			if acc_opt = comp_opt then
				fewer_optionals (fcc :: acc) comp
			else if acc_opt < comp_opt then
				fewer_optionals acc comp
			else
				fewer_optionals [fcc] comp

	let reduce_compatible compatible = match fewer_optionals [] (rm_duplicates [] compatible) with
		| [] -> []
		| [v] -> [v]
		| compatible ->
			let rate_arg t e = match e.eexpr with
				(* if the argument is an implicit cast, we need to start with a penalty *)
				(* The penalty should be higher than any other implicit cast - other than Dynamic *)
				(* since Dynamic has a penalty of max_int, we'll impose max_int - 1 to it *)
				| TMeta( (Meta.ImplicitCast,_,_), _) -> (max_int - 1, 0)
				| _ -> rate_conv 0 t e.etype
			in
			(* convert compatible into ( rate * compatible_type ) list *)
			let rec mk_rate acc elist args = match elist, args with
				| [], [] ->
					acc,false
				| _ :: elist, (_,true,_) :: args ->
					mk_rate acc elist args
				| elist, [n,o,t] when ExtType.is_rest (follow t) ->
					let t = match follow t with
						| TAbstract({a_path=["haxe"],"Rest"},[t]) -> t
						| _ -> die "" __LOC__
					in
					let rates = List.map (rate_arg t) elist in
					acc @ rates,true
				| e :: elist, (n,o,t) :: args ->
					mk_rate ((rate_arg t e) :: acc) elist args
				| [],_ ->
					(* this can happen on pf_pad_nulls = false targets, see #10434 *)
					acc,false
				| _ -> die "" __LOC__
			in

			let rec loop best l = match l with
				| [] ->
					begin match best with
						| Some(_,_,l) -> List.rev l
						| None -> []
					end
				| fcc :: l ->
					let args,ret = match follow fcc.fc_type with
						| TFun(args,ret) -> args,ret
						| _ -> die "" __LOC__
					in
					begin try
						let (rate,is_rest) = mk_rate [] fcc.fc_args args in
						let (best_rate,best_is_rest,best_l) = match best with
							| None ->
								(* If it's the first one, assume it's the best *)
								(rate,is_rest,[fcc])
							| Some(best_rate,best_is_rest,best_l) ->
								if is_best rate best_rate then
									(rate,is_rest,[fcc])
								else if is_best best_rate rate then
									(best_rate,best_is_rest,best_l)
								(* If they are equal, we prefer the one without Rest (issue #10205) *)
								else if is_rest && not best_is_rest then
									(best_rate,best_is_rest,best_l)
								else if not is_rest && best_is_rest then
									(rate,is_rest,[fcc])
								else
									(best_rate,best_is_rest,fcc :: best_l)
						in
						loop (Some(best_rate,best_is_rest,best_l)) l
					with Not_found ->
						loop best l
					end
			in
			loop None compatible
end
