open Globals
open Type
open Typecore

type overload_args_comparison =
  | Same
  | Different
  | Impl_conflict

let distinguishes_funs_as_params ctx =
  	match ctx.com.platform with
  	| Java -> false
  	| _ -> true

let compare_overload_args ?(get_vmtype) ?(ctx) t1 t2 f1 f2 =
	let get_vmtype = match get_vmtype with
		| None -> (fun f -> f)
		| Some f -> f
	in
	if List.length f1.cf_params <> List.length f2.cf_params then
		Different
	else
	let amb_funs =
		match ctx with
		| None -> false
		| Some ctx -> not (distinguishes_funs_as_params ctx) in
	let rec follow_skip_null t = match t with
		| TMono r ->
			(match r.tm_type with
			| Some t -> follow_skip_null t
			| _ -> t)
		| TLazy f ->
			follow_skip_null (lazy_type f)
		| TAbstract ({ a_path = [],"Null" } as a, [p]) ->
			TAbstract(a,[follow p])
		| TType (t,tl) ->
			follow_skip_null (apply_params t.t_params tl t.t_type)
		| _ -> t
	in
	let compare_type t1 t2 =
		(if type_iseq t1 t2 then
			Same
		else if amb_funs && type_iseq (ambiguate_funs t1) (ambiguate_funs t2) then
			Impl_conflict
		else
			Different) in
	let compare_arg t1 t2 =
		let t1 = get_vmtype (follow_skip_null t1) in
		let t2 = get_vmtype (follow_skip_null t2) in
		match t1, t2 with
			| TType _, TType _ -> compare_type t1 t2
			| TType _, _
			| _, TType _ -> Different
			| _ -> compare_type t1 t2
	in

	match follow (apply_params f1.cf_params (List.map (fun (_,t) -> t) f2.cf_params) t1), follow t2 with
		| TFun(a1,_), TFun(a2,_) ->
			let rec loop args1 args2 =
				match args1, args2 with
				| [], [] -> Same
				| [], _ | _, [] -> Different
				| (_,_,t1) :: rest1, (_,_,t2) :: rest2 ->
					match compare_arg t1 t2 with
					| Same -> loop rest1 rest2
					| result -> result
			in
			loop a1 a2
		| _ -> die "" __LOC__

let same_overload_args ?(get_vmtype) t1 t2 f1 f2 =
	compare_overload_args ?get_vmtype t1 t2 f1 f2 <> Different

let collect_overloads c i =
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
			| None when c.cl_interface ->
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
	loop (fun t -> t) c;
	List.rev !acc

let get_overloads (com : Common.context) c i =
	collect_overloads c i
	(* TODO: check why this kills Java *)
	(* try
		Hashtbl.find com.overload_cache (c.cl_path,i)
	with Not_found ->
		let l = collect_overloads c i in
		Hashtbl.add com.overload_cache (c.cl_path,i) l;
		l *)

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
			simplify_t (apply_params t.t_params tl t.t_type)
		| TMono r -> (match r.tm_type with
			| Some t -> simplify_t t
			| None -> t_dynamic)
		| TAnon _ -> t_dynamic
		| TDynamic -> t
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
		| TInst({ cl_interface = true } as cf, tlf), TInst(ca, tla) ->
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
		| TDynamic, TDynamic ->
			(cacc, 0)
		| TDynamic, _ ->
			(max_int, 0) (* a function with dynamic will always be worst of all *)
		| TAbstract(a, _), TDynamic when Meta.has Meta.CoreType a.a_meta && a.a_path <> ([],"Null") ->
			(cacc + 2, 0) (* a dynamic to a basic type will have an "unboxing" penalty *)
		| _, TDynamic ->
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
		| ( el, t, _ ) :: ret when List.exists (fun (_,t2,_) -> type_iseq t t2) acc ->
			rm_duplicates acc ret
		| r :: ret ->
			rm_duplicates (r :: acc) ret

	let s_options rated =
		String.concat ",\n" (List.map (fun ((elist,t,_),rate) ->
			"( " ^ (String.concat "," (List.map (fun(e,_) -> s_expr (s_type (print_context())) e) elist)) ^ " ) => " ^
			"( " ^ (String.concat "," (List.map (fun (i,i2) -> string_of_int i ^ ":" ^ string_of_int i2) rate)) ^ " ) => " ^ (s_type (print_context()) t)
		) rated)

	let count_optionals elist =
		List.fold_left (fun acc (_,is_optional) -> if is_optional then acc + 1 else acc) 0 elist

	let rec fewer_optionals acc compatible = match acc, compatible with
		| _, [] -> acc
		| [], c :: comp -> fewer_optionals [c] comp
		| (elist_acc, _, _) :: _, ((elist, _, _) as cur) :: comp ->
			let acc_opt = count_optionals elist_acc in
			let comp_opt = count_optionals elist in
			if acc_opt = comp_opt then
				fewer_optionals (cur :: acc) comp
			else if acc_opt < comp_opt then
				fewer_optionals acc comp
			else
				fewer_optionals [cur] comp

	let reduce_compatible compatible = match fewer_optionals [] (rm_duplicates [] compatible) with
		| [] -> []
		| [v] -> [v]
		| compatible ->
			(* convert compatible into ( rate * compatible_type ) list *)
			let rec mk_rate acc elist args = match elist, args with
				| [], [] -> acc
				| (_,true) :: elist, _ :: args -> mk_rate acc elist args
				| (e,false) :: elist, (n,o,t) :: args ->
					(* if the argument is an implicit cast, we need to start with a penalty *)
					(* The penalty should be higher than any other implicit cast - other than Dynamic *)
					(* since Dynamic has a penalty of max_int, we'll impose max_int - 1 to it *)
					(match e.eexpr with
						| TMeta( (Meta.ImplicitCast,_,_), _) ->
							mk_rate ((max_int - 1, 0) :: acc) elist args
						| _ ->
							mk_rate (rate_conv 0 t e.etype :: acc) elist args)
				| _ -> die "" __LOC__
			in

			let rated = ref [] in
			List.iter (function
				| (elist,TFun(args,ret),d) -> (try
					rated := ( (elist,TFun(args,ret),d), mk_rate [] elist args ) :: !rated
					with | Not_found -> ())
				| _ -> die "" __LOC__
			) compatible;

			let rec loop best rem = match best, rem with
				| _, [] -> best
				| [], r1 :: rem -> loop [r1] rem
				| (bover, bargs) :: b1, (rover, rargs) :: rem ->
					if is_best bargs rargs then
						loop best rem
					else if is_best rargs bargs then
						loop (loop b1 [rover,rargs]) rem
					else (* equally specific *)
						loop ( (rover,rargs) :: best ) rem
			in

			let r = loop [] !rated in
			List.map fst r
end
