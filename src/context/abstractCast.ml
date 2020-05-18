open Globals
open Common
open Ast
open Type
open Typecore
open Error

let cast_stack = new_rec_stack()

let rec make_static_call ctx c cf a pl args t p =
	if cf.cf_kind = Method MethMacro then begin
		match args with
			| [e] ->
				let e,f = push_this ctx e in
				ctx.with_type_stack <- (WithType.with_type t) :: ctx.with_type_stack;
				let e = match ctx.g.do_macro ctx MExpr c.cl_path cf.cf_name [e] p with
					| Some e -> type_expr ctx e (WithType.with_type t)
					| None ->  type_expr ctx (EConst (Ident "null"),p) WithType.value
				in
				ctx.with_type_stack <- List.tl ctx.with_type_stack;
				let e = try cast_or_unify_raise ctx t e p with Error(Unify _,_) -> raise Not_found in
				f();
				e
			| _ -> die "" __LOC__
	end else
		Typecore.make_static_call ctx c cf (apply_params a.a_params pl) args t p

and do_check_cast ctx tleft eright p =
	let uctx = default_unification_context in
	let recurse cf f =
		(*
			Without this special check for macro @:from methods we will always get "Recursive implicit cast" error
			unlike non-macro @:from methods, which generate unification errors if no other @:from methods are involved.
		*)
		if cf.cf_kind = Method MethMacro then begin
			match cast_stack.rec_stack with
			| previous_from :: _ when previous_from == cf ->
				raise (Error (Unify [cannot_unify eright.etype tleft], eright.epos));
			| _ -> ()
		end;
		if cf == ctx.curfield || rec_stack_memq cf cast_stack then error "Recursive implicit cast" p;
		rec_stack_loop cast_stack cf f ()
	in
	let find a tl f =
		let tcf,cf = f() in
		if (Meta.has Meta.MultiType a.a_meta) then
			mk_cast eright tleft p
		else match a.a_impl with
			| Some c -> recurse cf (fun () ->
				let ret = make_static_call ctx c cf a tl [eright] tleft p in
				{ ret with eexpr = TMeta( (Meta.ImplicitCast,[],ret.epos), ret) }
			)
			| None -> die "" __LOC__
	in
	if type_iseq tleft eright.etype then
		eright
	else begin
		let rec loop stack tleft tright =
			if List.exists (fun (tleft',tright') -> fast_eq tleft tleft' && fast_eq tright tright') stack then
				raise Not_found
			else begin
				let stack = (tleft,tright) :: stack in
				match follow tleft,follow tright with
				| TAbstract(a1,tl1),TAbstract(a2,tl2) ->
					Abstract.find_to_from uctx find a1 tl1 a2 tl2 tleft eright.etype
				| TAbstract(a,tl),_ ->
					begin try find a tl (fun () -> Abstract.find_from uctx a tl eright.etype tleft)
					with Not_found ->
						let rec loop2 tcl = match tcl with
							| tc :: tcl ->
								if not (type_iseq tc tleft) then loop stack (apply_params a.a_params tl tc) tright
								else loop2 tcl
							| [] -> raise Not_found
						in
						loop2 a.a_from
					end
				| _,TAbstract(a,tl) ->
					begin try find a tl (fun () -> Abstract.find_to uctx a tl tleft)
					with Not_found ->
						let rec loop2 tcl = match tcl with
							| tc :: tcl ->
								if not (type_iseq tc tright) then loop stack tleft (apply_params a.a_params tl tc)
								else loop2 tcl
							| [] -> raise Not_found
						in
						loop2 a.a_to
					end
				| _ ->
					raise Not_found
			end
		in
		loop [] tleft eright.etype
	end

and cast_or_unify_raise ctx tleft eright p =
	try
		(* can't do that anymore because this might miss macro calls (#4315) *)
		(* if ctx.com.display <> DMNone then raise Not_found; *)
		do_check_cast ctx tleft eright p
	with Not_found ->
		unify_raise ctx eright.etype tleft p;
		eright

and cast_or_unify ctx tleft eright p =
	try
		cast_or_unify_raise ctx tleft eright p
	with Error (Unify l,p) ->
		raise_or_display ctx l p;
		eright

let find_array_access_raise ctx a pl e1 e2o p =
	let is_set = e2o <> None in
	let ta = apply_params a.a_params pl a.a_this in
	let rec loop cfl =
		match cfl with
		| [] -> raise Not_found
		| cf :: cfl ->
			let monos = List.map (fun _ -> mk_mono()) cf.cf_params in
			let map t = apply_params a.a_params pl (apply_params cf.cf_params monos t) in
			let check_constraints () =
				List.iter2 (fun m (name,t) -> match follow t with
					| TInst ({ cl_kind = KTypeParameter constr },_) when constr <> [] ->
						List.iter (fun tc -> match follow m with TMono _ -> raise (Unify_error []) | _ -> Type.unify m (map tc) ) constr
					| _ -> ()
				) monos cf.cf_params;
			in
			let get_ta() =
				if has_meta Meta.Impl cf.cf_meta then ta
				else TAbstract(a,pl)
			in
			match follow (map cf.cf_type) with
			| TFun([(_,_,tab);(_,_,ta1);(_,_,ta2)],r) as tf when is_set ->
				begin try
					Type.unify tab (get_ta());
					let e1 = cast_or_unify_raise ctx ta1 e1 p in
					let e2o = match e2o with None -> None | Some e2 -> Some (cast_or_unify_raise ctx ta2 e2 p) in
					check_constraints();
					cf,tf,r,e1,e2o
				with Unify_error _ | Error (Unify _,_) ->
					loop cfl
				end
			| TFun([(_,_,tab);(_,_,ta1)],r) as tf when not is_set ->
				begin try
					Type.unify tab (get_ta());
					let e1 = cast_or_unify_raise ctx ta1 e1 p in
					check_constraints();
					cf,tf,r,e1,None
				with Unify_error _ | Error (Unify _,_) ->
					loop cfl
				end
			| _ -> loop cfl
	in
	loop a.a_array

let find_array_access ctx a tl e1 e2o p =
	try find_array_access_raise ctx a tl e1 e2o p
	with Not_found ->
		let s_type = s_type (print_context()) in
		match e2o with
		| None ->
			error (Printf.sprintf "No @:arrayAccess function for %s accepts argument of %s" (s_type (TAbstract(a,tl))) (s_type e1.etype)) p
		| Some e2 ->
			error (Printf.sprintf "No @:arrayAccess function for %s accepts arguments of %s and %s" (s_type (TAbstract(a,tl))) (s_type e1.etype) (s_type e2.etype)) p

let find_multitype_specialization com a pl p =
	let uctx = default_unification_context in
	let m = mk_mono() in
	let tl = match Meta.get Meta.MultiType a.a_meta with
		| _,[],_ -> pl
		| _,el,_ ->
			let relevant = Hashtbl.create 0 in
			List.iter (fun e ->
				let rec loop f e = match fst e with
					| EConst(Ident s) ->
						Hashtbl.replace relevant s f
					| EMeta((Meta.Custom ":followWithAbstracts",_,_),e1) ->
						loop Abstract.follow_with_abstracts e1;
					| _ ->
						error "Type parameter expected" (pos e)
				in
				loop (fun t -> t) e
			) el;
			let tl = List.map2 (fun (n,_) t ->
				try
					(Hashtbl.find relevant n) t
				with Not_found ->
					if not (has_mono t) then t
					else t_dynamic
			) a.a_params pl in
			if com.platform = Globals.Js && a.a_path = (["haxe";"ds"],"Map") then begin match tl with
				| t1 :: _ ->
					let stack = ref [] in
					let rec loop t =
						if List.exists (fun t2 -> fast_eq t t2) !stack then
							t
						else begin
							stack := t :: !stack;
							match follow t with
							| TAbstract ({ a_path = [],"Class" },_) ->
								error (Printf.sprintf "Cannot use %s as key type to Map because Class<T> is not comparable on JavaScript" (s_type (print_context()) t1)) p;
							| TEnum(en,tl) ->
								PMap.iter (fun _ ef -> ignore(loop ef.ef_type)) en.e_constrs;
								Type.map loop t
							| t ->
								Type.map loop t
						end
					in
					ignore(loop t1)
				| _ -> die "" __LOC__
			end;
			tl
	in
	let _,cf =
		try
			Abstract.find_to uctx a tl m
		with Not_found ->
			let at = apply_params a.a_params pl a.a_this in
			let st = s_type (print_context()) at in
			if has_mono at then
				error ("Type parameters of multi type abstracts must be known (for " ^ st ^ ")") p
			else
				error ("Abstract " ^ (s_type_path a.a_path) ^ " has no @:to function that accepts " ^ st) p;
	in
	cf, follow m

let handle_abstract_casts ctx e =
	let rec loop ctx e = match e.eexpr with
		| TNew({cl_kind = KAbstractImpl a} as c,pl,el) ->
			if not (Meta.has Meta.MultiType a.a_meta) then begin
				(* This must have been a @:generic expansion with a { new } constraint (issue #4364). In this case
					let's construct the underlying type. *)
				match Abstract.get_underlying_type a pl with
				| TInst(c,tl) as t -> {e with eexpr = TNew(c,tl,el); etype = t}
				| _ -> error ("Cannot construct " ^ (s_type (print_context()) (TAbstract(a,pl)))) e.epos
			end else begin
				(* a TNew of an abstract implementation is only generated if it is a multi type abstract *)
				let cf,m = find_multitype_specialization ctx.com a pl e.epos in
				let e = make_static_call ctx c cf a pl ((mk (TConst TNull) (TAbstract(a,pl)) e.epos) :: el) m e.epos in
				{e with etype = m}
			end
		| TCall({eexpr = TField(_,FStatic({cl_path=[],"Std"},{cf_name = "string"}))},[e1]) when (match follow e1.etype with TAbstract({a_impl = Some _},_) -> true | _ -> false) ->
			begin match follow e1.etype with
				| TAbstract({a_impl = Some c} as a,tl) ->
					begin try
						let cf = PMap.find "toString" c.cl_statics in
						make_static_call ctx c cf a tl [e1] ctx.t.tstring e.epos
					with Not_found ->
						e
					end
				| _ ->
					die "" __LOC__
			end
		| TCall(e1, el) ->
			begin try
				let rec find_abstract e t = match follow t,e.eexpr with
					| TAbstract(a,pl),_ when Meta.has Meta.MultiType a.a_meta -> a,pl,e
					| _,TCast(e1,None) -> find_abstract e1 e1.etype
					| _,TLocal {v_extra = Some(_,Some e')} ->
						begin match follow e'.etype with
						| TAbstract(a,pl) when Meta.has Meta.MultiType a.a_meta -> a,pl,mk (TCast(e,None)) e'.etype e.epos
						| _ -> raise Not_found
						end
					| _ -> raise Not_found
				in
				let rec find_field e1 =
					match e1.eexpr with
					| TCast(e2,None) ->
						{e1 with eexpr = TCast(find_field e2,None)}
					| TField(e2,fa) ->
						let a,pl,e2 = find_abstract e2 e2.etype in
						let m = Abstract.get_underlying_type a pl in
						let fname = field_name fa in
						let el = List.map (loop ctx) el in
						begin try
							let fa = quick_field m fname in
							let get_fun_type t = match follow t with
								| TFun(_,tr) as tf -> tf,tr
								| _ -> raise Not_found
							in
							let tf,tr = match fa with
								| FStatic(_,cf) -> get_fun_type cf.cf_type
								| FInstance(c,tl,cf) -> get_fun_type (apply_params c.cl_params tl cf.cf_type)
								| FAnon cf -> get_fun_type cf.cf_type
								| _ -> raise Not_found
							in
							let ef = mk (TField({e2 with etype = m},fa)) tf e2.epos in
							let ecall = make_call ctx ef el tr e.epos in
							if not (type_iseq ecall.etype e.etype) then
								mk (TCast(ecall,None)) e.etype e.epos
							else
								ecall
						with Not_found ->
							(* quick_field raises Not_found if m is an abstract, we have to replicate the 'using' call here *)
							match follow m with
							| TAbstract({a_impl = Some c} as a,pl) ->
								let cf = PMap.find fname c.cl_statics in
								make_static_call ctx c cf a pl (e2 :: el) e.etype e.epos
							| _ -> raise Not_found
						end
					| _ ->
						raise Not_found
				in
				find_field e1
			with Not_found ->
				Type.map_expr (loop ctx) e
			end
		| _ ->
			Type.map_expr (loop ctx) e
	in
	loop ctx e