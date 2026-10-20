open Globals
open Type
open Typecore
open MatcherGlobals
open Constructor
open DecisionTree

(* Array patterns also apply to abstracts with a single `from` type leading to an array or to Dynamic when they can't
   be accessed by index (see ExprToPattern.get_array_access). For those, this reports what the typer reports for
   `e[i]` and `e.length`, the latter first if [length_first]. If the typer only displays its error, the matcher goes
   on like the typer does, reading the subject as Dynamic. *)
let no_array_access ctx e length_first =
	let element () = ignore(Calls.acc_get ctx (Calls.array_access ctx e (Texpr.Builder.make_int ctx.com.basic 0 e.epos) MGet e.epos)) in
	let length () = ignore(ExprToPattern.type_field_access ctx e "length") in
	if length_first then begin length (); element () end else begin element (); length () end;
	if not ctx.com.part_scope.has_error then Error.raise_typing_error ("Array access is not allowed on " ^ s_type e.etype) e.epos;
	{ ArrayAccess.aa_type = t_dynamic; aa_get = AGDynamic; aa_length = ALForward(t_dynamic,ALField) }

(* A value the matcher still has to match: its expression, a constant or a local, and the binding of that local if the
   matcher introduced it. A switch on it keeps a DecisionTree.scrutinee, see [make_scrutinee]. *)
class subject (e : texpr) (bind : Bind.bind option) = object (self)
	val mutable array_access : ArrayAccess.array_access option = None

	method get_expr =
		begin match bind with
		| None ->
			()
		| Some bind ->
			bind.b_status <- BindUsed;
		end;
		e

	method get_scoped_expr = match bind with
		| Some ({b_status = BindDeferred} as bind) ->
			(* For deferred bindings, we generate `subject = expr` and mark the binding as used so that
			   subsequent calls to this function do not generate the assignment again. The returned function
			   can be used to reset the status back to BindDeferred, for cases where we need to initalize
			   again in an unrelated scope. *)
			let e = Texpr.Builder.binop OpAssign e bind.b_expr e.etype e.epos in
			bind.b_status <- BindUsed;
			e,(fun () ->
				bind.b_status <- BindDeferred
			)
		| Some ({b_status = BindUnused} as bind) ->
			bind.b_status <- BindUsed;
			e,(fun () -> ())
		| _ ->
			e,(fun () -> ())

	method get_assigned_expr =
		let e,reset = self#get_scoped_expr in
		reset();
		e

	method get_pos =
		e.epos

	method is_nullable =
		is_explicit_null_or_abstract_over_that e.etype

	(* How array patterns read this subject, looked up when they first do: for its length if [length_first], else for
	   an element. *)
	method get_array_access ctx length_first = match array_access with
		| Some aa ->
			aa
		| None ->
			let aa = match ExprToPattern.get_array_access ctx e.etype e.epos with
				| Some aa -> aa
				| None -> no_array_access ctx e length_first
			in
			array_access <- Some aa;
			aa

	method to_string =
		s_expr_pretty e
end

(* The scrutinee of a switch on [subject] with the constructors [cons], in the order of its cases. The switch uses
   the subject's expression as [e_switch], which assigns the subject's deferred binding if the switch comes first. *)
let make_scrutinee ctx (subject : subject) e_switch cons =
	let t = match follow e_switch.etype,cons with
		| (TDynamic _ | TMono _),con :: _ ->
			let fail() =
				(* error "Could not determine switch kind, make sure the type is known" e.epos; *)
				t_dynamic
			in
			begin match fst con with
				| ConEnum(en,_) -> TEnum(en,extract_param_types en.e_params)
				| ConArray _ -> ctx.t.tarray t_dynamic
				| ConConst ct ->
					begin match ct with
						| TString _ -> ctx.t.tstring
						| TInt _ -> ctx.t.tint
						| TFloat _ -> ctx.t.tfloat
						| TBool _ -> ctx.t.tbool
						| _ -> fail()
					end
				| ConStatic({cl_kind = KAbstractImpl a},_) -> (TAbstract(a,extract_param_types a.a_params))
				| ConTypeExpr mt -> ExprToPattern.get_general_module_type ctx mt e_switch.epos
				| ConFields _ | ConStatic _ -> fail()
			end
		| _ ->
			e_switch.etype
	in
	let constructors = ref [] in
	let add constructor =
		constructors := constructor :: !constructors
	in
	let rec loop deep t = match follow t with
		| TAbstract({a_path = [],"Bool"},_) ->
			if not deep then begin
				add (ConConst(TBool true),null_pos);
				add (ConConst(TBool false),null_pos);
			end;
			SKValue,RunTimeFinite
		| TAbstract({a_impl = Some c} as a,pl) when a.a_enum ->
			if not deep then List.iter (fun cf ->
				ignore(follow cf.cf_type);
				if has_class_field_flag cf CfImpl && has_class_field_flag cf CfEnum then match cf.cf_expr with
					| Some e ->
						begin match extract_ctor e with
						| Some (ConConst TNull) -> ()
						| Some ctor -> add (ctor,null_pos)
						| None -> add (ConStatic(c,cf),null_pos)
						end;
					| _ -> add (ConStatic(c,cf),null_pos)
			) c.cl_ordered_statics;
			let real_kind,_ = loop true (Abstract.get_underlying_type a pl) in
			real_kind,CompileTimeFinite
		| TAbstract(a,pl) when not (Meta.has Meta.CoreType a.a_meta) ->
			loop deep (Abstract.get_underlying_type a pl)
		| TInst({cl_path=[],"String"},_)
		| TInst({cl_kind = KTypeParameter _ },_) ->
			SKValue,Infinite
		| TInst({cl_path=[],"Array"},_) as t ->
			SKLength (ArrayAccess.get_array_access ctx t e_switch.epos),Infinite
		| TEnum(en,pl) ->
			if not deep then
				PMap.iter (fun _ ef -> add (ConEnum(en,ef),null_pos)) en.e_constrs;
			SKEnum,RunTimeFinite
		| TAnon _ ->
			SKValue,Infinite
		| TInst(_,_) ->
			SKValue,Infinite
		| _ ->
			SKValue,Infinite
	in
	let kind,finiteness = loop false t in
	(* Array patterns, and switches without cases on an array or an abstract over one, test the subject's length (#8012). *)
	let kind,finiteness = match kind,cons with
		| _,(ConArray _,_) :: _ ->
			constructors := [];
			SKLength (subject#get_array_access ctx true),Infinite
		| SKLength _,[] ->
			SKLength (subject#get_array_access ctx true),finiteness
		| _ ->
			kind,finiteness
	in
	{
		scr_expr = e_switch;
		scr_kind = kind;
		scr_finiteness = finiteness;
		scr_constructors = List.rev !constructors;
		scr_nullable = subject#is_nullable;
	}
