open Globals
open Type
open Typecore
open MatcherGlobals
open Constructor
open DecisionTree

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

	(* How array patterns read this subject, looked up when they first do. They are only typed against values which
	   can be accessed by index (see ExprToPattern), but the subject's own type can differ from the one its patterns
	   were typed against:
	   - an element of a Dynamic value, or an enum argument read through a cast, is a monomorph, which this unifies
	     with an array like the typer does for `e[i]` (Calls.array_access);
	   - an element of an abstract has the type of its @:arrayAccess getter, which can differ from the `from` type. *)
	method get_array_access ctx = match array_access with
		| Some aa ->
			aa
		| None ->
			begin match follow e.etype with
				| TMono _ ->
					(try unify_raise e.etype (ctx.t.tarray (spawn_monomorph ctx e.epos)) e.epos with Error.Error { err_message = Unify _ } -> ())
				| _ ->
					()
			end;
			let aa = match ExprToPattern.get_array_access ctx e.etype e.epos with
				| Some aa -> aa
				| None -> ExprToPattern.no_array_access e.etype e.epos
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
			(* Array patterns replace this with the subject's own access below, a switch without cases with SKValue, and
			   other constructors don't match this kind (see TexprConverter.unmatched_ctors): the access of `t` is never
			   read. *)
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
	(* Array patterns test the subject's length (#8012). A switch without cases doesn't test its subject at all. *)
	let kind,finiteness = match kind,cons with
		| _,(ConArray _,_) :: _ ->
			constructors := [];
			SKLength (subject#get_array_access ctx),Infinite
		| SKLength _,[] ->
			SKValue,finiteness
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
