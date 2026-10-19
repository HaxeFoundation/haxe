open Globals
open Type
open Typecore
open MatcherGlobals
open Constructor

type type_finiteness =
	| Infinite          (* type has inifite constructors (e.g. Int, String) *)
	| CompileTimeFinite (* type is considered finite only at compile-time but has inifite possible run-time values (enum abstracts) *)
	| RunTimeFinite     (* type is truly finite (Bool, enums) *)

(* How array patterns read the elements and the length of a subject. *)
type index_access =
	| IndexArray                            (* like those of an array *)
	| IndexAccess of ArrayAccess.array_access (* the way for loops read them (#8012) *)

(* How a switch tests its subject. *)
type match_kind =
	| SKValue                  (* against constants, statics and types *)
	| SKEnum                   (* by the index of its enum constructor *)
	| SKLength of index_access (* by its length *)

(* A switch's subject: its expression and what the switch makes of it, which [make] decides from the subject's type
   and the switch's constructors. *)
type t = {
	s_expr : texpr;
	s_kind : match_kind;
	s_finiteness : type_finiteness;
	s_constructors : Constructor.t list; (* the constructors of the subject's type *)
	s_nullable : bool;
}

class subject (e : texpr) (bind : Bind.bind option) = object (self)
	val mutable index_access : index_access option = None

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

	(* How array patterns read this subject, looked up when they first do. *)
	method get_index_access ctx = match index_access with
		| Some access ->
			access
		| None ->
			let access = match ExprToPattern.get_array_access ctx e.etype e.epos with
				| Some aa -> IndexAccess aa
				| None -> IndexArray
			in
			index_access <- Some access;
			access

	method to_string =
		s_expr_pretty e
end

let s_match_kind = function
	| SKValue -> "value"
	| SKEnum -> "enum"
	| SKLength _ -> "length"

let rec extract_ctor e = match e.eexpr with
	| TConst ct -> Some (ConConst ct)
	| TField(_,FEnum(en,ef)) -> Some (ConEnum(en,ef))
	| TCast(e1,None) -> extract_ctor e1
	| _ -> None

(* The subject of a switch on [subject] which uses it as [e] and has the constructors [cons], in the order of its
   cases. *)
let make ctx (subject : subject) e cons =
	let t = match follow e.etype,cons with
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
				| ConTypeExpr mt -> ExprToPattern.get_general_module_type ctx mt e.epos
				| ConFields _ | ConStatic _ -> fail()
			end
		| _ ->
			e.etype
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
		| TInst({cl_path=[],"Array"},_) ->
			SKLength IndexArray,Infinite
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
	(* Array patterns on values which can be accessed by index (#8012). *)
	let kind,finiteness = match cons with
		| (ConArray _,_) :: _ ->
			begin match subject#get_index_access ctx with
				| IndexAccess _ as access ->
					constructors := [];
					SKLength access,Infinite
				| IndexArray ->
					kind,finiteness
			end
		| _ ->
			kind,finiteness
	in
	{
		s_expr = e;
		s_kind = kind;
		s_finiteness = finiteness;
		s_constructors = List.rev !constructors;
		s_nullable = subject#is_nullable;
	}
