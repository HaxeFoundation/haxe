open Globals
open Ast
open Type
open Error

(*
	PurityState represents whether or not something has a side-effect. Unless otherwise stated
	by using `@:pure` (equivalent to `@:pure(true)`) or `@:pure(false)`, fields are originally
	supposed to be "maybe pure". Once all types and fields are known, this is refined by
	AnalyzerTexpr.Purity.

	There's a special case for fields that override a parent class field or implement an
	interface field: If the overridden/implemented field is explicitly marked as pure,
	the type loader marks the overriding/implementing as "expected pure". If during purity
	inference this assumption does not hold, an error is shown.
*)
type t =
	| Pure
	| Impure
	| MaybePure
	| InferredPure
	| ExpectPure of pos

let get_purity_from_meta meta =
	try
		begin match Meta.get Meta.Pure meta with
		| (_,[EConst(Ident s),p],_) ->
			begin match s with
			| "true" | "inferredPure" -> Pure
			| "false" -> Impure
			| "expect" -> ExpectPure p
			| _ -> error ("Unsupported purity value " ^ s ^ ", expected true or false") p
			end
		| (_,[],_) ->
			Pure
		| (_,_,p) ->
			error "Unsupported purity value" p
		end
	with Not_found ->
		MaybePure

let get_purity c cf = match get_purity_from_meta cf.cf_meta with
	| Pure | InferredPure -> Pure
	| Impure -> Impure
	| ExpectPure p -> ExpectPure p
	| _ -> get_purity_from_meta c.cl_meta

let is_pure c cf = get_purity c cf = Pure

let is_pure_field_access fa = match fa with
	| FInstance(c,_,cf) | FClosure(Some(c,_),cf) | FStatic(c,cf) -> is_pure c cf
	| FAnon cf | FClosure(None,cf) -> (get_purity_from_meta cf.cf_meta = Pure)
	| FEnum _ -> true
	| FDynamic _ -> false

let is_explicitly_impure fa = match fa with
	| FInstance(c,_,cf) | FClosure(Some(c,_),cf) | FStatic(c,cf) ->
		get_purity_from_meta cf.cf_meta = Impure
		|| get_purity_from_meta c.cl_meta = Impure
	| FAnon cf | FClosure(None,cf) ->
		get_purity_from_meta cf.cf_meta = Impure
	| _ -> false

let to_string = function
	| Pure -> "pure"
	| Impure -> "impure"
	| InferredPure -> "inferredPure"
	| MaybePure -> "maybe"
	| ExpectPure _ -> "expect"
