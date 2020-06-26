open Globals
open TType
open TUnification
open TFunctions
open TPrinting
open TOther

type call_error =
	| Not_enough_arguments of (string * bool * t) list
	| Too_many_arguments
	| Could_not_unify of error_msg
	| Cannot_skip_non_nullable of string

and error_msg =
	| Module_not_found of path
	| Type_not_found of path * string * type_not_found_reason
	| Unify of unify_error list
	| Custom of string
	| Unknown_ident of string
	| Stack of error_msg * error_msg
	| Call_error of call_error
	| No_constructor of module_type

and type_not_found_reason =
	| Private_type
	| Not_defined

exception Fatal_error of string * Globals.pos
exception Error of error_msg * Globals.pos

let string_source t = match follow t with
	| TInst(c,tl) -> PMap.foldi (fun s _ acc -> s :: acc) (TClass.get_all_fields c tl) []
	| TAnon a -> PMap.fold (fun cf acc -> cf.cf_name :: acc) a.a_fields []
	| TAbstract({a_impl = Some c},_) -> List.map (fun cf -> cf.cf_name) c.cl_ordered_statics
	| _ -> []

let short_type ctx t =
	let tstr = s_type ctx t in
	if String.length tstr > 150 then String.sub tstr 0 147 ^ "..." else tstr

let unify_error_msg ctx err = match err with
	| Cannot_unify (t1,t2) ->
		s_type ctx t1 ^ " should be " ^ s_type ctx t2
	| Invalid_field_type s ->
		"Invalid type for field " ^ s ^ " :"
	| Has_no_field (t,n) ->
		StringError.string_error n (string_source t) (short_type ctx t ^ " has no field " ^ n)
	| Has_no_runtime_field (t,n) ->
		s_type ctx t ^ "." ^ n ^ " is not accessible at runtime"
	| Has_extra_field (t,n) ->
		short_type ctx t ^ " has extra field " ^ n
	| Invalid_kind (f,a,b) ->
		(match a, b with
		| Var va, Var vb ->
			let name, stra, strb = if va.v_read = vb.v_read then
				"setter", s_access false va.v_write, s_access false vb.v_write
			else if va.v_write = vb.v_write then
				"getter", s_access true va.v_read, s_access true vb.v_read
			else
				"access", "(" ^ s_access true va.v_read ^ "," ^ s_access false va.v_write ^ ")", "(" ^ s_access true vb.v_read ^ "," ^ s_access false vb.v_write ^ ")"
			in
			"Inconsistent " ^ name ^ " for field " ^ f ^ " : " ^ stra ^ " should be " ^ strb
		| _ ->
			"Field " ^ f ^ " is " ^ s_kind a ^ " but should be " ^ s_kind b)
	| Invalid_visibility n ->
		"The field " ^ n ^ " is not public"
	| Not_matching_optional n ->
		"Optional attribute of parameter " ^ n ^ " differs"
	| Cant_force_optional ->
		"Optional parameters can't be forced"
	| Invariant_parameter _ ->
		"Type parameters are invariant"
	| Constraint_failure name ->
		"Constraint check failure for " ^ name
	| Missing_overload (cf, t) ->
		cf.cf_name ^ " has no overload for " ^ s_type ctx t
	| FinalInvariance ->
		"Cannot unify final and non-final fields"
	| Invalid_function_argument(i,_) ->
		Printf.sprintf "Cannot unify argument %i" i
	| Invalid_return_type ->
		"Cannot unify return types"
	| Unify_custom msg ->
		msg

module BetterErrors = struct
	type access_kind =
		| Field of string
		| FunctionArgument of int * int
		| FunctionReturn
		| TypeParameter of int
		| Root

	type access = {
		acc_kind : access_kind;
		mutable acc_expected : TType.t;
		mutable acc_actual : TType.t;
		mutable acc_messages : unify_error list;
		mutable acc_next : access option;
	}

	let s_access_kind = function
		| Field s -> "Field " ^ s
		| FunctionArgument(i,l) -> Printf.sprintf "FunctionArgument(%i, %i)" i l
		| FunctionReturn -> "FunctionReturn"
		| TypeParameter i -> Printf.sprintf "TypeParameter %i" i
		| Root -> "Root"

	let get_access_chain ctx l =
		let make_acc kind actual expected = {
			acc_kind = kind;
			acc_expected = expected;
			acc_actual = actual;
			acc_messages = [];
			acc_next = None;
		} in
		let root_acc = make_acc Root t_dynamic t_dynamic in
		let current_acc = ref root_acc in
		let add_message msg =
			!current_acc.acc_messages <- msg :: !current_acc.acc_messages
		in
		let add_access kind =
			let acc = make_acc kind t_dynamic t_dynamic in
			!current_acc.acc_next <- Some acc;
			current_acc := acc;
		in
		List.iter (fun err -> match err with
			| Cannot_unify(t1,t2) ->
				!current_acc.acc_actual <- t1;
				!current_acc.acc_expected <- t2;
				add_message err
			| Invalid_field_type s ->
				add_access (Field s);
			| Invalid_function_argument(i,l) ->
				add_access (FunctionArgument(i,l));
			| Invalid_return_type ->
				add_access FunctionReturn;
			| Invariant_parameter i ->
				add_access (TypeParameter i);
			| _ ->
				add_message err
		) l;
		root_acc

	(* non-recursive s_type *)
	let rec s_type ctx t =
		match t with
		| TMono r ->
			(match r.tm_type with
			| None -> Printf.sprintf "Unknown<%d>" (try List.assq t (!ctx) with Not_found -> let n = List.length !ctx in ctx := (t,n) :: !ctx; n)
			| Some t -> s_type ctx t)
		| TEnum (e,tl) ->
			s_type_path e.e_path ^ s_type_params ctx tl
		| TInst (c,tl) ->
			(match c.cl_kind with
			| KExpr e -> Ast.Printer.s_expr e
			| _ -> s_type_path c.cl_path ^ s_type_params ctx tl)
		| TType (t,tl) ->
			s_type_path t.t_path ^ s_type_params ctx tl
		| TAbstract (a,tl) ->
			s_type_path a.a_path ^ s_type_params ctx tl
		| TFun ([],_) ->
			"() -> ..."
		| TFun (l,t) ->
			let args = match l with
				| [] -> "()"
				| ["",b,t] -> ("...")
				| _ ->
					let args = String.concat ", " (List.map (fun (s,b,t) ->
						(if b then "?" else "") ^ ("...")
					) l) in
					"(" ^ args ^ ")"
			in
			Printf.sprintf "%s -> ..." args
		| TAnon a ->
			begin
				match !(a.a_status) with
				| Statics c -> Printf.sprintf "{ Statics %s }" (s_type_path c.cl_path)
				| EnumStatics e -> Printf.sprintf "{ EnumStatics %s }" (s_type_path e.e_path)
				| AbstractStatics a -> Printf.sprintf "{ AbstractStatics %s }" (s_type_path a.a_path)
				| _ ->
					let fl = PMap.fold (fun f acc -> ((if Meta.has Meta.Optional f.cf_meta then " ?" else " ") ^ f.cf_name) :: acc) a.a_fields [] in
					"{" ^ String.concat "," fl ^ " }"
			end
		| TDynamic t2 ->
			"Dynamic" ^ s_type_params ctx (if t == t2 then [] else [t2])
		| TLazy f ->
			s_type ctx (lazy_type f)

	and s_type_params ctx = function
		| [] -> ""
		| l -> "<" ^ String.concat ", " (List.map (fun _ -> "...") l) ^ ">"

	let better_error_message l =
		let ctx = print_context() in
		let rec loop acc l = match l with
			| (Cannot_unify _) as err1 :: (Cannot_unify _) :: l ->
				loop acc (err1 :: l)
			| x :: l ->
				loop (x :: acc) l
			| [] ->
				List.rev acc
		in
		let l = loop [] l in
		let access = get_access_chain ctx l in
		let message_buffer = Buffer.create 0 in
		let rec fill s i acc k l =
			if l = 0 then
				List.rev acc
			else begin
				if k = i then fill s i (s :: acc) (k + 1) (l - 1)
				else fill s i ("..." :: acc) (k + 1) (l - 1)
			end
		in
		let rec loop access access_prev =
			let loop () = match access.acc_next with
				| Some access' -> loop access' access
				| None ->
					begin match access.acc_messages with
						| err :: _ ->
							let msg = unify_error_msg ctx err in
							Buffer.add_string message_buffer msg;
						| [] ->
							()
					end;
					s_type ctx access.acc_actual,s_type ctx access.acc_expected
			in
			begin match access.acc_kind with
			| Field s ->
				let s1,s2 = loop() in
				Printf.sprintf "{ %s: %s }" s s1,Printf.sprintf "{ %s: %s }" s s2
			| FunctionArgument(i,l) ->
				let s1,s2 = loop() in
				let sl1 = fill s1 i [] 1 l in
				let sl2 = fill s2 i [] 1 l in
				Printf.sprintf "(%s) -> ..." (String.concat ", " sl2),Printf.sprintf "(%s) -> ..." (String.concat ", " sl1)
			| FunctionReturn ->
				let s1,s2 = loop() in
				Printf.sprintf "(...) -> %s" s1,Printf.sprintf "(...) -> %s" s2
			| TypeParameter i ->
				let rec get_params t = match t with
					| TInst({cl_path = path},params) | TEnum({e_path = path},params) | TAbstract({a_path = path},params) | TType({t_path = path},params) ->
						path,params
					| _ ->
						die "" __LOC__
				in
				let s1,s2 = loop() in
				let path1,params1 = get_params access_prev.acc_actual in
				let path2,params2 = get_params access_prev.acc_expected in
				let sl1 = fill s1 i [] 1 (List.length params1) in
				let sl2 = fill s2 i [] 1 (List.length params2) in
				Printf.sprintf "%s<%s>" (s_type_path path1) (String.concat ", " sl1),Printf.sprintf "%s<%s>" (s_type_path path2) (String.concat ", " sl2)
			| Root ->
				loop()
			end;
		in
		match access.acc_next with
		| None ->
			String.concat "\n" (List.rev_map (unify_error_msg ctx) access.acc_messages)
		| Some access_next ->
			let slhs,srhs = loop access_next access  in
			Printf.sprintf "error: %s\nhave: %s\nwant: %s" (Buffer.contents message_buffer) slhs srhs
end

let rec error_msg = function
	| Module_not_found m -> "Type not found : " ^ s_type_path m
	| Type_not_found (m,t,Private_type) -> "Cannot access private type " ^ t ^ " in module " ^ s_type_path m
	| Type_not_found (m,t,Not_defined) -> "Module " ^ s_type_path m ^ " does not define type " ^ t
	| Unify l -> BetterErrors.better_error_message l
	| Unknown_ident s -> "Unknown identifier : " ^ s
	| Custom s -> s
	| Stack (m1,m2) -> error_msg m1 ^ "\n" ^ error_msg m2
	| Call_error err -> s_call_error err
	| No_constructor mt -> (s_type_path (t_infos mt).mt_path ^ " does not have a constructor")

and s_call_error = function
	| Not_enough_arguments tl ->
		let pctx = print_context() in
		"Not enough arguments, expected " ^ (String.concat ", " (List.map (fun (n,_,t) -> n ^ ":" ^ (short_type pctx t)) tl))
	| Too_many_arguments -> "Too many arguments"
	| Could_not_unify err -> error_msg err
	| Cannot_skip_non_nullable s -> "Cannot skip non-nullable argument " ^ s

let error msg p = raise (Error (Custom msg,p))

let raise_error err p = raise (Error(err,p))

let error_require r p =
	if r = "" then
		error "This field is not available with the current compilation flags" p
	else
	let r = if r = "sys" then
		"a system platform (php,neko,cpp,etc.)"
	else try
		if String.sub r 0 5 <> "flash" then raise Exit;
		let _, v = ExtString.String.replace (String.sub r 5 (String.length r - 5)) "_" "." in
		"flash version " ^ v ^ " (use -swf-version " ^ v ^ ")"
	with _ ->
		"'" ^ r ^ "' to be enabled"
	in
	error ("Accessing this field requires " ^ r) p

let invalid_assign p = error "Invalid assign" p