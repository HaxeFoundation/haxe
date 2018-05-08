open Ast
open Globals
open Common
open Type
open Meta

type context = {
	com : Common.context;
}

let jnull = Json.JNull
let jstring s = Json.JString s
let jint i = Json.JInt i
let jbool b = Json.JBool b
let jarray l = Json.JArray l
let jobject l = Json.JObject l

let jtodo = Json.JNull
let jopt f o = Option.map_default f Json.JNull o
let jlist f o = jarray (List.map f o)

let generate_path path =
	jobject [
		"pack",jarray (List.map jstring (fst path));
		"name",jstring (snd path)
	]

let generate_adt ctx tpath name args =
	let field = ("kind",jstring name) in
	let fields = match args with
		| None -> [field]
		| Some arg -> [field;("args",arg)]
	in
	jobject fields

let class_ref ctx c = generate_path c.cl_path
let enum_ref ctx en = generate_path en.e_path
let typedef_ref ctx td = generate_path td.t_path
let abstract_ref ctx a = generate_path a.a_path
let classfield_ref ctx cf = jstring cf.cf_name

let generate_pos ctx p =
	jobject [
		"file",jstring p.pfile;
		"min",jint p.pmin;
		"max",jint p.pmax;
	]

(* AST expr *)

let rec generate_binop ctx op =
	let name,args = match op with
	| OpAdd -> "OpAdd",None
	| OpMult -> "OpMult",None
	| OpDiv -> "OpDiv",None
	| OpSub -> "OpSub",None
	| OpAssign -> "OpAssign",None
	| OpEq -> "OpEq",None
	| OpNotEq -> "OpNotEq",None
	| OpGt -> "OpGt",None
	| OpGte -> "OpGte",None
	| OpLt -> "OpLt",None
	| OpLte -> "OpLte",None
	| OpAnd -> "OpAnd",None
	| OpOr -> "OpOr",None
	| OpXor -> "OpXor",None
	| OpBoolAnd -> "OpBoolAnd",None
	| OpBoolOr -> "OpBoolOr",None
	| OpShl -> "OpShl",None
	| OpShr -> "OpShr",None
	| OpUShr -> "OpUShr",None
	| OpMod -> "OpMod",None
	| OpAssignOp op -> "OpAssignOp", (Some (generate_binop ctx op))
	| OpInterval -> "OpInterval",None
	| OpArrow -> "OpArrow",None
	| OpIn -> "OpIn",None
	in
	generate_adt ctx (Some (["haxe";"macro"],"Binop")) name args

let generate_unop ctx op =
	let name = match op with
		| Increment -> "OpIncrement"
		| Decrement -> "OpDecrement"
		| Not -> "OpNot"
		| Neg -> "OpNeg"
		| NegBits -> "OpNegBits"
	in
	jstring name

let rec generate_expr ctx e =
	jtodo

(* metadata *)

and generate_metadata_entry ctx (m,el,p) =
	jobject [
		"name",jstring (Meta.to_string m);
		"params",jlist (generate_expr ctx) el;
		"pos",generate_pos ctx p;
	]

and generate_metadata ctx ml =
	let ml = List.filter (fun (m,_,_) ->
		let (_,(_,flags)) = Meta.get_info m in
		not (List.mem UsedInternally flags)
	) ml in
	jlist (generate_metadata_entry ctx) ml

(* type instance *)

let rec generate_type ctx t =
	let rec loop t = match t with
		| TMono r ->
			begin match !r with
			| None -> "TMono",None
			| Some t -> loop t
			end
		| TLazy f -> loop (lazy_type f)
		| TDynamic t -> "TDynamic",Some (if t == t_dynamic then jnull else generate_type ctx t)
		| TInst(c,tl) -> "TInst",Some (generate_path_with_params ctx c.cl_path tl)
		| TEnum(en,tl) -> "TEnum",Some (generate_path_with_params ctx en.e_path tl)
		| TType(td,tl) -> "TType",Some (generate_path_with_params ctx td.t_path tl)
		| TAbstract(a,tl) -> "TAbstract",Some (generate_path_with_params ctx a.a_path tl)
		| TAnon an -> "TAnonymous", Some(generate_anon an)
		| TFun(tl,tr) -> "TFun", Some (generate_function_signature tl tr)
	and generate_function_argument (name,opt,t) =
		jobject [
			"name",jstring name;
			"opt",jbool opt;
			"t",generate_type ctx t;
		]
	and generate_function_signature tl tr =
		jobject [
			"args",jlist generate_function_argument tl;
			"ret",generate_type ctx tr;
		]
	and generate_anon an =
		let generate_anon_fields () =
			let fields = PMap.fold (fun cf acc -> generate_class_field ctx cf :: acc) an.a_fields [] in
			jarray fields
		in
		let generate_anon_status () =
			let name,args = match !(an.a_status) with
				| Closed -> "AClosed",None
				| Opened -> "AOpened",None
				| Const -> "AConst",None
				| Extend tl -> "AExtend", Some (generate_types ctx tl)
				| Statics c -> "AClassStatics",Some (class_ref ctx c)
				| EnumStatics en -> "AEnumStatics",Some (enum_ref ctx en)
				| AbstractStatics a -> "AAbstractStatics", Some (abstract_ref ctx a)
			in
			generate_adt ctx None name args
		in
		jobject [
			"fields",generate_anon_fields();
			"status",generate_anon_status ();
		]
	in
	let name,args = loop t in
	generate_adt ctx None name args

and generate_types ctx tl =
	jlist (generate_type ctx) tl

and generate_path_with_params ctx path tl =
	jobject [
		"path",generate_path path;
		"params",generate_types ctx tl;
	]

(* type parameter *)

and generate_type_parameter ctx (s,t) =
	let generate_constraints () = match follow t with
		| TInst({cl_kind = KTypeParameter tl},_) -> generate_types ctx tl
		| _ -> assert false
	in
	jobject [
		"name",jstring s;
		"constraints",generate_constraints ();
	]

(* texpr *)

and generate_texpr ctx e =
	jtodo

(* fields *)

and generate_class_field ctx cf =
	let generate_class_kind () =
		let generate_var_access va =
			let name,args = match va with
				| AccNormal -> "AccNormal",None
				| AccNo -> "AccNo",None
				| AccNever -> "AccNever",None
				| AccCtor -> "AccCtor",None
				| AccResolve -> "AccResolve",None
				| AccCall -> "AccCall",None
				| AccInline -> "AccInline",None
				| AccRequire(s,so) -> "AccRequire",Some (jobject ["require",jstring s;"message",jopt jstring so])
			in
			generate_adt ctx None name args
		in
		let generate_method_kind m =
			let name = match m with
				| MethNormal -> "MethNormal"
				| MethInline -> "MethInline"
				| MethDynamic -> "MethDynamic"
				| MethMacro -> "MethMacro"
			in
			jstring name
		in
		let name,args = match cf.cf_kind with
			| Var vk -> "FVar",Some (jobject ["read",generate_var_access vk.v_read;"write",generate_var_access vk.v_write])
			| Method m -> "FMethod", Some (generate_method_kind m)
		in
		generate_adt ctx None name args
	in
	jobject [
		"name",jstring cf.cf_name;
		"type",generate_type ctx cf.cf_type;
		"isPublic",jbool cf.cf_public;
		"params",jlist (generate_type_parameter ctx) cf.cf_params;
		"meta",generate_metadata ctx cf.cf_meta;
		"kind",generate_class_kind ();
		"expr",jopt (generate_texpr ctx) cf.cf_expr;
		"pos",generate_pos ctx cf.cf_pos;
		"doc",jopt jstring cf.cf_doc;
		"overloads",jlist (classfield_ref ctx) cf.cf_overloads;
	]

let generate_enum_field ctx ef =
	jobject [
		"name",jstring ef.ef_name;
		"type",generate_type ctx ef.ef_type;
		"pos",generate_pos ctx ef.ef_pos;
		"meta",generate_metadata ctx ef.ef_meta;
		"index",jint ef.ef_index;
		"doc",jopt jstring ef.ef_doc;
		"params",jlist (generate_type_parameter ctx) ef.ef_params;
	]

(* module type *)

let generate_module_type_fields ctx inf =
	[
		"pack",jlist jstring (fst inf.mt_path);
		"name",jstring (snd inf.mt_path);
		"module",jstring (snd inf.mt_module.m_path);
		"pos",generate_pos ctx inf.mt_pos;
		"isPrivate",jbool inf.mt_private;
		"params",jlist (generate_type_parameter ctx) inf.mt_params;
		"meta",generate_metadata ctx inf.mt_meta;
		"doc",jopt jstring inf.mt_doc;
	]

let generate_class ctx c =
	let generate_class_kind ck =
		let ctor,args = match ck with
		| KNormal -> "KNormal",None
		| KTypeParameter tl -> "KTypeParameter",Some (generate_types ctx tl)
		| KExpr e -> "KExpr",Some (generate_expr ctx e)
		| KGeneric -> "KGeneric",None
		| KGenericInstance(c,tl) -> "KGenericInstance",Some (generate_path_with_params ctx c.cl_path tl)
		| KMacroType -> "KMacroType",None
		| KGenericBuild _ -> "KGenericBuild",None
		| KAbstractImpl a -> "KAbstractImpl",Some (abstract_ref ctx a)
		in
		generate_adt ctx (Some (["haxe";"macro"],"ClassKind")) ctor args
	in
	let generate_class_relation (c,tl) =
		jobject [
			"t",class_ref ctx c;
			"params",generate_types ctx tl;
		]
	in
	[
		"kind",generate_class_kind c.cl_kind;
		"isInterface",jbool c.cl_interface;
		"superClass",jopt generate_class_relation c.cl_super;
		"interfaces",jlist generate_class_relation c.cl_implements;
		"fields",jlist (generate_class_field ctx) c.cl_ordered_fields;
		"statics",jlist (generate_class_field ctx) c.cl_ordered_statics;
		"constructor",jopt (generate_class_field ctx) c.cl_constructor;
		"init",jopt (generate_texpr ctx) c.cl_init;
		"overrides",jlist (classfield_ref ctx) c.cl_overrides;
		"isExtern",jbool c.cl_extern;
	]

let generate_enum ctx e =
	let generate_enum_constructors () =
		jarray (List.map (fun s ->
			let ef = PMap.find s e.e_constrs in
			generate_enum_field ctx ef
		) e.e_names)
	in
	[
		"constructors",generate_enum_constructors ();
		"isExtern",jbool e.e_extern;
	]

let generate_typedef ctx td =
	[
		"type",generate_type ctx td.t_type;
	]

let generate_abstract ctx a =
	let generate_cast_relation t cfo =
		jobject [
			"t",generate_type ctx t;
			"field",jopt (classfield_ref ctx) cfo
		]
	in
	let generate_casts fields casts =
		let l1 = List.map (fun (t,cf) -> generate_cast_relation t (Some cf)) fields in
		let l2 = List.map (fun t -> generate_cast_relation t None) casts in
		jarray (l1 @ l2)
	in
	let generate_binop (op,cf) =
		jobject [
			"op",generate_binop ctx op;
			"field",classfield_ref ctx cf;
		]
	in
	let generate_unop (op,flag,cf) =
		jobject [
			"op",generate_unop ctx op;
			"postFix",jbool (flag = Postfix);
			"field",classfield_ref ctx cf;
		]
	in
	[
		"type",generate_type ctx a.a_this;
		"impl",jopt (class_ref ctx) a.a_impl;
		"binops",jlist generate_binop a.a_ops;
		"unops",jlist generate_unop a.a_unops;
		"from",generate_casts a.a_from_field a.a_from;
		"to",generate_casts a.a_to_field a.a_to;
		"array",jlist (classfield_ref ctx) a.a_array;
		"resolve",jopt (classfield_ref ctx) a.a_resolve;
	]

let generate_module_type ctx mt =
	let fields1 = generate_module_type_fields ctx (t_infos mt) in
	let kind,fields2 = match mt with
		| TClassDecl c -> "class",generate_class ctx c
		| TEnumDecl e -> "enum",generate_enum ctx e
		| TTypeDecl t -> "typedef",generate_typedef ctx t
		| TAbstractDecl a -> "abstract",generate_abstract ctx a
	in
	let fields1 = ("kind",jstring kind) :: fields1 @ [("args",jobject fields2)] in
	jobject fields1

let create_context com = {
	com = com;
}

let generate com file =
	let t = Timer.timer ["generate";"json";"construct"] in
	let ctx = create_context com in
	let json = jarray (List.map (generate_module_type ctx) com.types) in
	t();
	let t = Timer.timer ["generate";"json";"write"] in
	let ch = open_out_bin file in
	Json.write_json (output_string ch) json;
	close_out ch;
	t()