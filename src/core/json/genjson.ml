open Ast
open Globals
open Type
open Meta

type generation_mode =
	| GMFull
	| GMWithoutDoc
	| GMMinimum

type context = {
	generation_mode : generation_mode;
	generate_abstract_impl : bool;
	request : JsonRequest.json_request option
}

let jnull = Json.JNull
let jstring s = Json.JString s
let jint i = Json.JInt i
let jfloat f = Json.JFloat f
let jbool b = Json.JBool b
let jarray l = Json.JArray l
let jobject l = Json.JObject l

let jtodo = Json.JNull
let jopt f o = Option.map_default f Json.JNull o
let jlist f o = jarray (List.map f o)

let generate_package_path' pack = [
	("pack",jarray (List.map jstring pack))
]

let generate_package_path pack = jobject (generate_package_path' pack)

let generate_module_path' mpath =
	("moduleName",jstring (snd mpath)) ::
	generate_package_path' (fst mpath)

let generate_module_path mpath = jobject (generate_module_path' mpath)

let generate_type_path' mpath tpath meta =
	("typeName",jstring (snd tpath)) ::
	generate_module_path' mpath

let generate_type_path mpath tpath meta =
 	let rec loop = function
 		| [] -> tpath
 		| (Meta.RealPath,[(Ast.EConst (Ast.String(s,_)),_)],_) :: _ -> parse_path s
 		| _ :: l -> loop l
 	in
 	let tpath = loop meta in
	jobject (generate_type_path' mpath tpath meta)

let generate_adt ctx tpath name args =
	let field = ("kind",jstring name) in
	let fields = match args with
		| None -> [field]
		| Some arg -> [field;("args",arg)]
	in
	jobject fields

let field_name name meta =
	try
 		begin match Meta.get Meta.RealPath meta with
 			| _,[EConst (String (s,_)),_],_ -> s
 			| _ -> raise Not_found
 		end;
 	with Not_found ->
 		name

let class_ref ctx c = generate_type_path c.cl_module.m_path c.cl_path c.cl_meta
let enum_ref ctx en = generate_type_path en.e_module.m_path  en.e_path en.e_meta
let typedef_ref ctx td = generate_type_path td.t_module.m_path td.t_path td.t_meta
let abstract_ref ctx a = generate_type_path a.a_module.m_path a.a_path a.a_meta
let moduletype_ref ctx mt = generate_module_path (t_path mt)
let classfield_ref ctx cf = jstring (field_name cf.cf_name cf.cf_meta)
let enumfield_ref ctx ef = jstring (field_name ef.ef_name ef.ef_meta)
let local_ref ctx v = jint v.v_id

let generate_pos ctx p =
	jobject [
		"file",jstring p.pfile;
		"min",jint p.pmin;
		"max",jint p.pmax;
	]

let generate_expr_pos ctx p =
	jtodo

let generate_doc ctx d = match ctx.generation_mode with
	| GMFull -> jopt jstring (gen_doc_text_opt d)
	| GMWithoutDoc | GMMinimum -> jnull

(** return a range JSON structure for given position
    positions are 0-based and the result object looks like this:
    {
        start: {line: 0, character: 0},
        end: {line: 3, character: 42},
    }
*)
let pos_to_range p =
	let l1, p1, l2, p2 = Lexer.get_pos_coords p in
	let to_json l c = jobject [("line", jint (l - 1)); ("character", jint (c - 1))] in
	[
		("start", to_json l1 p1);
		("end", to_json l2 p2);
	]

let generate_pos_as_range p =
	if p.pmin = -1 then jnull
	else jobject (pos_to_range p)

let generate_pos_as_location p =
	if p.pmin = -1 then
		jnull
	else
		jobject [("file",jstring (Path.get_real_path p.pfile));"range",generate_pos_as_range p]

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
		"args",jlist (generate_expr ctx) el;
		"pos",generate_pos ctx p;
	]

and generate_metadata ctx ml =
	let ml = List.filter (fun (m,_,_) ->
		let (_,(_,flags)) = Meta.get_info m in
		not (List.mem UsedInternally flags)
	) ml in
	jlist (generate_metadata_entry ctx) ml

and generate_minimum_metadata ctx ml =
	match ctx.request with
		| None -> None
		| Some request ->
			match request#get_requested_meta_list with
				| None -> None
				| Some requested ->
					let ml =
						List.filter
							(fun (m,_,_) -> List.exists (fun r -> r = to_string m) requested)
							ml
					in
					Some (jlist (generate_metadata_entry ctx) ml)

(* AST.ml structures *)

let rec generate_ast_type_param ctx tp = jobject [
	"name",jstring (fst tp.tp_name);
	"params",jlist (generate_ast_type_param ctx) tp.tp_params;
	"constraints",jtodo;
	"metadata",generate_metadata ctx tp.tp_meta
]

(* type instance *)

let rec generate_type ctx t =
	let rec loop t = match t with
		| TMono r ->
			begin match r.tm_type with
			| None -> "TMono",None
			| Some t -> loop t
			end
		| TLazy f ->
			(* return_partial_type := true; *)
			let t = lazy_type f in
			(* return_partial_type := false; *)
			loop t
		| TDynamic t -> "TDynamic",Some (if t == t_dynamic then jnull else generate_type ctx t)
		| TInst(c,tl) -> "TInst",Some (generate_type_path_with_params ctx c.cl_module.m_path c.cl_path tl c.cl_meta)
		| TEnum(en,tl) -> "TEnum",Some (generate_type_path_with_params ctx en.e_module.m_path en.e_path tl en.e_meta)
		| TType(td,tl) -> "TType",Some (generate_type_path_with_params ctx td.t_module.m_path td.t_path tl td.t_meta)
		| TAbstract(a,tl) -> "TAbstract",Some (generate_type_path_with_params ctx a.a_module.m_path a.a_path tl a.a_meta)
		| TAnon an -> "TAnonymous", Some(generate_anon ctx an)
		| TFun(tl,tr) -> "TFun", Some (jobject (generate_function_signature ctx tl tr))
	in
	let name,args = loop t in
	generate_adt ctx None name args

and generate_anon_status ctx status =
	let name,args = match status with
		| Closed -> "AClosed",None
		| Const -> "AConst",None
		| Extend tl -> "AExtend", Some (generate_types ctx tl)
		| Statics c -> "AClassStatics",Some (class_ref ctx c)
		| EnumStatics en -> "AEnumStatics",Some (enum_ref ctx en)
		| AbstractStatics a -> "AAbstractStatics", Some (abstract_ref ctx a)
	in
	generate_adt ctx None name args

and generate_anon ctx an =
	let generate_anon_fields () =
		let fields = PMap.fold (fun cf acc -> generate_class_field ctx CFSMember cf :: acc) an.a_fields [] in
		jarray fields
	in
	jobject [
		"fields",generate_anon_fields();
		"status",generate_anon_status ctx !(an.a_status);
	]

and generate_function_argument ctx (name,opt,t) =
	jobject [
		"name",jstring name;
		"opt",jbool opt;
		"t",generate_type ctx t;
	]

and generate_function_signature ctx tl tr =
	[
		"args",jlist (generate_function_argument ctx) tl;
		"ret",generate_type ctx tr;
	]

and generate_types ctx tl =
	jlist (generate_type ctx) tl

and generate_type_path_with_params ctx mpath tpath tl meta =
	jobject [
		"path",generate_type_path mpath tpath meta;
		"params",generate_types ctx tl;
	]

(* type parameter *)

and generate_type_parameter ctx (s,t) =
	let generate_constraints () = match follow t with
		| TInst({cl_kind = KTypeParameter tl},_) -> generate_types ctx tl
		| _ -> die "" __LOC__
	in
	jobject [
		"name",jstring s;
		"constraints",generate_constraints ();
	]

(* texpr *)

and generate_tvar ctx v =
	let generate_extra ve =
		let (params,eo) = (ve.v_params,ve.v_expr) in
		jobject (
		("params",jlist (generate_type_parameter ctx) params) ::
		(match eo with
		| None -> []
		| Some e ->	["expr",jobject [
			("string",jstring (s_expr_pretty false "" false (s_type (print_context())) e))
		]]);
	) in
	let fields = [
		"id",jint v.v_id;
		"name",jstring v.v_name;
		"type",generate_type ctx v.v_type;
		"capture",jbool (has_var_flag v VCaptured);
		"extra",jopt generate_extra v.v_extra;
		"meta",generate_metadata ctx v.v_meta;
		"pos",generate_pos ctx v.v_pos;
		"isFinal",jbool (has_var_flag v VFinal);
		"isInline",jbool (match v.v_extra with Some {v_expr = Some _} -> true | _ -> false);
	] in
	let origin_to_int = function
		| TVOLocalVariable -> 0
		| TVOArgument -> 1
		| TVOForVariable -> 2
		| TVOPatternVariable -> 3
		| TVOCatchVariable -> 4
		| TVOLocalFunction -> 5
	in
	let fields = match v.v_kind with
			| VUser origin -> ("origin",jint (origin_to_int origin)) :: fields
			| _ -> fields
	in
	jobject fields

and generate_tconstant ctx ct =
	let name,args = match ct with
		| TInt i32 -> "TInt",Some (jstring (Int32.to_string i32))
		| TFloat s -> "TFloat",Some (jstring s)
		| TString s -> "TString",Some (jstring s)
		| TBool b -> "TBool",Some (jbool b)
		| TNull -> "TNull",None
		| TThis -> "TThis",None
		| TSuper -> "TSuper",None
	in
	generate_adt ctx None name args

and generate_tfunction ctx tf =
	let generate_arg (v,cto) = jobject [
		"v",generate_tvar ctx v;
		"value",jopt (generate_texpr ctx) cto;
	] in
	jobject [
		"args",jlist generate_arg tf.tf_args;
		"ret",generate_type ctx tf.tf_type;
		"expr",generate_expr ctx tf.tf_expr;
	]

and generate_texpr ctx e =
	jtodo
	(* let name,args = match e.eexpr with
	| TConst ct ->
		"TConst",Some (generate_tconstant ctx ct)
	| TLocal v ->
		"TLocal",Some (local_ref ctx v)
	| TArray(e1,e2) ->
		"TArray",Some (jobject [
			"expr1",generate_texpr ctx e1;
			"expr2",generate_texpr ctx e2;
		])
	| TBinop(op,e1,e2) ->
		"TBinop",Some (jobject [
			"op",generate_binop ctx op;
			"expr1",generate_texpr ctx e1;
			"expr2",generate_texpr ctx e2;
		]);
	| TField(e1,fa) ->
		"TField",Some (jobject [
			"expr",generate_texpr ctx e1;
			"name",jstring (field_name fa);
			(* TODO *)
		]);
	| TTypeExpr mt ->
		"TTypeExpr",Some (moduletype_ref ctx mt)
	| TParenthesis e1 ->
		"TParenthesis",Some (generate_texpr ctx e1)
	| TObjectDecl fl ->
		let generate_quote_status qs =
			let name = match qs with
				| DoubleQuotes -> "DoubleQuotes"
				| NoQuotes -> "NoQuotes"
			in
			generate_adt ctx None name None
		in
		let generate_key (name,pos,qs) = jobject [
			"name",jstring name;
			"pos",generate_expr_pos ctx pos;
			"quoteStatus",generate_quote_status qs;
		] in
		let generate_entry (key,value) = jobject [
			"key",generate_key key;
			"value",generate_texpr ctx value;
		] in
		let fields = List.map generate_entry fl in
		"TObjectDecl",Some (jarray fields)
	| TArrayDecl el ->
		let fields = List.map (generate_texpr ctx) el in
		"TArrayDecl",Some (jarray fields)
	| TCall(e1,el) ->
		let args = List.map (generate_texpr ctx) el in
		"TCall",Some (jobject [
			"expr",generate_texpr ctx e1;
			"args",jarray args;
		]);
	| TNew(c,tl,el) ->
		let args = List.map (generate_texpr ctx) el in
		"TNew",Some (jobject [
			"path",generate_type_path_with_params ctx c.cl_path tl;
			"args",jarray args;
		]);
	| TUnop(op,flag,e1) ->
		"TUnop",Some (jobject [
			"op",generate_unop ctx op;
			"prefix",jbool (flag = Prefix);
			"expr",generate_texpr ctx e1;
		]);
	| TFunction tf ->
		"TFunction",Some (generate_tfunction ctx tf)
	| TVar(v,eo) ->
		"TVar",Some (jobject [
			"v",generate_tvar ctx v;
			"expr",jopt (generate_texpr ctx) eo;
		])
	| TBlock el ->
		let el = List.map (generate_texpr ctx) el in
		"TBlock",Some (jarray el)
	| TFor(v,e1,e2) ->
		"TFor",Some (jobject [
			"v",generate_tvar ctx v;
			"expr1",generate_texpr ctx e1;
			"expr2",generate_texpr ctx e2;
		]);
	| TIf(e1,e2,eo) ->
		"TIf",Some (jobject [
			"eif",generate_texpr ctx e1;
			"ethen",generate_expr ctx e1;
			"eelse",jopt (generate_expr ctx) eo;
		]);
	| TWhile(e1,e2,flag) ->
		"TWhile",Some (jobject [
			"econd",generate_texpr ctx e1;
			"ebody",generate_texpr ctx e2;
			"isDoWhile",jbool (flag = DoWhile);
		]);
	| TSwitch(e1,cases,edef) ->
		let generate_case (el,e) = jobject [
			"patterns",jlist (generate_texpr ctx) el;
			"expr",generate_texpr ctx e;
		] in
		"TSwitch",Some (jobject [
			"subject",generate_texpr ctx e1;
			"cases",jlist generate_case cases;
			"def",jopt (generate_texpr ctx) edef;
		])
	| TTry(e1,catches) ->
		let generate_catch (v,e) = jobject [
			"v",generate_tvar ctx v;
			"expr",generate_texpr ctx e;
		] in
		"TTry",Some (jobject [
			"expr",generate_texpr ctx e1;
			"catches",jlist generate_catch catches;
		])
	| TReturn eo ->
		"TReturn",Option.map (generate_texpr ctx) eo
	| TBreak ->
		"TBreak",None
	| TContinue ->
		"TContinue",None
	| TThrow e1 ->
		"TThrow",Some (generate_texpr ctx e1)
	| TCast(e1,mto) ->
		"TCast",Some (jobject [
			"expr",generate_texpr ctx e1;
			"moduleType",jopt (moduletype_ref ctx) mto;
		]);
	| TMeta(m,e1) ->
		"TMeta",Some (jobject [
			"meta",generate_metadata_entry ctx m;
			"expr",generate_texpr ctx e1;
		])
	| TEnumParameter(e1,ef,i) ->
		"TEnumParameter",Some (jobject [
			"expr",generate_texpr ctx e1;
			"enumField",enumfield_ref ctx ef;
			"index",jint i;
		]);
	| TEnumIndex e1 ->
		"TEnumIndex",Some (generate_texpr ctx e1)
	| TIdent s ->
		"TIdent",Some (jstring s)
	in
	jobject [
		"expr",generate_adt ctx None name args;
		(* TODO: pos? *)
		"type",generate_type ctx e.etype;
	] *)

(* fields *)

and generate_class_field' ctx cfs cf =
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
	let expr = match ctx.generation_mode with
		| GMFull | GMWithoutDoc ->
			let value = match cf.cf_kind with
				| Method _ -> None
				| Var _ ->
					try
						begin match Meta.get Meta.Value cf.cf_meta with
							| (_,[e],_) -> Some e
							| _ -> None
						end
					with Not_found ->
						None
			in
			begin match value with
				| None ->
					if Meta.has (Meta.Custom ":testHack") cf.cf_meta then begin match cf.cf_expr with
						| Some e -> jobject ["testHack",jstring (s_expr_pretty false "" false (s_type (print_context())) e)] (* TODO: haha *)
						| None -> jnull
					end else
						jnull
				| Some e -> jobject ["string",jstring (Ast.Printer.s_expr e)]
			end
		| GMMinimum ->
			jnull
	in
	[
		"name",jstring (field_name cf.cf_name cf.cf_meta);
		"type",generate_type ctx cf.cf_type;
		"isPublic",jbool (has_class_field_flag cf CfPublic);
		"isFinal",jbool (has_class_field_flag cf CfFinal);
		"params",jlist (generate_type_parameter ctx) cf.cf_params;
		"meta",generate_metadata ctx cf.cf_meta;
		"kind",generate_class_kind ();
		"expr",expr;
		"pos",generate_pos ctx cf.cf_pos;
		"doc",generate_doc ctx cf.cf_doc;
		"overloads",jlist (generate_class_field ctx cfs) cf.cf_overloads;
		"scope",jint (Obj.magic cfs);
	]

and generate_class_field ctx cfs cf =
	jobject (generate_class_field' ctx cfs cf)

let generate_enum_field ctx ef =
	jobject [
		"name",jstring (field_name ef.ef_name ef.ef_meta);
		"type",generate_type ctx ef.ef_type;
		"pos",generate_pos ctx ef.ef_pos;
		"meta",generate_metadata ctx ef.ef_meta;
		"index",jint ef.ef_index;
		"doc",generate_doc ctx ef.ef_doc;
		"params",jlist (generate_type_parameter ctx) ef.ef_params;
	]

(* module type *)

let generate_module_type_fields ctx inf =
	[
		"pack",jlist jstring (fst inf.mt_path);
		"name",jstring (snd inf.mt_path);
		"moduleName",jstring (snd inf.mt_module.m_path);
		"pos",generate_pos ctx inf.mt_pos;
		"isPrivate",jbool inf.mt_private;
		"params",jlist (generate_type_parameter ctx) inf.mt_params;
		"meta",generate_metadata ctx inf.mt_meta;
		"doc",generate_doc ctx inf.mt_doc;
	]

let generate_class ctx c =
	let generate_class_kind ck =
		let ctor,args = match ck with
		| KNormal -> "KNormal",None
		| KTypeParameter tl -> "KTypeParameter",Some (generate_types ctx tl)
		| KExpr e -> "KExpr",Some (generate_expr ctx e)
		| KGeneric -> "KGeneric",None
		| KGenericInstance(c,tl) -> "KGenericInstance",Some (generate_type_path_with_params ctx c.cl_module.m_path c.cl_path tl c.cl_meta)
		| KMacroType -> "KMacroType",None
		| KGenericBuild _ -> "KGenericBuild",None
		| KAbstractImpl a -> "KAbstractImpl",Some (abstract_ref ctx a)
		| KModuleFields m -> "KModuleFields",Some (generate_module_path m.m_path)
		in
		generate_adt ctx (Some (["haxe";"macro"],"ClassKind")) ctor args
	in
	let generate_class_relation (c,tl) =
		jobject [
			"path",class_ref ctx c;
			"params",generate_types ctx tl;
		]
	in
	[
		"kind",generate_class_kind c.cl_kind;
		"isInterface",jbool c.cl_interface;
		"superClass",jopt generate_class_relation c.cl_super;
		"interfaces",jlist generate_class_relation c.cl_implements;
		"fields",jlist (generate_class_field ctx CFSMember) c.cl_ordered_fields;
		"statics",jlist (generate_class_field ctx CFSStatic) c.cl_ordered_statics;
		"constructor",jopt (generate_class_field ctx CFSConstructor) c.cl_constructor;
		"init",jopt (generate_texpr ctx) c.cl_init;
		"overrides",jlist (classfield_ref ctx) (List.filter (fun cf -> has_class_field_flag cf CfOverride) c.cl_ordered_fields);
		"isExtern",jbool c.cl_extern;
		"isFinal",jbool (has_class_flag c CFinal);
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
	let impl = match a.a_impl with
		| None -> jnull
		| Some c ->
			if ctx.generate_abstract_impl then jobject (generate_class ctx c)
			else class_ref ctx c
	in
	[
		"type",generate_type ctx a.a_this;
		"impl",impl;
		"binops",jlist generate_binop a.a_ops;
		"unops",jlist generate_unop a.a_unops;
		"from",generate_casts a.a_from_field a.a_from;
		"to",generate_casts a.a_to_field a.a_to;
		"array",jlist (classfield_ref ctx) a.a_array;
		"read",jopt (classfield_ref ctx) a.a_read;
		"write",jopt (classfield_ref ctx) a.a_write;
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

(* module *)

let generate_module ctx m =
	jobject [
		"id",jint m.m_id;
		"path",generate_module_path m.m_path;
		"types",jlist (fun mt -> generate_type_path m.m_path (t_infos mt).mt_path (t_infos mt).mt_meta) m.m_types;
		"file",jstring (Path.UniqueKey.lazy_path m.m_extra.m_file);
		"sign",jstring (Digest.to_hex m.m_extra.m_sign);
		"dependencies",jarray (PMap.fold (fun m acc -> (jobject [
			"path",jstring (s_type_path m.m_path);
			"sign",jstring (Digest.to_hex m.m_extra.m_sign);
		]) :: acc) m.m_extra.m_deps []);
	]

let create_context ?jsonrpc gm = {
	generation_mode = gm;
	generate_abstract_impl = false;
	request = match jsonrpc with None -> None | Some jsonrpc -> Some (new JsonRequest.json_request jsonrpc)
}

let generate types file =
	let t = Timer.timer ["generate";"json";"construct"] in
	let ctx = create_context GMFull in
	let json = jarray (List.map (generate_module_type ctx) types) in
	t();
	let t = Timer.timer ["generate";"json";"write"] in
	let ch = open_out_bin file in
	Json.write_json (output_string ch) json;
	close_out ch;
	t()