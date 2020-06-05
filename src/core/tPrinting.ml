open Globals
open Ast
open TType
open TFunctions

let print_context() = ref []

let rec s_type_kind t =
	let map tl = String.concat ", " (List.map s_type_kind tl) in
	match t with
	| TMono r ->
		begin match r.tm_type with
			| None -> Printf.sprintf "TMono (None)"
			| Some t -> "TMono (Some (" ^ (s_type_kind t) ^ "))"
		end
	| TEnum(en,tl) -> Printf.sprintf "TEnum(%s, [%s])" (s_type_path en.e_path) (map tl)
	| TInst(c,tl) -> Printf.sprintf "TInst(%s, [%s])" (s_type_path c.cl_path) (map tl)
	| TType(t,tl) -> Printf.sprintf "TType(%s, [%s])" (s_type_path t.t_path) (map tl)
	| TAbstract(a,tl) -> Printf.sprintf "TAbstract(%s, [%s])" (s_type_path a.a_path) (map tl)
	| TFun(tl,r) -> Printf.sprintf "TFun([%s], %s)" (String.concat ", " (List.map (fun (n,b,t) -> Printf.sprintf "%s%s:%s" (if b then "?" else "") n (s_type_kind t)) tl)) (s_type_kind r)
	| TAnon an -> "TAnon"
	| TDynamic t2 -> "TDynamic"
	| TLazy _ -> "TLazy"

let s_module_type_kind = function
	| TClassDecl c -> "TClassDecl(" ^ (s_type_path c.cl_path) ^ ")"
	| TEnumDecl en -> "TEnumDecl(" ^ (s_type_path en.e_path) ^ ")"
	| TAbstractDecl a -> "TAbstractDecl(" ^ (s_type_path a.a_path) ^ ")"
	| TTypeDecl t -> "TTypeDecl(" ^ (s_type_path t.t_path) ^ ")"

let rec s_type ctx t =
	match t with
	| TMono r ->
		(match r.tm_type with
		| None ->
			begin try
				let id = List.assq t (!ctx) in
				Printf.sprintf "Unknown<%d>" id
			with Not_found ->
				let id = List.length !ctx in
				ctx := (t,id) :: !ctx;
				Printf.sprintf "Unknown<%d>" id
			end
		| Some t -> s_type ctx t)
	| TEnum (e,tl) ->
		s_type_path e.e_path ^ s_type_params ctx tl
	| TInst (c,tl) ->
		(match c.cl_kind with
		| KExpr e -> Ast.Printer.s_expr e
		| _ -> s_type_path c.cl_path ^ s_type_params ctx tl)
	| TType ({ t_type = TAnon { a_status = { contents = Statics { cl_kind = KAbstractImpl a }}}}, _) ->
		"Abstract<" ^ (s_type_path a.a_path) ^ ">"
	| TType (t,tl) ->
		s_type_path t.t_path ^ s_type_params ctx tl
	| TAbstract (a,tl) ->
		s_type_path a.a_path ^ s_type_params ctx tl
	| TFun ([],t) ->
		"() -> " ^ s_fun ctx t false
	| TFun (l,t) ->
		let args = match l with
			| [] -> "()"
			| ["",b,t] -> Printf.sprintf "%s%s" (if b then "?" else "") (s_fun ctx t true)
			| _ ->
				let args = String.concat ", " (List.map (fun (s,b,t) ->
					(if b then "?" else "") ^ (if s = "" then "" else s ^ " : ") ^ s_fun ctx t true
				) l) in
				"(" ^ args ^ ")"
		in
		Printf.sprintf "%s -> %s" args (s_fun ctx t false)
	| TAnon a ->
		begin
			match !(a.a_status) with
			| Statics c -> Printf.sprintf "{ Statics %s }" (s_type_path c.cl_path)
			| EnumStatics e -> Printf.sprintf "{ EnumStatics %s }" (s_type_path e.e_path)
			| AbstractStatics a -> Printf.sprintf "{ AbstractStatics %s }" (s_type_path a.a_path)
			| _ ->
				let fl = PMap.fold (fun f acc -> ((if Meta.has Meta.Optional f.cf_meta then " ?" else " ") ^ f.cf_name ^ " : " ^ s_type ctx f.cf_type) :: acc) a.a_fields [] in
				"{" ^ (if not (is_closed a) then "+" else "") ^  String.concat "," fl ^ " }"
		end
	| TDynamic t2 ->
		"Dynamic" ^ s_type_params ctx (if t == t2 then [] else [t2])
	| TLazy f ->
		s_type ctx (lazy_type f)

and s_fun ctx t void =
	match t with
	| TFun _ ->
		"(" ^ s_type ctx t ^ ")"
	| TAbstract ({ a_path = ([],"Void") },[]) when void ->
		"(" ^ s_type ctx t ^ ")"
	| TMono r ->
		(match r.tm_type with
		| None -> s_type ctx t
		| Some t -> s_fun ctx t void)
	| TLazy f ->
		s_fun ctx (lazy_type f) void
	| _ ->
		s_type ctx t

and s_type_params ctx = function
	| [] -> ""
	| l -> "<" ^ String.concat ", " (List.map (s_type ctx) l) ^ ">"

let s_access is_read = function
	| AccNormal -> "default"
	| AccNo -> "null"
	| AccNever -> "never"
	| AccResolve -> "resolve"
	| AccCall -> if is_read then "get" else "set"
	| AccInline	-> "inline"
	| AccRequire (n,_) -> "require " ^ n
	| AccCtor -> "ctor"

let s_kind = function
	| Var { v_read = AccNormal; v_write = AccNormal } -> "var"
	| Var v -> "(" ^ s_access true v.v_read ^ "," ^ s_access false v.v_write ^ ")"
	| Method m ->
		match m with
		| MethNormal -> "method"
		| MethDynamic -> "dynamic method"
		| MethInline -> "inline method"
		| MethMacro -> "macro method"

let s_expr_kind e =
	match e.eexpr with
	| TConst _ -> "Const"
	| TLocal _ -> "Local"
	| TArray (_,_) -> "Array"
	| TBinop (_,_,_) -> "Binop"
	| TEnumParameter (_,_,_) -> "EnumParameter"
	| TEnumIndex _ -> "EnumIndex"
	| TField (_,_) -> "Field"
	| TTypeExpr _ -> "TypeExpr"
	| TParenthesis _ -> "Parenthesis"
	| TObjectDecl _ -> "ObjectDecl"
	| TArrayDecl _ -> "ArrayDecl"
	| TCall (_,_) -> "Call"
	| TNew (_,_,_) -> "New"
	| TUnop (_,_,_) -> "Unop"
	| TFunction _ -> "Function"
	| TVar _ -> "Vars"
	| TBlock _ -> "Block"
	| TFor (_,_,_) -> "For"
	| TIf (_,_,_) -> "If"
	| TWhile (_,_,_) -> "While"
	| TSwitch (_,_,_) -> "Switch"
	| TTry (_,_) -> "Try"
	| TReturn _ -> "Return"
	| TBreak -> "Break"
	| TContinue -> "Continue"
	| TThrow _ -> "Throw"
	| TCast _ -> "Cast"
	| TMeta _ -> "Meta"
	| TIdent _ -> "Ident"

let s_const = function
	| TInt i -> Int32.to_string i
	| TFloat s -> s
	| TString s -> Printf.sprintf "\"%s\"" (StringHelper.s_escape s)
	| TBool b -> if b then "true" else "false"
	| TNull -> "null"
	| TThis -> "this"
	| TSuper -> "super"

let s_field_access s_type fa = match fa with
	| FStatic (c,f) -> "static(" ^ s_type_path c.cl_path ^ "." ^ f.cf_name ^ ")"
	| FInstance (c,_,f) -> "inst(" ^ s_type_path c.cl_path ^ "." ^ f.cf_name ^ " : " ^ s_type f.cf_type ^ ")"
	| FClosure (c,f) -> "closure(" ^ (match c with None -> f.cf_name | Some (c,_) -> s_type_path c.cl_path ^ "." ^ f.cf_name)  ^ ")"
	| FAnon f -> "anon(" ^ f.cf_name ^ ")"
	| FEnum (en,f) -> "enum(" ^ s_type_path en.e_path ^ "." ^ f.ef_name ^ ")"
	| FDynamic f -> "dynamic(" ^ f ^ ")"

let rec s_expr s_type e =
	let sprintf = Printf.sprintf in
	let slist f l = String.concat "," (List.map f l) in
	let loop = s_expr s_type in
	let s_var v = v.v_name ^ ":" ^ string_of_int v.v_id ^ if has_var_flag v VCaptured then "[c]" else "" in
	let str = (match e.eexpr with
	| TConst c ->
		"Const " ^ s_const c
	| TLocal v ->
		"Local " ^ s_var v
	| TArray (e1,e2) ->
		sprintf "%s[%s]" (loop e1) (loop e2)
	| TBinop (op,e1,e2) ->
		sprintf "(%s %s %s)" (loop e1) (s_binop op) (loop e2)
	| TEnumIndex e1 ->
		sprintf "EnumIndex %s" (loop e1)
	| TEnumParameter (e1,_,i) ->
		sprintf "%s[%i]" (loop e1) i
	| TField (e,f) ->
		let fstr = s_field_access s_type f in
		sprintf "%s.%s" (loop e) fstr
	| TTypeExpr m ->
		sprintf "TypeExpr %s" (s_type_path (t_path m))
	| TParenthesis e ->
		sprintf "Parenthesis %s" (loop e)
	| TObjectDecl fl ->
		sprintf "ObjectDecl {%s}" (slist (fun ((f,_,qs),e) -> sprintf "%s : %s" (s_object_key_name f qs) (loop e)) fl)
	| TArrayDecl el ->
		sprintf "ArrayDecl [%s]" (slist loop el)
	| TCall (e,el) ->
		sprintf "Call %s(%s)" (loop e) (slist loop el)
	| TNew (c,pl,el) ->
		sprintf "New %s%s(%s)" (s_type_path c.cl_path) (match pl with [] -> "" | l -> sprintf "<%s>" (slist s_type l)) (slist loop el)
	| TUnop (op,f,e) ->
		(match f with
		| Prefix -> sprintf "(%s %s)" (s_unop op) (loop e)
		| Postfix -> sprintf "(%s %s)" (loop e) (s_unop op))
	| TFunction f ->
		let args = slist (fun (v,o) -> sprintf "%s : %s%s" (s_var v) (s_type v.v_type) (match o with None -> "" | Some c -> " = " ^ loop c)) f.tf_args in
		sprintf "Function(%s) : %s = %s" args (s_type f.tf_type) (loop f.tf_expr)
	| TVar (v,eo) ->
		sprintf "Vars %s" (sprintf "%s : %s%s" (s_var v) (s_type v.v_type) (match eo with None -> "" | Some e -> " = " ^ loop e))
	| TBlock el ->
		sprintf "Block {\n%s}" (String.concat "" (List.map (fun e -> sprintf "%s;\n" (loop e)) el))
	| TFor (v,econd,e) ->
		sprintf "For (%s : %s in %s,%s)" (s_var v) (s_type v.v_type) (loop econd) (loop e)
	| TIf (e,e1,e2) ->
		sprintf "If (%s,%s%s)" (loop e) (loop e1) (match e2 with None -> "" | Some e -> "," ^ loop e)
	| TWhile (econd,e,flag) ->
		(match flag with
		| NormalWhile -> sprintf "While (%s,%s)" (loop econd) (loop e)
		| DoWhile -> sprintf "DoWhile (%s,%s)" (loop e) (loop econd))
	| TSwitch (e,cases,def) ->
		sprintf "Switch (%s,(%s)%s)" (loop e) (slist (fun (cl,e) -> sprintf "case %s: %s" (slist loop cl) (loop e)) cases) (match def with None -> "" | Some e -> "," ^ loop e)
	| TTry (e,cl) ->
		sprintf "Try %s(%s) " (loop e) (slist (fun (v,e) -> sprintf "catch( %s : %s ) %s" (s_var v) (s_type v.v_type) (loop e)) cl)
	| TReturn None ->
		"Return"
	| TReturn (Some e) ->
		sprintf "Return %s" (loop e)
	| TBreak ->
		"Break"
	| TContinue ->
		"Continue"
	| TThrow e ->
		"Throw " ^ (loop e)
	| TCast (e,t) ->
		sprintf "Cast %s%s" (match t with None -> "" | Some t -> s_type_path (t_path t) ^ ": ") (loop e)
	| TMeta ((n,el,_),e) ->
		sprintf "@%s%s %s" (Meta.to_string n) (match el with [] -> "" | _ -> "(" ^ (String.concat ", " (List.map Ast.Printer.s_expr el)) ^ ")") (loop e)
	| TIdent s ->
		"Ident " ^ s
	) in
	sprintf "(%s : %s)" str (s_type e.etype)

let rec s_expr_pretty print_var_ids tabs top_level s_type e =
	let sprintf = Printf.sprintf in
	let loop = s_expr_pretty print_var_ids tabs false s_type in
	let slist c f l = String.concat c (List.map f l) in
	let clist f l = slist ", " f l in
	let local v = if print_var_ids then sprintf "%s<%i>" v.v_name v.v_id else v.v_name in
	match e.eexpr with
	| TConst c -> s_const c
	| TLocal v -> local v
	| TArray (e1,e2) -> sprintf "%s[%s]" (loop e1) (loop e2)
	| TBinop (op,e1,e2) -> sprintf "%s %s %s" (loop e1) (s_binop op) (loop e2)
	| TEnumParameter (e1,_,i) -> sprintf "%s[%i]" (loop e1) i
	| TEnumIndex e1 -> sprintf "enumIndex %s" (loop e1)
	| TField (e1,s) -> sprintf "%s.%s" (loop e1) (field_name s)
	| TTypeExpr mt -> (s_type_path (t_path mt))
	| TParenthesis e1 -> sprintf "(%s)" (loop e1)
	| TObjectDecl fl -> sprintf "{%s}" (clist (fun ((f,_,qs),e) -> sprintf "%s : %s" (s_object_key_name f qs) (loop e)) fl)
	| TArrayDecl el -> sprintf "[%s]" (clist loop el)
	| TCall (e1,el) -> sprintf "%s(%s)" (loop e1) (clist loop el)
	| TNew (c,pl,el) ->
		sprintf "new %s(%s)" (s_type_path c.cl_path) (clist loop el)
	| TUnop (op,f,e) ->
		(match f with
		| Prefix -> sprintf "%s %s" (s_unop op) (loop e)
		| Postfix -> sprintf "%s %s" (loop e) (s_unop op))
	| TFunction f ->
		let args = clist (fun (v,o) -> sprintf "%s:%s%s" (local v) (s_type v.v_type) (match o with None -> "" | Some c -> " = " ^ loop c)) f.tf_args in
		sprintf "%s(%s) %s" (if top_level then "" else "function") args (loop f.tf_expr)
	| TVar (v,eo) ->
		sprintf "var %s" (sprintf "%s%s" (local v) (match eo with None -> "" | Some e -> " = " ^ loop e))
	| TBlock el ->
		let ntabs = tabs ^ "\t" in
		let s = sprintf "{\n%s" (String.concat "" (List.map (fun e -> sprintf "%s%s;\n" ntabs (s_expr_pretty print_var_ids ntabs top_level s_type e)) el)) in
		(match el with
			| [] -> "{}"
			| _ ->  s ^ tabs ^ "}")
	| TFor (v,econd,e) ->
		sprintf "for (%s in %s) %s" (local v) (loop econd) (loop e)
	| TIf (e,e1,e2) ->
		sprintf "if (%s) %s%s" (loop e) (loop e1) (match e2 with None -> "" | Some e -> " else " ^ loop e)
	| TWhile (econd,e,flag) ->
		(match flag with
		| NormalWhile -> sprintf "while (%s) %s" (loop econd) (loop e)
		| DoWhile -> sprintf "do (%s) while(%s)" (loop e) (loop econd))
	| TSwitch (e,cases,def) ->
		let ntabs = tabs ^ "\t" in
		let s = sprintf "switch (%s) {\n%s%s" (loop e) (slist "" (fun (cl,e) -> sprintf "%scase %s: %s;\n" ntabs (clist loop cl) (s_expr_pretty print_var_ids ntabs top_level s_type e)) cases) (match def with None -> "" | Some e -> ntabs ^ "default: " ^ (s_expr_pretty print_var_ids ntabs top_level s_type e) ^ "\n") in
		s ^ tabs ^ "}"
	| TTry (e,cl) ->
		sprintf "try %s%s" (loop e) (clist (fun (v,e) -> sprintf " catch (%s:%s) %s" (local v) (s_type v.v_type) (loop e)) cl)
	| TReturn None ->
		"return"
	| TReturn (Some e) ->
		sprintf "return %s" (loop e)
	| TBreak ->
		"break"
	| TContinue ->
		"continue"
	| TThrow e ->
		"throw " ^ (loop e)
	| TCast (e,None) ->
		sprintf "cast %s" (loop e)
	| TCast (e,Some mt) ->
		sprintf "cast (%s,%s)" (loop e) (s_type_path (t_path mt))
	| TMeta ((n,el,_),e) ->
		sprintf "@%s%s %s" (Meta.to_string n) (match el with [] -> "" | _ -> "(" ^ (String.concat ", " (List.map Ast.Printer.s_expr el)) ^ ")") (loop e)
	| TIdent s ->
		s

let rec s_expr_ast print_var_ids tabs s_type e =
	let sprintf = Printf.sprintf in
	let loop ?(extra_tabs="") = s_expr_ast print_var_ids (tabs ^ "\t" ^ extra_tabs) s_type in
	let tag_args tabs sl = match sl with
		| [] -> ""
		| [s] when not (String.contains s '\n') -> " " ^ s
		| _ ->
			let tabs = "\n" ^ tabs ^ "\t" in
			tabs ^ (String.concat tabs sl)
	in
	let tag s ?(t=None) ?(extra_tabs="") sl =
		let st = match t with
			| None -> s_type e.etype
			| Some t -> s_type t
		in
		sprintf "[%s:%s]%s" s st (tag_args (tabs ^ extra_tabs) sl)
	in
	let var_id v = if print_var_ids then v.v_id else 0 in
	let const c t = tag "Const" ~t [s_const c] in
	let local v t = sprintf "[Local %s(%i):%s%s]" v.v_name (var_id v) (s_type v.v_type) (match t with None -> "" | Some t -> ":" ^ (s_type t)) in
	let var v sl = sprintf "[Var %s(%i):%s]%s" v.v_name (var_id v) (s_type v.v_type) (tag_args tabs sl) in
	let module_type mt = sprintf "[TypeExpr %s:%s]" (s_type_path (t_path mt)) (s_type e.etype) in
	match e.eexpr with
	| TConst c -> const c (Some e.etype)
	| TLocal v -> local v (Some e.etype)
	| TArray (e1,e2) -> tag "Array" [loop e1; loop e2]
	| TBinop (op,e1,e2) -> tag "Binop" [loop e1; s_binop op; loop e2]
	| TUnop (op,flag,e1) -> tag "Unop" [s_unop op; if flag = Postfix then "Postfix" else "Prefix"; loop e1]
	| TEnumParameter (e1,ef,i) -> tag "EnumParameter" [loop e1; ef.ef_name; string_of_int i]
	| TEnumIndex e1 -> tag "EnumIndex" [loop e1]
	| TField (e1,fa) ->
		let sfa = match fa with
			| FInstance(c,tl,cf) -> tag "FInstance" ~extra_tabs:"\t" [s_type (TInst(c,tl)); Printf.sprintf "%s:%s" cf.cf_name (s_type cf.cf_type)]
			| FStatic(c,cf) -> tag "FStatic" ~extra_tabs:"\t" [s_type_path c.cl_path; Printf.sprintf "%s:%s" cf.cf_name (s_type cf.cf_type)]
			| FClosure(co,cf) -> tag "FClosure" ~extra_tabs:"\t" [(match co with None -> "None" | Some (c,tl) -> s_type (TInst(c,tl))); Printf.sprintf "%s:%s" cf.cf_name (s_type cf.cf_type)]
			| FAnon cf -> tag "FAnon" ~extra_tabs:"\t" [Printf.sprintf "%s:%s" cf.cf_name (s_type cf.cf_type)]
			| FDynamic s -> tag "FDynamic" ~extra_tabs:"\t" [s]
			| FEnum(en,ef) -> tag "FEnum" ~extra_tabs:"\t" [s_type_path en.e_path; ef.ef_name]
		in
		tag "Field" [loop e1; sfa]
	| TTypeExpr mt -> module_type mt
	| TParenthesis e1 -> tag "Parenthesis" [loop e1]
	| TObjectDecl fl -> tag "ObjectDecl" (List.map (fun ((s,_,qs),e) -> sprintf "%s: %s" (s_object_key_name s qs) (loop e)) fl)
	| TArrayDecl el -> tag "ArrayDecl" (List.map loop el)
	| TCall (e1,el) -> tag "Call" (loop e1 :: (List.map loop el))
	| TNew (c,tl,el) -> tag "New" ((s_type (TInst(c,tl))) :: (List.map loop el))
	| TFunction f ->
		let arg (v,cto) =
			tag "Arg" ~t:(Some v.v_type) ~extra_tabs:"\t" (match cto with None -> [local v None] | Some ct -> [local v None;loop ct])
		in
		tag "Function" ((List.map arg f.tf_args) @ [loop f.tf_expr])
	| TVar (v,eo) -> var v (match eo with None -> [] | Some e -> [loop e])
	| TBlock el -> tag "Block" (List.map loop el)
	| TIf (e,e1,e2) -> tag "If" (loop e :: (Printf.sprintf "[Then:%s] %s" (s_type e1.etype) (loop e1)) :: (match e2 with None -> [] | Some e -> [Printf.sprintf "[Else:%s] %s" (s_type e.etype) (loop e)]))
	| TCast (e1,None) -> tag "Cast" [loop e1]
	| TCast (e1,Some mt) -> tag "Cast" [loop e1; module_type mt]
	| TThrow e1 -> tag "Throw" [loop e1]
	| TBreak -> tag "Break" []
	| TContinue -> tag "Continue" []
	| TReturn None -> tag "Return" []
	| TReturn (Some e1) -> tag "Return" [loop e1]
	| TWhile (e1,e2,NormalWhile) -> tag "While" [loop e1; loop e2]
	| TWhile (e1,e2,DoWhile) -> tag "Do" [loop e1; loop e2]
	| TFor (v,e1,e2) -> tag "For" [local v None; loop e1; loop e2]
	| TTry (e1,catches) ->
		let sl = List.map (fun (v,e) ->
			sprintf "Catch %s%s" (local v None) (tag_args (tabs ^ "\t") [loop ~extra_tabs:"\t" e]);
		) catches in
		tag "Try" ((loop e1) :: sl)
	| TSwitch (e1,cases,eo) ->
		let sl = List.map (fun (el,e) ->
			tag "Case" ~t:(Some e.etype) ~extra_tabs:"\t" ((List.map loop el) @ [loop ~extra_tabs:"\t" e])
		) cases in
		let sl = match eo with
			| None -> sl
			| Some e -> sl @ [tag "Default" ~t:(Some e.etype) ~extra_tabs:"\t" [loop ~extra_tabs:"\t" e]]
		in
		tag "Switch" ((loop e1) :: sl)
	| TMeta ((m,el,_),e1) ->
		let s = Meta.to_string m in
		let s = match el with
			| [] -> s
			| _ -> sprintf "%s(%s)" s (String.concat ", " (List.map Ast.Printer.s_expr el))
		in
		tag "Meta" [s; loop e1]
	| TIdent s ->
		tag "Ident" [s]

(**
	Shortcut to pretty-printing expressions for debugging purposes.
*)
let s_expr_debug e =
	s_expr_pretty false "  " false (s_type (print_context())) e

let s_types ?(sep = ", ") tl =
	let pctx = print_context() in
	String.concat sep (List.map (s_type pctx) tl)

let s_class_kind = function
	| KNormal ->
		"KNormal"
	| KTypeParameter tl ->
		Printf.sprintf "KTypeParameter [%s]" (s_types tl)
	| KExpr _ ->
		"KExpr"
	| KGeneric ->
		"KGeneric"
	| KGenericInstance(c,tl) ->
		Printf.sprintf "KGenericInstance %s<%s>" (s_type_path c.cl_path) (s_types tl)
	| KMacroType ->
		"KMacroType"
	| KGenericBuild _ ->
		"KGenericBuild"
	| KAbstractImpl a ->
		Printf.sprintf "KAbstractImpl %s" (s_type_path a.a_path)
	| KModuleFields m ->
		Printf.sprintf "KModuleFields %s" (s_type_path m.m_path)

module Printer = struct

	let s_type t =
		s_type (print_context()) t

	let s_pair s1 s2 =
		Printf.sprintf "(%s,%s)" s1 s2

	let s_record_field name value =
		Printf.sprintf "%s = %s;" name value

	let s_pos p =
		Printf.sprintf "%s: %i-%i" p.pfile p.pmin p.pmax

	let s_record_fields tabs fields =
		let sl = List.map (fun (name,value) -> s_record_field name value) fields in
		Printf.sprintf "{\n%s\t%s\n%s}" tabs (String.concat ("\n\t" ^ tabs) sl) tabs

	let s_list sep f l =
		"[" ^ (String.concat sep (List.map f l)) ^ "]"

	let s_opt f o = match o with
		| None -> "None"
		| Some v -> f v

	let s_pmap fk fv pm =
		"{" ^ (String.concat ", " (PMap.foldi (fun k v acc -> (Printf.sprintf "%s = %s" (fk k) (fv v)) :: acc) pm [])) ^ "}"

	let s_doc doc_opt =
		match doc_opt with
		| None -> "None"
		| Some d -> gen_doc_text d

	let s_metadata_entry (s,el,_) =
		Printf.sprintf "@%s%s" (Meta.to_string s) (match el with [] -> "" | el -> "(" ^ (String.concat ", " (List.map Ast.Printer.s_expr el)) ^ ")")

	let s_metadata metadata =
		s_list " " s_metadata_entry metadata

	let s_type_param (s,t) = match follow t with
		| TInst({cl_kind = KTypeParameter tl1},tl2) ->
			begin match tl1 with
			| [] -> s
			| _ -> Printf.sprintf "%s:%s" s (String.concat " & " (List.map s_type tl1))
			end
		| _ -> die "" __LOC__

	let s_type_params tl =
		s_list ", " s_type_param tl

	let s_tclass_field tabs cf =
		s_record_fields tabs [
			"cf_name",cf.cf_name;
			"cf_doc",s_doc cf.cf_doc;
			"cf_type",s_type_kind (follow cf.cf_type);
			"cf_pos",s_pos cf.cf_pos;
			"cf_name_pos",s_pos cf.cf_name_pos;
			"cf_meta",s_metadata cf.cf_meta;
			"cf_kind",s_kind cf.cf_kind;
			"cf_params",s_type_params cf.cf_params;
			"cf_expr",s_opt (s_expr_ast true "\t\t" s_type) cf.cf_expr;
		]

	let s_tclass tabs c =
		s_record_fields tabs [
			"cl_path",s_type_path c.cl_path;
			"cl_module",s_type_path c.cl_module.m_path;
			"cl_pos",s_pos c.cl_pos;
			"cl_name_pos",s_pos c.cl_name_pos;
			"cl_private",string_of_bool c.cl_private;
			"cl_doc",s_doc c.cl_doc;
			"cl_meta",s_metadata c.cl_meta;
			"cl_params",s_type_params c.cl_params;
			"cl_kind",s_class_kind c.cl_kind;
			"cl_extern",string_of_bool c.cl_extern;
			"cl_final",string_of_bool c.cl_final;
			"cl_interface",string_of_bool c.cl_interface;
			"cl_super",s_opt (fun (c,tl) -> s_type (TInst(c,tl))) c.cl_super;
			"cl_implements",s_list ", " (fun (c,tl) -> s_type (TInst(c,tl))) c.cl_implements;
			"cl_array_access",s_opt s_type c.cl_array_access;
			"cl_init",s_opt (s_expr_ast true "" s_type) c.cl_init;
			"cl_constructor",s_opt (s_tclass_field (tabs ^ "\t")) c.cl_constructor;
			"cl_ordered_fields",s_list "\n\t" (s_tclass_field (tabs ^ "\t")) c.cl_ordered_fields;
			"cl_ordered_statics",s_list "\n\t" (s_tclass_field (tabs ^ "\t")) c.cl_ordered_statics;
		]

	let s_tdef tabs t =
		s_record_fields tabs [
			"t_path",s_type_path t.t_path;
			"t_module",s_type_path t.t_module.m_path;
			"t_pos",s_pos t.t_pos;
			"t_name_pos",s_pos t.t_name_pos;
			"t_private",string_of_bool t.t_private;
			"t_doc",s_doc t.t_doc;
			"t_meta",s_metadata t.t_meta;
			"t_params",s_type_params t.t_params;
			"t_type",s_type_kind t.t_type
		]

	let s_tenum_field tabs ef =
		s_record_fields tabs [
			"ef_name",ef.ef_name;
			"ef_doc",s_doc ef.ef_doc;
			"ef_pos",s_pos ef.ef_pos;
			"ef_name_pos",s_pos ef.ef_name_pos;
			"ef_type",s_type_kind ef.ef_type;
			"ef_index",string_of_int ef.ef_index;
			"ef_params",s_type_params ef.ef_params;
			"ef_meta",s_metadata ef.ef_meta
		]

	let s_tenum tabs en =
		s_record_fields tabs [
			"e_path",s_type_path en.e_path;
			"e_module",s_type_path en.e_module.m_path;
			"e_pos",s_pos en.e_pos;
			"e_name_pos",s_pos en.e_name_pos;
			"e_private",string_of_bool en.e_private;
			"d_doc",s_doc en.e_doc;
			"e_meta",s_metadata en.e_meta;
			"e_params",s_type_params en.e_params;
			"e_type",s_tdef "\t" en.e_type;
			"e_extern",string_of_bool en.e_extern;
			"e_constrs",s_list "\n\t" (s_tenum_field (tabs ^ "\t")) (PMap.fold (fun ef acc -> ef :: acc) en.e_constrs []);
			"e_names",String.concat ", " en.e_names
		]

	let s_tabstract tabs a =
		s_record_fields tabs [
			"a_path",s_type_path a.a_path;
			"a_modules",s_type_path a.a_module.m_path;
			"a_pos",s_pos a.a_pos;
			"a_name_pos",s_pos a.a_name_pos;
			"a_private",string_of_bool a.a_private;
			"a_doc",s_doc a.a_doc;
			"a_meta",s_metadata a.a_meta;
			"a_params",s_type_params a.a_params;
			"a_ops",s_list ", " (fun (op,cf) -> Printf.sprintf "%s: %s" (s_binop op) cf.cf_name) a.a_ops;
			"a_unops",s_list ", " (fun (op,flag,cf) -> Printf.sprintf "%s (%s): %s" (s_unop op) (if flag = Postfix then "postfix" else "prefix") cf.cf_name) a.a_unops;
			"a_impl",s_opt (fun c -> s_type_path c.cl_path) a.a_impl;
			"a_this",s_type_kind a.a_this;
			"a_from",s_list ", " s_type_kind a.a_from;
			"a_to",s_list ", " s_type_kind a.a_to;
			"a_from_field",s_list ", " (fun (t,cf) -> Printf.sprintf "%s: %s" (s_type_kind t) cf.cf_name) a.a_from_field;
			"a_to_field",s_list ", " (fun (t,cf) -> Printf.sprintf "%s: %s" (s_type_kind t) cf.cf_name) a.a_to_field;
			"a_array",s_list ", " (fun cf -> cf.cf_name) a.a_array;
			"a_read",s_opt (fun cf -> cf.cf_name) a.a_read;
			"a_write",s_opt (fun cf -> cf.cf_name) a.a_write;
		]

	let s_tvar_extra (tl,eo) =
		Printf.sprintf "Some(%s, %s)" (s_type_params tl) (s_opt (s_expr_ast true "" s_type) eo)

	let s_tvar v =
		s_record_fields "" [
			"v_id",string_of_int v.v_id;
			"v_name",v.v_name;
			"v_type",s_type v.v_type;
			"v_capture",string_of_bool (has_var_flag v VCaptured);
			"v_extra",s_opt s_tvar_extra v.v_extra;
			"v_meta",s_metadata v.v_meta;
			"v_pos",s_pos v.v_pos;
		]

	let s_module_kind = function
		| MCode -> "MCode"
		| MMacro -> "MMacro"
		| MFake -> "MFake"
		| MExtern -> "MExtern"
		| MImport -> "MImport"

	let s_module_def_extra tabs me =
		s_record_fields tabs [
			"m_file",Path.UniqueKey.lazy_path me.m_file;
			"m_sign",me.m_sign;
			"m_time",string_of_float me.m_time;
			"m_dirty",s_opt s_type_path me.m_dirty;
			"m_added",string_of_int me.m_added;
			"m_mark",string_of_int me.m_mark;
			"m_deps",s_pmap string_of_int (fun m -> snd m.m_path) me.m_deps;
			"m_processed",string_of_int me.m_processed;
			"m_kind",s_module_kind me.m_kind;
			"m_binded_res",""; (* TODO *)
			"m_if_feature",""; (* TODO *)
			"m_features",""; (* TODO *)
		]

	let s_module_def m =
		s_record_fields "" [
			"m_id",string_of_int m.m_id;
			"m_path",s_type_path m.m_path;
			"m_extra",s_module_def_extra "\t" m.m_extra
		]

	let s_type_path tp =
		s_record_fields "" [
			"tpackage",s_list "." (fun s -> s) tp.tpackage;
			"tname",tp.tname;
			"tparams","";
			"tsub",s_opt (fun s -> s) tp.tsub;
		]

	let s_class_flag = function
		| HInterface -> "HInterface"
		| HExtern -> "HExtern"
		| HPrivate -> "HPrivate"
		| HExtends tp -> "HExtends " ^ (s_type_path (fst tp))
		| HImplements tp -> "HImplements " ^ (s_type_path (fst tp))
		| HFinal -> "HFinal"

	let s_placed f (x,p) =
		s_pair (f x) (s_pos p)

	let s_class_field cff =
		s_record_fields "" [
			"cff_name",s_placed (fun s -> s) cff.cff_name;
			"cff_doc",s_doc cff.cff_doc;
			"cff_pos",s_pos cff.cff_pos;
			"cff_meta",s_metadata cff.cff_meta;
			"cff_access",s_list ", " Ast.s_placed_access cff.cff_access;
		]
end
