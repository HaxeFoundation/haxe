open Ast
open Type
open Common

module Transformer = struct
	type adjusted_expr = {
		a_expr : texpr;
		a_blocks : texpr list;
		a_next_id : unit -> string;
		a_is_value : bool;
	}

	let t_bool = ref t_dynamic
	let t_void = ref t_dynamic

	let init com =
		t_bool := com.basic.tbool;
		t_void := com.basic.tvoid

	let new_counter () =
		let n = ref (-1) in
		(fun () ->
			incr n;
			Printf.sprintf "_hx_local_%i" !n
		)

	let to_expr ae =
		match ae.a_blocks with
			| [] ->
				ae.a_expr
			| el ->
				match ae.a_expr.eexpr with
					| TBlock el2 ->
						{ ae.a_expr with eexpr = TBlock (el @ el2) }
					| _ ->
						{ ae.a_expr with eexpr = TBlock (el @ [ae.a_expr])}

	let lift_expr ?(is_value = false) ?(next_id = None) ?(blocks = []) e =
		let next_id = match next_id with
			| None ->
				new_counter()
			| Some f ->
				f
		in
		{
			a_expr = e;
			a_blocks = blocks;
			a_next_id = next_id;
			a_is_value = is_value
		}

	let add_non_locals_to_func e =
		e

	let rec transform_function tf ae is_value =
		let p = tf.tf_expr.epos in
		let assigns = List.fold_left (fun acc (v,value) -> match value with
			| None ->
				acc
			| Some ct ->
				let a_local = mk (TLocal v) v.v_type p in
				let a_null = mk (TConst TNull) v.v_type p in
				let a_cmp = mk (TBinop(OpEq,a_local,a_null)) !t_bool p in
				let a_value = mk (TConst(ct)) v.v_type p in
				let a_assign = mk (TBinop(OpAssign,a_local,a_value)) v.v_type p in
				let a_if = mk (TIf(a_cmp,a_assign,None)) !t_void p in
				a_if :: acc
		) [] tf.tf_args in
		let body = match assigns with
			| [] ->
				tf.tf_expr
			| _ ->
				let eb = mk (TBlock (List.rev assigns)) t_dynamic p in
				Codegen.concat tf.tf_expr eb
		in
		let e1 = to_expr (transform_expr ~next_id:(Some ae.a_next_id) body) in
		let fn = mk (TFunction({
			tf_expr = e1;
			tf_args = tf.tf_args;
			tf_type = tf.tf_type;
		})) ae.a_expr.etype p in
		if is_value then begin
			let new_name = ae.a_next_id() in
			let new_var = alloc_var new_name tf.tf_type in
			let new_local = mk (TLocal new_var) fn.etype p in
			let def = mk (TVar(new_var,Some fn)) fn.etype p in
			lift_expr ~next_id:(Some ae.a_next_id) ~blocks:[def] new_local
		end else
			lift_expr fn

	and transform_var_expr ae eo v =
		let b,new_expr = match eo with
			| None ->
				[],None
			| Some e1 ->
				let f = transform_expr ~is_value:true ~next_id:(Some ae.a_next_id) e1 in
				let b = f.a_blocks in
				b,Some(f.a_expr)
		in
		let e = mk (TVar(v,new_expr)) ae.a_expr.etype ae.a_expr.epos in
		lift_expr ~next_id:(Some ae.a_next_id) ~blocks:b e

	and transform1 ae : adjusted_expr = match ae.a_is_value,ae.a_expr.eexpr with
		| (is_value,TBlock [x]) ->
			transform_expr ~is_value:is_value ~next_id:(Some ae.a_next_id) x
		| (_,TBlock []) ->
			lift_expr (mk (TConst TNull) ae.a_expr.etype ae.a_expr.epos)
		| (false,TBlock el) ->
			transform_exprs_to_block el ae.a_expr.etype false ae.a_expr.epos ae.a_next_id
		| (true,TBlock el) ->
			let name = ae.a_next_id() in
			let block,tr = match List.rev el with
				| e :: el ->
					List.rev ((mk (TReturn (Some e)) t_dynamic e.epos) :: el),e.etype
				| [] ->
					assert false
			in
			let my_block = transform_exprs_to_block block tr false ae.a_expr.epos ae.a_next_id in
			let fn = mk (TFunction {
				tf_args = [];
				tf_type = tr;
				tf_expr = my_block.a_expr;
			}) ae.a_expr.etype ae.a_expr.epos in
			let t_var = alloc_var name ae.a_expr.etype in
			let f = add_non_locals_to_func fn in
			let fn_assign = mk (TVar (t_var,Some f)) ae.a_expr.etype ae.a_expr.epos in
			let ev = mk (TLocal t_var) ae.a_expr.etype ae.a_expr.epos in
			let substitute = mk (TCall(ev,[])) ae.a_expr.etype ae.a_expr.epos in
			lift_expr ~blocks:[fn_assign] substitute
		| (is_value,TFunction(f)) ->
			transform_function f ae is_value
		| (_,TVar(v,None)) ->
			transform_var_expr ae None v
		| (false, TVar(v,Some({ eexpr = TUnop((Increment | Decrement as unop),post_fix,({eexpr = TLocal _ | TField({eexpr = TConst TThis},_)} as ve))} as e1))) ->
			let one = {e1 with eexpr = TConst (TInt (Int32.of_int 1))} in
			let op = if unop = Increment then OpAdd else OpSub in
			let inc = {e1 with eexpr = TBinop(op,ve,one)} in
			let inc_assign = {e1 with eexpr = TBinop(OpAssign,ve,inc)} in
			let var_assign = {e1 with eexpr = TVar(v,Some ve)} in
			let block = if post_fix = Postfix then [var_assign;inc_assign] else [inc_assign;var_assign] in
			transform_exprs_to_block block ae.a_expr.etype false ae.a_expr.epos ae.a_next_id
		| (_,TVar(v,eo)) ->
			transform_var_expr ae eo v
		| (_,TFor(v,e1,e2)) ->
			let e1 = transform_expr ~is_value:true ~next_id:(Some ae.a_next_id) e1 in
			let e2 = to_expr (transform_expr ~is_value:false ~next_id:(Some ae.a_next_id) e2) in
			let new_expr = mk (TFor(v,e1.a_expr,e2)) ae.a_expr.etype ae.a_expr.epos in
			lift_expr ~blocks:e1.a_blocks new_expr
		| (_,TReturn None) ->
			ae
		| (_,TReturn (Some ({eexpr = TFunction f} as ef))) ->
			let n = ae.a_next_id() in
			let e1 = to_expr (transform_expr ~next_id:(Some ae.a_next_id) f.tf_expr) in
			let f = mk (TFunction {
				tf_args = f.tf_args;
				tf_type = f.tf_type;
				tf_expr = e1;
			}) ef.etype ef.epos in
			let f1 = add_non_locals_to_func f in
			let var_n = alloc_var n ef.etype in
			let f1_assign = mk (TVar(var_n,Some f1)) !t_void f1.epos in
			let var_local = mk (TLocal var_n) ef.etype f1.epos in
			let er = mk (TReturn (Some var_local)) t_dynamic  ae.a_expr.epos in
			lift_expr ~is_value:true ~next_id:(Some ae.a_next_id) ~blocks:[f1_assign] er
(* 		| (_,TReturn x) ->
			let x1 = transform_expr ~is_value:true ~next_id:(Some ae.a_next_id) x in
			let res = match x1.a_blocks with
				| [] ->
					let f = expr_t *)
			(* TODO: tell frabbit to complete this mess *)
		| _ ->
			ae

	and transform_expr ?(is_value = false) ?(next_id = None) ?(blocks = []) (e : texpr) : adjusted_expr =
		transform1 (lift_expr ~is_value ~next_id ~blocks e)

	and transform_exprs_to_block el tb is_value p next_id =
		match el with
			| [e] ->
				transform_expr ~is_value ~next_id:(Some next_id) e
			| _ ->
				let res = ref [] in
				List.iter (fun e ->
					(* TODO: urgh *)
					let ae = transform_expr ~is_value ~next_id:(Some next_id) e in
					res := ae.a_expr :: !res;
					res := ae.a_blocks @ !res;
				) el;
				lift_expr (mk (TBlock (List.rev !res)) tb p)

	let transform e =
		to_expr (transform1 (lift_expr e))

end

module Printer = struct

	let create_context indent =
		""

	let print_base_type tp =
		try
			begin match Meta.get Meta.Native tp.mt_meta with
				| _,[EConst(String s),_],_ -> s
				| _ -> raise Not_found
			end
		with Not_found ->
			(* let pre = if is_definition then "" else "_hx_c." in *)
			(s_type_path tp.mt_path)

	let print_function f pctx name =
		""

	let print_expr e pctx =
		""

	let handle_keywords s =
		s
end

module Generator = struct
	type context = {
		com : Common.context;
		buf : Buffer.t;
		packages : (string,int) Hashtbl.t;
		mutable static_inits : (unit -> unit) list;
		mutable indent_count : int;
		transform_time : float;
		print_time : float;
	}

	type class_field_infos = {
		cfd_fields : string list;
		cfd_props : string list;
		cfd_methods : string list;
	}

	let mk_context com = {
		com = com;
		buf = Buffer.create 16000;
		packages = Hashtbl.create 0;
		static_inits = [];
		indent_count = 0;
		transform_time = 0.;
		print_time = 0.;
	}

	(* Transformer interface *)

	let transform_expr e =
		Transformer.transform e

	let transform_to_value e =
		(* TODO *)
		e

	(* Printer interface *)

	let get_path mt =
		Printer.print_base_type mt

	let tfunc_str f pctx name =
		Printer.print_function f pctx name

	let texpr_str e pctx =
		Printer.print_expr e pctx

	let handle_keywords s =
		Printer.handle_keywords s

	(* Helper *)

	let get_full_name mt =
		(* TODO: haxe source is crazy *)
		s_type_path mt.mt_path

	let collect_class_field_data cfl =
		let fields = DynArray.create () in
		let props = DynArray.create () in
		let methods = DynArray.create () in
		List.iter (fun cf ->
			match cf.cf_kind with
				| Var({v_read = AccResolve}) ->
					()
				| Var({v_read = AccCall}) ->
					if Meta.has Meta.IsVar cf.cf_meta then
						DynArray.add fields cf.cf_name
					else
						DynArray.add props cf.cf_name
				| Var _ ->
					DynArray.add fields cf.cf_name
				| _ ->
					DynArray.add methods cf.cf_name
		) cfl;
		{
			cfd_fields = DynArray.to_list fields;
			cfd_props = DynArray.to_list props;
			cfd_methods = DynArray.to_list methods;
		}


	let filter_py_metas metas =
		List.filter (fun (n,_,_) -> match n with Meta.Custom ":python" -> true | _ -> false) metas

	let get_members_with_init_expr c =
		List.filter (fun cf -> match cf.cf_kind with
			| Var({v_read = AccResolve | AccCall _}) -> false
			| Var _ when cf.cf_expr <> None -> true
			| _ -> false
		) c.cl_ordered_fields

	let get_meta_entries meta =
		(* TODO *)
		""

	(* Printing *)

	let spr ctx s =
		Buffer.add_string ctx.buf s

	let spr_line ctx s =
		Buffer.add_string ctx.buf s;
		Buffer.add_string ctx.buf "\n"

	let print ctx =
		Printf.kprintf (fun s -> begin
			Buffer.add_string ctx.buf s
		end)

	let newline ctx =
		spr ctx "\n"

	let open_block ctx =
		newline ctx;
		ctx.indent_count <- ctx.indent_count + 1;
		newline ctx

	let close_block ctx =
		ctx.indent_count <- ctx.indent_count - 1;
		newline ctx;
		newline ctx;
		newline ctx

	(* Generating functions *)

	let gen_pre_code_meta ctx metadata =
		try
			begin match Meta.get (Meta.Custom ":preCode") metadata with
				| _,[(EConst(String s)),_],_ -> spr ctx s
				| _ -> raise Not_found
			end
		with Not_found ->
			()

	let gen_py_metas ctx metas indent =
		List.iter (fun (n,el,_) ->
			match el with
				| [EConst(String s),_] ->
					print ctx "%s@%s\n" indent s
				| _ ->
					assert false
		) metas

	let gen_expr ctx e field indent =
		let pctx = Printer.create_context ("\t" ^ indent) in
		let e = match e.eexpr with
			| TFunction(f) ->
				{e with eexpr = TBlock [e]}
			| _ ->
				e
		in
		let expr2 = transform_to_value e in
		let name = "_hx_init_" ^ (String.concat "_" (ExtString.String.nsplit field ".")) in
		let maybe_split_expr expr2 = match expr2.eexpr with
			| TBlock es when es <> [] && field <> "" ->
				begin match List.rev es with
					| e_last :: el ->
						let new_last = {e_last with eexpr = TReturn (Some e_last)} in
						let new_block = {expr2 with eexpr = TBlock (List.rev (new_last :: el))} in
						let v_name = alloc_var name (tfun [] e_last.etype) in
						let f_name = mk (TLocal v_name) v_name.v_type e_last.epos in
						let call_f = mk (TCall(f_name,[])) e_last.etype e_last.epos in
						Some new_block,call_f
					| _ ->
						assert false
				end
			| _ ->
				None,expr2
		in
		let r = maybe_split_expr expr2 in
		match r with
			| Some e1,e2 ->
				let expr_string_1 = texpr_str e1 pctx in
				let expr_string_2 = texpr_str e2 pctx in
				print ctx "%sdef %s():\n\t%s\n" indent name expr_string_1;
				print ctx "%s%s = %s" indent field expr_string_2;
			| None,e2 ->
				let expr_string_2 = texpr_str e2 pctx in
				if field = "" then
					spr ctx expr_string_2
				else
					print ctx "%s %s = %s" indent field expr_string_2

	let gen_func_expr ctx e c name metas extra_args indent stat =
		let pctx = Printer.create_context indent in
		let e = match e.eexpr with
			| TFunction(f) ->
				let args = List.map (fun s ->
					alloc_var s t_dynamic,None
				) extra_args in
				{e with eexpr = TFunction {f with tf_args = args}}
			| _ ->
				e
		in
		let expr1 = transform_expr e in
		let field_name = if stat then
			Printf.sprintf "%s_statics_%s" (snd c.cl_path) name
		else
			name
		in
		let expr_string = match expr1.eexpr with
			| TFunction f ->
				tfunc_str f pctx field_name
			| _ ->
				Printf.sprintf "%s = %s" field_name (texpr_str expr1 pctx)
		in
		gen_py_metas ctx metas indent;
		spr ctx indent;
		spr ctx expr_string;
		if stat then print ctx "%s.%s = %s" (get_path (t_infos (TClassDecl c))) name field_name

	let gen_class_constructor ctx c = match c.cl_constructor with
		| None ->
			()
		| Some cf ->
			let member_inits = get_members_with_init_expr c in
			newline ctx;
			let py_metas = filter_py_metas cf.cf_meta in
			begin match member_inits,cf.cf_expr with
				| _,Some {eexpr = TFunction f} ->
					(* TODO: what's going on here? *)
					()
				| _ ->
					(* TODO: is this correct? *)
					()
			end;
			gen_func_expr ctx (match cf.cf_expr with None -> assert false | Some e -> e) c "__init__" py_metas ["self"] "\t" false;
			newline ctx

	let gen_class_field ctx c p cf =
		let field = handle_keywords cf.cf_name in
		begin match cf.cf_expr with
			| None ->
				print ctx "\t# var %s" field
			| Some e ->
				match cf.cf_kind with
					| Method _ ->
						let py_metas = filter_py_metas cf.cf_meta in
						gen_func_expr ctx e c field py_metas ["self"] "\t" false;
						newline ctx
					| _ ->
						gen_expr ctx e (Printf.sprintf "# var %s" field) "\t";
						newline ctx
		end;
		newline ctx

	let gen_static_field ctx c p cf =
		let p = get_path (t_infos (TClassDecl c)) in
		let field = handle_keywords cf.cf_name in
		match cf.cf_expr with
			| None ->
				print ctx "%s.%s = None;\n" p field
			| Some e ->
				match cf.cf_kind with
					| Method _ ->
						let py_metas = filter_py_metas cf.cf_meta in
						gen_func_expr ctx e c field py_metas [] "" true;
						newline ctx
					| _ ->
						gen_expr ctx e (Printf.sprintf "%s.%s" p field) "";
						newline ctx

	let gen_class_data ctx c cfd p_super p_interfaces p p_name =
		let field_str = String.concat "," (List.map (fun s -> "\"" ^ s ^ "\"") cfd.cfd_fields) in
		let props_str = String.concat "," (List.map (fun s -> "\"" ^ s ^ "\"") cfd.cfd_props) in
		let method_str = String.concat "," (List.map (fun s -> "\"" ^ s ^ "\"") cfd.cfd_methods) in
		newline ctx;
		print ctx "%s._hx_class = %s\n" p p;
		print ctx "%s._hx_class_name = \"%s\"\n" p p_name;
		print ctx "_hx_classes[\"%s\"] = %s\n" p_name p;
		print ctx "_hx_c.%s = %s\n" p p;
		print ctx "%s._hx_fields = [%s]\n" p field_str;
		print ctx "%s._hx_props = [%s]\n" p props_str;
		print ctx "%s._hx_methods = [%s]\n" p method_str;
		(* TODO: statics *)
		print ctx "%s._hx_interfaces = [%s]\n" p (String.concat "," p_interfaces);
		match p_super with
			| None ->
				()
			| Some ps ->
				print ctx "%s._hx_super = %s\n" p ps

	let gen_meta_members ctx fields =
		spr ctx "_hx_c._hx_AnonObject(";
		List.iter (fun cf ->
			()
		) fields;
		spr ctx ")"

	let gen_class_metadata ctx c p =
		(* TODO: can we use Codegen.make_meta? *)
		print ctx "%s._hx_meta = _hx_c._hx_AnonObject(" p;
		spr ctx "obj=";
		spr ctx (get_meta_entries c.cl_meta);
		spr ctx ",statics=";
		gen_meta_members ctx c.cl_ordered_statics;
		spr ctx ",fields=";
		let fields = match c.cl_constructor with
			| None -> c.cl_ordered_fields
			| Some cf -> cf :: c.cl_ordered_fields
		in
		gen_meta_members ctx fields;
		spr ctx ")\n"

	let gen_enum_metadata ctx en p =
		(* TODO: can we use Codegen.make_meta? *)
		()

	let gen_class_empty_constructor ctx p cfl =
		let s_name = p ^ "_hx_empty_init" in
		print ctx "def %s (_hx_o):" s_name;
		let found_fields = ref false in
		List.iter (fun cf -> match cf.cf_kind with
				| Var ({v_read = AccResolve | AccCall}) ->
					()
				| Var _ ->
					found_fields := true;
					print ctx "\t_hx_o.%s = None\n" (handle_keywords cf.cf_name)
				| _ ->
					()
		) cfl;
		if !found_fields then
			spr ctx "\tpass\n";
		print ctx "%s._hx_empty_init = %s" p s_name

	let gen_class_statics ctx c p =
		let f = fun () ->
			List.iter (fun cf -> gen_static_field ctx c p cf) c.cl_ordered_statics;
			spr ctx "\n";
		in
		ctx.static_inits <- f :: ctx.static_inits

	let gen_class ctx c =
		gen_pre_code_meta ctx c.cl_meta;
		print ctx "# print %s.%s\n" (s_type_path c.cl_module.m_path) (snd c.cl_path);
		if not c.cl_extern then begin
			let mt = (t_infos (TClassDecl c)) in
			let p = get_path mt in
			let p_name = get_full_name mt in
			print ctx "class %s" p;
			let p_super = match c.cl_super with
				| None ->
					None
				| Some (csup,_) ->
					let p = get_path (t_infos (TClassDecl csup)) in
					print ctx "(%s)" p;
					Some p
			in
			let p_interfaces = List.map (fun (c,tl) ->
				get_path (t_infos (TClassDecl c))
			) c.cl_implements in
			spr ctx ":";
			open_block ctx;
			gen_class_constructor ctx c;
			List.iter (fun cf -> gen_class_field ctx c p cf) c.cl_ordered_fields;
			let x = collect_class_field_data c.cl_ordered_fields in
			let use_pass = match x.cfd_methods with
				| [] -> c.cl_constructor = None
				| _ -> c.cl_interface
			in
			if use_pass then spr_line ctx "\tpass";
			close_block ctx;
			gen_class_data ctx c x p_super p_interfaces p p_name;
			gen_class_metadata ctx c p;
			gen_class_empty_constructor ctx p c.cl_ordered_fields;
			gen_class_statics ctx c p;
		end

	let gen_enum ctx en =
		let mt = (t_infos (TEnumDecl en)) in
		let p = get_path mt in
		let p_name = get_full_name mt in
		print ctx "class %s(_hx_c.Enum):\n" p;
		spr ctx "\tdef __init__(self, t, i, p):\n";
		print ctx "\t\tsuper(%s,self).__init__(t, i, p)" p;
		newline ctx;
		let enum_constructs = PMap.foldi (fun k ef acc ->
			let f = handle_keywords ef.ef_name in
			begin match follow ef.ef_type with
				| TFun(args,_) ->
					let param_str = String.concat "," (List.map (fun (n,o,_) -> Printf.sprintf "%s%s" (handle_keywords n) (if o then " = None" else "")) args) in
					let args_str = String.concat "," (List.map (fun (n,_,_) -> handle_keywords n) args) in
					print ctx "def _%s_statics_%s (%s):\n" p f param_str;
					print ctx "\treturn %s(\"%s\", %i, [%s])\n" p ef.ef_name ef.ef_index args_str;
					print ctx "%s.%s = _%s_statics_%s\n" p f p f;
				| _ ->
					(* TODO: haxe source has api.quoteString for ef.ef_name *)
					print ctx "%s.%s = %s(%s, %i, list())\n" p f p ef.ef_name ef.ef_index
			end;
			newline ctx;
			ef :: acc
		) en.e_constrs [] in
		let fix = match enum_constructs with [] -> "" | _ -> "\"" in
		let enum_constructs = List.sort (fun a b -> if a.ef_index < b.ef_index then -1 else if a.ef_index > b.ef_index then 1 else 0) enum_constructs in
		let enum_constructs_str = fix ^ (String.concat ("\",\"") (List.map (fun ef -> ef.ef_name) enum_constructs)) ^ fix in
		print ctx "%s._hx_constructs = [%s]\n" p enum_constructs_str;
		print ctx "%s._hx_class = %s\n" p p;
		print ctx "%s._hx_class_name = \"%s\"\n" p p_name;
		print ctx "_hx_classes[\"%s\"] = %s\n" p_name p;
		print ctx "_hx_c.%s = %s\n" p p;
		gen_enum_metadata ctx en p

	let gen_type ctx mt = match mt with
		| TClassDecl c -> gen_class ctx c
		| TEnumDecl en -> gen_enum ctx en
		| _ -> ()

	(* Generator parts *)

	let gen_resources ctx =
		if Hashtbl.length ctx.com.resources > 0 then begin
			spr ctx "def _hx_resources__():\n\treturn {";
			let first = ref true in
			Hashtbl.iter (fun k v ->
				let prefix = if !first then begin
					first := true;
					"";
				end else
					","
				in
				print ctx "%s'%s':'%s'" prefix k v
			) ctx.com.resources;
			spr ctx "}\n"
		end

	let gen_boot_code ctx =
		(* TODO *)
		()

	let gen_boot_class ctx =
		let boot = List.find (fun mt -> match mt with
			| TClassDecl {cl_path = ["python"],"Boot"} -> true
			| _ -> false
		) ctx.com.types in
		gen_type ctx boot

	let gen_types ctx =
		List.iter (fun mt -> match mt with
			| TClassDecl {cl_path = ["python"],"Boot"} ->
				()
			| _ ->
				gen_type ctx mt
		) ctx.com.types

	let gen_static_inits ctx =
		List.iter (fun f -> f()) (List.rev ctx.static_inits)

	let gen_main ctx =
		match ctx.com.main with
			| None ->
				()
			| Some e ->
				gen_expr ctx e "" ""

	(* Entry point *)

	let run com =
		let ctx = mk_context com in
		gen_resources ctx;
		gen_boot_code ctx;
		gen_boot_class ctx;
		gen_types ctx;
		gen_static_inits ctx;
		gen_main ctx;

		let ch = open_out_bin com.file in
		output_string ch (Buffer.contents ctx.buf);
		close_out ch
end

let generate com =
	Generator.run com