open Globals
open Common
open Type


let add_field_inits cl_path locals com t =
	let apply c =
		let ethis = mk (TConst TThis) (TInst (c,extract_param_types c.cl_params)) c.cl_pos in
		(* TODO: we have to find a variable name which is not used in any of the functions *)
		let v = alloc_var VGenerated "_g" ethis.etype ethis.epos in
		let need_this = ref false in
		let inits,fields = List.fold_left (fun (inits,fields) cf ->
			match cf.cf_kind,cf.cf_expr with
			| Var _, Some _ -> (cf :: inits, cf :: fields)
			| _ -> (inits, cf :: fields)
		) ([],[]) c.cl_ordered_fields in
		c.cl_ordered_fields <- (List.rev fields);
		match inits with
		| [] -> ()
		| _ ->
			let el = List.map (fun cf ->
				match cf.cf_expr with
				| None -> die "" __LOC__
				| Some e ->
					let lhs = mk (TField({ ethis with epos = cf.cf_pos },FInstance (c,extract_param_types c.cl_params,cf))) cf.cf_type cf.cf_pos in
					cf.cf_expr <- None;
					mk (TBinop(OpAssign,lhs,e)) cf.cf_type e.epos
			) inits in
			let el = if !need_this then (mk (TVar((v, Some ethis))) ethis.etype ethis.epos) :: el else el in
			let cf = match c.cl_constructor with
			| None ->
				let ct = TFun([],com.basic.tvoid) in
				let ce = mk (TFunction {
					tf_args = [];
					tf_type = com.basic.tvoid;
					tf_expr = mk (TBlock el) com.basic.tvoid c.cl_pos;
				}) ct c.cl_pos in
				let ctor = mk_field "new" ct c.cl_pos null_pos in
				ctor.cf_kind <- Method MethNormal;
				{ ctor with cf_expr = Some ce }
			| Some cf ->
				match cf.cf_expr with
				| Some { eexpr = TFunction f } ->
					let bl = match f.tf_expr with {eexpr = TBlock b } -> b | x -> [x] in
					let ce = mk (TFunction {f with tf_expr = mk (TBlock (el @ bl)) com.basic.tvoid c.cl_pos }) cf.cf_type cf.cf_pos in
					{cf with cf_expr = Some ce };
				| _ ->
					die "" __LOC__
			in
			let config = AnalyzerConfig.get_field_config com c cf in
			remove_class_field_flag cf CfPostProcessed;
			Analyzer.Run.run_on_field com config c cf;
			add_class_field_flag cf CfPostProcessed;
			(match cf.cf_expr with
			| Some e ->
				(* This seems a bit expensive, but hopefully constructor expressions aren't that massive. *)
				let e = RenameVars.run cl_path locals e in
				let e = Optimizer.sanitize com e in
				cf.cf_expr <- Some e
			| _ ->
				());
			c.cl_constructor <- Some cf
	in
	match t with
	| TClassDecl c ->
		apply c
	| _ ->
		()