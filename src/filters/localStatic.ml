open Common
open Type
open Typecore
open Error

type lscontext = {
	ctx : typer;
	lut : (int,tclass_field) Hashtbl.t;
	mutable added_fields : tclass_field list;
}

let promote_local_static lsctx run v eo =
	let name = Printf.sprintf "%s_%s" lsctx.ctx.curfield.cf_name v.v_name in
	let c = lsctx.ctx.curclass in
	begin try
		let cf = PMap.find name c.cl_statics in
		display_error lsctx.ctx.com (Printf.sprintf "The expanded name of this local (%s) conflicts with another static field" name) v.v_pos;
		raise_typing_error ~depth:1 "Conflicting field was found here" cf.cf_name_pos;
	with Not_found ->
		let cf = mk_field name ~static:true v.v_type v.v_pos v.v_pos in
		cf.cf_meta <- v.v_meta;
		begin match eo with
		| None ->
			()
		| Some e ->
			let no_local_in_static p =
				raise_typing_error "Accessing local variables in static initialization is not allowed" p
			in
			let rec loop e = match e.eexpr with
				| TLocal v when has_var_flag v VStatic ->
					run e
				| TFunction _ | TLocal _ ->
					no_local_in_static e.epos
				| TConst (TThis | TSuper) ->
					raise_typing_error "Accessing `this` in static initialization is not allowed" e.epos
				| TReturn _ | TBreak | TContinue ->
					raise_typing_error "This kind of control flow in static initialization is not allowed" e.epos
				| _ ->
					map_expr loop e
			in
			let e = loop e in
			cf.cf_expr <- Some e
		end;
		lsctx.added_fields <- cf :: lsctx.added_fields;
		(* Add to lookup early so that the duplication check works. *)
		c.cl_statics <- PMap.add cf.cf_name cf c.cl_statics;
		Hashtbl.add lsctx.lut v.v_id cf
	end

let find_local_static lut v =
	Hashtbl.find lut v.v_id

let run ctx e =
	let lsctx = {
		ctx = ctx;
		lut = Hashtbl.create 0;
		added_fields = [];
	} in
	let c = ctx.curclass in
	let rec run e = match e.eexpr with
		| TBlock el ->
			let el = ExtList.List.filter_map (fun e -> match e.eexpr with
				| TVar(v,eo) when has_var_flag v VStatic ->
					promote_local_static lsctx run v eo;
					None
				| _ ->
					Some (run e)
			) el in
			{ e with eexpr = TBlock el }
		| TLocal v when has_var_flag v VStatic ->
			begin try
				let cf = find_local_static lsctx.lut v in
				Texpr.Builder.make_static_field c cf e.epos
			with Not_found ->
				raise_typing_error (Printf.sprintf "Could not find local static %s (id %i)" v.v_name v.v_id) e.epos
			end
		| _ ->
			Type.map_expr run e
	in
	let e = run e in
	(* Add to ordered list in reverse order *)
	List.iter (fun cf ->
		c.cl_ordered_statics <- cf :: c.cl_ordered_statics
	) lsctx.added_fields;
	e
