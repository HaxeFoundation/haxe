open Global
open Common
open Type
open Typecore
open Error

let promote_local_static ctx lut v eo =
	let name = Printf.sprintf "%s_%s" ctx.curfield.cf_name v.v_name in
	begin try
		let cf = PMap.find name ctx.curclass.cl_statics in
		display_error ctx.com (Printf.sprintf "The expanded name of this local (%s) conflicts with another static field" name) v.v_pos;
		raise_typing_error ~depth:1 "Conflicting field was found here" cf.cf_name_pos;
	with Not_found ->
		let cf = mk_field name ~static:true v.v_type v.v_pos v.v_pos in
		cf.cf_meta <- v.v_meta;
		begin match eo with
		| None ->
			()
		| Some e ->
			let rec loop e = match e.eexpr with
				| TLocal _ | TFunction _ ->
					raise_typing_error "Accessing local variables in static initialization is not allowed" e.epos
				| TConst (TThis | TSuper) ->
					raise_typing_error "Accessing `this` in static initialization is not allowed" e.epos
				| TReturn _ | TBreak | TContinue ->
					raise_typing_error "This kind of control flow in static initialization is not allowed" e.epos
				| _ ->
					iter loop e
			in
			loop e;
			cf.cf_expr <- Some e
		end;
		TClass.add_field ctx.curclass cf;
		Hashtbl.add lut v.v_id cf
	end

let find_local_static lut v =
	Hashtbl.find lut v.v_id

let run ctx e =
	let local_static_lut = Hashtbl.create 0 in
	let c = ctx.curclass in
	let rec run e = match e.eexpr with
		| TBlock el ->
			let el = ExtList.List.filter_map (fun e -> match e.eexpr with
				| TVar(v,eo) when has_var_flag v VStatic ->
					promote_local_static ctx local_static_lut v eo;
					None
				| _ ->
					Some (run e)
			) el in
			{ e with eexpr = TBlock el }
		| TLocal v when has_var_flag v VStatic ->
			begin try
				let cf = find_local_static local_static_lut v in
				Texpr.Builder.make_static_field c cf e.epos
			with Not_found ->
				raise_typing_error (Printf.sprintf "Could not find local static %s (id %i)" v.v_name v.v_id) e.epos
			end
		| _ ->
			Type.map_expr run e
	in
	run e