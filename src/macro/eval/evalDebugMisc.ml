open Ast
open Globals
open MacroApi
open EvalContext
open EvalHash
open EvalValue
open EvalEncode

type debug_connection = {
	wait : context -> (env -> value) -> env -> value;
	bp_stop : context -> env -> unit;
	exc_stop : context -> value -> pos -> unit;
}

exception BreakHere

(* Breakpoints *)

let make_breakpoint =
	let id = ref (-1) in
	(fun file line state column ->
		incr id;
		{
			bpid = !id;
			bpfile = file;
			bpline = line;
			bpstate = state;
			bpcolumn = column;
		}
	)

let iter_breakpoints ctx f =
	Hashtbl.iter (fun _ breakpoints ->
		Hashtbl.iter (fun _ breakpoint -> f breakpoint) breakpoints
	) ctx.debug.breakpoints

let add_breakpoint ctx file line column =
	let hash = hash_s (Path.unique_full_path (Common.find_file (ctx.curapi.get_com()) file)) in
	let h = try
		Hashtbl.find ctx.debug.breakpoints hash
	with Not_found ->
		let h = Hashtbl.create 0 in
		Hashtbl.add ctx.debug.breakpoints hash h;
		h
	in
	let breakpoint = make_breakpoint hash line BPEnabled column in
	Hashtbl.replace h line breakpoint;
	breakpoint

let delete_breakpoint ctx file line =
	let hash = hash_s (Path.unique_full_path (Common.find_file (ctx.curapi.get_com()) file)) in
	let h = Hashtbl.find ctx.debug.breakpoints hash in
	Hashtbl.remove h line

let find_breakpoint ctx sid =
	let found = ref None in
	let id = try int_of_string sid with _ -> raise Not_found in
	try
		iter_breakpoints ctx (fun breakpoint ->
			if breakpoint.bpid = id then begin
				found := Some breakpoint;
				raise Exit
			end
		);
		raise Not_found
	with Exit ->
		match !found with None -> assert false | Some breakpoint -> breakpoint


(* Helper *)

exception Parse_expr_error of string

let parse_expr ctx s p =
	let error s = raise (Parse_expr_error s) in
	Parser.parse_expr_string (ctx.curapi.get_com()) s p error false

(* Vars *)

let get_var_slot_by_name scopes name =
	let rec loop scopes = match scopes with
		| scope :: scopes ->
			begin try
				let id = Hashtbl.find scope.local_ids name in
				let slot = Hashtbl.find scope.locals id in
				slot + scope.local_offset
			with Not_found ->
				loop scopes
			end
		| [] ->
			raise Not_found
	in
	loop scopes

let get_capture_slot_by_name capture_infos name =
	let ret = ref None in
	try
		Hashtbl.iter (fun slot name' ->
			if name = name' then begin
				ret := (Some slot);
				raise Exit
			end
		) capture_infos;
		raise Not_found
	with Exit ->
		match !ret with None -> assert false | Some name -> name

let get_variable capture_infos scopes name env =
	try
		let slot = get_var_slot_by_name scopes name in
		let value = env.env_locals.(slot) in
		value
	with Not_found ->
		let slot = get_capture_slot_by_name capture_infos name in
		let value = try env.env_captures.(slot) with _ -> raise Not_found in
		!value

(* Expr to value *)

let resolve_ident ctx env s =
	let key = hash_s s in
	try
		(* 1. Variable *)
		get_variable env.env_info.capture_infos env.env_debug.scopes s env
	with Not_found -> try
		(* 2. Instance *)
		if env.env_info.static then raise Not_found;
		let v = env.env_locals.(0) in
		EvalField.field_raise v key
	with Not_found -> try
		(* 3. Static *)
		begin match env.env_info.kind with
			| EKMethod(i1,_) ->
				let proto = get_static_prototype_raise ctx i1 in
				EvalField.proto_field_raise proto key
			| _ ->
				raise Not_found
		end
	with Not_found -> try
		(* 4. Type *)
		VPrototype (IntMap.find key ctx.static_prototypes)
	with Not_found ->
		raise Exit

let expr_to_value ctx env e =
	let rec loop e = match fst e with
		| EConst cst ->
			begin match cst with
				| String s -> "",encode_string s
				| Int s -> "",VInt32 (Int32.of_string s)
				| Float s -> "",VFloat (float_of_string s)
				| Ident "true" -> "",VTrue
				| Ident "false" -> "",VFalse
				| Ident "null" -> "",VNull
				| Ident s ->
					let value = resolve_ident ctx env s in
					s,value
				| _ -> raise Exit
			end
		| EArray(e1,eidx) ->
			let n1,v1 = loop e1 in
			let nidx,vidx = loop eidx in
			let idx = match vidx with VInt32 i -> Int32.to_int i | _ -> raise Exit in
			let n = Printf.sprintf "%s[%d]" n1 idx in
			begin match v1 with
				| VArray va ->
					let v = EvalArray.get va idx in
					(n,v)
				| VVector vv ->
					let v = Array.get vv idx in
					(n,v)
				| VEnumValue ev ->
					let v = Array.get ev.eargs idx in
					(n,v)
				| _ ->
					raise Exit
			end
		| EField(e1,s) ->
			let n1,v1 = loop e1 in
			let v = EvalField.field v1 (hash_s s) in
			(Printf.sprintf "%s.%s" n1 s),v
		| _ ->
			raise Exit
	in
	loop e
