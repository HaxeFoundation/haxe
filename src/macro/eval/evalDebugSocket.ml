open Gc
open Ast
open Type
open Globals
open MacroApi
open JsonRpcSocket
open Json
open EvalContext
open EvalValue
open EvalHash
open EvalPrinting
open EvalMisc
open EvalDebugMisc

(* Printing *)

let var_to_json name value access =
	let jv t v structured =
		JObject ["name",JString name;"type",JString t;"value",JString v;"structured",JBool structured;"access",JString access]
	in
	let string_repr s = "\"" ^ (Ast.s_escape (EvalString.get s)) ^ "\"" in
	let rec level2_value_repr = function
		| VNull -> "null"
		| VTrue -> "true"
		| VFalse -> "false"
		| VInt32 i -> Int32.to_string i
		| VFloat f -> string_of_float f
		| VEnumValue ve ->
			let name = EvalPrinting.s_enum_ctor_name ve in
			begin match ve.eargs with
				| [||] -> name
				| vl -> name ^ "(...)"
			end
		| VObject o -> "{...}"
		| VString s -> string_repr s
		| VArray _ | VVector _ -> "[...]"
		| VInstance vi -> (rev_hash_s vi.iproto.ppath) ^ " {...}"
		| VPrototype proto -> EvalString.get (s_proto_kind proto)
		| VFunction _ | VFieldClosure _ -> "<fun>"
		| VLazy f -> level2_value_repr (!f())
	in
	let fields_string fields =
		let l = List.map (fun (name, value) -> Printf.sprintf "%s: %s" (rev_hash_s name) (level2_value_repr value)) fields in
		Printf.sprintf "{%s}" (String.concat ", " l)
	in
	let array_elems l =
		let l = List.map level2_value_repr l in
		Printf.sprintf "[%s]" (String.concat ", " l)
	in
	let rec value_string v = match v with
		| VNull -> jv "NULL" "null" false
		| VTrue -> jv "Bool" "true" false
		| VFalse -> jv "Bool" "false" false
		| VInt32 i -> jv "Int" (Int32.to_string i) false
		| VFloat f -> jv "Float" (string_of_float f) false
		| VEnumValue ve ->
			let type_s = rev_hash_s ve.epath in
			let name = EvalPrinting.s_enum_ctor_name ve in
			let value_s,is_structured = match ve.eargs with
				| [||] -> name, false
				| vl ->
					let l = Array.to_list (Array.map level2_value_repr vl) in
					let s = Printf.sprintf "%s(%s)" name (String.concat ", " l) in
					s, true
			in
			jv type_s value_s is_structured
		| VObject o -> jv "Anonymous" (fields_string (object_fields o)) true (* TODO: false for empty structures *)
		| VString s -> jv "String" (string_repr s) false
		| VArray va -> jv "Array" (array_elems (EvalArray.to_list va)) true (* TODO: false for empty arrays *)
		| VVector vv -> jv "Vector" (array_elems (Array.to_list vv)) true
		| VInstance vi ->
			let class_name = rev_hash_s vi.iproto.ppath in
			jv class_name (class_name ^ " " ^ (fields_string (instance_fields vi))) true
		| VPrototype proto -> jv "Anonymous" (EvalString.get (s_proto_kind proto)) false (* TODO: show statics *)
		| VFunction _ | VFieldClosure _ -> jv "Function" "<fun>" false
		| VLazy f -> value_string (!f())
	in
	value_string value

let get_call_stack_envs ctx kind p =
	let envs = match call_stack ctx with
		| _ :: envs -> envs
		| [] -> []
	in
	let rec loop delta envs = match envs with
		| _ :: envs when delta < 0 -> loop (delta + 1) envs
		| _ -> envs
	in
	loop ctx.debug.environment_offset_delta envs

let output_call_stack ctx kind p =
	let envs = get_call_stack_envs ctx kind p in
	let id = ref (-1) in
	let stack_item kind p artificial =
		incr id;
		let line1,col1,line2,col2 = Lexer.get_pos_coords p in
		JObject [
			"id",JInt !id;
			"name",JString (kind_name (get_eval ctx) kind);
			"source",JString (Path.get_real_path p.pfile);
			"line",JInt line1;
			"column",JInt col1;
			"endLine",JInt line2;
			"endColumn",JInt col2;
			"artificial",JBool artificial;
		]
	in
	let l = [stack_item kind p false] in
	let stack = List.fold_left (fun acc env ->
		let p = {pmin = env.env_leave_pmin; pmax = env.env_leave_pmax; pfile = rev_file_hash env.env_info.pfile} in
		(stack_item env.env_info.kind p (env.env_leave_pmin < 0)) :: acc
	) l envs in
	JArray (List.rev stack)

let output_scopes capture_infos scopes =
	let mk_scope id name pos =
		let fl = ["id",JInt id; "name",JString name] in
		let fl =
			if pos <> null_pos then
				let line1,col1,line2,col2 = Lexer.get_pos_coords pos in
				("pos",JObject [
					"source",JString (Path.get_real_path pos.pfile);
					"line",JInt line1;
					"column",JInt col1;
					"endLine",JInt line2;
					"endColumn",JInt col2;
				]) :: fl
			else
				fl
		in
		JObject fl
	in
	let _,scopes = List.fold_left (fun (id,acc) scope ->
		if Hashtbl.length scope.local_infos <> 0 then
			(id + 1), (mk_scope id "Locals" scope.pos) :: acc
		else
			(id + 1), acc
	) (1,[]) scopes in
	let scopes = List.rev scopes in
	let scopes = if Hashtbl.length capture_infos = 0 then scopes else (mk_scope 0 "Captures" null_pos) :: scopes in
	JArray scopes

let output_capture_vars env =
	let infos = env.env_info.capture_infos in
	let vars = Hashtbl.fold (fun slot name acc ->
		let value = !(env.env_captures.(slot)) in
		(var_to_json name value name) :: acc
	) infos [] in
	JArray vars

let output_scope_vars env scope =
	let vars = Hashtbl.fold (fun local_slot name acc ->
		let slot = local_slot + scope.local_offset in
		let value = env.env_locals.(slot) in
		(var_to_json name value name) :: acc
	) scope.local_infos [] in
	JArray vars

let output_inner_vars v access =
	let rec loop v = match v with
		| VNull | VTrue | VFalse | VInt32 _ | VFloat _ | VFunction _ | VFieldClosure _ -> []
		| VEnumValue ve ->
			begin match ve.eargs with
				| [||] -> []
				| vl ->
					Array.to_list (Array.mapi (fun i v ->
						let n = Printf.sprintf "[%d]" i in
						let a = access ^ n in
						n, v, a
					) vl)
			end
		| VObject o ->
			let fields = object_fields o in
			List.map (fun (n,v) ->
				let n = rev_hash_s n in
				let a = access ^ "." ^ n in
				n, v, a
			) fields
		| VString _ -> []
		| VArray va ->
			let l = EvalArray.to_list va in
			List.mapi (fun i v ->
				let n = Printf.sprintf "[%d]" i in
				let a = access ^ n in
				n, v, a
			) l
		| VVector vv ->
			let l = Array.to_list vv in
			List.mapi (fun i v ->
				let n = Printf.sprintf "[%d]" i in
				let a = access ^ n in
				n, v, a
			) l
		| VInstance {ikind = IStringMap h} ->
			StringHashtbl.fold (fun k v acc ->
				let s = EvalString.get k in
				let n = Printf.sprintf "[%s]" s in
				let a = access ^ n in
				(s,v,a) :: acc
			) h []
		| VInstance vi ->
			let fields = instance_fields vi in
			List.map (fun (n,v) ->
				let n = rev_hash_s n in
				let a = access ^ "." ^ n in
				n, v, a
			) fields
		| VPrototype proto -> [] (* TODO *)
		| VLazy f -> loop (!f())
	in
	let children = loop v in
	let vars = List.map (fun (n,v,a) -> var_to_json n v a) children in
	JArray vars

type command_outcome =
	| Loop of Json.t
	| Run of Json.t * EvalContext.env
	| Wait of Json.t * EvalContext.env

let make_connection socket =
	(* Reads input and reacts accordingly. *)
	let rec wait ctx run env =
		let get_real_env ctx =
			ctx.debug.environment_offset_delta <- 0;
			DynArray.get (get_eval ctx).environments ((get_eval ctx).environment_offset - 1);
		in
		let rec loop () =
			let handle_request id name params =
				let error msg =
					let open JsonRpc in
					raise (JsonRpc_error (Custom (id, 1, msg)))
				in
				let invalid_params () =
					let open JsonRpc in
					raise (JsonRpc_error (Invalid_params id))
				in
				let parse_breakpoint = function
					| JObject fl ->
						let rec loop (line,column,condition) fl = match fl with
							| ("line",JInt i) :: fl -> loop (i,column,condition) fl
							| ("column",JInt i) :: fl -> loop (line,BPColumn i,condition) fl
							| ("condition",JString s) :: fl -> loop (line,column,Some (parse_expr ctx s env.env_debug.expr.epos)) fl
							| _ :: fl -> loop (line,column,condition) fl
							| [] -> line,column,condition
						in
						loop (0,BPAny,None) fl
					| _ -> invalid_params ()
				in
				let rec move_frame offset =
					if offset < 0 || offset >= (get_eval ctx).environment_offset then begin
						error (Printf.sprintf "Frame out of bounds: %i (valid range is %i - %i)" offset 0 ((get_eval ctx).environment_offset - 1))
					end else begin
						ctx.debug.environment_offset_delta <- ((get_eval ctx).environment_offset - offset - 1);
						Wait (JNull, (DynArray.get (get_eval ctx).environments offset))
					end
				in
				match name with
				| "continue" ->
					let env = get_real_env ctx in
					ctx.debug.debug_state <- (if ctx.debug.debug_state = DbgStart then DbgRunning else DbgContinue);
					Run (JNull,env)
				| "stepIn" ->
					let env = get_real_env ctx in
					Run (JNull,env)
				| "next" ->
					let env = get_real_env ctx in
					ctx.debug.debug_state <- DbgNext (get_eval ctx).environment_offset;
					Run (JNull,env)
				| "stepOut" ->
					let env = get_real_env ctx in
					ctx.debug.debug_state <- DbgFinish (get_eval ctx).environment_offset;
					Run (JNull,env)
				| "stackTrace" ->
					Loop (output_call_stack ctx env.env_info.kind env.env_debug.expr.epos)
				| "setBreakpoints" ->
					let file, bps =
						match params with
						| Some (JObject fl) ->
							let file = try List.find (fun (n,_) -> n = "file") fl with Not_found -> invalid_params () in
							let file = match (snd file) with JString s -> s | _ -> invalid_params () in
							let bps = try List.find (fun (n,_) -> n = "breakpoints") fl with Not_found -> invalid_params () in
							let bps = match (snd bps) with JArray jl -> jl | _ -> invalid_params () in
							let bps = List.map parse_breakpoint bps in
							file, bps
						| _ ->
							invalid_params ();
					in
					let hash = hash_s (Path.unique_full_path (Common.find_file (ctx.curapi.get_com()) file)) in
					let h =
						try
							let h = Hashtbl.find ctx.debug.breakpoints hash in
							Hashtbl.clear h;
							h
						with Not_found ->
							let h = Hashtbl.create (List.length bps) in
							Hashtbl.add ctx.debug.breakpoints hash h;
							h
					in
					let bps = List.map (fun (line,column,condition) ->
						let bp = make_breakpoint hash line BPEnabled column condition in
						Hashtbl.add h line bp;
						JObject ["id",JInt bp.bpid]
					) bps in
					Loop (JArray bps)
				| "setBreakpoint" ->
					let file,line,column,condition =
						match params with
						| Some (JObject fl as jo) ->
							let file = try List.find (fun (n,_) -> n = "file") fl with Not_found -> invalid_params () in
							let file = match (snd file) with JString s -> s | _ -> invalid_params () in
							let line,column,condition = parse_breakpoint jo in
							file,line,column,condition
						| _ ->
							invalid_params ();
					in
					begin try
						let breakpoint = add_breakpoint ctx file line column condition in
						Loop (JObject ["id",JInt breakpoint.bpid])
					with Not_found ->
						invalid_params ();
					end
				| "removeBreakpoint" ->
					let id =
						match params with
						| Some (JObject fl) ->
							let id = try List.find (fun (n,_) -> n = "id") fl with Not_found -> invalid_params () in
							(match (snd id) with JInt s -> s | _ -> invalid_params ())
						| _ -> invalid_params ()
					in
					begin try
						Hashtbl.iter (fun _ h ->
							let to_delete = ref [] in
							Hashtbl.iter (fun k breakpoint -> if breakpoint.bpid = id then to_delete := k :: !to_delete) h;
							List.iter (fun k -> Hashtbl.remove h k) !to_delete;
						) ctx.debug.breakpoints
					with Not_found ->
						error (Printf.sprintf "Unknown breakpoint: %d" id)
					end;
					Loop JNull
				| "switchFrame" ->
					let frame =
						match params with
						| Some (JObject fl) ->
							let id = try List.find (fun (n,_) -> n = "id") fl with Not_found -> invalid_params () in
							(match (snd id) with JInt s -> s | _ -> invalid_params ())
						| _ -> invalid_params ()
					in
					move_frame ((get_eval ctx).environment_offset - frame - 1)
				| "getScopes" ->
					Loop (output_scopes env.env_info.capture_infos env.env_debug.scopes);
				| "getScopeVariables" ->
					let sid =
						match params with
						| Some (JObject fl) ->
							let id = try List.find (fun (n,_) -> n = "id") fl with Not_found -> invalid_params () in
							(match (snd id) with JInt s -> s | _ -> invalid_params ())
						| _ -> invalid_params ()
					in
					begin
						let vars =
							try
								if sid = 0 then begin
									output_capture_vars env
								end else begin
									let scope = try List.nth env.env_debug.scopes (sid - 1) with _ -> raise Exit in
									output_scope_vars env scope
								end
							with Exit ->
								error "Invalid scope id"
						in
						Loop vars
					end
				| "getStructure" ->
					let e =
						match params with
						| Some (JObject fl) ->
							let id = try List.find (fun (n,_) -> n = "expr") fl with Not_found -> invalid_params () in
							(match (snd id) with JString s -> s | _ -> invalid_params ())
						| _ -> invalid_params ()
					in
					begin try
						let e = parse_expr ctx e env.env_debug.expr.epos in
						begin try
							let v = expr_to_value ctx env e in
							Loop (output_inner_vars v (Ast.s_expr e))
						with Exit ->
							error ("Don't know how to handle this expression: " ^ (Ast.s_expr e))
						end
					with Parse_expr_error e ->
						error e
					end
				| "setVariable" ->
					let expr_s,value =
						match params with
						| Some (JObject fl) ->
							let expr = try List.find (fun (n,_) -> n = "expr") fl with Not_found -> invalid_params () in
							let expr = match (snd expr) with JString s -> s | _ -> invalid_params () in
							let value = try List.find (fun (n,_) -> n = "value") fl with Not_found -> invalid_params () in
							let value = match (snd value) with JString s -> s | _ -> invalid_params () in
							expr,value
						| _ ->
							invalid_params ();
					in
					let parse s = parse_expr ctx s env.env_debug.expr.epos in
					begin try
						let expr,value = parse expr_s,parse value in
						begin try
							let value = expr_to_value ctx env value in
							write_expr ctx env expr value;
							Loop (var_to_json name value expr_s)
						with Exit ->
							error "Don't know how to handle this expression"
						end
					with Parse_expr_error e ->
						error e
					end
				| "evaluate" ->
					let s =
						match params with
						| Some (JObject fl) ->
							let id = try List.find (fun (n,_) -> n = "expr") fl with Not_found -> invalid_params () in
							(match (snd id) with JString s -> s | _ -> invalid_params ())
						| _ -> invalid_params ()
					in
					begin try
						let e = parse_expr ctx s env.env_debug.expr.epos in
						let v = expr_to_value ctx env e in
						Loop (var_to_json "" v (Ast.s_expr e))
					with
					| Parse_expr_error e ->
						error e
					| Exit ->
						error "Don't know how to handle this expression"
					end
				| "setExceptionOptions" ->
					let sl = match params with
						| Some (JArray ja) -> List.map (function JString s -> s | _ -> invalid_params()) ja
						| _ -> invalid_params()
					in
					ctx.debug.exception_mode <- if List.mem "all" sl then CatchAll
						else if List.mem "uncaught" sl then CatchUncaught
						else CatchNone;
					Loop(JNull)
				| meth ->
					let open JsonRpc in
					raise (JsonRpc_error (Method_not_found (id, meth)))
			in
			let process_outcome id outcome =
				let output j = send_json socket (JsonRpc.result id j) in
				match outcome with
				| Loop result ->
					output result;
					loop ()
				| Run (result,env) ->
					output result;
					run env
				| Wait (result,env) ->
					output result;
					wait ctx run env;
			in
			let send_output_and_continue json =
				send_json socket json;
				loop ();
			in
			JsonRpc.handle_jsonrpc_error (fun () -> JsonRpc.process_request (Socket.read_string socket) handle_request process_outcome) send_output_and_continue;
		in
		loop ()
	in
	let output_breakpoint_stop _ _ =
		send_event socket "breakpointStop" None
	in
	let output_exception_stop _ v _ =
		send_event socket "exceptionStop" (Some (JObject ["text",JString (value_string v)]))
	in
	{
		wait = wait;
		bp_stop = output_breakpoint_stop;
		exc_stop = output_exception_stop;
	}
