open Gc
open Ast
open Type
open Globals
open MacroApi
open EvalContext
open EvalValue
open EvalExceptions
open EvalHash
open EvalPrinting
open EvalMisc
open EvalDebugMisc

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

(* Printing *)

let value_string value =
	let rec fields_string depth fields =
		let tabs = String.make (depth * 2) ' ' in
		let l = List.map (fun (name,value) ->
			let s_type,s_value = value_string depth value in
			Printf.sprintf "%s%s : %s = %s" tabs (rev_hash_s name) s_type s_value
		) fields in
		Printf.sprintf "{\n%s\n%s}" (String.concat "\n" l) tabs
	and instance_fields depth vi =
		let fields = IntMap.fold (fun name key acc ->
			(name,vi.ifields.(key)) :: acc
		) vi.iproto.pinstance_names [] in
		fields_string (depth + 1) fields
	and value_string depth v = match v with
		| VNull -> "NULL","null"
		| VTrue -> "Bool","true"
		| VFalse -> "Bool","false"
		| VInt32 i -> "Int",Int32.to_string i
		| VFloat f -> "Float",string_of_float f
		| VEnumValue ev -> rev_hash_s ev.epath,Rope.to_string (s_enum_value 0 ev)
		| VObject o -> "Anonymous",fields_string (depth + 1) (object_fields o)
		| VString(_,s) -> "String","\"" ^ (Ast.s_escape (Lazy.force s)) ^ "\""
		| VArray va -> "Array",Rope.to_string (s_array (depth + 1) va)
		| VVector vv -> "Vector",Rope.to_string (s_vector (depth + 1) vv)
		| VInstance vi -> rev_hash_s vi.iproto.ppath,instance_fields (depth + 1) vi
		| VPrototype proto -> "Anonymous",Rope.to_string (s_proto_kind proto)
		| VFunction _ | VFieldClosure _ -> "Function","fun"
	in
	let s_type,s_value = value_string 0 value in
	Printf.sprintf "%s = %s" s_type s_value

let send_string s =
	print_endline s

let output_info = send_string
let output_error = send_string

let output_exception_stop ctx v pos =
	output_info (uncaught_exception_string v pos "")

let output_variable_name name =
	send_string (Printf.sprintf "%s" name)

let output_value name value =
	send_string (Printf.sprintf "%s : %s" name (value_string value))

let output_call_stack_position ctx i kind p =
	let line = Lexer.get_error_line p in
	send_string (Printf.sprintf "%6i : %s at %s:%i" i (kind_name (get_eval ctx) kind) (Path.get_real_path p.pfile) line)

let output_call_stack ctx kind p =
	let envs = get_call_stack_envs ctx kind p in
	let i = ref ((get_eval ctx).environment_offset - 1) in
	output_call_stack_position ctx !i kind {p with pfile = Path.unique_full_path p.Globals.pfile};
	List.iter (fun env ->
		if env.env_leave_pmin >= 0 then begin
			let p = {pmin = env.env_leave_pmin; pmax = env.env_leave_pmax; pfile = rev_file_hash env.env_info.pfile} in
			decr i;
			output_call_stack_position ctx !i env.env_info.kind p
		end
	) envs

let output_file_path s = send_string (Path.get_real_path s)

let output_type_name = send_string
let output_breakpoint breakpoint =
	let flag = match breakpoint.bpstate with
		| BPHit | BPEnabled -> "E"
		| BPDisabled -> "d"
	in
	send_string (Printf.sprintf "%i %s" breakpoint.bpid flag)

let output_breakpoints ctx =
	iter_breakpoints ctx (fun breakpoint ->
		output_breakpoint breakpoint
	)

let output_breakpoint_set breakpoint =
	output_info (Printf.sprintf "Breakpoint %i set and enabled" breakpoint.bpid)

let output_breakpoint_stop ctx env =
	output_info (Printf.sprintf "Thread %i stopped in %s at %s:%i." 0 (kind_name (get_eval ctx) env.env_info.kind) (rev_file_hash env.env_info.pfile) env.env_debug.line)

let output_breakpoint_description breakpoint =
	let s_col = match breakpoint.bpcolumn with
		| BPAny -> ""
		| BPColumn i -> ":" ^ (string_of_int i)
	in
	send_string (Printf.sprintf "%s:%i%s" ((Path.get_real_path (rev_file_hash breakpoint.bpfile))) breakpoint.bpline s_col)

let read_line () =
	input_line Pervasives.stdin

let parse_breakpoint_pattern pattern =
	(* TODO: more than file:line patterns? *)
	try
		let split = ExtString.String.nsplit pattern ":" in
		let file,line,column = match List.rev split with
			| first :: rest ->
				let first = int_of_string first in
				begin match rest with
					| second :: file ->
						begin try
							file,(int_of_string second),BPColumn first
						with _ ->
							(second :: file),first,BPAny
						end
					| file ->
						file,first,BPAny
				end
			| [] -> raise Exit
		in
		let file = String.concat ":" (List.rev file) in
		file,line,column
	with _ ->
		raise Exit

let print_variables ctx capture_infos scopes env =
	let rec loop scopes = match scopes with
		| scope :: scopes ->
			Hashtbl.iter (fun _ name -> output_variable_name name) scope.local_infos;
			loop scopes
		| [] ->
			()
	in
	loop scopes;
	Hashtbl.iter (fun slot name ->
		if slot < Array.length env.env_captures then
			output_variable_name name
	) capture_infos


let set_variable ctx scopes name value env =
	try
		let slot = get_var_slot_by_name scopes name in
		env.env_locals.(slot) <- value;
		output_value name value;
	with Not_found ->
		output_error ("No variable found: " ^ name)

(* Reads input and reacts accordingly. *)
let rec wait ctx run env =
	let get_real_env ctx =
		ctx.debug.environment_offset_delta <- 0;
		DynArray.get (get_eval ctx).environments ((get_eval ctx).environment_offset - 1);
	in
	let rec move_frame offset : value =
		if offset < 0 || offset >= (get_eval ctx).environment_offset then begin
			output_error (Printf.sprintf "Frame out of bounds: %i (valid range is %i - %i)" offset 0 ((get_eval ctx).environment_offset - 1));
			loop()
		end else begin
			ctx.debug.environment_offset_delta <- ((get_eval ctx).environment_offset - offset - 1);
			wait ctx run (DynArray.get (get_eval ctx).environments offset);
		end
	and loop () =
		print_string "1> ";
		flush stdout;
		let line = read_line () in
		match ExtString.String.nsplit line " " with
		| ["quit" | "exit"] ->
			(* TODO: Borrowed from interp.ml *)
			if (get_ctx()).curapi.use_cache() then raise (Error.Fatal_error ("",Globals.null_pos));
			raise (Interp.Sys_exit 0);
		| ["detach"] ->
			Hashtbl.iter (fun _ h ->
				Hashtbl.clear h
			) ctx.debug.breakpoints;
			ctx.debug.debug_state <- DbgRunning;
			run env
		(* source | history *)
		| ["files" | "filespath"] ->
			Hashtbl.iter (fun i _ ->
				output_file_path (rev_file_hash i);
			) ctx.debug.breakpoints;
			loop()
		| ["classes"] ->
			IntMap.iter (fun i _ ->
				output_type_name (rev_hash_s i)
			) ctx.type_cache;
			loop()
		| ["mem"] ->
			output_info (Printf.sprintf "%i" (Gc.stat()).live_words);
			loop()
		| ["compact"] ->
			let before = (Gc.stat()).live_words in
			Gc.compact();
			let after = (Gc.stat()).live_words in
			output_info (Printf.sprintf "before: %i\nafter: %i" before after);
			loop()
		| ["collect"] ->
			let before = (Gc.stat()).live_words in
			Gc.full_major();
			let after = (Gc.stat()).live_words in
			output_info (Printf.sprintf "before: %i\nafter: %i" before after);
			loop()
		| ["break" | "b";pattern] ->
			begin try
				let file,line,column = parse_breakpoint_pattern pattern in
				begin try
					let breakpoint = add_breakpoint ctx file line column in
					output_breakpoint_set breakpoint;
				with Not_found ->
					output_error ("Could not find file " ^ file);
				end;
			with Exit ->
				output_error "Unrecognized breakpoint pattern";
			end;
			loop()
		| ["list" | "l"] ->
			(* TODO: other list syntax *)
			output_breakpoints ctx;
			loop()
		| ["describe" | "desc";bpid] ->
			(* TODO: range patterns? *)
			begin try
				let breakpoint = find_breakpoint ctx bpid in
				output_breakpoint_description breakpoint;
			with Not_found ->
				output_error (Printf.sprintf "Unknown breakpoint: %s" bpid);
			end;
			loop()
		| ["disable" | "dis";bpid] ->
			(* TODO: range patterns? *)
			if bpid = "all" then
				iter_breakpoints ctx (fun breakpoint -> breakpoint.bpstate <- BPDisabled)
			else begin try
				let breakpoint = find_breakpoint ctx bpid in
				breakpoint.bpstate <- BPDisabled;
				output_info (Printf.sprintf "Breakpoint %s disabled" bpid);
			with Not_found ->
				output_error (Printf.sprintf "Unknown breakpoint: %s" bpid);
			end;
			loop()
		| ["enable" | "en";bpid] ->
			(* TODO: range patterns? *)
			if bpid = "all" then
				iter_breakpoints ctx (fun breakpoint -> breakpoint.bpstate <- BPEnabled)
			else begin try
				let breakpoint = find_breakpoint ctx bpid in
				breakpoint.bpstate <- BPEnabled;
				output_info (Printf.sprintf "Breakpoint %s enabled" bpid);
			with Not_found ->
				output_error (Printf.sprintf "Unknown breakpoint: %s" bpid);
			end;
			loop()
		| ["delete" | "d";bpid] ->
			(* TODO: range patterns? *)
			if bpid = "all" then
				Hashtbl.iter (fun _ h ->
					Hashtbl.clear h
				) ctx.debug.breakpoints
			else begin try
				let id = try int_of_string bpid with _ -> raise Not_found in
				Hashtbl.iter (fun _ h ->
					let to_delete = ref [] in
					Hashtbl.iter (fun k breakpoint -> if breakpoint.bpid = id then to_delete := k :: !to_delete) h;
					List.iter (fun k -> Hashtbl.remove h k) !to_delete;
				) ctx.debug.breakpoints;
				output_info (Printf.sprintf "Breakpoint %s deleted" bpid);
			with Not_found ->
				output_error (Printf.sprintf "Unknown breakpoint: %s" bpid);
			end;
			loop()
		| ["clear";pattern] ->
			(* TODO: range patterns? *)
			begin try
				let file,line,column = parse_breakpoint_pattern pattern in
				begin try
					delete_breakpoint ctx file line
				with Not_found ->
					output_info (Printf.sprintf "Could not find breakpoint %s:%i" file line);
				end
			with Exit ->
				output_error ("Unrecognized breakpoint pattern");
			end;
			loop()
		(* thread | unsafe | safe *)
		| ["continue" | "c"] ->
			let env = get_real_env ctx in
			ctx.debug.debug_state <- (if ctx.debug.debug_state = DbgStart then DbgRunning else DbgContinue);
			run env
		| ["step" | "s" | ""] ->
			let env = get_real_env ctx in
			run env
		| ["next" | "n"] ->
			let env = get_real_env ctx in
			ctx.debug.debug_state <- DbgNext (get_eval ctx).environment_offset;
			run env
		| ["finish" | "f"] ->
			let env = get_real_env ctx in
			ctx.debug.debug_state <- DbgFinish (get_eval ctx).environment_offset;
			run env
		| ["where" | "w"] ->
			output_call_stack ctx env.env_info.kind env.env_debug.expr.epos;
			loop()
		| ["up"] ->
			let offset = (get_eval ctx).environment_offset - ctx.debug.environment_offset_delta in
			move_frame (offset - 2)
		| ["down"] ->
			let offset = (get_eval ctx).environment_offset - ctx.debug.environment_offset_delta in
			move_frame offset
		| ["frame";sframe] ->
			let frame = try
				Some (int_of_string sframe)
			with _ ->
				None
			in
			begin match frame with
				| Some frame -> move_frame ((get_eval ctx).environment_offset - frame - 1)
				| None ->
					output_error ("Invalid frame format: " ^ sframe);
					loop()
			end
		| ["variables" | "vars"] ->
			print_variables ctx env.env_info.capture_infos env.env_debug.scopes env;
			loop()
		| ["print" | "p";e] ->
			begin try
				let e = parse_expr ctx e env.env_debug.expr.epos in
				begin try
					let name,v = expr_to_value ctx env e in
					output_value name v
				with Exit ->
					output_error ("Don't know how to handle this expression: " ^ (Ast.s_expr e))
				end
			with Parse_expr_error e ->
				output_error e
			end;
			loop()
		| ["set" | "s";expr_s;"=";value] ->
			let parse s = parse_expr ctx s env.env_debug.expr.epos in
			begin try
				let expr,value = parse expr_s,parse value in
				begin try
					let _,value = expr_to_value ctx env value in
					begin match fst expr with
						(* TODO: support setting array elements and enum values *)
						| EField(e1,s) ->
							let _,v1 = expr_to_value ctx env e1 in
							set_field v1 (hash_s s) value;
						| EConst (Ident s) ->
							set_variable ctx env.env_debug.scopes s value env;
						| _ ->
							raise Exit
					end
				with Exit ->
					output_error ("Don't know how to handle this expression")
				end
			with Parse_expr_error e ->
				output_error e
			end;
			loop()
		| s ->
			output_error (Printf.sprintf "Unknown command: %s" (String.concat " " s));
			loop()
	in
	loop ()

let connection : debug_connection = {
	wait = wait;
	bp_stop = output_breakpoint_stop;
	exc_stop = output_exception_stop;
}
