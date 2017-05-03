open Gc
open Globals
open Ast
open Type
open EvalJitContext
open EvalContext
open EvalValue
open EvalExceptions
open EvalPrinting
open EvalHash
open EvalEncode
open EvalMisc
open MacroApi

let make_breakpoint =
	let id = ref (-1) in
	(fun file line state ->
		incr id;
		{
			bpid = !id;
			bpfile = file;
			bpline = line;
			bpstate = state
		}
	)

let add_breakpoint ctx file line =
	let hash = hash_s (Path.unique_full_path (Common.find_file (ctx.curapi.get_com()) file)) in
	let h = try
		Hashtbl.find ctx.debug.breakpoints hash
	with Not_found ->
		let h = Hashtbl.create 0 in
		Hashtbl.add ctx.debug.breakpoints hash h;
		h
	in
	Hashtbl.replace h line (make_breakpoint hash line BPEnabled)

let delete_breakpoint ctx file line =
	let hash = hash_s (Path.unique_full_path (Common.find_file (ctx.curapi.get_com()) file)) in
	let h = Hashtbl.find ctx.debug.breakpoints hash in
	Hashtbl.remove h line

let output_variable_name = print_endline
let output_variable_value = print_endline
let output_call_stack_position p = print_endline (format_pos p)
let output_file_path = print_endline
let output_type_name = print_endline

let output_breakpoint breakpoint =
	let flag = match breakpoint.bpstate with
		| BPHit | BPEnabled -> "E"
		| BPDisabled -> "d"
	in
	print_endline (Printf.sprintf "%i %s" breakpoint.bpid flag)

let output_breakpoint_description breakpoint =
	print_endline (Printf.sprintf "%s:%i" (rev_hash_s breakpoint.bpfile) breakpoint.bpline)

let output_info = print_endline
let output_error = print_endline

let print_variables jit scopes env =
	let rec loop scopes = match scopes with
		| scope :: scopes ->
			Hashtbl.iter (fun _ name ->
				output_variable_name name
			) scope.local_names;
			loop scopes
		| [] ->
			()
	in
	loop scopes;
	Hashtbl.iter (fun _ name ->
		output_variable_name name
	) jit.capture_names

let get_var_slot_by_name scopes name =
	let rec loop scopes = match scopes with
		| scope :: scopes ->
			begin try
				let id = Hashtbl.find scope.local_ids name in
				let slot = Hashtbl.find scope.locals id + scope.local_offset in
				slot
			with Not_found ->
				loop scopes
			end
		| [] ->
			raise Not_found
	in
	if name = "this" then 0 else loop scopes

let get_capture_slot_by_name jit name =
	let ret = ref (-1) in
	try
		Hashtbl.iter (fun slot name' ->
			if name = name' then begin
				ret := slot;
				raise Exit
			end
		) jit.capture_names;
		raise Not_found
	with Exit ->
		!ret

let print_variable jit scopes name env =
	try
		let slot = get_var_slot_by_name scopes name in
		let value = env.env_locals.(slot) in
		output_variable_value (value_string value);
	with Not_found -> try
		let slot = get_capture_slot_by_name jit name in
		let value = env.env_captures.(slot) in
		output_variable_value (value_string !value)
	with Not_found ->
		output_error ("No variable found: " ^ name)

let set_variable scopes name value env =
	try
		let slot = get_var_slot_by_name scopes name in
		env.env_locals.(slot) <- value;
		output_info (Printf.sprintf "set variable %s = %s" name (value_string value));
	with Not_found ->
		output_error ("No variable found: " ^ name)

let print_call_stack ctx p =
	let envs = match call_stack ctx with
		| _ :: envs -> envs
		| [] -> []
	in
	output_call_stack_position p;
	List.iter (fun env ->
		let p = {pmin = env.env_leave_pmin; pmax = env.env_leave_pmax; pfile = rev_hash_s env.env_info.pfile} in
		output_call_stack_position p
	) envs

let iter_breakpoints ctx f =
	Hashtbl.iter (fun _ breakpoints ->
		Hashtbl.iter (fun _ breakpoint -> f breakpoint) breakpoints
	) ctx.debug.breakpoints

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

let parse_breakpoint_pattern pattern =
	(* TODO: more than file:line patterns? *)
	try
		let file,line = ExtString.String.split pattern ":" in
		let line = int_of_string line in
		file,line
	with _ ->
		raise Exit

let rec expr_to_value e = match fst e with
	| EConst cst ->
		begin match cst with
			| String s -> encode_string s
			| Int s -> VInt32 (Int32.of_string s)
			| Float s -> VFloat (float_of_string s)
			| _ -> raise Exit
		end
	| _ ->
		raise Exit

let is_caught ctx v =
	try
		Hashtbl.iter (fun path _ -> if is v path then raise Exit) ctx.debug.caught_types;
		false
	with Exit ->
		true

let debug_loop jit e f =
	let ctx = jit.ctx in
	let scopes = jit.scopes in
	let line = Lexer.get_error_line e.epos in
	let rec loop env : value =
		if ctx.debug.breakpoint.bpstate = BPHit && line <> ctx.debug.breakpoint.bpline then ctx.debug.breakpoint.bpstate <- BPEnabled;
		match ctx.debug.debug_state with
			| DbgRunning ->
				run_safe env
			| DbgNext offset ->
				if offset < ctx.environment_offset then
					run_safe env
				else begin
					ctx.debug.debug_state <- DbgWaiting;
					loop env
				end
			| DbgFinish offset ->
				if offset <= ctx.environment_offset then
					run_safe env
				else begin
					ctx.debug.debug_state <- DbgWaiting;
					loop env
				end
			| DbgSingleStep ->
				ctx.debug.debug_state <- DbgWaiting;
				run_safe env
			| DbgWaiting ->
				wait env
	and wait env =
		output_info (Printf.sprintf "> %s" (s_expr_pretty e));
		match ExtString.String.nsplit (input_line stdin) " " with
			| ["quit" | "exit"] ->
				(* TODO: Borrowed from interp.ml *)
				if (get_ctx()).curapi.use_cache() then raise (Error.Fatal_error ("",Globals.null_pos));
				raise (Interp.Sys_exit 0);
			| ["detach"] ->
				Hashtbl.iter (fun _ h ->
					Hashtbl.clear h
				) ctx.debug.breakpoints;
				ctx.debug.debug_state <- DbgRunning;
				run_safe env
			(* source | history *)
			| ["files" | "filespath"] ->
				Hashtbl.iter (fun i _ ->
					output_file_path (rev_hash_s i);
				) ctx.debug.breakpoints;
				loop env
			| ["classes"] ->
				IntMap.iter (fun i _ ->
					output_type_name (rev_hash_s i)
				) ctx.type_cache;
				loop env
			| ["mem"] ->
				output_info (Printf.sprintf "%i" (Gc.stat()).live_words);
				loop env
			| ["compact"] ->
				let before = (Gc.stat()).live_words in
				Gc.compact();
				let after = (Gc.stat()).live_words in
				output_info (Printf.sprintf "before: %i\nafter: %i" before after);
				loop env
			| ["collect"] ->
				let before = (Gc.stat()).live_words in
				Gc.full_major();
				let after = (Gc.stat()).live_words in
				output_info (Printf.sprintf "before: %i\nafter: %i" before after);
				loop env
			| ["break" | "b";pattern] ->
				begin try
					let file,line = parse_breakpoint_pattern pattern in
					begin try
						add_breakpoint ctx file line;
						output_info (Printf.sprintf "Added breakpoint at %s:%i" file line);
					with Not_found ->
						output_error ("Could not find file " ^ file);
					end;
				with Exit ->
					output_error ("Unrecognized breakpoint pattern");
				end;
				loop env
			| ["list" | "l"] ->
				(* TODO: other list syntax *)
				iter_breakpoints ctx (fun breakpoint ->
					output_breakpoint breakpoint
				);
				loop env
			| ["describe" | "desc";bpid] ->
				(* TODO: range patterns? *)
				begin try
					let breakpoint = find_breakpoint ctx bpid in
					output_breakpoint_description breakpoint;
				with Not_found ->
					output_error (Printf.sprintf "Unknown breakpoint: %s" bpid);
				end;
				loop env
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
				loop env
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
				loop env
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
				loop env
			| ["clear";pattern] ->
				(* TODO: range patterns? *)
				begin try
					let file,line = parse_breakpoint_pattern pattern in
					begin try
						delete_breakpoint ctx file line
					with Not_found ->
						output_info (Printf.sprintf "Could not find breakpoint %s:%i" file line);
					end
				with Exit ->
					output_error ("Unrecognized breakpoint pattern");
				end;
				loop env
			(* thread | unsafe | safe *)
			| ["continue" | "c"] ->
				ctx.debug.debug_state <- DbgRunning;
				run_safe env
			| ["step" | "s" | ""] ->
				ctx.debug.debug_state <- DbgSingleStep;
				loop env
			| ["next" | "n"] ->
				ctx.debug.debug_state <- DbgNext ctx.environment_offset;
				f env
			| ["finish" | "f"] ->
				ctx.debug.debug_state <- DbgFinish ctx.environment_offset;
				loop env
			| ["where" | "w"] ->
				print_call_stack ctx e.epos;
				loop env
			(* up | down | frame *)
			| ["variables" | "vars"] ->
				print_variables jit scopes env;
				loop env
			| ["print" | "p";name] ->
				print_variable jit scopes name env;
				loop env
			| ["set" | "s";name;"=";value] ->
				let msg = ref "" in
				let error s =
					msg := s;
					raise Exit
				in
				begin try
					let e = Parser.parse_expr_string (ctx.curapi.get_com()) value e.epos error false in
					let v = expr_to_value e in
					set_variable scopes name v env
				with Exit ->
					output_error (Printf.sprintf "Error parsing %s: %s" value !msg);
				end;
				loop env
			| s :: _ ->
				output_error (Printf.sprintf "Unknown command: %s" s);
				loop env
			| [] ->
				loop env
	and run env =
		try
			f env
		with RunTimeException(v,_,_) when not (is_caught ctx v) ->
			output_info (uncaught_exception_string v e.epos "");
			ctx.debug.debug_state <- DbgWaiting;
			loop env
	and run_safe env =
		try
			let h = Hashtbl.find ctx.debug.breakpoints env.env_info.pfile in
			let breakpoint = Hashtbl.find h line in
			begin match breakpoint.bpstate with
				| BPEnabled ->
					breakpoint.bpstate <- BPHit;
					ctx.debug.breakpoint <- breakpoint;
					output_info (Printf.sprintf "Hit breakpoint at %s:%i" (rev_hash_s env.env_info.pfile) line);
					ctx.debug.debug_state <- DbgWaiting;
					loop env
				| _ ->
					run env
			end
		with Not_found ->
			run env
	in
	loop