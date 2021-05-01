open Globals
open EvalContext
open EvalExceptions
open EvalValue
open EvalEncode
open EvalDecode
open EvalHash
open EvalString

let make_stack envs =
	let l = DynArray.create () in
	List.iter (fun (pos,kind) ->
		let file_pos s =
			let line1,col1,_,_ = Lexer.get_pos_coords pos in
			encode_enum_value key_haxe_StackItem 2 [|s;create_unknown pos.pfile;vint line1;vint col1|] None
		in
		match kind with
		| EKLocalFunction i ->
			let local_function = encode_enum_value key_haxe_StackItem 4 [|vint i|] None in
			DynArray.add l (file_pos local_function);
		| EKMethod(st,sf) ->
			let local_function = encode_enum_value key_haxe_StackItem 3 [|create_unknown (rev_hash st); create_unknown (rev_hash sf)|] None in
			DynArray.add l (file_pos local_function);
		| EKEntrypoint ->
			()
	) envs;
	encode_array (DynArray.to_list l)

let make_stack_value envs =
	make_stack (List.map (fun env -> {pfile = rev_hash env.env_info.pfile;pmin = env.env_leave_pmin; pmax = env.env_leave_pmax},env.env_info.kind) envs)

let getCallStack = vfun0 (fun () ->
	let ctx = get_ctx() in
	let envs = call_stack (get_eval ctx) in
	let envs = match envs with
		| _ :: _ :: envs -> envs (* Skip calls to callStack() and getCallStack() *)
		| _ -> envs
	in
	make_stack_value  envs
)

let getExceptionStack = vfun0 (fun () ->
	let ctx = get_ctx() in
	let envs = ctx.exception_stack in
	make_stack (List.rev envs)
)