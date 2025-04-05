open Globals
open Common
open ExtString

class hxb_library timer_ctx file_path hxb_times = object(self)
	inherit abstract_hxb_lib
	val zip = lazy (Zip.open_in file_path)

	val mutable cached_files = []
	val modules = Hashtbl.create 0
	val mutable closed = false
	val mutable loaded = false
	val mutable string_pool : string array option = None
	val mutable macro_string_pool : string array option = None

	method private do_load =
		List.iter (function
		| ({ Zip.is_directory = false; Zip.filename = filename } as entry) when String.ends_with filename ".hxb" ->
			let pack = String.nsplit filename "/" in
			begin match List.rev pack with
				| [] -> ()
				| name :: pack ->
					let name = String.sub name 0 (String.length name - 4) in
					let pack = List.rev pack in
					Hashtbl.add modules (pack,name) (filename,entry);
				end
		| _ -> ()
	) (Zip.entries (Lazy.force zip));

	method load =
		if not loaded then begin
			loaded <- true;
			Timer.time timer_ctx ["hxblib";"read"] (fun () -> self#do_load) ()
		end

	method get_bytes (target : string) (path : path) =
		try
			let path = (target :: fst path,snd path) in
			let (filename,entry) = Hashtbl.find modules path in
			let data = Timer.time timer_ctx ["hxblib";"get bytes"] (fun () ->
				let zip = Lazy.force zip in
				Zip.read_entry zip entry
			) () in
			Some (Bytes.unsafe_of_string data)
		with Not_found ->
			None

	method close =
		if not closed then begin
			closed <- true;
			Zip.close_in (Lazy.force zip)
		end

	method get_file_path = file_path
	method get_string_pool target =
		if target = "macro" && Option.is_some macro_string_pool then macro_string_pool
		else string_pool
end


let create_hxb_lib com file_path =
	let file = if Sys.file_exists file_path then
		file_path
	else try
		Common.find_file com file_path
	with Not_found ->
		failwith ("hxb lib " ^ file_path ^ " not found")
	in
	new hxb_library com.timer_ctx file (if Common.defined com Define.HxbTimes then Some com.timer_ctx else None)
