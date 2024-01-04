open Globals
open Common
open ExtString

class hxb_library file_path = object(self)
	inherit abstract_hxb_lib
	val zip = lazy (Zip.open_in file_path)

	val mutable cached_files = []
	val modules = Hashtbl.create 0
	val mutable closed = false
	val mutable loaded = false

	method load =
		if not loaded then begin
			loaded <- true;
			let close = Timer.timer ["hxblib";"read"] in
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
			close();
		end

	method load_module (target : string) (path : path) =
		try
			(* HXB_TODO: See if we can bucket by target a bit nicer. *)
			let path = (target :: fst path,snd path) in
			let (filename,entry) = Hashtbl.find modules path in
			let close = Timer.timer ["hxblib";"load_module"] in
			let zip = Lazy.force zip in
			let data = Zip.read_entry zip entry in
			let input = IO.input_string data in
			close();
			Some input
		with Not_found ->
			None

	method close =
		if not closed then begin
			closed <- true;
			Zip.close_in (Lazy.force zip)
		end

	method get_file_path = file_path
end


let create_hxb_lib com file_path =
	let file = if Sys.file_exists file_path then
		file_path
	else try
		Common.find_file com file_path
	with Not_found ->
			failwith ("hxb lib " ^ file_path ^ " not found")
	in
	new hxb_library file