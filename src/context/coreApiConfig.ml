open Globals

(**
	Determines how the core API check is performed for a class.
	- On: Always perform the core API check.
	- Off: Never check. Can be set explicitly to override an Implied check.
	- Implied: Check only if a core definition type exists, otherwise ignore.
*)
type core_api_check =
	| On
	| Off
	| Implied

type core_api_config = {
	mutable check : core_api_check;
}

let default_core_api_config () = {
	check = On;
}

module CoreApiConfigReader (API : DataReaderApi.DataReaderApi) = struct
	let read_core_api_check data =
		match API.read_ident data with
		| "On" -> On
		| "Off" -> Off
		| "Implied" -> Implied
		| s -> Error.raise_typing_error (Printf.sprintf "Unknown value for check: %s (expected On, Off, or Implied)" s) null_pos

	let read_core_api_config config data =
		let read data =
			let fl = API.read_object data in
			List.iter (fun (s, data) -> match s with
				| "check" ->
					config.check <- read_core_api_check data
				| s ->
					Error.raise_typing_error (Printf.sprintf "Unknown key for coreApi config: %s" s) null_pos
			) fl
		in
		API.read_optional data read
end

module CoreApiConfigReaderMeta = CoreApiConfigReader(MetaDataApi.MetaReaderApi)

let of_core_api_metadata_entry entry =
	let config = default_core_api_config () in
	CoreApiConfigReaderMeta.read_core_api_config config (MetaDataApi.of_metadata_entry entry);
	config

(**
	Returns true if the given file path is in an implied core API location:
	- Directly in a `_std/` directory (non-recursive, e.g. `_std/Array.hx`)
	- In `_std/haxe/` (recursive)
	- In `_std/sys/` (recursive)
*)
let is_implied_core_api_file file_path =
	(* Normalize path separators to forward slashes *)
	let normalized_path = String.concat "/" (ExtString.String.nsplit file_path "\\") in
	let std_marker = "/_std/" in
	let marker_len = String.length std_marker in
	try
		let idx = ExtString.String.find normalized_path std_marker in
		let after_std = String.sub normalized_path (idx + marker_len) (String.length normalized_path - idx - marker_len) in
		(* Implied for: direct file in _std/, or files in _std/haxe/ or _std/sys/ subdirectories *)
		not (String.contains after_std '/')
		|| ExtString.String.starts_with after_std "haxe/"
		|| ExtString.String.starts_with after_std "sys/"
	with ExtString.Invalid_string ->
		false

(**
	Determines the core_api_check value for a class based on its metadata and file location.
*)
let get_core_api_check cl_meta file_path =
	match Meta.get Meta.CoreApi cl_meta with
	| entry ->
		let config = of_core_api_metadata_entry entry in
		config.check
	| exception Not_found ->
		if is_implied_core_api_file file_path then
			Implied
		else
			Off

let s_core_api_check = function
	| On -> "On"
	| Off -> "Off"
	| Implied -> "Implied"
