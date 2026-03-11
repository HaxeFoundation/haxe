open Globals

type coro_assert = {
	mutable num_states : int option;
	mutable num_hoisted : int option;
}

type coro_outcome = {
	mutable no_suspend : bool;
	mutable no_return : bool;
	mutable no_throw : bool;
}

let default_outcome () = {
	no_suspend = false;
	no_return = false;
	no_throw = false;
}

type t = {
	mutable debug : bool;
	mutable transformed : bool;
	mutable assert_config : coro_assert option;
	mutable outcome : coro_outcome;
}

let create () = {
	debug = false;
	transformed = false;
	assert_config = None;
	outcome = default_outcome ();
}

module CoroConfigReader (API : DataReaderApi.DataReaderApi) = struct
	let read_coro_assert config_assert data =
		let fl = API.read_object data in
		List.iter (fun (s, data) -> match s with
			| "numStates" ->
				config_assert.num_states <- Some (API.read_int data)
			| "numHoisted" ->
				config_assert.num_hoisted <- Some (API.read_int data)
			| s ->
				Error.raise_typing_error (Printf.sprintf "Unknown key for coroutine assert config: %s" s) null_pos
		) fl

	let read_coro_outcome outcome data =
		let fl = API.read_object data in
		List.iter (fun (s, data) -> match s with
			| "noSuspend" ->
				outcome.no_suspend <- API.read_bool data
			| "noReturn" ->
				outcome.no_return <- API.read_bool data
			| "noThrow" ->
				outcome.no_throw <- API.read_bool data
			| s ->
				Error.raise_typing_error (Printf.sprintf "Unknown key for coroutine outcome config: %s" s) null_pos
		) fl

	let read_coro_config config data =
		let read data =
			let fl = API.read_object data in
			List.iter (fun (s, data) -> match s with
				| "debug" ->
					config.debug <- API.read_bool data
				| "transformed" ->
					config.transformed <- API.read_bool data
				| "assert" ->
					let config_assert = { num_states = None; num_hoisted = None } in
					read_coro_assert config_assert data;
					config.assert_config <- Some config_assert
				| "outcome" ->
					read_coro_outcome config.outcome data
				| s ->
					Error.raise_typing_error (Printf.sprintf "Unknown key for coroutine config: %s" s) null_pos
			) fl
		in
		API.read_optional data read
end

module CoroConfigReaderMeta = CoroConfigReader(MetaDataApi.MetaReaderApi)

let of_metadata_entry entry =
	let config = create () in
	CoroConfigReaderMeta.read_coro_config config (MetaDataApi.of_metadata_entry entry);
	config

let of_meta_list meta =
	match Meta.get Meta.Coroutine meta with
	| entry -> of_metadata_entry entry
	| exception Not_found -> create ()

let get_coroutine_config meta =
	let config = of_meta_list meta in
	if config.transformed then None
	else Some config
