open Atomic

type 'a t = {
	mutable value: 'a option;
	mutex: Mutex.t;
	compute: unit->'a
}

let from_fun f =
	{ value = None; mutex = Mutex.create (); compute = (fun () -> f()) }

let force lazy_val =
	if Option.is_none lazy_val.value then
		Mutex.protect lazy_val.mutex (fun () ->
			let result = lazy_val.compute () in
			lazy_val.value <- Some result;
		);
	match lazy_val.value with
	| Some v -> v
	| None -> failwith "Value not computed"

