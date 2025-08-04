open Atomic

type 'a atomic_lazy = {
  mutable value: 'a option;
  computed: bool Atomic.t;
  compute: unit->'a
}

let from_fun f =
  { value = None; computed = Atomic.make false; compute = (fun () -> f()) }

let force lazy_val =
  if not (Atomic.get lazy_val.computed) then begin
    let result = lazy_val.compute () in
    lazy_val.value <- Some result;
    Atomic.set lazy_val.computed true;
	end;
  match lazy_val.value with
  | Some v -> v
  | None -> failwith "Value not computed"

