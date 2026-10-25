(*
	A trace of the waits and claims of the domains typing in parallel, for
	finding what serializes typing (HAXE_TYPING_TRACE=<file>): one line per
	event, with its time, its domain and what happened.
*)
let file = try Some (Sys.getenv "HAXE_TYPING_TRACE") with Not_found -> None

let on = file <> None

let mutex = Mutex.create ()
let events : (float * int * string) list ref = ref []

let event f =
	if on then begin
		let t = Unix.gettimeofday () in
		let d = (Domain.self () :> int) in
		let s = f () in
		Mutex.protect mutex (fun () -> events := (t,d,s) :: !events)
	end

(* Writes the events (to the file of HAXE_TYPING_TRACE). *)
let flush () =
	match file with
	| None -> ()
	| Some file ->
		let l = Mutex.protect mutex (fun () -> let l = !events in events := []; List.rev l) in
		if l <> [] then begin
			let ch = open_out_gen [Open_append;Open_creat] 0o644 file in
			List.iter (fun (t,d,s) -> Printf.fprintf ch "%.6f %d %s\n" t d s) l;
			close_out ch
		end
