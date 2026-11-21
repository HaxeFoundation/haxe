open Globals
open Type

(* The fields of the macro context whose typing reported an error, including an
   error of another field typed meanwhile, or which inlined such a field: their
   code may be what the typer recovered, so a macro must not run it. *)

let failed : (pos * string,unit) Hashtbl.t = Hashtbl.create 0
let marks = Atomic.make 0
let mutex = Mutex.create ()

(* The number of errors reported by this domain, which a typing that reported
   one sees change. *)
let reported = Domain.DLS.new_key (fun () -> ref 0)

let report () =
	incr (Domain.DLS.get reported)

let key cf = (cf.cf_pos,cf.cf_name)

let mark cf =
	report ();
	Mutex.protect mutex (fun () -> Hashtbl.replace failed (key cf) ());
	Atomic.incr marks

let has_failed cf =
	Mutex.protect mutex (fun () -> Hashtbl.mem failed (key cf))

let reset () =
	Mutex.protect mutex (fun () -> Hashtbl.reset failed)

(* Runs [f], the typing of [cf], marking [cf] if an error is reported meanwhile. *)
let typing (com : Common.context) cf f =
	if not com.is_macro_context then
		f ()
	else begin
		let r = Domain.DLS.get reported in
		let n = !r in
		match f () with
		| v ->
			if !r <> n then mark cf;
			v
		| exception exc ->
			let bt = Printexc.get_raw_backtrace () in
			mark cf;
			Printexc.raise_with_backtrace exc bt
	end
