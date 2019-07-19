open Uv

;;

print_string "init loop...\n"; flush_all ();
let loop = Uv.loop_init () in
(*let cb_c () = print_string "closed\n" in
let cb file = print_string "hey I got a file I guess\n"; flush_all (); Uv.fs_close loop file cb_c in*)
let cb = function
	| CbError err -> print_string ("got an error: " ^ err ^ "\n"); flush_all ();
	| CbSuccess file ->
		print_string "hey I got a file I guess\n"; flush_all ();
		let stat = Uv.fs_fstat_sync loop file in
		print_string ("length: " ^ (Int64.to_string stat.size) ^ "\n"); flush_all ();
		Uv.fs_close_sync loop file;
		print_string "closed\n"; flush_all ();
in
print_string "open files...\n"; flush_all ();
Uv.fs_open loop "uv.ml" 0 511 cb;
Uv.fs_open loop "non-ext" 0 511 cb;
print_string "sync open...\n"; flush_all ();
let other_file = Uv.fs_open_sync loop "Makefile" 0 511 in
print_string "run gc...\n"; flush_all ();
Gc.full_major ();
Uv.fs_close_sync loop other_file;
print_string "scandir...\n"; flush_all ();
let dirs = Uv.fs_scandir_sync loop "." 0 in
let rec pdirs = function
	| [] -> ()
	| (name, kind) :: rest -> print_string ("entry: " ^ name ^ "\n"); pdirs rest
in
pdirs dirs;
print_string "run loop...\n"; flush_all ();
while (Uv.loop_alive loop) do
	ignore (Uv.run loop 0)
done;
print_string "close loop...\n"; flush_all ();
Uv.loop_close loop;
print_string "done\n"
