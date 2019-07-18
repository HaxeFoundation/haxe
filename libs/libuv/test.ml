open Libuv

;;

let loop = Libuv.loop_init () in
(*let cb_c () = print_string "closed\n" in
let cb file = print_string "hey I got a file I guess\n"; flush_all (); Libuv.fs_close loop file cb_c in*)
let cb file =
	print_string "hey I got a file I guess\n"; flush_all ();
	Libuv.fs_close_sync loop file;
	print_string "closed\n"; flush_all ();
in
print_string "open files...\n"; flush_all ();
Libuv.fs_open loop "libuv.ml" 0 511 cb;
Libuv.fs_open loop "Makefile" 0 511 cb;
print_string "sync open...\n"; flush_all ();
let other_file = Libuv.fs_open_sync loop "dummy2.txt" 0 511 in
print_string "run gc...\n"; flush_all ();
Gc.full_major ();
Libuv.fs_close_sync loop other_file;
print_string "scandir...\n"; flush_all ();
let dirs = Libuv.fs_scandir_sync loop "." 0 in
let rec pdirs = function
	| [] -> ()
	| (name, kind) :: rest -> print_string ("entry: " ^ name ^ "\n"); pdirs rest
in
pdirs dirs;
print_string "run loop...\n"; flush_all ();
while (Libuv.loop_alive loop) do
	ignore (Libuv.run loop 0)
done;
print_string "close loop...\n"; flush_all ();
Libuv.loop_close loop;
print_string "done\n"
