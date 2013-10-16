
let main () =
	if Array.length Sys.argv <> 2 then
		print_endline "Usage: dump <exe-path>"
	else begin
		(* let r = open_in Sys.argv.(1) in *)
		print_endline "ok"
	end;;

main()
