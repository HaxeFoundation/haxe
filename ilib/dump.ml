open IDataDebug;;
open IData;;

let main () =
	if Array.length Sys.argv <> 2 then
		print_endline "Usage: dump <exe-path>"
	else begin
		let r = open_in Sys.argv.(1) in
		let pe = IReader.read Sys.argv.(1) r in
		print_endline (coff_header_s pe.pe_coff_header);
		print_endline (pe_header_s pe)
	end;;

main()
