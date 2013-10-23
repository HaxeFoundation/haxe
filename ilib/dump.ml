open PeDataDebug;;
open PeData;;
open PeReader;;

let main () =
	if Array.length Sys.argv <> 2 then
		print_endline "Usage: dump <exe-path>"
	else begin
		let r = create_r (open_in Sys.argv.(1)) PMap.empty in
		let ctx = read r in
		let pe = ctx.pe_header in
		print_endline (coff_header_s pe.pe_coff_header);
		print_endline (pe_header_s pe);
		let idata = read_idata ctx in
		List.iter (fun t -> print_endline (idata_table_s t)) idata;
		let clr_header = read_clr_header ctx in
		print_endline (clr_header_s (clr_header))
	end;;

main()
