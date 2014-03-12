open PeDataDebug;;
open PeData;;
open PeReader;;
open Printf;;
open IlData;;
open IlMetaTools;;
open IlMetaDebug;;

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
		print_endline (clr_header_s (clr_header));
		let cache = IlMetaReader.create_cache () in
		let meta = IlMetaReader.read_meta_tables ctx clr_header cache in
		Hashtbl.iter (fun path _ ->
			print_endline ("\n\nclass " ^ path_s path ^ ": ");
			let cls = convert_class meta path in
			List.iter (fun t -> printf "%d: <%s> " t.tnumber (if t.tname = None then "_" else Option.get t.tname)) cls.ctypes;
			printf "\n\tis nested: %s - %s\n" (string_of_bool (cls.cenclosing <> None)) (if cls.cenclosing = None then "None" else path_s (Option.get cls.cenclosing));
			print_endline "\tfields:";
			List.iter (fun f -> printf "\t\t%s : %s\n" f.fname (ilsig_s f.fsig.ssig)) cls.cfields;
			print_endline "\tmethods:";
			List.iter (fun m -> printf "\t\t%s : %s\n" m.mname (ilsig_s m.msig.ssig)) cls.cmethods;
			print_endline "\tprops:";
			List.iter (fun p -> printf "\t\t%s : %s\n" p.pname (ilsig_s p.psig.ssig)) cls.cprops;
		) meta.il_typedefs
	end;;

main()
