open Ast
open Type

let apply com e =
	let rec loop var_inits e = match e.eexpr with
		| TVar(v,Some e1) when v.v_kind = VExtractorVariable ->
			let var_inits = PMap.add v.v_id e1 var_inits in
			var_inits,{e with eexpr = TVar(v,None)}
		| TLocal v ->
			begin try
				let e_init = PMap.find v.v_id var_inits in
				let var_inits,e_init = loop var_inits e_init in
				let e = {e with eexpr = TBinop(OpAssign,e,e_init)} in
				let e = {e with eexpr = TParenthesis e} in
				let var_inits = PMap.remove v.v_id var_inits in
				var_inits,e
			with Not_found ->
				var_inits,e
			end
		| TIf(e1,e2,eo) ->
			let var_inits,e1 = loop var_inits e1 in
			let _,e2 = loop var_inits e2 in
			let eo = match eo with None -> None | Some e -> Some (snd (loop var_inits e)) in
			var_inits,{e with eexpr = TIf(e1,e2,eo)}
		| TWhile(e1,e2,flag) ->
			let var_inits,e1 = loop var_inits e1 in
			let _,e2 = loop var_inits e2 in
			var_inits,{e with eexpr = TWhile(e1,e2,flag)}
		| TTry(e1,catches) ->
			let _,e1 = loop var_inits e1 in
			let catches = List.map (fun (v,e) ->
				let _,e = loop var_inits e in
				(v,e)
			) catches in
			var_inits,{e with eexpr = TTry(e1,catches)}
		| TSwitch(e1,cases,edef) ->
			let var_inits,e1 = loop var_inits e1 in
			let cases = List.map (fun (el,e) ->
				let _,e = loop var_inits e in
				el,e
			) cases in
			let edef = match edef with None -> None | Some e -> Some (snd (loop var_inits e)) in
			var_inits,{e with eexpr = TSwitch(e1,cases,edef)}
		| _ ->
			Texpr.foldmap loop var_inits e
	in
	snd (loop PMap.empty e)