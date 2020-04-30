(*
	The Haxe Compiler
	Copyright (C) 2005-2019  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)

open Globals
open Ast
open Type
open Common
open ExtString

let rec create_dir acc = function
	| [] -> ()
	| d :: l ->
		let path = acc ^ "/" ^ d in
		(try Unix.mkdir path 0o777 with _ -> ());
		create_dir path l


let conv_path p =
	match List.rev (fst p) with
	| x :: l when starts_with x '_' -> List.rev (("priv" ^ x) :: l), snd p
	| _ -> p

let get_real_path meta path =
	try
		let real_path = match Meta.get Meta.RealPath meta with
			| (_,[(EConst(String(s,_)),_)],_) ->
				s
			| _ -> raise Not_found
		in
		match List.rev (String.nsplit real_path ".") with
			| name :: pack ->
				(List.rev pack), name
			| _ -> raise Not_found
	with | Not_found ->
		path

let generate_type com t =
	let base_path = "hxclasses" in
	match t with TClassDecl { cl_kind = KAbstractImpl _ } -> () | _ ->
	let pack, name =
		let info = t_infos t in
		get_real_path info.mt_meta info.mt_path
	in
	create_dir "." (base_path :: pack);
	match pack, name with
	| ["flash";"net"], "NetStreamPlayTransitions"
	| ["flash";"filters"], "BitmapFilterQuality"
	| ["flash";"display"], ("BitmapDataChannel" | "GraphicsPathCommand")  -> ()
	| _ ->
	let f = open_out_bin (base_path ^ "/" ^ (match pack with [] -> "" | l -> String.concat "/" l ^ "/") ^ name ^ ".hx") in
	let ch = IO.output_channel f in
	let p fmt = IO.printf ch fmt in
	if pack <> [] then IO.printf ch "package %s;\n\n" (String.concat "." pack);
	let rec notnull t =
		match t with
		| TMono r ->
			(match r.tm_type with
			| None -> t
			| Some t -> notnull t)
		| TLazy f ->
			notnull (lazy_type f)
		| TAbstract ({ a_path = [],"Null" },[t]) ->
			t
		| _ ->
			t
	in
	let rec path meta p tl =
		let p = conv_path (get_real_path meta p) in
		(if fst p = pack then snd p else s_type_path p) ^ (match tl with [] -> "" | _ -> "<" ^ String.concat "," (List.map stype tl) ^ ">")
	and stype t =
		match t with
		| TMono r ->
			(match r.tm_type with
			| None -> "Unknown"
			| Some t -> stype t)
		| TInst ({ cl_kind = KTypeParameter _ } as c,tl) ->
			path [] ([],snd c.cl_path) tl
		| TInst (c,tl) ->
			path c.cl_meta c.cl_path tl
		| TEnum (e,tl) ->
			path e.e_meta e.e_path tl
		| TType (t,tl) ->
			path t.t_meta t.t_path tl
		| TAbstract (a,tl) ->
			path a.a_meta a.a_path tl
		| TAnon a ->
			let fields = PMap.fold (fun f acc -> (f.cf_name ^ " : " ^ stype f.cf_type) :: acc) a.a_fields [] in
			"{" ^ String.concat ", " fields ^ "}"
		| TLazy f ->
			stype (lazy_type f)
		| TDynamic t2 ->
			if t == t2 then "Dynamic" else "Dynamic<" ^ stype t2 ^ ">"
		| TFun ([],ret) ->
			"Void -> " ^ ftype ret
		| TFun (args,ret) ->
			String.concat " -> " (List.map (fun (_,_,t) -> ftype t) args) ^ " -> " ^ ftype ret
	and ftype t =
		match t with
		| TMono r ->
			(match r.tm_type with
			| None -> stype t
			| Some t -> ftype t)
		| TLazy f ->
			ftype (lazy_type f)
		| TFun _ ->
			"(" ^ stype t ^ ")"
		| _ ->
			stype t
	in
	let sparam (n,v,t) =
		match v with
		| None ->
			n ^ " : " ^ stype t
		| Some (Ident "null") ->
			if is_nullable (notnull t) then
				"?" ^ n ^ " : " ^ stype (notnull t)
			else
				(* we have not found a default value stored in metadata, let's generate it *)
				n ^ " : " ^ stype t ^ " = " ^ (match follow t with
					| TAbstract ({ a_path = [],("Int"|"Float"|"UInt") },_) -> "0"
					| TAbstract ({ a_path = [],"Bool" },_) -> "false"
					| _ -> "null")
		| Some v ->
			n ^ " : " ^ stype t ^ " = " ^ (match s_constant v with "nan" -> "0./*NaN*/" | v -> v)
	in
	let print_meta ml =
		List.iter (fun (m,pl,_) ->
			match m with
			| Meta.DefParam | Meta.CoreApi | Meta.Used | Meta.MaybeUsed | Meta.FlatEnum | Meta.Value | Meta.DirectlyUsed | Meta.Enum | Meta.Impl -> ()
			| _ ->
			match pl with
			| [] -> p "@%s " (Meta.to_string m)
			| l -> p "@%s(%s) " (Meta.to_string m) (String.concat "," (List.map Ast.Printer.s_expr pl))
		) ml
	in
	let access is_read a = s_access is_read a in
	let rec print_field stat f =
		p "\t";
		print_meta f.cf_meta;
		if not (has_class_field_flag f CfPublic) then p "private ";
		if stat then p "static ";
		let name = try (match Meta.get Meta.RealPath f.cf_meta with
				| (Meta.RealPath, [EConst( String(s,_) ), _], _) ->
					s
				| _ ->
					raise Not_found)
			with Not_found ->
				f.cf_name
		in
		(match f.cf_kind with
		| Var v ->
			if has_class_field_flag f CfFinal then
				p "final %s" name
			else begin
				p "var %s" name;
				if v.v_read <> AccNormal || v.v_write <> AccNormal then p "(%s,%s)" (access true v.v_read) (access false v.v_write);
			end;
			p " : %s" (stype f.cf_type);
		| Method m ->
			let params, ret = (match follow f.cf_type with
				| TFun (args,ret) ->
					List.map (fun (a,o,t) ->
						let rec loop = function
							| [] -> Ident "null"
							| (Meta.DefParam,[(EConst (String(p,_)),_);(EConst v,_)],_) :: _ when p = a ->
								(match v with
								| Float "1.#QNAN" -> Float "0./*NaN*/"
								| Float "4294967295." -> Int "0xFFFFFFFF"
								| Int "16777215" -> Int "0xFFFFFF"
								| Float x ->
									(try
										let f = float_of_string x in
										let s = string_of_int (int_of_float f) in
										if s ^ "." = x then Int s else v
									with _ ->
										v)
								| _ -> v)
							| _ :: l -> loop l
						in
						a,(if o then Some (loop f.cf_meta) else None ),t
					) args, ret
				| _ ->
					die "" __LOC__
			) in
			let tparams = (match f.cf_params with [] -> "" | l -> "<" ^ String.concat "," (List.map fst l) ^ ">") in
			p "function %s%s(%s) : %s" name tparams (String.concat ", " (List.map sparam params)) (stype ret);
		);
		p ";\n";
		if Meta.has Meta.Overload f.cf_meta then List.iter (fun f -> print_field stat f) f.cf_overloads
	in
	(match t with
	| TClassDecl c ->
		print_meta c.cl_meta;
		let finalmod = if c.cl_final then "final " else "" in
		p "extern %s%s %s" finalmod (if c.cl_interface then "interface" else "class") (stype (TInst (c,List.map snd c.cl_params)));
		let ext = (match c.cl_super with
		| None -> []
		| Some (c,pl) -> [" extends " ^ stype (TInst (c,pl))]
		) in
		let ext = List.fold_left (fun acc (i,pl) -> ((if c.cl_interface then " extends " else " implements ") ^ stype (TInst (i,pl))) :: acc) ext c.cl_implements in
		let ext = (match c.cl_dynamic with
			| None -> ext
			| Some t ->
				(match c.cl_path with
				| ["flash";"errors"], _ -> ext
				| _ when t == t_dynamic -> " implements Dynamic" :: ext
				| _ -> (" implements Dynamic<" ^ stype t ^ ">") :: ext)
		) in
		let ext = (match c.cl_path with
			| ["flash";"utils"], "ByteArray" -> " implements ArrayAccess<Int>" :: ext
			| ["flash";"utils"], "Dictionary" -> [" implements ArrayAccess<Dynamic>"]
			| ["flash";"xml"], "XML" -> [" implements Dynamic<XMLList>"]
			| ["flash";"xml"], "XMLList" -> [" implements ArrayAccess<XML>"]
			| ["flash";"display"],"MovieClip" -> [" extends Sprite #if !flash_strict implements Dynamic #end"]
			| ["flash";"errors"], "Error" -> [" #if !flash_strict implements Dynamic #end"]
			| _ -> ext
		) in
		p "%s" (String.concat "" (List.rev ext));
		p " {\n";
		let sort l =
			let a = Array.of_list (List.filter (fun f -> not (List.memq f c.cl_overrides)) l) in
			let name = function "new" -> "" | n -> n in
			Array.sort (fun f1 f2 ->
				match f1.cf_kind, f2.cf_kind with
				| Var _, Var _ | Method _ , Method _ -> compare (name f1.cf_name) (name f2.cf_name)
				| Var _, _ -> -1
				| _ -> 1
			) a;
			Array.to_list a
		in
		List.iter (print_field false) (sort (match c.cl_constructor with None -> c.cl_ordered_fields | Some f -> f :: c.cl_ordered_fields));
		List.iter (print_field true) (sort c.cl_ordered_statics);
		p "}\n";
	| TEnumDecl e ->
		print_meta e.e_meta;
		p "extern enum %s {\n" (stype (TEnum(e,List.map snd e.e_params)));
		List.iter (fun n ->
			let c = PMap.find n e.e_constrs in
			p "\t%s" c.ef_name;
			(match follow c.ef_type with
			| TFun (args,_) -> p "(%s)" (String.concat ", " (List.map sparam (List.map (fun (a,o,t) -> a,(if o then Some (Ident "null") else None),t) args)))
			| _ -> ());
			p ";\n";
		) e.e_names;
		p "}\n"
	| TTypeDecl t ->
		print_meta t.t_meta;
		p "typedef %s = " (stype (TType (t,List.map snd t.t_params)));
		p "%s" (stype t.t_type);
		p "\n";
	| TAbstractDecl a ->
		print_meta a.a_meta;
		Option.may (fun c -> try print_meta [Meta.get Meta.Require c.cl_meta] with Not_found -> ()) a.a_impl;
		p "extern ";
		let is_enum = Meta.has Meta.Enum a.a_meta in
		if is_enum then p "enum ";
		p "abstract %s" (stype (TAbstract (a,List.map snd a.a_params)));
		if not (Meta.has Meta.CoreType a.a_meta) then p "(%s)" (stype a.a_this);
		p " {\n";
		Option.may (fun c ->
			let fields = c.cl_ordered_statics in
			let fields =
				if is_enum then
					let sort l =
						let a = Array.of_list l in
						Array.sort (fun a b -> compare a.cf_name b.cf_name) a;
						Array.to_list a
					in
					sort fields
				else
					fields
			in

			List.iter (fun f ->
				let static = not (Meta.has Meta.Impl f.cf_meta) in
				if not static && is_enum && Meta.has Meta.Enum f.cf_meta then begin
					p "\tvar %s;\n" f.cf_name;
				end else
					print_field static f
			) fields
		) a.a_impl;
		p "}\n";
	);
	IO.close_out ch

let generate com =
	List.iter (generate_type com) com.types
