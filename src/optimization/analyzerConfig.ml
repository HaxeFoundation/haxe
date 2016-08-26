(*
	The Haxe Compiler
	Copyright (C) 2005-2016  Haxe Foundation

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

open Ast
open Type
open Common

type debug_kind =
	| DebugNone
	| DebugDot
	| DebugFull

type t = {
	optimize : bool;
	const_propagation : bool;
	copy_propagation : bool;
	code_motion : bool;
	local_dce : bool;
	fusion : bool;
	purity_inference : bool;
	debug_kind : debug_kind;
	detail_times : bool;
	user_var_fusion : bool;
}

let flag_const_propagation = "const_propagation"
let flag_copy_propagation = "copy_propagation"
let flag_code_motion = "code_motion"
let flag_local_dce = "local_dce"
let flag_fusion = "fusion"
let flag_purity_inference = "purity_inference"
let flag_ignore = "ignore"
let flag_dot_debug = "dot_debug"
let flag_full_debug = "full_debug"
let flag_user_var_fusion = "user_var_fusion"

let all_flags =
	List.fold_left (fun acc flag ->
		flag :: ("no_" ^ flag) :: acc
	) [] [flag_const_propagation;flag_copy_propagation;flag_code_motion;flag_local_dce;flag_fusion;flag_purity_inference;flag_ignore;flag_dot_debug;flag_user_var_fusion]

let has_analyzer_option meta s =
	try
		let rec loop ml = match ml with
			| (Meta.Analyzer,el,_) :: ml ->
				if List.exists (fun (e,p) ->
					match e with
						| EConst(Ident s2) when s = s2 -> true
						| _ -> false
				) el then
					true
				else
					loop ml
			| _ :: ml ->
				loop ml
			| [] ->
				false
		in
		loop meta
	with Not_found ->
		false

let is_ignored meta =
	has_analyzer_option meta flag_ignore

let get_base_config com =
	{
		optimize = not (Common.defined com Define.NoAnalyzer);
		const_propagation = not (Common.raw_defined com "analyzer-no-const-propagation");
		copy_propagation = not (Common.raw_defined com "analyzer-no-copy-propagation");
		code_motion = Common.raw_defined com "analyzer-code-motion";
		local_dce = not (Common.raw_defined com "analyzer-no-local-dce");
		fusion = not (Common.raw_defined com "analyzer-no-fusion") && (match com.platform with Flash | Java | Cs -> false | _ -> true);
		purity_inference = not (Common.raw_defined com "analyzer-no-purity-inference");
		debug_kind = DebugNone;
		detail_times = Common.raw_defined com "analyzer-times";
		user_var_fusion = Common.raw_defined com "analyzer-user-var-fusion" || (not com.debug && not (Common.raw_defined com "analyzer-no-user-var-fusion"));
	}

let update_config_from_meta com config meta =
	List.fold_left (fun config meta -> match meta with
		| (Meta.Analyzer,el,_) ->
			List.fold_left (fun config e -> match fst e with
				| EConst (Ident s) when s = "no_" ^ flag_const_propagation -> { config with const_propagation = false}
				| EConst (Ident s) when s = flag_const_propagation -> { config with const_propagation = true}
				| EConst (Ident s) when s = "no_" ^ flag_copy_propagation -> { config with copy_propagation = false}
				| EConst (Ident s) when s = flag_copy_propagation -> { config with copy_propagation = true}
				| EConst (Ident s) when s = "no_" ^ flag_code_motion -> { config with code_motion = false}
				| EConst (Ident s) when s = flag_code_motion -> { config with code_motion = true}
				| EConst (Ident s) when s = "no_" ^ flag_local_dce -> { config with local_dce = false}
				| EConst (Ident s) when s = flag_local_dce -> { config with local_dce = true}
				| EConst (Ident s) when s = "no_" ^ flag_fusion -> { config with fusion = false}
				| EConst (Ident s) when s = flag_fusion -> { config with fusion = true}
				| EConst (Ident s) when s = "no_" ^ flag_purity_inference -> { config with purity_inference = false}
				| EConst (Ident s) when s = flag_purity_inference -> { config with purity_inference = true}
				| EConst (Ident s) when s = flag_dot_debug -> {config with debug_kind = DebugDot}
				| EConst (Ident s) when s = flag_full_debug -> {config with debug_kind = DebugFull}
				| EConst (Ident s) when s = flag_user_var_fusion -> {config with user_var_fusion = true}
				| EConst (Ident s) when s = "no_" ^ flag_user_var_fusion -> {config with user_var_fusion = false}
				| _ ->
					let s = Ast.s_expr e in
					com.warning (StringError.string_error s all_flags ("Unrecognized analyzer option: " ^ s)) (pos e);
					config
			) config el
		| (Meta.HasUntyped,_,_) ->
			{config with optimize = false}
		| _ ->
			config
	) config meta

let get_class_config com c =
	let config = get_base_config com in
	update_config_from_meta com config c.cl_meta

let get_field_config com c cf =
	let config = get_class_config com c in
	update_config_from_meta com config cf.cf_meta