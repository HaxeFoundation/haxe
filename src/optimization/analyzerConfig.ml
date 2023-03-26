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

open Ast
open Type
open Common
open Globals

type debug_kind =
	| DebugNone
	| DebugDot
	| DebugFull

type t = {
	optimize : bool;
	const_propagation : bool;
	copy_propagation : bool;
	local_dce : bool;
	fusion : bool;
	purity_inference : bool;
	debug_kind : debug_kind;
	detail_times : int;
	user_var_fusion : bool;
	fusion_debug : bool;
}

let flag_optimize = "optimize"
let flag_const_propagation = "const_propagation"
let flag_copy_propagation = "copy_propagation"
let flag_local_dce = "local_dce"
let flag_fusion = "fusion"
let flag_ignore = "ignore"
let flag_dot_debug = "dot_debug"
let flag_full_debug = "full_debug"
let flag_user_var_fusion = "user_var_fusion"
let flag_fusion_debug = "fusion_debug"

let all_flags =
	List.fold_left (fun acc flag ->
		flag :: ("no_" ^ flag) :: acc
	) [] [flag_optimize;flag_const_propagation;flag_copy_propagation;flag_local_dce;flag_fusion;flag_ignore;flag_dot_debug;flag_user_var_fusion]

let has_analyzer_option meta s =
	Ast.has_meta_option meta Meta.Analyzer s

let is_ignored meta =
	has_analyzer_option meta flag_ignore

let get_base_config com =
	{
		optimize = Common.defined com Define.AnalyzerOptimize;
		const_propagation = not (Common.raw_defined com "analyzer_no_const_propagation");
		copy_propagation = not (Common.raw_defined com "analyzer_no_copy_propagation");
		local_dce = not (Common.raw_defined com "analyzer_no_local_dce");
		fusion = not (Common.raw_defined com "analyzer_no_fusion");
		purity_inference = not (Common.raw_defined com "analyzer_no_purity_inference");
		debug_kind = DebugNone;
		detail_times = (try int_of_string (Common.defined_value_safe com ~default:"0" Define.AnalyzerTimes) with _ -> 0);
		user_var_fusion = (match com.platform with Flash | Java -> false | _ -> true) && (Common.raw_defined com "analyzer_user_var_fusion" || (not com.debug && not (Common.raw_defined com "analyzer_no_user_var_fusion")));
		fusion_debug = false;
	}

let update_config_from_meta com config ml =
	List.fold_left (fun config meta -> match meta with
		| (Meta.Analyzer,el,_) ->
			List.fold_left (fun config e -> match fst e with
				| EConst (Ident s) ->
					begin match s with
						| "optimize" -> { config with optimize = true }
						| "no_optimize" -> { config with optimize = false }
						| "const_propagation" -> { config with const_propagation = true }
						| "no_const_propagation" -> { config with const_propagation = false }
						| "copy_propagation" -> { config with copy_propagation = true }
						| "no_copy_propagation" -> { config with copy_propagation = false }
						| "local_dce" -> { config with local_dce = true }
						| "no_local_dce" -> { config with local_dce = false }
						| "fusion" -> { config with fusion = true }
						| "no_fusion" -> { config with fusion = false }
						| "user_var_fusion" -> { config with user_var_fusion = true }
						| "no_user_var_fusion" -> { config with user_var_fusion = false }
						| "dot_debug" -> { config with debug_kind = DebugDot }
						| "full_debug" -> { config with debug_kind = DebugFull }
						| "fusion_debug" -> { config with fusion_debug = true }
						| "as_var" -> config
						| _ ->
							let options = Warning.from_meta ml in
							com.warning WOptimizer options (StringError.string_error s all_flags ("Unrecognized analyzer option: " ^ s)) (pos e);
							config
					end
				| _ ->
					let s = Ast.Printer.s_expr e in
					let options = Warning.from_meta ml in
					com.warning WOptimizer options (StringError.string_error s all_flags ("Unrecognized analyzer option: " ^ s)) (pos e);
					config
			) config el
		| (Meta.HasUntyped,_,_) ->
			{config with optimize = false}
		| _ ->
			config
	) config ml

let get_class_config com c =
	let config = get_base_config com in
	update_config_from_meta com config c.cl_meta

let get_field_config com c cf =
	let config = get_class_config com c in
	update_config_from_meta com config cf.cf_meta