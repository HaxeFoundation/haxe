(*
	The Haxe Compiler
	Copyright (C) 2005-2018  Haxe Foundation

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
open Type
open Gencommon

(*
	- Filters out enum constructor type parameters from the AST; See Issue #1796
	- Filters out monomorphs
	- Filters out all non-whitelisted AST metadata

	dependencies:
		No dependencies; but it still should be one of the first filters to run,
		as it will help normalize the AST
*)

let rec filter_param t =
	match t with
	| TInst({ cl_kind = KTypeParameter _ } as c,_) when Meta.has Meta.EnumConstructorParam c.cl_meta ->
		t_dynamic
	| TMono r ->
		(match !r with
		| None -> t_dynamic
		| Some t -> filter_param t)
	| TInst(_,[]) | TEnum(_,[]) | TType(_,[]) | TAbstract(_,[]) ->
		t
	| TType(t,tl) ->
		TType(t,List.map filter_param tl)
	| TInst(c,tl) ->
		TInst(c,List.map filter_param tl)
	| TEnum(e,tl) ->
		TEnum(e,List.map filter_param tl)
	| TAbstract({ a_path = (["haxe";"extern"],"Rest") } as a,tl) ->
		TAbstract(a, List.map filter_param tl)
	| TAbstract({a_path = [],"Null"} as a,[t]) ->
		TAbstract(a,[filter_param t])
	| TAbstract(a,tl) when not (Meta.has Meta.CoreType a.a_meta) ->
		filter_param (Abstract.get_underlying_type a tl)
	| TAbstract(a,tl) ->
		TAbstract(a, List.map filter_param tl)
	| TAnon a ->
		TAnon {
			a_fields = PMap.map (fun f -> { f with cf_type = filter_param f.cf_type }) a.a_fields;
			a_status = a.a_status;
		}
	| TFun(args,ret) ->
		TFun(List.map (fun (n,o,t) -> (n,o,filter_param t)) args, filter_param ret)
	| TDynamic _ ->
		t
	| TLazy f ->
		filter_param (lazy_type f)

let init_expr_filter allowed_metas =
	let rec run e =
		match e.eexpr with
		| TMeta ((m,_,_), e) when not (Hashtbl.mem allowed_metas m) ->
			run e
		| _ ->
			map_expr_type (fun e -> run e) filter_param (fun v -> v.v_type <- filter_param v.v_type; v) e
	in
	run

let type_filter = function
	| TClassDecl cl ->
		let rec map cf =
			cf.cf_type <- filter_param cf.cf_type;
			List.iter map cf.cf_overloads
		in
		List.iter map cl.cl_ordered_fields;
		List.iter map cl.cl_ordered_statics;
		Option.may map cl.cl_constructor
	| _ ->
		()

let name = "normalize_type"
let priority = max_dep

let configure gen ~allowed_metas =
	let run = init_expr_filter allowed_metas in
	gen.gexpr_filters#add name (PCustom priority) run;

	let map md = type_filter md; md in
	gen.gmodule_filters#add name (PCustom priority) map
