(*
	The Haxe Compiler
	Copyright (C) 2005-2017  Haxe Foundation

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
	When we pass a class as an object, in some languages we will need a special construct to be able to
	access its statics as if they were normal object fields. On C# and Java the way found to do that is
	by handling statics reflection also by a normal instance. This also happens in hxcpp and neko, so I
	guess it's a valid practice.

	So if we want to handle the reflection of the static MyClass, here's roughly how it will be done:

	var x = MyClass;
	gets converted into
	var x = typeof(MyClass);
*)
let add_typeof =
	let rec run e =
		match e.eexpr with
		| TCall (({ eexpr = TIdent ("__is__" | "__as__" | "__typeof__") } as elocal), args) ->
			let args = List.map (fun e -> match e.eexpr with TTypeExpr _ -> e | _ -> run e) args in
			{ e with eexpr = TCall (elocal, args) }
		| TField ({ eexpr = TTypeExpr _ }, _) ->
			e
		| TField (ef, f) ->
			(match anon_class ef.etype with
			| None -> Type.map_expr run e
			| Some t -> { e with eexpr = TField ({ ef with eexpr = TTypeExpr t }, f)})
		| TTypeExpr _ ->
			{ e with eexpr = TCall (mk (TIdent "__typeof__") t_dynamic e.epos, [e]) }
		| _ ->
			Type.map_expr run e
	in
	run

let name = "class_instance"
let priority = solve_deps name []

let configure gen =
	gen.gsyntax_filters#add name (PCustom priority) add_typeof
