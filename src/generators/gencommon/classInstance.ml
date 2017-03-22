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

(* ******************************************* *)
(* Anonymous Class object handling *)
(* ******************************************* *)
(*
	(syntax)
	When we pass a class as an object, in some languages we will need a special construct to be able to
	access its statics as if they were normal object fields. On C# and Java the way found to do that is
	by handling statics reflection also by a normal instance. This also happens in hxcpp and neko, so I
	guess it's a valid practice.
	So if we want to handle the reflection of the static MyClass, here's roughly how it will be done:

	var x = MyClass;
	gets converted into
	Haxe.Lang.Class x = Haxe.Lang.Runtime.GetType(typeof(MyClass).RuntimeHandle);

	which will in turn look in its cache but roughly would do:
	Haxe.Lang.Class x = new Haxe.Lang.Class(new MyClass(EmptyObject.EMPTY));

	This module will of course let the caller choose how this will be implemented. It will just identify all
	uses of class that will require it to be cast as an object.
*)
let priority = solve_deps "class_instance" []

let configure gen (change_expr:texpr->module_type->texpr) =
	let rec run e =
		match e.eexpr with
		| TCall( ({ eexpr = TLocal({ v_name = ("__is__" | "__as__" | "__typeof__") } as v) } as local), calls ) when Hashtbl.mem gen.gspecial_vars v.v_name ->
			{ e with eexpr = TCall(local, List.map (fun e ->
				match e.eexpr with
				| TTypeExpr _ -> e
				| _ -> run e) calls) }
		| TField({ eexpr = TTypeExpr(mt) }, f) ->
				e
		| TField(ef, f) ->
			(match anon_class ef.etype with
				| None -> Type.map_expr run e
				| Some t ->
					{ e with eexpr = TField( { ef with eexpr = TTypeExpr(t) }, f) }
			)
		| TTypeExpr(mt) -> change_expr e mt
		| _ -> Type.map_expr run e
	in
	let map e = Some(run e) in
	gen.gsyntax_filters#add ~name:"class_instance" ~priority:(PCustom priority) map
