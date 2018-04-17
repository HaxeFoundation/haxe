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
open Ast
open Type

type usage =
	| Block of ((usage -> unit) -> unit)
	| Loop of ((usage -> unit) -> unit)
	| Function of ((usage -> unit) -> unit)
	| Declare of tvar
	| Use of tvar
	| Assign of tvar

let rec local_usage f e =
	match e.eexpr with
	| TBinop ((OpAssign | OpAssignOp _), { eexpr = TLocal v }, e2) ->
		local_usage f e2;
		f (Assign v)
	| TUnop ((Increment | Decrement), _, { eexpr = TLocal v }) ->
		f (Assign v)
	| TLocal v ->
		f (Use v)
	| TVar (v,eo) ->
		(match eo with None -> () | Some e -> local_usage f e);
		f (Declare v);
	| TFunction tf ->
		let cc f =
			List.iter (fun (v,_) -> f (Declare v)) tf.tf_args;
			local_usage f tf.tf_expr;
		in
		f (Function cc)
	| TBlock l ->
		f (Block (fun f -> List.iter (local_usage f) l))
	| TFor (v,it,e) ->
		local_usage f it;
		f (Loop (fun f ->
			f (Declare v);
			local_usage f e;
		))
	| TWhile _ ->
		f (Loop (fun f ->
			iter (local_usage f) e
		))
	| TTry (e,catchs) ->
		local_usage f e;
		List.iter (fun (v,e) ->
			f (Block (fun f ->
				f (Declare v);
				local_usage f e;
			))
		) catchs;
	| _ ->
		iter (local_usage f) e
