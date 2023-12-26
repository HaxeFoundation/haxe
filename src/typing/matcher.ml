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
open MatcherGlobals

module Match = struct
	open Typecore

	let match_expr ctx e cases def with_type postfix_match p =
		let match_debug = Meta.has (Meta.Custom ":matchDebug") ctx.curfield.cf_meta in
		let rec loop e = match fst e with
			| EArrayDecl el when (match el with [(EFor _ | EWhile _),_] -> false | _ -> true) ->
				let el = List.map (fun e -> type_expr ctx e WithType.value) el in
				let t = ExprToPattern.tuple_type (List.map (fun e -> e.etype) el) in
				t,el
			| EParenthesis e1 ->
				loop e1
			| _ ->
				let e = type_expr ctx e WithType.value in
				e.etype,[e]
		in
		let t,subjects = loop e in
		let cases = match def with
			| None -> cases
			| Some (eo,p) -> cases @ [[EConst (Ident "_"),p],None,eo,p]
		in
		let tmono,with_type,allow_min_void = match with_type with
			| WithType.WithType(t,src) ->
				(match follow t, src with
				| ((TMono _) | (TAbstract({a_path=[],"Void"},_))), Some ImplicitReturn -> Some t, WithType.Value src, true
				| TMono _, _ -> Some t,WithType.value,false
				| _ -> None,with_type,false)
			| _ -> None,with_type,false
		in
		let cases = List.map (fun (el,eg,eo,p) ->
			let p = match eo with Some e when p = null_pos -> pos e | _ -> p in
			let case,bindings,pat = Case.make ctx t el eg eo with_type postfix_match p in
			case,bindings,[pat]
		) cases in
		let infer_switch_type () =
			match with_type with
				| WithType.NoValue -> ctx.t.tvoid
				| WithType.Value(_) ->
					begin match cases with
					| [] ->
						(* If there are no cases we assume Void. This then causes a "Cannot use Void as value" error.
						   Note that we cannot rely on an exhaustiveness error because the switch could be over an empty enum. *)
						ctx.t.tvoid
					| _ ->
						let el = List.map (fun (case,_,_) ->
							match case.Case.case_expr with
							| Some e ->
								(* If we have a block, use the position of the last element. *)
								begin match e.eexpr with
								| TBlock el when el <> [] -> List.hd (List.rev el)
								| _ -> e
								end
							| None ->
								(* If we have no block we have to use the `case pattern` position because that's all we have. *)
								mk (TBlock []) ctx.t.tvoid case.Case.case_pos
						) cases in
						if allow_min_void then unify_min_for_type_source ctx el (Some WithType.ImplicitReturn)
						else unify_min ctx el
					end
				| WithType.WithType(t,_) -> t
		in
		if match_debug then begin
			print_endline "CASES BEGIN";
			List.iter (fun (case,_,patterns) ->
				print_endline (String.concat "" (List.map (Pattern.to_string) patterns));
			) cases;
			print_endline "CASES END";
		end;
		let dt = Compile.compile ctx match_debug subjects cases p in
		if match_debug then begin
			print_endline "DECISION TREE BEGIN";
			print_endline (DecisionTree.to_string dt);
			print_endline "DECISION TREE END";
		end;
		let e =
			let t_switch = infer_switch_type() in
			(match tmono with
			| Some t when allow_min_void && ExtType.is_void (follow t) -> ()
			| Some t -> unify ctx t_switch t p
			| _ -> ()
			);
			TexprConverter.to_texpr ctx t_switch with_type dt
		in
		if match_debug then begin
			print_endline "TEXPR BEGIN";
			print_endline (s_expr_pretty e);
			print_endline "TEXPR END";
		end;
		{e with epos = p}

	let match_expr ctx e cases def with_type postfix_match p = match cases,def with
		| [],None when (match with_type with WithType.NoValue -> true | _ -> false) ->
			type_expr ctx e WithType.value
		| _ ->
			match_expr ctx e cases def with_type postfix_match p
end
