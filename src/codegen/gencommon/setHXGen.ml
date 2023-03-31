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
open Common
open Type

(* ******************************************* *)
(* set hxgen module *)
(* ******************************************* *)
(*
	Goes through all module types and adds the @:hxGen or @:nativeGen meta to them.
	Basically, everything that is extern is assumed to not be hxgen, unless meta :hxGen is set,
	and everything that is not extern is assumed to be hxgen, unless meta :nativeGgen is set.
*)

(*
	The only option is to run this filter eagerly, because it must be one of the first filters to run,
	since many others depend of it.
*)
let run_filter com types =
	let rec is_hxgen md =
		match md with
		| TClassDecl { cl_kind = KAbstractImpl a } ->
			is_hxgen (TAbstractDecl a)
		| TClassDecl cl ->
			let rec is_hxgen_class (c,_) =
				if (has_class_flag c CExtern) then begin
					if Meta.has Meta.HxGen c.cl_meta then
						true
					else
						Option.map_default (is_hxgen_class) false c.cl_super || List.exists is_hxgen_class c.cl_implements
				end else begin
					if Meta.has Meta.NativeChildren c.cl_meta || Meta.has Meta.NativeGen c.cl_meta || Meta.has Meta.Struct c.cl_meta then
						Option.map_default is_hxgen_class false c.cl_super || List.exists is_hxgen_class c.cl_implements
					else
						let rec has_nativec (c,p) =
							if is_hxgen_class (c,p) then
								false
							else if Meta.has Meta.Struct c.cl_meta then begin
								com.error ("Struct types cannot be subclassed") c.cl_pos;
								true
							end else
								(Meta.has Meta.NativeChildren c.cl_meta && not (Option.map_default is_hxgen_class false c.cl_super || List.exists is_hxgen_class c.cl_implements))
								|| Option.map_default has_nativec false c.cl_super
						in
						if Option.map_default has_nativec false c.cl_super && not (List.exists is_hxgen_class c.cl_implements) then
							false
						else
							true
				end
			in
			is_hxgen_class (cl,[])
		| TEnumDecl e ->
			if e.e_extern then
				Meta.has Meta.HxGen e.e_meta
			else if Meta.has Meta.NativeGen e.e_meta then
				if Meta.has Meta.FlatEnum e.e_meta then
					false
				else begin
					com.error "Only flat enums may be @:nativeGen" e.e_pos;
					true
				end
			else
				true
		| TAbstractDecl a when Meta.has Meta.CoreType a.a_meta ->
			not (Meta.has Meta.NativeGen a.a_meta)
		| TAbstractDecl a ->
			(match follow a.a_this with
			| TInst _ | TEnum _ | TAbstract _ ->
				is_hxgen (module_type_of_type (follow a.a_this))
			| _ ->
				not (Meta.has Meta.NativeGen a.a_meta))
		| TTypeDecl t -> (* TODO see when would we use this *)
			false
	in

	let filter md =
		let meta = if is_hxgen md then Meta.HxGen else Meta.NativeGen in
		match md with
		| TClassDecl cl -> cl.cl_meta <- (meta, [], cl.cl_pos) :: cl.cl_meta
		| TEnumDecl e -> e.e_meta <- (meta, [], e.e_pos) :: e.e_meta
		| TTypeDecl t -> t.t_meta <- (meta, [], t.t_pos) :: t.t_meta
		| TAbstractDecl a -> a.a_meta <- (meta, [], a.a_pos) :: a.a_meta
	in

	List.iter filter types
