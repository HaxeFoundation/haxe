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
open ExtString

type native_lib_flags =
	| FlagIsStd
	| FlagIsExtern

class virtual ['a,'data] native_library (name : string) (file_path : string) = object(self)
	val mutable flags : native_lib_flags list = []

	method add_flag flag = flags <- flag :: flags
	method has_flag flag = List.mem flag flags

	method get_name = name
	method get_file_path = file_path

	method virtual build : path -> pos -> Ast.package option
	method virtual close : unit
	method virtual list_modules : path list
	method virtual load : unit
	method virtual lookup : path -> 'a
	method virtual get_data : 'data
end

type java_lib_type = (JData.jclass * string * string) option
type net_lib_type = IlData.ilclass option
type swf_lib_type = As3hl.hl_class option

type native_libraries = {
	mutable java_libs : (java_lib_type,unit) native_library list;
	mutable net_libs : (net_lib_type,unit) native_library list;
	mutable swf_libs : (swf_lib_type,Swf.swf) native_library list;
	mutable all_libs : string list;
}

let create_native_libs () = {
	java_libs = [];
	net_libs = [];
	swf_libs = [];
	all_libs = [];
}