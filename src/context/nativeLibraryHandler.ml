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
open CompilationContext

let add_native_lib com lib =
	let file = lib.lib_file in
	let is_extern = lib.lib_extern in
	match lib.lib_kind with
	| SwfLib ->
		SwfLoader.add_swf_lib com file is_extern
	| JavaLib ->
		let use_modern = Common.defined com Define.Jvm && not (Common.defined com Define.JarLegacyLoader) in
		let add file =
			let std = file = "lib/hxjava-std.jar" in
			Java.add_java_lib com file std is_extern use_modern
		in
		if try Sys.is_directory file with Sys_error _ -> false then
			let dir = file in
			(fun _ -> Array.iter (fun file ->
				if ExtString.String.ends_with file ".jar" then add (dir ^ "/" ^ file) ()
			) (Sys.readdir file))
		else
			add file
	| NetLib ->
		let file, is_std = match ExtString.String.nsplit file "@" with
			| [file] ->
				file,false
			| [file;"std"] ->
				file,true
			| _ -> failwith ("unsupported file@`std` format: " ^ file)
		in
		Dotnet.add_net_lib com file is_std is_extern
	| HxbLib ->
		let hxb_lib = HxbLib.create_hxb_lib com file in
		com.hxb_libs <- hxb_lib :: com.hxb_libs;
		(fun () ->
			hxb_lib#load
		)