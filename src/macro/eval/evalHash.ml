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

let reverse_map = Hashtbl.create 0
let file_map = Hashtbl.create 0

let rev_hash i = Hashtbl.find reverse_map i

let rev_hash_s i = Rope.to_string (rev_hash i)

let hash f =
	let i = Hashtbl.hash (Rope.to_string f) in
	Hashtbl.replace reverse_map i f;
	i

let hash_s f =
	let i = Hashtbl.hash f in
	Hashtbl.replace reverse_map i (Rope.of_string f);
	i

let path_hash path = hash_s (Globals.s_type_path path)

let file_hash file =
	let unique_file = Path.unique_full_path file in
	Hashtbl.replace file_map unique_file file;
	hash_s unique_file

let rev_file_hash i =
	let s = rev_hash_s i in
	try Hashtbl.find file_map s with Not_found -> s

let key_length = hash_s "length"
let key_toString = hash_s "toString"
let key_OutsideBounds = hash_s "OutsideBounds"
let key_low = hash_s "low"
let key_high = hash_s "high"
let key_next = hash_s "next"
let key_hasNext = hash_s "hasNext"
let key___meta__ = hash_s "__meta__"
let key_get = hash_s "get"
let key_pos = hash_s "pos"
let key_len = hash_s "len"
let key_message = hash_s "message"
let key_Array = hash_s "Array"
let key_eval_Vector = hash_s "eval.Vector"
let key_String = hash_s "String"
let key_haxe_ds_StringMap = hash_s "haxe.ds.StringMap"
let key_haxe_ds_IntMap = hash_s "haxe.ds.IntMap"
let key_haxe_ds_ObjectMap = hash_s "haxe.ds.ObjectMap"
let key_haxe_macro_Position = hash_s "haxe.macro.Position"
let key_haxe_macro_LazyType = hash_s "haxe.macro.LazyType"
let key_haxe_macro_TypeDecl = hash_s "haxe.macro.TypeDecl"
let key_haxe_Utf8 = hash_s "haxe.Utf8"
let key_haxe_macro_Ref = hash_s "haxe.macro.Ref"
let key_haxe_io_Error = hash_s "haxe.io.Error"
let key_haxe_io_Bytes = hash_s "haxe.io.Bytes"
let key_Date = hash_s "Date"
let key_Dynamic = hash_s "Dynamic"
let key_ValueType = hash_s "ValueType"
let key_EReg = hash_s "EReg"
let key_haxe_io_BytesBuffer = hash_s "haxe.io.BytesBuffer"
let key_StringBuf = hash_s "StringBuf"
let key_haxe_macro_Error = hash_s "haxe.macro.Error"
let key_Int = hash_s "Int"
let key_Float = hash_s "Float"
let key_Bool = hash_s "Bool"
let key_Class = hash_s "Class"
let key_Enum = hash_s "Enum"
let key_EnumValue = hash_s "EnumValue"
let key_gid = hash_s "gid"
let key_uid = hash_s "uid"
let key_atime = hash_s "atime"
let key_mtime = hash_s "mtime"
let key_ctime = hash_s "ctime"
let key_dev = hash_s "dev"
let key_ino = hash_s "ino"
let key_nlink = hash_s "nlink"
let key_rdev = hash_s "rdev"
let key_size = hash_s "size"
let key_mode = hash_s "mode"
let key_haxe__Int64____Int64 = hash_s "haxe._Int64.___Int64"
let key_haxe_macro_Unsafe = hash_s "haxe.macro.Unsafe"
let key_sys_io__Process_NativeProcess = hash_s "sys.io._Process.NativeProcess"
let key_sys_io_FileOutput = hash_s "sys.io.FileOutput"
let key_sys_io_FileInput = hash_s "sys.io.FileInput"
let key_haxe_io_Eof = hash_s "haxe.io.Eof"
let key_haxe_macro_ExprDef = hash_s "haxe.macro.ExprDef"
let key_haxe_macro_Binop = hash_s "haxe.macro.Binop"
let key_haxe_macro_Unop = hash_s "haxe.macro.Unop"
let key_haxe_macro_Constant = hash_s "haxe.macro.Constant"
let key_haxe_macro_TypeParam = hash_s "haxe.macro.TypeParam"
let key_haxe_macro_ComplexType = hash_s "haxe.macro.ComplexType"
let key_haxe_macro_FieldType = hash_s "haxe.macro.FieldType"
let key_haxe_macro_Type = hash_s "haxe.macro.Type"
let key_haxe_macro_FieldKind = hash_s "haxe.macro.FieldKind"
let key_haxe_macro_MethodKind = hash_s "haxe.macro.MethodKind"
let key_haxe_macro_VarAccess = hash_s "haxe.macro.VarAccess"
let key_haxe_macro_Access = hash_s "haxe.macro.Access"
let key_haxe_macro_ClassKind = hash_s "haxe.macro.ClassKind"
let key_haxe_macro_TypedExprDef = hash_s "haxe.macro.TypedExprDef"
let key_haxe_macro_TConstant = hash_s "haxe.macro.TConstant"
let key_haxe_macro_ModuleType = hash_s "haxe.macro.ModuleType"
let key_haxe_macro_FieldAccess = hash_s "haxe.macro.FieldAccess"
let key_haxe_macro_AnonStatus = hash_s "haxe.macro.AnonStatus"
let key_haxe_macro_ImportMode = hash_s "haxe.macro.ImportMode"
let key_haxe_macro_QuoteStatus = hash_s "haxe.macro.QuoteStatus"
let key_haxe_macro_StringKind = hash_s "haxe.macro.StringKind"
let key_haxe_CallStack = hash_s "haxe.CallStack"
let key___init__ = hash_s "__init__"
let key_new = hash_s "new"
let key_questionmark = hash_s "?"
let key_haxe_StackItem = hash_s "haxe.StackItem"
let key_sys_net__Socket_NativeSocket = hash_s "sys.net._Socket.NativeSocket"
let key_ip = hash_s "ip"
let key_port = hash_s "port"
let key_sys_net_Socket = hash_s "sys.net.Socket"
let key_socket = hash_s "socket"
let key_read = hash_s "read"
let key_write = hash_s "write"
let key_others = hash_s "others"
let key_eval_vm_Thread = hash_s "eval.vm.Thread"
let key_haxe_zip_Compress = hash_s "haxe.zip.Compress"
let key_haxe_zip_Uncompress = hash_s "haxe.zip.Uncompress"
let key_done = hash_s "done"