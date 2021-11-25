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

let reverse_map = Hashtbl.create 0

let rev_hash i = Hashtbl.find reverse_map i

let hash f =
	let i = Hashtbl.hash f in
	Hashtbl.replace reverse_map i f;
	i

let path_hash path = hash (Globals.s_type_path path)

let key_length = hash "length"
let key_toString = hash "toString"
let key_OutsideBounds = hash "OutsideBounds"
let key_low = hash "low"
let key_high = hash "high"
let key_next = hash "next"
let key_hasNext = hash "hasNext"
let key___meta__ = hash "__meta__"
let key_get = hash "get"
let key_pos = hash "pos"
let key_min = hash "min"
let key_max = hash "max"
let key_file = hash "file"
let key_len = hash "len"
let key_message = hash "message"
let key_name = hash "name"
let key_error = hash "error"
let key_exception_message = hash "__exceptionMessage"
let key_native_exception = hash "__nativeException"
let key_native_stack = hash "__nativeStack"
let key_Array = hash "Array"
let key_eval_Vector = hash "eval.Vector"
let key_String = hash "String"
let key_haxe_Exception = hash "haxe.Exception"
let key_haxe_ds_Option = hash "haxe.ds.Option"
let key_haxe_ds_StringMap = hash "haxe.ds.StringMap"
let key_haxe_ds_IntMap = hash "haxe.ds.IntMap"
let key_haxe_ds_ObjectMap = hash "haxe.ds.ObjectMap"
let key_haxe_macro_Position = hash "haxe.macro.Position"
let key_haxe_macro_LazyType = hash "haxe.macro.LazyType"
let key_haxe_macro_TypeDecl = hash "haxe.macro.TypeDecl"
let key_haxe_Utf8 = hash "haxe.Utf8"
let key_haxe_macro_Ref = hash "haxe.macro.Ref"
let key_haxe_io_Error = hash "haxe.io.Error"
let key_haxe_io_Bytes = hash "haxe.io.Bytes"
let key_Date = hash "Date"
let key_Dynamic = hash "Dynamic"
let key_ValueType = hash "ValueType"
let key_EReg = hash "EReg"
let key_haxe_io_BytesBuffer = hash "haxe.io.BytesBuffer"
let key_StringBuf = hash "StringBuf"
let key_haxe_macro_Error = hash "haxe.macro.Error"
let key_Int = hash "Int"
let key_Float = hash "Float"
let key_Bool = hash "Bool"
let key_Class = hash "Class"
let key_Enum = hash "Enum"
let key_EnumValue = hash "EnumValue"
let key_gid = hash "gid"
let key_uid = hash "uid"
let key_atime = hash "atime"
let key_mtime = hash "mtime"
let key_ctime = hash "ctime"
let key_dev = hash "dev"
let key_ino = hash "ino"
let key_nlink = hash "nlink"
let key_rdev = hash "rdev"
let key_size = hash "size"
let key_mode = hash "mode"
let key_haxe__Int64____Int64 = hash "haxe._Int64.___Int64"
let key_haxe_macro_Unsafe = hash "haxe.macro.Unsafe"
let key_sys_io__Process_NativeProcess = hash "sys.io._Process.NativeProcess"
let key_sys_io_FileOutput = hash "sys.io.FileOutput"
let key_sys_io_FileInput = hash "sys.io.FileInput"
let key_haxe_io_Eof = hash "haxe.io.Eof"
let key_haxe_macro_ExprDef = hash "haxe.macro.ExprDef"
let key_haxe_macro_Binop = hash "haxe.macro.Binop"
let key_haxe_macro_Unop = hash "haxe.macro.Unop"
let key_haxe_macro_Constant = hash "haxe.macro.Constant"
let key_haxe_macro_TypeParam = hash "haxe.macro.TypeParam"
let key_haxe_macro_ComplexType = hash "haxe.macro.ComplexType"
let key_haxe_macro_FieldType = hash "haxe.macro.FieldType"
let key_haxe_macro_Type = hash "haxe.macro.Type"
let key_haxe_macro_FieldKind = hash "haxe.macro.FieldKind"
let key_haxe_macro_MethodKind = hash "haxe.macro.MethodKind"
let key_haxe_macro_VarAccess = hash "haxe.macro.VarAccess"
let key_haxe_macro_Access = hash "haxe.macro.Access"
let key_haxe_macro_ClassKind = hash "haxe.macro.ClassKind"
let key_haxe_macro_TypedExprDef = hash "haxe.macro.TypedExprDef"
let key_haxe_macro_TConstant = hash "haxe.macro.TConstant"
let key_haxe_macro_ModuleType = hash "haxe.macro.ModuleType"
let key_haxe_macro_FieldAccess = hash "haxe.macro.FieldAccess"
let key_haxe_macro_AnonStatus = hash "haxe.macro.AnonStatus"
let key_haxe_macro_ImportMode = hash "haxe.macro.ImportMode"
let key_haxe_macro_QuoteStatus = hash "haxe.macro.QuoteStatus"
let key_haxe_macro_DisplayKind = hash "haxe.macro.DisplayKind"
let key_haxe_macro_Message = hash "haxe.macro.Message"
let key_haxe_macro_FunctionKind = hash "haxe.macro.FunctionKind"
let key_haxe_macro_StringLiteralKind = hash "haxe.macro.StringLiteralKind"
let key___init__ = hash "__init__"
let key_new = hash "new"
let key_questionmark = hash "?"
let key_haxe_StackItem = hash "haxe.StackItem"
let key_eval_vm_NativeSocket = hash "eval.vm.NativeSocket"
let key_ip = hash "ip"
let key_port = hash "port"
let key_sys_net_Socket = hash "sys.net.Socket"
let key_socket = hash "socket"
let key_read = hash "read"
let key_write = hash "write"
let key_others = hash "others"
let key_eval_vm_Thread = hash "eval.vm.NativeThread"
let key_haxe_zip_Compress = hash "haxe.zip.Compress"
let key_haxe_zip_Uncompress = hash "haxe.zip.Uncompress"
let key_done = hash "done"
let key_eval_toplevel = hash "eval-toplevel"
let key_haxe_iterators_array_key_value_iterator = hash "haxe.iterators.ArrayKeyValueIterator"
let key_haxe_iterators_map_key_value_iterator = hash "haxe.iterators.MapKeyValueIterator"
let key_sys_net_Mutex = hash "sys.thread.Mutex"
let key_sys_net_Lock = hash "sys.thread.Lock"
let key_sys_net_Tls = hash "sys.thread.Tls"
let key_sys_net_Deque = hash "sys.thread.Deque"
let key_sys_thread_EventLoop = hash "sys.thread.EventLoop"
let key_sys_thread_Semaphore = hash "sys.thread.Semaphore"
let key_sys_thread_Condition = hash "sys.thread.Condition"
let key_mbedtls_Config = hash "mbedtls.Config"
let key_mbedtls_CtrDrbg = hash "mbedtls.CtrDrbg"
let key_mbedtls_Entropy = hash "mbedtls.Entropy"
let key_mbedtls_PkContext = hash "mbedtls.PkContext"
let key_mbedtls_Ssl = hash "mbedtls.Ssl"
let key_mbedtls_X509Crt = hash "mbedtls.X509Crt"

let key_eval_luv_Result = hash "eval.luv.Result"
let key_eval_luv_LuvException = hash "eval.luv.LuvException"
let key_eval_luv_ReceiveHandle = hash "eval.luv.ReceiveHandle"
let key_eval_luv_AddressFamily = hash "eval.luv.AddressFamily"
let key_eval_luv_SocketType = hash "eval.luv.SocketType"
let key_onExit = hash "onExit"
let key_environment = hash "environment"
let key_redirect = hash "redirect"
let key_workingDirectory = hash "workingDirectory"
let key_uid = hash "uid"
let key_gid = hash "gid"
let key_windowsVerbatimArguments = hash "windowsVerbatimArguments"
let key_detached = hash "detached"
let key_windowsHide = hash "windowsHide"
let key_windowsHideConsole = hash "windowsHideConsole"
let key_windowsHideGui = hash "windowsHideGui"
let key_request = hash "request"
let key_family = hash "family"
let key_sockType = hash "sockType"
let key_protocol = hash "protocol"
let key_flags = hash "flags"
let key_addr = hash "addr"
let key_data = hash "data"
let key_canonName = hash "canonName"
let key_node = hash "node"
let key_service = hash "service"
let key_sec = hash "sec"
let key_nsec = hash "nsec"
let key_usec = hash "usec"
let key_blksize = hash "blksize"
let key_blocks = hash "blocks"
let key_gen = hash "gen"
let key_atim = hash "atim"
let key_mtim = hash "mtim"
let key_ctim = hash "ctim"
let key_birthtim = hash "birthtim"
let key_width = hash "width"
let key_height = hash "height"
let key_type = hash "type"
let key_bsize = hash "bsize"
let key_bfree = hash "bfree"
let key_bavail = hash "bavail"
let key_files = hash "files"
let key_ffree = hash "ffree"
let key_fspare = hash "fspare"
let key_kind = hash "kind"
let key_end = hash "end"
let key_events = hash "events"
let key_isInternal = hash "isInternal"
let key_physical = hash "physical"
let key_address = hash "address"
let key_netmask = hash "netmask"
let key_previous = hash "previous"
let key_current = hash "current"