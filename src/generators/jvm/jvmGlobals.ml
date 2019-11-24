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

type jvm_constant_pool_index = int

(* Constants *)

type jvm_bootstrap_method_index = int

type jvm_constant =
	| ConstUtf8 of string
	| ConstInt of int32
	| ConstFloat of float
	| ConstLong of int64
	| ConstDouble of float
	| ConstClass of jvm_constant_pool_index
	| ConstString of jvm_constant_pool_index
	| ConstFieldref of jvm_constant_pool_index * jvm_constant_pool_index
	| ConstMethodref of jvm_constant_pool_index * jvm_constant_pool_index
	| ConstInterfaceMethodref of jvm_constant_pool_index * jvm_constant_pool_index
	| ConstNameAndType of jvm_constant_pool_index * jvm_constant_pool_index
	| ConstMethodHandle of int * jvm_constant_pool_index
	| ConstMethodType of jvm_constant_pool_index
	| ConstInvokeDynamic of jvm_bootstrap_method_index * jvm_constant_pool_index

type field_kind =
	| FKField
	| FKMethod
	| FKInterfaceMethod

type numeric_range =
	| Int8Range
	| Int16Range
	| Int32Range

let i320xFF = Int32.of_int 0xFF

let get_numeric_range i =
	if i >= -128 && i <= 127 then Int8Range
	else if i >= -32768 && i <= 32767 then Int16Range
	else Int32Range

let get_numeric_range_unsigned i =
	if i <= 0xFF then Int8Range
	else if i <= 0xFFFF then Int16Range
	else Int32Range

let in_range unsigned range i = match (if unsigned then get_numeric_range_unsigned else get_numeric_range) i,range with
	| Int8Range,(Int8Range | Int16Range | Int32Range) -> true
	| Int16Range,(Int16Range | Int32Range) -> true
	| Int32Range,Int32Range -> true
	| _ -> false

let jerror s =
	failwith s

module ClassAccessFlags = struct
	type t =
		| CPublic
		| CFinal
		| CSuper
		| CInterface
		| CAbstract
		| CSynthetic
		| CAnnotation
		| CEnum

	let to_int = function
		| CPublic -> 0x1
		| CFinal -> 0x10
		| CSuper -> 0x20
		| CInterface -> 0x200
		| CAbstract -> 0x400
		| CSynthetic -> 0x1000
		| CAnnotation -> 0x2000
		| CEnum -> 0x4000

	let has_flag b flag =
		b land (to_int flag) <> 0
end

module MethodAccessFlags = struct
	type t =
		| MPublic
		| MPrivate
		| MProtected
		| MStatic
		| MFinal
		| MSynchronized
		| MBridge
		| MVarargs
		| MNative
		| MAbstract
		| MStrict
		| MSynthetic

	let to_int = function
		| MPublic -> 0x1
		| MPrivate -> 0x2
		| MProtected -> 0x4
		| MStatic -> 0x8
		| MFinal -> 0x10
		| MSynchronized -> 0x20
		| MBridge -> 0x40
		| MVarargs -> 0x80
		| MNative -> 0x100
		| MAbstract -> 0x400
		| MStrict -> 0x800
		| MSynthetic -> 0x1000

	let has_flag b flag =
		b land (to_int flag) <> 0
end

module FieldAccessFlags = struct
	type t =
		| FdPublic
		| FdPrivate
		| FdProtected
		| FdStatic
		| FdFinal
		| FdVolatile
		| FdTransient
		| FdSynthetic
		| FdEnum

	let to_int = function
		| FdPublic -> 0x1
		| FdPrivate -> 0x2
		| FdProtected -> 0x4
		| FdStatic -> 0x8
		| FdFinal -> 0x10
		| FdVolatile -> 0x40
		| FdTransient -> 0x80
		| FdSynthetic -> 0x1000
		| FdEnum -> 0x4000

	let has_flag b flag =
		b land (to_int flag) <> 0
end

let write_byte ch i = IO.write_byte ch i
let write_bytes ch b = IO.nwrite ch b
let write_ui16 ch i = IO.BigEndian.write_ui16 ch i
let write_ui32 ch i = IO.BigEndian.write_real_i32 ch (Int32.of_int i)
let write_string ch s = IO.nwrite_string ch s

let write_array16 ch f a =
	write_ui16 ch (Array.length a);
	Array.iter (f ch) a