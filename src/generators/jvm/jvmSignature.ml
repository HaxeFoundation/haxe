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

open JvmGlobals

type jpath = (string list) * string

type jwildcard =
	| WExtends (* + *)
	| WSuper (* -  *)
	| WNone

type jtype_argument =
	| TType of jwildcard * jsignature
	| TAny (* * *)

and jsignature =
	| TByte (* B *)
	| TChar (* C *)
	| TDouble (* D *)
	| TFloat (* F *)
	| TInt (* I *)
	| TLong (* J *)
	| TShort (* S *)
	| TBool (* Z *)
	| TObject of jpath * jtype_argument list (* L Classname *)
	| TObjectInner of (string list) * (string * jtype_argument list) list (* L Classname ClassTypeSignatureSuffix *)
	| TArray of jsignature * int option (* [ *)
	| TMethod of jmethod_signature (* ( *)
	| TTypeParameter of string (* T *)
	| TUninitialized of int option

(* ( jsignature list ) ReturnDescriptor (| V | jsignature) *)
and jmethod_signature = jsignature list * jsignature option

let rec has_type_parameter = function
	| TTypeParameter _ -> true
	| TArray(jsig,_) -> has_type_parameter jsig
	| TObject(_,jsigs) -> List.exists (function TType(_,jsig) -> has_type_parameter jsig | _ -> false) jsigs
	| _ -> false

module NativeSignatures = struct
	let object_path = ["java";"lang"],"Object"
	let object_sig = TObject(object_path,[])

	let string_path = ["java";"lang"],"String"
	let string_sig = TObject(string_path,[])

	let boolean_path = ["java";"lang"],"Boolean"
	let boolean_sig = TObject(boolean_path,[])

	let character_path = ["java";"lang"],"Character"
	let character_sig = TObject(character_path,[])

	let method_type_path = (["java";"lang";"invoke"],"MethodType")
	let method_type_sig = TObject(method_type_path,[])

	let method_lookup_path = (["java";"lang";"invoke"],"MethodHandles$Lookup")
	let method_lookup_sig = TObject(method_lookup_path,[])

	let call_site_path = (["java";"lang";"invoke"],"CallSite")
	let call_site_sig = TObject(call_site_path,[])

	let java_class_path = ["java";"lang"],"Class"
	let java_class_sig = TObject(java_class_path,[TType(WNone,object_sig)])

	let haxe_jvm_path = ["haxe";"jvm"],"Jvm"

	let haxe_dynamic_object_path = ["haxe";"jvm"],"DynamicObject"
	let haxe_dynamic_object_sig = TObject(haxe_dynamic_object_path,[])

	let haxe_exception_path = ["haxe"],"Exception"
	let haxe_exception_sig = TObject(haxe_exception_path,[])

	let haxe_object_path = ["haxe";"jvm"],"Object"
	let haxe_object_sig = TObject(haxe_object_path,[])

	let throwable_path = (["java";"lang"],"Throwable")
	let throwable_sig = TObject(throwable_path,[])

	let exception_path = (["java";"lang"],"Exception")
	let exception_sig = TObject(exception_path,[])

	let runtime_exception_path = (["java";"lang"],"RuntimeException")
	let runtime_exception_sig = TObject(runtime_exception_path,[])

	let retention_path = (["java";"lang";"annotation"],"Retention")
	let retention_sig = TObject(retention_path,[])

	let retention_policy_path = (["java";"lang";"annotation"],"RetentionPolicy")
	let retention_policy_sig = TObject(retention_policy_path,[])

	let java_enum_path = (["java";"lang"],"Enum")
	let java_enum_sig jsig = TObject(java_enum_path,[TType(WNone,jsig)])

	let haxe_enum_path = (["haxe";"jvm"],"Enum")
	let haxe_enum_sig jsig = TObject(haxe_enum_path,[TType(WNone,jsig)])

	let haxe_empty_constructor_path = (["haxe";"jvm"],"EmptyConstructor")
	let haxe_empty_constructor_sig = TObject(haxe_empty_constructor_path,[])

	let haxe_function_path = (["haxe";"jvm"],"Function")
	let haxe_function_sig = TObject(haxe_function_path,[])

	let haxe_compiled_pattern_path = (["haxe";"jvm"],"CompiledPattern")
	let haxe_compiled_pattern_sig = TObject(haxe_compiled_pattern_path,[])

	let haxe_ereg_path = (["haxe";"root"],"EReg")
	let haxe_ereg_sig = TObject(haxe_ereg_path,[])

	let void_path = ["java";"lang"],"Void"
	let void_sig = TObject(void_path,[])

	(* numeric *)

	let number_path = ["java";"lang"],"Number"
	let number_sig = TObject(number_path,[])

	let byte_path = ["java";"lang"],"Byte"
	let byte_sig = TObject(byte_path,[])

	let short_path = ["java";"lang"],"Short"
	let short_sig = TObject(short_path,[])

	let integer_path = ["java";"lang"],"Integer"
	let integer_sig = TObject(integer_path,[])

	let long_path = ["java";"lang"],"Long"
	let long_sig = TObject(long_path,[])

	let float_path = ["java";"lang"],"Float"
	let float_sig = TObject(float_path,[])

	let double_path = ["java";"lang"],"Double"
	let double_sig = TObject(double_path,[])

	(* compound *)

	let array_sig jsig = TArray(jsig,None)

	let method_sig jsigs jsig = TMethod(jsigs,jsig)

	let object_path_sig path = TObject(path,[])

	let get_boxed_type jsig = match jsig with
		| TBool -> boolean_sig
		| TChar -> character_sig
		| TByte -> byte_sig
		| TShort -> short_sig
		| TInt -> integer_sig
		| TLong -> long_sig
		| TFloat -> float_sig
		| TDouble -> double_sig
		| _ -> jsig

	let get_unboxed_type jsig = match jsig with
		| TObject((["java";"lang"],"Boolean"),_) -> TBool
		| TObject((["java";"lang"],"Charcter"),_) -> TChar
		| TObject((["java";"lang"],"Byte"),_) -> TByte
		| TObject((["java";"lang"],"Short"),_) -> TShort
		| TObject((["java";"lang"],"Integer"),_) -> TInt
		| TObject((["java";"lang"],"Long"),_) -> TLong
		| TObject((["java";"lang"],"Float"),_) -> TFloat
		| TObject((["java";"lang"],"Double"),_) -> TDouble
		| _ -> jsig

	let is_unboxed jsig = match jsig with
		| TBool | TChar
		| TByte | TShort | TInt | TLong
		| TFloat | TDouble ->
			true
		| _ ->
			false

	let is_dynamic_at_runtime = function
		| TObject((["java";"lang"],"Object"),_)
		| TTypeParameter _ ->
			true
		| _ ->
			false
end

let equals_at_runtime jsig1 jsig2 = match jsig1,jsig2 with
	| TByte,TByte
	| TChar,TChar
	| TDouble,TDouble
	| TFloat,TFloat
	| TInt,TInt
	| TLong,TLong
	| TShort,TShort
	| TBool,TBool
	| TObjectInner _,TObjectInner _
	| TArray _,TArray _
	| TMethod _,TMethod _
	| TTypeParameter _,TTypeParameter _
	| TUninitialized _,TUninitialized _ ->
		true
	| TObject(path1,_),TObject(path2,_) ->
		path1 = path2
	| TObject(path,_),TTypeParameter _
	| TTypeParameter _,TObject(path,_) ->
		path = NativeSignatures.object_path
	| _ -> false

let s_wildcard = function
	| WExtends -> "WExtends"
	| WSuper -> "WSuper"
	| WNone -> "WNone"

let rec s_signature_kind = function
	| TByte -> "TByte"
	| TChar -> "TChar"
	| TDouble -> "TDouble"
	| TFloat -> "TFloat"
	| TInt -> "TInt"
	| TLong -> "TLong"
	| TShort -> "TShort"
	| TBool -> "TBool"
	| TObject(path,params) -> Printf.sprintf "TObject(%s,[%s])" (Globals.s_type_path path) (String.concat "," (List.map s_signature_param_kind params))
	| TObjectInner _ -> "TObjectInner"
	| TArray(jsig,io) -> Printf.sprintf "TArray(%s,%s)" (s_signature_kind jsig) (Option.map_default string_of_int "None" io)
	| TMethod(jsigs,jsig) -> Printf.sprintf "TMethod([%s],%s)" (String.concat "," (List.map s_signature_kind jsigs)) (Option.map_default s_signature_kind "None" jsig)
	| TTypeParameter name -> Printf.sprintf "TTypeParameter(%s)" name
	| TUninitialized io -> Printf.sprintf "TUninitilaized(%s)" (Option.map_default string_of_int "None" io)

and s_signature_param_kind = function
	| TAny -> "TAny"
	| TType(wc,jsig) -> Printf.sprintf "TType(%s,%s)" (s_wildcard wc) (s_signature_kind jsig)

let encode_path (pack,name) =
	String.concat "/" (pack @ [name])

let rec write_param full ch param = match param with
	| TAny -> write_byte ch (Char.code '*')
	| TType(w, s) ->
		begin match w with
			| WExtends -> write_byte ch (Char.code '+')
			| WSuper -> write_byte ch (Char.code '-')
			| WNone -> ()
		end;
		write_signature full ch s

and write_signature full ch jsig = match jsig with
	| TByte -> write_byte ch (Char.code 'B')
	| TChar -> write_byte ch (Char.code 'C')
	| TDouble -> write_byte ch (Char.code 'D')
	| TFloat -> write_byte ch (Char.code 'F')
	| TInt -> write_byte ch (Char.code 'I')
	| TLong -> write_byte ch (Char.code 'J')
	| TShort -> write_byte ch (Char.code 'S')
	| TBool -> write_byte ch (Char.code 'Z')
	| TObject(path, params) ->
		write_byte ch (Char.code 'L');
		write_string ch (encode_path path);
		if params <> [] && full then begin
			write_byte ch (Char.code '<');
			List.iter (write_param full ch) params;
			write_byte ch (Char.code '>')
		end;
		write_byte ch (Char.code ';')
	| TObjectInner(pack, inners) ->
		write_byte ch (Char.code 'L');
		List.iter (fun p ->
			write_string ch p;
			write_byte ch (Char.code '/')
		) pack;
		let first = ref true in
		List.iter (fun (name,params) ->
			(if !first then first := false else write_byte ch (Char.code '.'));
			write_string ch name;
			if params <> [] then begin
				write_byte ch (Char.code '<');
				List.iter (write_param full ch) params;
				write_byte ch (Char.code '>')
			end;
		) inners;
		write_byte ch (Char.code ';')
	| TArray(s,size) ->
		write_byte ch (Char.code '[');
		begin match size with
			| Some size ->
				write_string ch (string_of_int size);
			| None -> ()
		end;
		write_signature full ch s
	| TMethod _ ->
		write_signature full ch NativeSignatures.haxe_function_sig
	| TTypeParameter name ->
		if full then begin
			write_byte ch (Char.code 'T');
			write_string ch name;
			write_byte ch (Char.code ';')
		end else
			write_string ch "Ljava/lang/Object;"
	| TUninitialized io ->
		write_string ch "uninitialized";
		match io with
		| None -> write_string ch " this"
		| Some i -> write_string ch (Printf.sprintf "(%i)" i)

let generate_signature full jsig =
	let ch = IO.output_bytes () in
	write_signature full ch jsig;
	Bytes.unsafe_to_string (IO.close_out ch)

let generate_method_signature full jsig =
	let ch = IO.output_bytes () in
	begin match jsig with
	| TMethod(args, ret) ->
		write_byte ch (Char.code '(');
		List.iter (write_signature full ch) args;
		write_byte ch (Char.code ')');
		begin match ret with
			| None -> write_byte ch (Char.code 'V')
			| Some jsig -> write_signature full ch jsig
		end
	| _ ->
		write_signature full ch jsig;
	end;
	Bytes.unsafe_to_string (IO.close_out ch)

let signature_size = function
	| TDouble | TLong -> 2
	| _ -> 1