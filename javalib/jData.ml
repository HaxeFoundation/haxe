(*
 *  This file is part of JavaLib
 *  Copyright (c)2004-2012 Nicolas Cannasse and Caue Waneck
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

type jpath = (string list) * string

type jversion = int * int (* minor + major *)

(** unqualified names cannot have the characters '.', ';', '[' or '/' *)
type unqualified_name = string

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

(* ( jsignature list ) ReturnDescriptor (| V | jsignature) *)
and jmethod_signature = jsignature list * jsignature option

(* InvokeDynamic-specific: Method handle *)
type reference_type =
  | RGetField (* constant must be ConstField *)
  | RGetStatic (* constant must be ConstField *)
  | RPutField (* constant must be ConstField *)
  | RPutStatic (* constant must be ConstField *)
  | RInvokeVirtual (* constant must be Method *)
  | RInvokeStatic (* constant must be Method *)
  | RInvokeSpecial (* constant must be Method *)
  | RNewInvokeSpecial (* constant must be Method with name <init> *)
  | RInvokeInterface (* constant must be InterfaceMethod *)

(* TODO *)
type bootstrap_method = int

type jconstant =
  (** references a class or an interface - jpath must be encoded as StringUtf8 *)
  | ConstClass of jpath (* tag = 7 *)
  (** field reference *)
  | ConstField of (jpath * unqualified_name * jsignature) (* tag = 9 *)
  (** method reference; string can be special "<init>" and "<clinit>" values *)
  | ConstMethod of (jpath * unqualified_name * jmethod_signature) (* tag = 10 *)
  (** interface method reference *)
  | ConstInterfaceMethod of (jpath * unqualified_name * jmethod_signature) (* tag = 11 *)
  (** constant values *)
  | ConstString of string  (* tag = 8 *)
  | ConstInt of int32 (* tag = 3 *)
  | ConstFloat of float (* tag = 4 *)
  | ConstLong of int64 (* tag = 5 *)
  | ConstDouble of float (* tag = 6 *)
  (** name and type: used to represent a field or method, without indicating which class it belongs to *)
  | ConstNameAndType of unqualified_name * jsignature
  (** UTF8 encoded strings. Note that when reading/writing, take into account Utf8 modifications of java *)
  (* (http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.4.7) *)
  | ConstUtf8 of string
  (** invokeDynamic-specific *)
  | ConstMethodHandle of (reference_type * jconstant) (* tag = 15 *)
  | ConstMethodType of jmethod_signature (* tag = 16 *)
  | ConstInvokeDynamic of (bootstrap_method * unqualified_name * jsignature) (* tag = 18 *)
  | ConstUnusable

type jcode = unit (* TODO *)

type jaccess_flag =
  | JPublic (* 0x0001 *)
  | JPrivate (* 0x0002 *)
  | JProtected (* 0x0004 *)
  | JStatic (* 0x0008 *)
  | JFinal (* 0x0010 *)
  | JSynchronized (* 0x0020 *)
  | JVolatile (* 0x0040 *)
  | JTransient (* 0x0080 *)
  (** added if created by the compiler *)
  | JSynthetic (* 0x1000 *)
  | JEnum (* 0x4000 *)
  | JUnusable (* should not be present *)
  (** class flags *)
  | JSuper (* 0x0020 *)
  | JInterface (* 0x0200 *)
  | JAbstract (* 0x0400 *)
  | JAnnotation (* 0x2000 *)
  (** method flags *)
  | JBridge (* 0x0040 *)
  | JVarArgs (* 0x0080 *)
  | JNative (* 0x0100 *)
  | JStrict (* 0x0800 *)

type jaccess = jaccess_flag list

(* type parameter name, extends signature, implements signatures *)
type jtypes = (string * jsignature option * jsignature list) list

type jannotation = {
  ann_type : jsignature;
  ann_elements : (string * jannotation_value) list;
}

and jannotation_value =
  | ValConst of jconstant (* B, C, D, E, F, I, J, S, Z, s *)
  | ValEnum of jsignature * string (* e *)
  | ValClass of jsignature (* c *) (* V -> Void *)
  | ValAnnotation of jannotation (* @ *)
  | ValArray of jannotation_value list (* [ *)

type jattribute =
  | AttrDeprecated
  | AttrVisibleAnnotations of jannotation list
  | AttrInvisibleAnnotations of jannotation list
  | AttrUnknown of string * string

type jfield_kind =
  | JKField
  | JKMethod

type jfield = {
  jf_name : string;
  jf_kind : jfield_kind;
  (* signature, as used by the vm *)
  jf_vmsignature : jsignature;
  (* actual signature, as used in java code *)
  jf_signature : jsignature;
  jf_throws : jsignature list;
  jf_types : jtypes;
  jf_flags : jaccess;
  jf_attributes : jattribute list;
  jf_constant : jconstant option;
  jf_code : jcode option;
}

type jclass = {
  cversion : jversion;
  cpath : jpath;
  csuper : jsignature;
  cflags : jaccess;
  cinterfaces : jsignature list;
  cfields : jfield list;
  cmethods : jfield list;
  cattributes : jattribute list;

  cinner_types : (jpath * jpath option * string option * jaccess) list;
  ctypes : jtypes;
}

(* reading/writing *)
type utf8ref = int
type classref = int
type nametyperef = int
type dynref = int
type bootstrapref = int

type jconstant_raw =
  | KClass of utf8ref (* 7 *)
  | KFieldRef of (classref * nametyperef) (* 9 *)
  | KMethodRef of (classref * nametyperef) (* 10 *)
  | KInterfaceMethodRef of (classref * nametyperef) (* 11 *)
  | KString of utf8ref (* 8 *)
  | KInt of int32 (* 3 *)
  | KFloat of float (* 4 *)
  | KLong of int64 (* 5 *)
  | KDouble of float (* 6 *)
  | KNameAndType of (utf8ref * utf8ref) (* 12 *)
  | KUtf8String of string (* 1 *)
  | KMethodHandle of (reference_type * dynref) (* 15 *)
  | KMethodType of utf8ref (* 16 *)
  | KInvokeDynamic of (bootstrapref * nametyperef) (* 18 *)
  | KUnusable

(* jData debugging *)
let is_override_attrib = (function
    (* TODO: pass anotations as @:meta *)
    | AttrVisibleAnnotations ann ->
      List.exists (function
        | { ann_type = TObject( (["java";"lang"], "Override"), [] ) } ->
            true
        | _ -> false
      ) ann
    | _ -> false
  )

let is_override field =
  List.exists is_override_attrib field.jf_attributes

let path_s = function
  | (pack,name) -> String.concat "." (pack @ [name])

let rec s_sig = function
  | TByte (* B *) -> "byte"
  | TChar (* C *) -> "char"
  | TDouble (* D *) -> "double"
  | TFloat (* F *) -> "float"
  | TInt (* I *) -> "int"
  | TLong (* J *) -> "long"
  | TShort (* S *) -> "short"
  | TBool (* Z *) -> "bool"
  | TObject(path,args) -> path_s  path ^ s_args args
  | TObjectInner (sl, sjargl) -> String.concat "." sl ^ "." ^ (String.concat "." (List.map (fun (s,arg) -> s ^ s_args arg) sjargl))
  | TArray (s,i) -> s_sig s ^ "[" ^ (match i with | None -> "" | Some i -> string_of_int i) ^ "]"
  | TMethod (sigs, sopt) -> (match sopt with | None -> "" | Some s -> s_sig s ^ " ") ^ "(" ^ String.concat ", " (List.map s_sig sigs) ^ ")"
  | TTypeParameter s -> s

and s_args = function
  | [] -> ""
  | args -> "<" ^ String.concat ", " (List.map (fun t ->
      match t with
      | TAny -> "*"
      | TType (wc, s) ->
        (match wc with
          | WNone -> ""
          | WExtends -> "+"
          | WSuper -> "-") ^
        (s_sig s))
    args) ^ ">"

let s_field f = (if is_override f then "override " else "") ^ s_sig f.jf_signature ^ " " ^ f.jf_name

let s_fields fs = "{ \n\t" ^ String.concat "\n\t" (List.map s_field fs) ^ "\n}"

