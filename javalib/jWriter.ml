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
open JData;;
open IO.BigEndian;;
open ExtString;;
open ExtList;;

exception Writer_error_message of string

type context = {
  cpool : unit output;
  mutable ccount : int;
  ch : string output;
  mutable constants : (jconstant,int) PMap.t;
}

let error msg = raise (Writer_error_message msg)

let get_reference_type i =
  match i with
  | RGetField ->  1
  | RGetStatic ->  2
  | RPutField ->  3
  | RPutStatic ->  4
  | RInvokeVirtual ->  5
  | RInvokeStatic ->  6
  | RInvokeSpecial ->  7
  | RNewInvokeSpecial ->  8
  | RInvokeInterface ->  9

let encode_path ctx (pack,name) =
  String.concat "/" (pack @ [name])

let encode_sig ctx jsig = ""

let encode_utf8 ctx s = s (* TODO *)

let rec const ctx c =
  try
    PMap.find c ctx.constants
  with
  | Not_found ->
    (match c with
    (** references a class or an interface - jpath must be encoded as StringUtf8 *)
    | ConstClass path -> (* tag = 7 *)
        write_byte ctx.cpool 7;
        write_ui16 ctx.cpool (const ctx (ConstUtf8 (encode_path ctx path)))
    (** field reference *)
    | ConstField (jpath, unqualified_name, jsignature) (* tag = 9 *) ->
        write_byte ctx.cpool 9;
        write_ui16 ctx.cpool (const ctx (ConstClass jpath));
        write_ui16 ctx.cpool (const ctx (ConstNameAndType (unqualified_name, jsignature)))
    (** method reference; string can be special "<init>" and "<clinit>" values *)
    | ConstMethod (jpath, unqualified_name, jmethod_signature) (* tag = 10 *) ->
        write_byte ctx.cpool 10;
        write_ui16 ctx.cpool (const ctx (ConstClass jpath));
        write_ui16 ctx.cpool (const ctx (ConstNameAndType (unqualified_name, TMethod jmethod_signature)))
    (** interface method reference *)
    | ConstInterfaceMethod of (jpath, unqualified_name, jmethod_signature) (* tag = 11 *) ->
        write_byte ctx.cpool 11;
        write_ui16 ctx.cpool (const ctx (ConstClass jpath));
        write_ui16 ctx.cpool (const ctx (ConstNameAndType (unqualified_name, TMethod jmethod_signature)))
    (** constant values *)
    | ConstString s  (* tag = 8 *) ->
        write_byte ctx.cpool 8;
        write_ui16 ctx.cpool (const ctx (ConstUtf8 s))
    | ConstInt i (* tag = 3 *) ->
        write_byte ctx.cpool 3;
        write_real_i32 ctx.cpool i
    | ConstFloat f (* tag = 4 *) ->
        write_byte ctx.cpool 4;
        (match classify_float f with
        | FP_normal | FP_subnormal | FP_zero ->
            write_real_i32 ctx.cpool (Int32.bits_of_float f)
        | FP_infinity when f > 0 ->
            write_real_i32 ctx.cpool 0x7f800000l
        | FP_infinity when f < 0 ->
            write_real_i32 ctx.cpool 0xff800000l
        | FP_nan ->
            write_real_i32 ctx.cpool 0x7f800001l)
    | ConstLong i (* tag = 5 *) ->
        write_byte ctx.cpool 5;
        write_i64 ctx.cpool i;
        ctx.ccount <- ctx.ccount + 1
    | ConstDouble d (* tag = 6 *) ->
        write_byte ctx.cpool 6;
        write_double ctx.cpool d;
        ctx.ccount <- ctx.ccount + 1
    (** name and type: used to represent a field or method, without indicating which class it belongs to *)
    | ConstNameAndType (unqualified_name, jsignature) ->
        write_byte ctx.cpool 12;
        write_ui16 ctx.cpool (const ctx (ConstUtf8 (unqualified_name)));
        write_ui16 ctx.cpool (const ctx (ConstUtf8 (encode_sig ctx jsignature)))
    (** UTF8 encoded strings. Note that when reading/writing, take into account Utf8 modifications of java *)
    (* (http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.4.7) *)
    | ConstUtf8 s ->
        write_byte ctx.cpool 1;
        write_ui16 ctx.cpool (String.length s);
        write_string ctx.cpool (encode_utf8 s)
    (** invokeDynamic-specific *)
    | ConstMethodHandle (reference_type, jconstant) (* tag = 15 *) ->
        write_byte ctx.cpool 15;
        write_byte ctx.cpool (get_reference_type reference_type);
        write_ui16 ctx.cpool (const ctx jconstant)
    | ConstMethodType jmethod_signature (* tag = 16 *) ->
        write_byte ctx.cpool 16;
        write_ui16 ctx.cpool (const ctx (ConstUtf8 (encode_sig ctx (TMethod jmethod_signature))))
    | ConstInvokeDynamic (bootstrap_method, unqualified_name, jsignature) (* tag = 18 *) ->
        write_byte ctx.cpool 18;
        write_ui16 ctx.cpool bootstrap_method;
        write_ui16 ctx.cpool (const ctx (ConstNameAndType(unqualified_name, jsignature)))
    | ConstUnusable -> assert false);
    let ret = ctx.ccount in
    ctx.ccount <- ret + 1;
    ret
