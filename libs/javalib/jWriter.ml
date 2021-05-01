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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
 *)
open JData;;
open IO.BigEndian;;
open IO;;
open ExtString;;
open ExtList;;

exception Writer_error_message of string

type context = {
  cpool : unit IO.output;
  mutable ccount : int;
  ch : string IO.output;
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

let rec encode_param ctx ch param =
  match param with
  | TAny -> write_byte ch (Char.code '*')
  | TType(w, s) ->
    (match w with
    | WExtends -> write_byte ch (Char.code '+')
    | WSuper -> write_byte ch (Char.code '-')
    | WNone -> ());
    encode_sig_part ctx ch s

and encode_sig_part ctx ch jsig = match jsig with
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
    write_string ch (encode_path ctx path);
    if params <> [] then begin
      write_byte ch (Char.code '<');
      List.iter (encode_param ctx ch) params;
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
        List.iter (encode_param ctx ch) params;
        write_byte ch (Char.code '>')
      end;
    ) inners;
    write_byte ch (Char.code ';')
  | TArray(s,size) ->
    write_byte ch (Char.code '[');
    (match size with
    | Some size ->
      write_string ch (string_of_int size);
    | None -> ());
    encode_sig_part ctx ch s
  | TMethod(args, ret) ->
    write_byte ch (Char.code '(');
    List.iter (encode_sig_part ctx ch) args;
    (match ret with
      | None -> write_byte ch (Char.code 'V')
      | Some jsig -> encode_sig_part ctx ch jsig)
  | TTypeParameter name ->
    write_byte ch (Char.code 'T');
    write_string ch name;
    write_byte ch (Char.code ';')

let encode_sig ctx jsig =
  let buf = IO.output_string() in
  encode_sig_part ctx buf jsig;
  close_out buf

let write_utf8 ch s =
  String.iter (fun c ->
    let c = Char.code c in
    if c = 0 then begin
      write_byte ch 0xC0;
      write_byte ch 0x80
    end else
      write_byte ch c
  ) s

let rec const ctx c =
  try
    PMap.find c ctx.constants
  with
  | Not_found ->
    let ret = ctx.ccount in
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
    | ConstInterfaceMethod (jpath, unqualified_name, jmethod_signature) (* tag = 11 *) ->
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
        | FP_infinite when f > 0.0 ->
            write_real_i32 ctx.cpool 0x7f800000l
        | FP_infinite ->
            write_real_i32 ctx.cpool 0xff800000l
        | FP_nan ->
            write_real_i32 ctx.cpool 0x7f800001l)
    | ConstLong i (* tag = 5 *) ->
        write_byte ctx.cpool 5;
        write_i64 ctx.cpool i;
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
        write_utf8 ctx.cpool s
    (** invokeDynamic-specific *)
    | ConstMethodHandle (reference_type, jconstant) (* tag = 15 *) ->
        write_byte ctx.cpool 15;
        write_byte ctx.cpool (get_reference_type reference_type);
        write_ui16 ctx.cpool (const ctx jconstant)
    | ConstMethodType jmethod_signature (* tag = 16 *) ->
        write_byte ctx.cpool 16;
        write_ui16 ctx.cpool (const ctx (ConstUtf8 (encode_sig ctx (TMethod jmethod_signature))))
    | ConstDynamic (bootstrap_method, unqualified_name, jsignature) (* tag = 17 *) ->
        write_byte ctx.cpool 17;
        write_ui16 ctx.cpool bootstrap_method;
        write_ui16 ctx.cpool (const ctx (ConstNameAndType(unqualified_name, jsignature)))
    | ConstInvokeDynamic (bootstrap_method, unqualified_name, jsignature) (* tag = 18 *) ->
        write_byte ctx.cpool 18;
        write_ui16 ctx.cpool bootstrap_method;
        write_ui16 ctx.cpool (const ctx (ConstNameAndType(unqualified_name, jsignature)))
    | ConstModule unqualified_name (* tag = 19 *) ->
        write_byte ctx.cpool 19;
        write_ui16 ctx.cpool (const ctx (ConstUtf8 (unqualified_name)));
    | ConstPackage unqualified_name (* tag = 20 *) ->
        write_byte ctx.cpool 20;
        write_ui16 ctx.cpool (const ctx (ConstUtf8 (unqualified_name)));
    | ConstUnusable -> assert false);
    ctx.ccount <- ret + 1;
    ret

let write_const ctx ch cconst =
  write_ui16 ch (const ctx cconst)
;;

let write_formal_type_params ctx ch tparams =
  write_byte ch (Char.code '<');
  List.iter (fun (name,ext,impl) ->
    write_string ch name;
    (match ext with
    | None -> ()
    | Some jsig ->
      write_byte ch (Char.code ':');
      write_string ch (encode_sig ctx jsig));
    List.iter (fun jsig ->
      write_byte ch (Char.code ':');
      write_string ch (encode_sig ctx jsig)
    ) impl
  ) tparams;
  write_byte ch (Char.code '>');
;;

let write_complete_method_signature ctx ch (tparams : jtypes) msig throws =
  if tparams <> [] then write_formal_type_params ctx ch tparams;
  write_string ch (encode_sig ctx (TMethod(msig)));
  if throws <> [] then List.iter (fun jsig ->
    write_byte ch (Char.code '^');
    write_string ch (encode_sig ctx jsig)
  ) throws
;;

let write_access_flags ctx ch all_flags flags =
  let value = List.fold_left (fun acc flag ->
    try
      acc lor (Hashtbl.find all_flags flag)
    with Not_found ->
      error ("Not found flag: " ^ (string_of_int (Obj.magic flag)))
  ) 0 flags in
  write_ui16 ch value
;;

let rec write_ann_element ctx ch (name,eval) =
  write_const ctx ch (ConstUtf8 name);
  write_element_value ctx ch eval

and write_annotation ctx ch ann =
  write_const ctx ch (ConstUtf8 (encode_sig ctx ann.ann_type));
  write_ui16 ch (List.length ann.ann_elements);
  List.iter (write_ann_element ctx ch) ann.ann_elements

and write_element_value ctx ch value = match value with
  | ValConst(jsig, cconst) -> (match jsig with
    | TObject((["java";"lang"],"String"), []) ->
      write_byte ch (Char.code 's')
    | TByte | TChar | TDouble | TFloat | TInt | TLong | TShort | TBool ->
      write_string ch (encode_sig ctx jsig)
    | _ ->
      let s = encode_sig ctx jsig in
      error ("Invalid signature " ^ s ^ " for constant value"));
    write_ui16 ch (const ctx cconst)
  | ValEnum(jsig,name) ->
    write_byte ch (Char.code 'e');
    write_const ctx ch (ConstUtf8 (encode_sig ctx jsig));
    write_const ctx ch (ConstUtf8 name)
  | ValClass(jsig) ->
    write_byte ch (Char.code 'c');
    let esig = match jsig with
      | TObject(([],"Void"),[])
      | TObject((["java";"lang"],"Void"),[]) ->
        "V"
      | _ ->
        encode_sig ctx jsig
    in
    write_const ctx ch (ConstUtf8 (esig))
  | ValAnnotation ann ->
    write_byte ch (Char.code '@');
    write_annotation ctx ch ann
  | ValArray(lvals) ->
    write_byte ch (Char.code '[');
    write_ui16 ch (List.length lvals);
    List.iter (write_element_value ctx ch) lvals
;;

