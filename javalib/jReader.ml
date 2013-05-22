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

exception Error_message of string

let error msg = raise (Error_message msg)

let get_reference_type i constid =
  match i with
  | 1 -> RGetField
  | 2 -> RGetStatic
  | 3 -> RPutField
  | 4 -> RPutStatic
  | 5 -> RInvokeVirtual
  | 6 -> RInvokeStatic
  | 7 -> RInvokeSpecial
  | 8 -> RNewInvokeSpecial
  | 9 -> RInvokeInterface
  | _ -> error (string_of_int constid ^ ": Invalid reference type " ^ string_of_int i)

let parse_constant max idx ch =
  let cid = IO.read_byte ch in
  let error() = error (string_of_int idx ^ ": Invalid constant " ^ string_of_int cid) in
  let index() =
    let n = read_ui16 ch in
    if n = 0 || n >= max then error();
    n
  in
  match cid with
  | 7 ->
    KClass (index())
  | 9 ->
    let n1 = index() in
    let n2 = index() in
    KFieldRef (n1,n2)
  | 10 ->
    let n1 = index() in
    let n2 = index() in
    KMethodRef (n1,n2)
  | 11 ->
    let n1 = index() in
    let n2 = index() in
    KInterfaceMethodRef (n1,n2)
  | 8 ->
    KString (index())
  | 3 ->
    KInt (read_real_i32 ch)
  | 4 ->
    let f = Int32.float_of_bits (read_real_i32 ch) in
    KFloat f
  | 5 ->
    KLong (read_i64 ch)
  | 6 ->
    KDouble (read_double ch)
  | 12 ->
    let n1 = index() in
    let n2 = index() in
    KNameAndType (n1, n2)
  | 1 ->
    let len = read_ui16 ch in
    let str = IO.nread ch len in
    (* TODO: correctly decode modified UTF8 *)
    KUtf8String str
  | 15 ->
  	let reft = get_reference_type (read_ui16 ch) idx in
  	let dynref = index() in
  	KMethodHandle (reft, dynref)
  | 16 ->
  	KMethodType (index())
  | 18 ->
    let bootstrapref = read_ui16 ch in (* not index *)
    let nametyperef = index() in
    KInvokeDynamic (bootstrapref, nametyperef)
  | n ->
    error()

let expand_path s =
  let rec loop remaining acc =
    match remaining with
    | name :: [] -> List.rev acc, name
    | v :: tl -> loop tl (v :: acc)
    | _ -> assert false
  in
  loop (String.nsplit s "/") []

let rec parse_type_parameter_part s =
  match s.[0] with
  | '*' -> TAny, 1
  | c ->
    let wildcard, i = match c with
      | '+' -> WExtends, 1
      | '-' -> WSuper, 1
      | _ -> WNone, 0
    in
    let jsig, l = parse_signature_part (String.sub s i (String.length s - 1)) in
    (TType (wildcard, jsig), l + i)

and parse_signature_part s =
  let len = String.length s in
  if len = 0 then raise Exit;
  match s.[0] with
  | 'B' -> TByte, 1
  | 'C' -> TChar, 1
  | 'D' -> TDouble, 1
  | 'F' -> TFloat, 1
  | 'I' -> TInt, 1
  | 'J' -> TLong, 1
  | 'S' -> TShort, 1
  | 'Z' -> TBool, 1
  | 'L' ->
    (try
      let orig_s = s in
      let rec loop start i acc =
        match s.[i] with
        | '/' -> loop (i + 1) (i + 1) (String.sub s start (i - start) :: acc)
        | ';' | '.' -> List.rev acc, (String.sub s start (i - start)), [], (i)
        | '<' ->
          let name = String.sub s start (i - start) in
          let rec loop_params i acc =
            let s = String.sub s i (len - i) in
            match s.[0] with
            | '>' -> List.rev acc, i + 1
            | _ ->
              let tp, l = parse_type_parameter_part s in
              loop_params (l + i) (tp :: acc)
          in
          let params, _end = loop_params (i + 1) [] in
          List.rev acc, name, params, (_end)
        | _ -> loop start (i+1) acc
      in
      let pack, name, params, _end = loop 1 1 [] in
      let rec loop_inner i acc =
        match s.[i] with
        | '.' ->
          let pack, name, params, _end = loop (i+1) (i+1) [] in
          if pack <> [] then error ("Inner types must not define packages. For '" ^ orig_s ^ "'.");
          loop_inner _end ( (name,params) :: acc )
        | ';' -> List.rev acc, i + 1
        | c -> error ("End of complex type signature expected after type parameter. Got '" ^ Char.escaped c ^ "' for '" ^ orig_s ^ "'." );
      in
      let inners, _end = loop_inner _end [] in
      match inners with
      | [] -> TObject((pack,name), params), _end
      | _ -> TObjectInner( pack, (name,params) :: inners ), _end
    with
      Invalid_string -> raise Exit)
  | '[' ->
    let p = ref 1 in
    while !p < String.length s && s.[!p] >= '0' && s.[!p] <= '9' do
      incr p;
    done;
    let size = (if !p > 1 then Some (int_of_string (String.sub s 1 (!p - 1))) else None) in
    let s , l = parse_signature_part (String.sub s !p (String.length s - !p)) in
    TArray (s,size) , l + !p
  | '(' ->
    let p = ref 1 in
    let args = ref [] in
    while !p < String.length s && s.[!p] <> ')' do
      let a , l = parse_signature_part (String.sub s !p (String.length s - !p)) in
      args := a :: !args;
      p := !p + l;
    done;
    incr p;
    if !p >= String.length s then raise Exit;
    let ret , l = (match s.[!p] with 'V' -> None , 1 | _ ->
      let s, l = parse_signature_part (String.sub s !p (String.length s - !p)) in
      Some s, l
    ) in
    TMethod (List.rev !args,ret) , !p + l
  | 'T' ->
    (try
      let s1 , _ = String.split s ";" in
      let len = String.length s1 in
      TTypeParameter (String.sub s1 1 (len - 1)) , len + 1
    with
      Invalid_string -> raise Exit)
  | _ ->
    raise Exit

let parse_signature s =
  try
    let sign , l = parse_signature_part s in
    if String.length s <> l then raise Exit;
    sign
  with
    Exit -> error ("Invalid signature '" ^ s ^ "'")

let parse_method_signature s =
  match parse_signature s with
  | (TMethod m) -> m
  | _ -> error ("Unexpected signature '" ^ s ^ "'. Expecting method")

let parse_formal_type_params s =
  match s.[0] with
  | '<' ->
    let rec read_id i =
      match s.[i] with
      | ':' | '>' -> i
      | _ -> read_id (i + 1)
    in
    let len = String.length s in
    let rec parse_params idx acc =
      let idi = read_id (idx + 1) in
      let id = String.sub s (idx + 1) (idi - idx - 1) in
      (* next must be a : *)
      (match s.[idi] with | ':' -> () | _ -> error ("Invalid formal type signature character: " ^ Char.escaped s.[idi] ^ " ; from " ^ s));
      let ext, l = match s.[idi + 1] with
        | ':' | '>' -> None, idi + 1
        | _ ->
          let sgn, l = parse_signature_part (String.sub s (idi + 1) (len - idi - 1)) in
          Some sgn, l + idi + 1
      in
      let rec loop idx acc =
        match s.[idx] with
        | ':' ->
          let ifacesig, ifacei = parse_signature_part (String.sub s (idx + 1) (len - idx - 1)) in
          loop (idx + ifacei + 1) (ifacesig :: acc)
        | _ -> acc, idx
      in
      let ifaces, idx = loop l [] in
      let acc = (id, ext, ifaces) :: acc in
      if s.[idx] = '>' then List.rev acc, idx + 1 else parse_params (idx - 1) acc
    in
    parse_params 0 []
  | _ -> [], 0

let parse_throws s =
  let len = String.length s in
  let rec loop idx acc =
    if idx > len then raise Exit
    else if idx = len then acc, idx
    else match s.[idx] with
    | '^' ->
      let tsig, l = parse_signature_part (String.sub s (idx+1) (len - idx - 1)) in
      loop (idx + l + 1) (tsig :: acc)
    | _ -> acc, idx
  in
  loop 0 []

let parse_complete_method_signature s =
  try
    let len = String.length s in
    let tparams, i = parse_formal_type_params s in
    let sign, l = parse_signature_part (String.sub s i (len - i)) in
    let throws, l2 = parse_throws (String.sub s (i+l) (len - i - l)) in
    if (i + l + l2) <> len then raise Exit;

    match sign with
    | TMethod msig -> tparams, msig, throws
    | _ -> raise Exit
  with
    Exit -> error ("Invalid method extended signature '" ^ s ^ "'")


let rec expand_constant consts i =
  let unexpected i = error (string_of_int i ^ ": Unexpected constant type") in
  let expand_path n = match Array.get consts n with
    | KUtf8String s -> expand_path s
    | _ -> unexpected n
  in
  let expand_cls n = match expand_constant consts n with
    | ConstClass p -> p
    | _ -> unexpected n
  in
  let expand_nametype n = match expand_constant consts n with
    | ConstNameAndType (s,jsig) -> s, jsig
    | _ -> unexpected n
  in
  let expand_string n = match Array.get consts n with
    | KUtf8String s -> s
    | _ -> unexpected n
  in
  let expand_nametype_m n = match expand_nametype n with
    | (n, TMethod m) -> n, m
    | _ -> unexpected n
  in
  let expand ncls nt = match expand_cls ncls, expand_nametype nt with
    | path, (n, m) -> path, n, m
  in
  let expand_m ncls nt = match expand_cls ncls, expand_nametype_m nt with
    | path, (n, m) -> path, n, m
  in

  match Array.get consts i with
  | KClass utf8ref ->
    ConstClass (expand_path utf8ref)
  | KFieldRef (classref, nametyperef) ->
    ConstField (expand classref nametyperef)
  | KMethodRef (classref, nametyperef) ->
    ConstMethod (expand_m classref nametyperef)
  | KInterfaceMethodRef (classref, nametyperef) ->
    ConstInterfaceMethod (expand_m classref nametyperef)
  | KString utf8ref ->
    ConstString (expand_string utf8ref)
  | KInt i32 ->
    ConstInt i32
  | KFloat f ->
    ConstFloat f
  | KLong i64 ->
    ConstLong i64
  | KDouble d ->
    ConstDouble d
  | KNameAndType (n, t) ->
    ConstNameAndType(expand_string n, parse_signature (expand_string t))
  | KUtf8String s ->
    ConstUtf8 s (* TODO: expand UTF8 characters *)
  | KMethodHandle (reference_type, dynref) ->
    ConstMethodHandle (reference_type, expand_constant consts dynref)
  | KMethodType utf8ref ->
    ConstMethodType (parse_method_signature (expand_string utf8ref))
  | KInvokeDynamic (bootstrapref, nametyperef) ->
    let n, t = expand_nametype nametyperef in
    ConstInvokeDynamic(bootstrapref, n, t)
  | KUnusable ->
    ConstUnusable

let parse_access_flags ch all_flags =
  let fl = read_ui16 ch in
  let flags = ref [] in
  let fbit = ref 0 in
  List.iter (fun f ->
    if fl land (1 lsl !fbit) <> 0 then begin
      flags := f :: !flags;
      if f = JUnusable then error ("Unusable flag: " ^ string_of_int fl)
    end;
    incr fbit
  ) all_flags;
  (*if fl land (0x4000 - (1 lsl !fbit)) <> 0 then error ("Invalid access flags " ^ string_of_int fl);*)
  !flags

let get_constant c n =
  if n < 1 || n >= Array.length c then error ("Invalid constant index " ^ string_of_int n);
  match c.(n) with
  | ConstUnusable -> error "Unusable constant index";
  | x -> x

let get_class consts ch =
  match get_constant consts (read_ui16 ch) with
  | ConstClass n -> n
  | _ -> error "Invalid class index"

let get_string consts ch =
  let i = read_ui16 ch in
  match get_constant consts i with
  | ConstUtf8 s -> s
  | _ -> error ("Invalid string index " ^ string_of_int i)

let rec parse_element_value consts ch =
  let tag = IO.read_byte ch in
  match Char.chr tag with
  | 'B' | 'C' | 'D' | 'E' | 'F' | 'I' | 'J' | 'S' | 'Z' | 's' ->
    ValConst (get_constant consts (read_ui16 ch))
  | 'e' ->
    let path = parse_signature (get_string consts ch) in
    let name = get_string consts ch in
    ValEnum (path, name)
  | 'c' ->
    let name = get_string consts ch in
    let jsig = if name = "V" then
      TObject(([], "Void"), [])
    else
      parse_signature name
    in
    ValClass jsig
  | '@' ->
    ValAnnotation (parse_annotation consts ch)
  | '[' ->
    let num_vals = read_ui16 ch in
    ValArray (List.init (num_vals) (fun _ -> parse_element_value consts ch))
  | tag -> error ("Invalid element value: '" ^  Char.escaped tag ^ "'")

and parse_ann_element consts ch =
  let name = get_string consts ch in
  let element_value = parse_element_value consts ch in
  name, element_value

and parse_annotation consts ch =
  let anntype = parse_signature (get_string consts ch) in
  let count = read_ui16 ch in
  {
    ann_type = anntype;
    ann_elements = List.init count (fun _ -> parse_ann_element consts ch)
  }

let parse_attribute on_special consts ch =
  let aname = get_string consts ch in
  let error() = error ("Malformed attribute " ^ aname) in
  let alen = read_i32 ch in
  match aname with
  | "Deprecated" ->
    if alen <> 0 then error();
    Some (AttrDeprecated)
  | "RuntimeVisibleAnnotations" ->
    let anncount = read_ui16 ch in
    Some (AttrVisibleAnnotations (List.init anncount (fun _ -> parse_annotation consts ch)))
  | "RuntimeInvisibleAnnotations" ->
    let anncount = read_ui16 ch in
    Some (AttrInvisibleAnnotations (List.init anncount (fun _ -> parse_annotation consts ch)))
  | _ ->
    let do_default () =
      Some (AttrUnknown (aname,IO.nread ch alen))
    in
    match on_special with
    | None -> do_default()
    | Some fn -> fn consts ch aname alen do_default

let parse_attributes ?on_special consts ch count =
  let rec loop i acc =
    if i >= count then List.rev acc
    else match parse_attribute on_special consts ch with
    | None -> loop (i + 1) acc
    | Some attrib -> loop (i + 1) (attrib :: acc)
  in
  loop 0 []

let parse_field kind consts ch =
  let all_flags = match kind with
    | JKField ->
      [JPublic; JPrivate; JProtected; JStatic; JFinal; JUnusable; JVolatile; JTransient; JSynthetic; JEnum]
    | JKMethod ->
      [JPublic; JPrivate; JProtected; JStatic; JFinal; JSynchronized; JBridge; JVarArgs; JNative; JUnusable; JAbstract; JStrict; JSynthetic]
  in
  let acc = ref (parse_access_flags ch all_flags) in
  let name = get_string consts ch in
  let sign = parse_signature (get_string consts ch) in

  let jsig = ref sign in
  let throws = ref [] in
  let types = ref [] in
  let constant = ref None in
  let code = ref None in

  let attrib_count = read_ui16 ch in
  let attribs = parse_attributes ~on_special:(fun _ _ aname alen do_default ->
    match kind, aname with
    | JKField, "ConstantValue" ->
      constant := Some (get_constant consts (read_ui16 ch));
      None
    | JKField, "Synthetic" ->
      if not (List.mem JSynthetic !acc) then acc := !acc @ [JSynthetic];
      None
    | JKField, "Signature" ->
      let s = get_string consts ch in
      jsig := parse_signature s;
      None
    | JKMethod, "Code" -> (* TODO *)
      do_default()
    | JKMethod, "Exceptions" ->
      let num = read_ui16 ch in
      throws := List.init num (fun _ -> TObject(get_class consts ch,[]));
      None
    | JKMethod, "Signature" ->
      let s = get_string consts ch in
      let tp, sgn, thr = parse_complete_method_signature s in
      if thr <> [] then throws := thr;
      types := tp;
      jsig := TMethod(sgn);
      None
    | _ -> do_default()
  ) consts ch attrib_count in
  {
    jf_name = name;
    jf_kind = kind;
    (* signature, as used by the vm *)
    jf_vmsignature = sign;
    (* actual signature, as used in java code *)
    jf_signature = !jsig;
    jf_throws = !throws;
    jf_types = !types;
    jf_flags = !acc;
    jf_attributes = attribs;
    jf_constant = !constant;
    jf_code = !code;
  }

let parse_class ch =
  if read_real_i32 ch <> 0xCAFEBABEl then error "Invalid header";
  let minorv = read_ui16 ch in
  let majorv = read_ui16 ch in
  let constant_count = read_ui16 ch in
  let const_big = ref true in
  let consts = Array.init constant_count (fun idx ->
  	if !const_big then begin
  	  const_big := false;
  	  KUnusable
  	end else
  	  let c = parse_constant constant_count idx ch in
  	  (match c with KLong _ | KDouble _ -> const_big := true | _ -> ());
  	  c
  ) in
  let consts = Array.mapi (fun i _ -> expand_constant consts i) consts in
  let flags = parse_access_flags ch [JPublic; JUnusable; JUnusable; JUnusable; JFinal; JSuper; JUnusable; JUnusable; JUnusable; JInterface; JAbstract; JUnusable; JSynthetic; JAnnotation; JEnum] in
  let this = get_class consts ch in
  let super_idx = read_ui16 ch in
  let super = match super_idx with
  	| 0 -> TObject((["java";"lang"], "Object"), []);
  	| idx -> match get_constant consts idx with
  	  | ConstClass path -> TObject(path,[])
  	  | _ -> error "Invalid super index"
  in
  let interfaces = List.init (read_ui16 ch) (fun _ -> TObject (get_class consts ch, [])) in
  let fields = List.init (read_ui16 ch) (fun _ -> parse_field JKField consts ch) in
  let methods = List.init (read_ui16 ch) (fun _ -> parse_field JKMethod consts ch) in

  let inner = ref [] in
  let types = ref [] in
  let super = ref super in
  let interfaces = ref interfaces in

  let attribs = read_ui16 ch in
  let attribs = parse_attributes ~on_special:(fun _ _ aname alen do_default ->
    match aname with
    | "InnerClasses" ->
      let count = read_ui16 ch in
      let classes = List.init count (fun _ ->
        let inner_ci = get_class consts ch in
        let outeri = read_ui16 ch in
        let outer_ci = match outeri with
          | 0 -> None
          | _ -> match get_constant consts outeri with
          | ConstClass n -> Some n
          | _ -> error "Invalid class index"
        in

        let inner_namei = read_ui16 ch in
        let inner_name = match inner_namei with
          | 0 -> None
          | _ -> match get_constant consts inner_namei with
          | ConstUtf8 s -> Some s
          | _ -> error ("Invalid string index " ^ string_of_int inner_namei)
        in
        let flags = parse_access_flags ch [JPublic; JPrivate; JProtected; JStatic; JFinal; JUnusable; JUnusable; JUnusable; JUnusable; JInterface; JAbstract; JSynthetic; JAnnotation; JEnum] in
        inner_ci, outer_ci, inner_name, flags
      ) in
      inner := classes;
      None
    | "Signature" ->
      let s = get_string consts ch in
      let formal, idx = parse_formal_type_params s in
      types := formal;
      let s = String.sub s idx (String.length s - idx) in
      let len = String.length s in
      let sup, idx = parse_signature_part s in
      let rec loop idx acc =
        if idx = len then
          acc
        else begin
          let s = String.sub s idx (len - idx) in
          let iface, i2 = parse_signature_part s in
          loop (idx + i2) (iface :: acc)
        end
      in
      interfaces := loop idx [];
      super := sup;
      None
    | _ -> do_default()
  ) consts ch attribs in
  {
    cversion = majorv, minorv;
    cpath = this;
    csuper = !super;
    cflags = flags;
    cinterfaces = !interfaces;
    cfields = fields;
    cmethods = methods;
    cattributes = attribs;
    cinner_types = !inner;
    ctypes = !types;
  }

