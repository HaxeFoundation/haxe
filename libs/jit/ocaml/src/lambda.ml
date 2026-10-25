(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Misc
open Asttypes

type compile_time_constant =
  | Big_endian
  | Word_size
  | Int_size
  | Max_wosize
  | Ostype_unix
  | Ostype_win32
  | Ostype_cygwin
  | Backend_type

type immediate_or_pointer =
  | Immediate
  | Pointer

type initialization_or_assignment =
  | Assignment
  | Heap_initialization
  | Root_initialization

type is_safe =
  | Safe
  | Unsafe

type primitive =
  | Pbytes_to_string
  | Pbytes_of_string
  | Pignore
    (* Globals *)
  | Pgetglobal of Ident.t
  | Psetglobal of Ident.t
  (* Operations on heap blocks *)
  | Pmakeblock of int * mutable_flag * block_shape
  | Pfield of int * immediate_or_pointer * mutable_flag
  | Pfield_computed
  | Psetfield of int * immediate_or_pointer * initialization_or_assignment
  | Psetfield_computed of immediate_or_pointer * initialization_or_assignment
  | Pfloatfield of int
  | Psetfloatfield of int * initialization_or_assignment
  | Pduprecord of Types.record_representation * int
  (* Context switches *)
  | Prunstack
  | Pperform
  | Presume
  | Preperform
  (* External call *)
  | Pccall of Primitive.description
  (* Exceptions *)
  | Praise of raise_kind
  (* Boolean operations *)
  | Psequand | Psequor | Pnot
  (* Integer operations *)
  | Pnegint | Paddint | Psubint | Pmulint
  | Pdivint of is_safe | Pmodint of is_safe
  | Pandint | Porint | Pxorint
  | Plslint | Plsrint | Pasrint
  | Pintcomp of integer_comparison
  | Pcompare_ints | Pcompare_floats | Pcompare_bints of boxed_integer
  | Poffsetint of int
  | Poffsetref of int
  (* Float operations *)
  | Pintoffloat | Pfloatofint
  | Pnegfloat | Pabsfloat
  | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat
  | Pfloatcomp of float_comparison
  (* String operations *)
  | Pstringlength | Pstringrefu  | Pstringrefs
  | Pbyteslength | Pbytesrefu | Pbytessetu | Pbytesrefs | Pbytessets
  (* Array operations *)
  | Pmakearray of array_kind * mutable_flag
  | Pduparray of array_kind * mutable_flag
  | Parraylength of array_kind
  | Parrayrefu of array_kind
  | Parraysetu of array_kind
  | Parrayrefs of array_kind
  | Parraysets of array_kind
  (* Test if the argument is a block or an immediate integer *)
  | Pisint
  (* Test if the (integer) argument is outside an interval *)
  | Pisout
  (* Operations on boxed integers (Nativeint.t, Int32.t, Int64.t) *)
  | Pbintofint of boxed_integer
  | Pintofbint of boxed_integer
  | Pcvtbint of boxed_integer (*source*) * boxed_integer (*destination*)
  | Pnegbint of boxed_integer
  | Paddbint of boxed_integer
  | Psubbint of boxed_integer
  | Pmulbint of boxed_integer
  | Pdivbint of { size : boxed_integer; is_safe : is_safe }
  | Pmodbint of { size : boxed_integer; is_safe : is_safe }
  | Pandbint of boxed_integer
  | Porbint of boxed_integer
  | Pxorbint of boxed_integer
  | Plslbint of boxed_integer
  | Plsrbint of boxed_integer
  | Pasrbint of boxed_integer
  | Pbintcomp of boxed_integer * integer_comparison
  (* Operations on Bigarrays: (unsafe, #dimensions, kind, layout) *)
  | Pbigarrayref of bool * int * bigarray_kind * bigarray_layout
  | Pbigarrayset of bool * int * bigarray_kind * bigarray_layout
  (* size of the nth dimension of a Bigarray *)
  | Pbigarraydim of int
  (* load/set 16,32,64 bits from a string: (unsafe)*)
  | Pstring_load_16 of bool
  | Pstring_load_32 of bool
  | Pstring_load_64 of bool
  | Pbytes_load_16 of bool
  | Pbytes_load_32 of bool
  | Pbytes_load_64 of bool
  | Pbytes_set_16 of bool
  | Pbytes_set_32 of bool
  | Pbytes_set_64 of bool
  (* load/set 16,32,64 bits from a
     (char, int8_unsigned_elt, c_layout) Bigarray.Array1.t : (unsafe) *)
  | Pbigstring_load_16 of bool
  | Pbigstring_load_32 of bool
  | Pbigstring_load_64 of bool
  | Pbigstring_set_16 of bool
  | Pbigstring_set_32 of bool
  | Pbigstring_set_64 of bool
  (* Compile time constants *)
  | Pctconst of compile_time_constant
  (* byte swap *)
  | Pbswap16
  | Pbbswap of boxed_integer
  (* Integer to external pointer *)
  | Pint_as_pointer
  (* Atomic operations *)
  | Patomic_load of {immediate_or_pointer : immediate_or_pointer}
  | Patomic_exchange
  | Patomic_cas
  | Patomic_fetch_add
  (* Inhibition of optimisation *)
  | Popaque
  (* Fetching domain-local state *)
  | Pdls_get
  (* Poll for runtime actions *)
  | Ppoll

and integer_comparison =
    Ceq | Cne | Clt | Cgt | Cle | Cge

and float_comparison =
    CFeq | CFneq | CFlt | CFnlt | CFgt | CFngt | CFle | CFnle | CFge | CFnge

and value_kind =
    Pgenval | Pfloatval | Pboxedintval of boxed_integer | Pintval

and block_shape =
  value_kind list option

and array_kind =
    Pgenarray | Paddrarray | Pintarray | Pfloatarray

and boxed_integer = Primitive.boxed_integer =
    Pnativeint | Pint32 | Pint64

and bigarray_kind =
    Pbigarray_unknown
  | Pbigarray_float16 | Pbigarray_float32 | Pbigarray_float64
  | Pbigarray_sint8 | Pbigarray_uint8
  | Pbigarray_sint16 | Pbigarray_uint16
  | Pbigarray_int32 | Pbigarray_int64
  | Pbigarray_caml_int | Pbigarray_native_int
  | Pbigarray_complex32 | Pbigarray_complex64

and bigarray_layout =
    Pbigarray_unknown_layout
  | Pbigarray_c_layout
  | Pbigarray_fortran_layout

and raise_kind =
  | Raise_regular
  | Raise_reraise
  | Raise_notrace

let equal_boxed_integer = Primitive.equal_boxed_integer

let equal_primitive =
  (* Should be implemented like [equal_value_kind] of [equal_boxed_integer],
     i.e. by matching over the various constructors but the type has more
     than 100 constructors... *)
  (=)

let equal_value_kind x y =
  match x, y with
  | Pgenval, Pgenval -> true
  | Pfloatval, Pfloatval -> true
  | Pboxedintval bi1, Pboxedintval bi2 -> equal_boxed_integer bi1 bi2
  | Pintval, Pintval -> true
  | (Pgenval | Pfloatval | Pboxedintval _ | Pintval), _ -> false


type structured_constant =
    Const_base of constant
  | Const_block of int * structured_constant list
  | Const_float_array of string list
  | Const_immstring of string

type tailcall_attribute =
  | Tailcall_expectation of bool
    (* [@tailcall] and [@tailcall true] have [true],
       [@tailcall false] has [false] *)
  | Default_tailcall (* no [@tailcall] attribute *)

type inline_attribute =
  | Always_inline (* [@inline] or [@inline always] *)
  | Never_inline (* [@inline never] *)
  | Hint_inline (* [@inlined hint] attribute *)
  | Unroll of int (* [@unroll x] *)
  | Default_inline (* no [@inline] attribute *)

let equal_inline_attribute x y =
  match x, y with
  | Always_inline, Always_inline
  | Never_inline, Never_inline
  | Hint_inline, Hint_inline
  | Default_inline, Default_inline
    ->
    true
  | Unroll u, Unroll v ->
    u = v
  | (Always_inline | Never_inline
    | Hint_inline | Unroll _ | Default_inline), _ ->
    false

type specialise_attribute =
  | Always_specialise (* [@specialise] or [@specialise always] *)
  | Never_specialise (* [@specialise never] *)
  | Default_specialise (* no [@specialise] attribute *)

let equal_specialise_attribute x y =
  match x, y with
  | Always_specialise, Always_specialise
  | Never_specialise, Never_specialise
  | Default_specialise, Default_specialise ->
    true
  | (Always_specialise | Never_specialise | Default_specialise), _ ->
    false

type local_attribute =
  | Always_local (* [@local] or [@local always] *)
  | Never_local (* [@local never] *)
  | Default_local (* [@local maybe] or no [@local] attribute *)

type poll_attribute =
  | Error_poll (* [@poll error] *)
  | Default_poll (* no [@poll] attribute *)

type function_kind = Curried | Tupled

type let_kind = Strict | Alias | StrictOpt

type meth_kind = Self | Public | Cached

let equal_meth_kind x y =
  match x, y with
  | Self, Self -> true
  | Public, Public -> true
  | Cached, Cached -> true
  | (Self | Public | Cached), _ -> false

type shared_code = (int * int) list

type function_attribute = {
  inline : inline_attribute;
  specialise : specialise_attribute;
  local: local_attribute;
  poll: poll_attribute;
  is_a_functor: bool;
  stub: bool;
  tmc_candidate: bool;
  may_fuse_arity: bool;
}

type scoped_location = Debuginfo.Scoped_location.t

type lambda =
    Lvar of Ident.t
  | Lmutvar of Ident.t
  | Lconst of structured_constant
  | Lapply of lambda_apply
  | Lfunction of lfunction
  | Llet of let_kind * value_kind * Ident.t * lambda * lambda
  | Lmutlet of value_kind * Ident.t * lambda * lambda
  | Lletrec of rec_binding list * lambda
  | Lprim of primitive * lambda list * scoped_location
  | Lswitch of lambda * lambda_switch * scoped_location
  | Lstringswitch of
      lambda * (string * lambda) list * lambda option * scoped_location
  | Lstaticraise of int * lambda list
  | Lstaticcatch of lambda * (int * (Ident.t * value_kind) list) * lambda
  | Ltrywith of lambda * Ident.t * lambda
  | Lifthenelse of lambda * lambda * lambda
  | Lsequence of lambda * lambda
  | Lwhile of lambda * lambda
  | Lfor of Ident.t * lambda * lambda * direction_flag * lambda
  | Lassign of Ident.t * lambda
  | Lsend of meth_kind * lambda * lambda * lambda list * scoped_location
  | Levent of lambda * lambda_event
  | Lifused of Ident.t * lambda

and rec_binding = {
  id : Ident.t;
  def : lfunction;
}

and lfunction =
  { kind: function_kind;
    params: (Ident.t * value_kind) list;
    return: value_kind;
    body: lambda;
    attr: function_attribute; (* specified with [@inline] attribute *)
    loc: scoped_location; }

and lambda_apply =
  { ap_func : lambda;
    ap_args : lambda list;
    ap_loc : scoped_location;
    ap_tailcall : tailcall_attribute;
    ap_inlined : inline_attribute;
    ap_specialised : specialise_attribute; }

and lambda_switch =
  { sw_numconsts: int;
    sw_consts: (int * lambda) list;
    sw_numblocks: int;
    sw_blocks: (int * lambda) list;
    sw_failaction : lambda option}

and lambda_event =
  { lev_loc: scoped_location;
    lev_kind: lambda_event_kind;
    lev_repr: int ref option;
    lev_env: Env.t }

and lambda_event_kind =
    Lev_before
  | Lev_after of Types.type_expr
  | Lev_function
  | Lev_pseudo

type program =
  { module_ident : Ident.t;
    main_module_block_size : int;
    required_globals : Ident.Set.t;
    code : lambda }

let const_int n = Const_base (Const_int n)

let const_unit = const_int 0

let dummy_constant = Lconst (const_int (0xBBBB / 2))

let max_arity () =
  if !Clflags.native_code then 126 else max_int
  (* 126 = 127 (the maximal number of parameters supported in C--)
           - 1 (the hidden parameter containing the environment) *)

let lfunction' ~kind ~params ~return ~body ~attr ~loc =
  assert (List.length params <= max_arity ());
  { kind; params; return; body; attr; loc }

let lfunction ~kind ~params ~return ~body ~attr ~loc =
  Lfunction (lfunction' ~kind ~params ~return ~body ~attr ~loc)

let lambda_unit = Lconst const_unit

let default_function_attribute = {
  inline = Default_inline;
  specialise = Default_specialise;
  local = Default_local;
  poll = Default_poll;
  is_a_functor = false;
  stub = false;
  tmc_candidate = false;
  (* Plain functions ([fun] and [function]) set [may_fuse_arity] to [false] so
     that runtime arity matches syntactic arity in more situations.

     Many things compile to functions without having a notion of syntactic arity
     that survives typechecking, e.g. functors. Multi-arg functors are compiled
     as nested unary functions, and rely on the arity fusion in simplif to make
     them multi-argument. So, we keep arity fusion turned on by default for now.
  *)
  may_fuse_arity = true;
}

let default_stub_attribute =
  { default_function_attribute with stub = true }

(* Build sharing keys *)
(*
   Those keys are later compared with Stdlib.compare.
   For that reason, they should not include cycles.
*)

let max_raw = 32

let make_key e =
  let exception Not_simple in
  let count = ref 0   (* Used for controlling size *)
  and make_key = Ident.make_key_generator () in
  (* make_key is used for normalizing let-bound variables *)
  let rec tr_rec env e =
    incr count ;
    if !count > max_raw then raise Not_simple ; (* Too big ! *)
    match e with
    | Lvar id
    | Lmutvar id ->
      begin
        try Ident.find_same id env
        with Not_found -> e
      end
    | Lconst  (Const_base (Const_string _)) ->
        (* Mutable constants are not shared *)
        raise Not_simple
    | Lconst _ -> e
    | Lapply ap ->
        Lapply {ap with ap_func = tr_rec env ap.ap_func;
                        ap_args = tr_recs env ap.ap_args;
                        ap_loc = Loc_unknown}
    | Llet (Alias,_k,x,ex,e) -> (* Ignore aliases -> substitute *)
        let ex = tr_rec env ex in
        tr_rec (Ident.add x ex env) e
    | Llet ((Strict | StrictOpt),_k,x,ex,Lvar v) when Ident.same v x ->
        tr_rec env ex
    | Llet (str,k,x,ex,e) ->
     (* Because of side effects, keep other lets with normalized names *)
        let ex = tr_rec env ex in
        let y = make_key x in
        Llet (str,k,y,ex,tr_rec (Ident.add x (Lvar y) env) e)
    | Lmutlet (k,x,ex,e) ->
        let ex = tr_rec env ex in
        let y = make_key x in
        Lmutlet (k,y,ex,tr_rec (Ident.add x (Lmutvar y) env) e)
    | Lprim (p,es,_) ->
        Lprim (p,tr_recs env es, Loc_unknown)
    | Lswitch (e,sw,loc) ->
        Lswitch (tr_rec env e,tr_sw env sw,loc)
    | Lstringswitch (e,sw,d,_) ->
        Lstringswitch
          (tr_rec env e,
           List.map (fun (s,e) -> s,tr_rec env e) sw,
           tr_opt env d,
          Loc_unknown)
    | Lstaticraise (i,es) ->
        Lstaticraise (i,tr_recs env es)
    | Lstaticcatch (e1,xs,e2) ->
        Lstaticcatch (tr_rec env e1,xs,tr_rec env e2)
    | Ltrywith (e1,x,e2) ->
        Ltrywith (tr_rec env e1,x,tr_rec env e2)
    | Lifthenelse (cond,ifso,ifnot) ->
        Lifthenelse (tr_rec env cond,tr_rec env ifso,tr_rec env ifnot)
    | Lsequence (e1,e2) ->
        Lsequence (tr_rec env e1,tr_rec env e2)
    | Lassign (x,e) ->
        Lassign (x,tr_rec env e)
    | Lsend (m,e1,e2,es,_loc) ->
        Lsend (m,tr_rec env e1,tr_rec env e2,tr_recs env es,Loc_unknown)
    | Lifused (id,e) -> Lifused (id,tr_rec env e)
    | Lletrec _|Lfunction _
    | Lfor _ | Lwhile _
(* Beware: (PR#6412) the event argument to Levent
   may include cyclic structure of type Type.typexpr *)
    | Levent _  ->
        raise Not_simple

  and tr_recs env es = List.map (tr_rec env) es

  and tr_sw env sw =
    { sw with
      sw_consts = List.map (fun (i,e) -> i,tr_rec env e) sw.sw_consts ;
      sw_blocks = List.map (fun (i,e) -> i,tr_rec env e) sw.sw_blocks ;
      sw_failaction = tr_opt env sw.sw_failaction ; }

  and tr_opt env = function
    | None -> None
    | Some e -> Some (tr_rec env e) in

  try
    Some (tr_rec Ident.empty e)
  with Not_simple -> None

(***************)

let name_lambda strict arg fn =
  match arg with
    Lvar id -> fn id
  | _ ->
      let id = Ident.create_local "let" in
      Llet(strict, Pgenval, id, arg, fn id)

let name_lambda_list args fn =
  let rec name_list names = function
    [] -> fn (List.rev names)
  | (Lvar _ as arg) :: rem ->
      name_list (arg :: names) rem
  | arg :: rem ->
      let id = Ident.create_local "let" in
      Llet(Strict, Pgenval, id, arg, name_list (Lvar id :: names) rem) in
  name_list [] args


let iter_opt f = function
  | None -> ()
  | Some e -> f e

let shallow_iter ~tail ~non_tail:f = function
    Lvar _
  | Lmutvar _
  | Lconst _ -> ()
  | Lapply{ap_func = fn; ap_args = args} ->
      f fn; List.iter f args
  | Lfunction{body} ->
      f body
  | Llet(_, _k, _id, arg, body)
  | Lmutlet(_k, _id, arg, body) ->
      f arg; tail body
  | Lletrec(decl, body) ->
      tail body;
      List.iter (fun { def } -> f (Lfunction def)) decl
  | Lprim (Psequand, [l1; l2], _)
  | Lprim (Psequor, [l1; l2], _) ->
      f l1;
      tail l2
  | Lprim(_p, args, _loc) ->
      List.iter f args
  | Lswitch(arg, sw,_) ->
      f arg;
      List.iter (fun (_key, case) -> tail case) sw.sw_consts;
      List.iter (fun (_key, case) -> tail case) sw.sw_blocks;
      iter_opt tail sw.sw_failaction
  | Lstringswitch (arg,cases,default,_) ->
      f arg ;
      List.iter (fun (_,act) -> tail act) cases ;
      iter_opt tail default
  | Lstaticraise (_,args) ->
      List.iter f args
  | Lstaticcatch(e1, _, e2) ->
      tail e1; tail e2
  | Ltrywith(e1, _, e2) ->
      f e1; tail e2
  | Lifthenelse(e1, e2, e3) ->
      f e1; tail e2; tail e3
  | Lsequence(e1, e2) ->
      f e1; tail e2
  | Lwhile(e1, e2) ->
      f e1; f e2
  | Lfor(_v, e1, e2, _dir, e3) ->
      f e1; f e2; f e3
  | Lassign(_, e) ->
      f e
  | Lsend (_k, met, obj, args, _) ->
      List.iter f (met::obj::args)
  | Levent (e, _evt) ->
      tail e
  | Lifused (_v, e) ->
      tail e

let iter_head_constructor f l =
  shallow_iter ~tail:f ~non_tail:f l

let is_evaluated = function
  | Lconst _ | Lvar _ | Lfunction _ -> true
  | _ -> false

let rec free_variables = function
  | Lvar id
  | Lmutvar id -> Ident.Set.singleton id
  | Lconst _ -> Ident.Set.empty
  | Lapply{ap_func = fn; ap_args = args} ->
      free_variables_list (free_variables fn) args
  | Lfunction{body; params} ->
      Ident.Set.diff (free_variables body)
        (Ident.Set.of_list (List.map fst params))
  | Llet(_, _k, id, arg, body)
  | Lmutlet(_k, id, arg, body) ->
      Ident.Set.union
        (free_variables arg)
        (Ident.Set.remove id (free_variables body))
  | Lletrec(decl, body) ->
      let set =
        free_variables_list (free_variables body)
          (List.map (fun { def } -> Lfunction def) decl)
      in
      Ident.Set.diff set
        (Ident.Set.of_list (List.map (fun { id } -> id) decl))
  | Lprim(_p, args, _loc) ->
      free_variables_list Ident.Set.empty args
  | Lswitch(arg, sw,_) ->
      let set =
        free_variables_list
          (free_variables_list (free_variables arg)
             (List.map snd sw.sw_consts))
          (List.map snd sw.sw_blocks)
      in
      begin match sw.sw_failaction with
      | None -> set
      | Some failaction -> Ident.Set.union set (free_variables failaction)
      end
  | Lstringswitch (arg,cases,default,_) ->
      let set =
        free_variables_list (free_variables arg)
          (List.map snd cases)
      in
      begin match default with
      | None -> set
      | Some default -> Ident.Set.union set (free_variables default)
      end
  | Lstaticraise (_,args) ->
      free_variables_list Ident.Set.empty args
  | Lstaticcatch(body, (_, params), handler) ->
      Ident.Set.union
        (Ident.Set.diff
           (free_variables handler)
           (Ident.Set.of_list (List.map fst params)))
        (free_variables body)
  | Ltrywith(body, param, handler) ->
      Ident.Set.union
        (Ident.Set.remove
           param
           (free_variables handler))
        (free_variables body)
  | Lifthenelse(e1, e2, e3) ->
      Ident.Set.union
        (Ident.Set.union (free_variables e1) (free_variables e2))
        (free_variables e3)
  | Lsequence(e1, e2) ->
      Ident.Set.union (free_variables e1) (free_variables e2)
  | Lwhile(e1, e2) ->
      Ident.Set.union (free_variables e1) (free_variables e2)
  | Lfor(v, lo, hi, _dir, body) ->
      let set = Ident.Set.union (free_variables lo) (free_variables hi) in
      Ident.Set.union set (Ident.Set.remove v (free_variables body))
  | Lassign(id, e) ->
      Ident.Set.add id (free_variables e)
  | Lsend (_k, met, obj, args, _) ->
      free_variables_list
        (Ident.Set.union (free_variables met) (free_variables obj))
        args
  | Levent (lam, _evt) ->
      free_variables lam
  | Lifused (_v, e) ->
      (* Shouldn't v be considered a free variable ? *)
      free_variables e

and free_variables_list set exprs =
  List.fold_left (fun set expr -> Ident.Set.union (free_variables expr) set)
    set exprs

(* Check if an action has a "when" guard *)
let raise_count = ref 0

let next_raise_count () =
  incr raise_count ;
  !raise_count

(* Anticipated staticraise, for guards *)
let staticfail = Lstaticraise (0,[])

let rec is_guarded = function
  | Lifthenelse(_cond, _body, Lstaticraise (0,[])) -> true
  | Llet(_str, _k, _id, _lam, body) -> is_guarded body
  | Levent(lam, _ev) -> is_guarded lam
  | _ -> false

let rec patch_guarded patch = function
  | Lifthenelse (cond, body, Lstaticraise (0,[])) ->
      Lifthenelse (cond, body, patch)
  | Llet(str, k, id, lam, body) ->
      Llet (str, k, id, lam, patch_guarded patch body)
  | Levent(lam, ev) ->
      Levent (patch_guarded patch lam, ev)
  | _ -> fatal_error "Lambda.patch_guarded"

(* Translate an access path *)

let rec transl_address loc = function
  | Env.Aident id ->
      if Ident.global id
      then Lprim(Pgetglobal id, [], loc)
      else Lvar id
  | Env.Adot(addr, pos) ->
      Lprim(Pfield(pos, Pointer, Immutable),
                   [transl_address loc addr], loc)

let transl_path find loc env path =
  match find path env with
  | exception Not_found ->
      fatal_error ("Cannot find address for: " ^ (Path.name path))
  | addr -> transl_address loc addr

(* Translation of identifiers *)

let transl_module_path loc env path =
  transl_path Env.find_module_address loc env path

let transl_value_path loc env path =
  transl_path Env.find_value_address loc env path

let transl_extension_path loc env path =
  transl_path Env.find_constructor_address loc env path

let transl_class_path loc env path =
  transl_path Env.find_class_address loc env path

let transl_prim mod_name name =
  let pers = Ident.create_persistent mod_name in
  let env = Env.add_persistent_structure pers Env.empty in
  let lid = Longident.Ldot (Longident.Lident mod_name, name) in
  match Env.find_value_by_name lid env with
  | path, _ -> transl_value_path Loc_unknown env path
  | exception Not_found ->
      fatal_error ("Primitive " ^ name ^ " not found.")

(* Compile a sequence of expressions *)

let rec make_sequence fn = function
    [] -> lambda_unit
  | [x] -> fn x
  | x::rem ->
      let lam = fn x in Lsequence(lam, make_sequence fn rem)

(* Apply a substitution to a lambda-term.
   Assumes that the image of the substitution is out of reach
   of the bound variables of the lambda-term (no capture). *)

type substitution_functions = {
  subst_lambda : lambda -> lambda;
  subst_lfunction : lfunction -> lfunction;
}

let build_substs update_env ?(freshen_bound_variables = false) s =
  (* [s] contains a partial substitution for the free variables of the
     input term.

     During our traversal of the term we maintain a second environment
     [l] with all the bound variables of the input term in the current
     scope, mapped to either themselves or freshened versions of
     themselves when [freshen_bound_variables] is set. *)
  let bind id l =
    let id' = if not freshen_bound_variables then id else Ident.rename id in
    id', Ident.Map.add id id' l
  in
  let bind_many ids l =
    List.fold_right (fun (id, rhs) (ids', l) ->
        let id', l = bind id l in
        ((id', rhs) :: ids' , l)
      ) ids ([], l)
  in
  let bind_rec ids l =
    List.fold_right (fun rb (ids', l) ->
        let id', l = bind rb.id l in
        ({ rb with id = id' } :: ids' , l)
      ) ids ([], l)
  in
  let rec subst s l lam =
    match lam with
    | Lvar id as lam ->
        begin match Ident.Map.find id l with
          | id' -> Lvar id'
          | exception Not_found ->
             (* note: as this point we know [id] is not a bound
                variable of the input term, otherwise it would belong
                to [l]; it is a free variable of the input term. *)
             begin try Ident.Map.find id s with Not_found -> lam end
        end
    | Lmutvar id as lam ->
       begin match Ident.Map.find id l with
          | id' -> Lmutvar id'
          | exception Not_found ->
             (* Note: a mutable [id] should not appear in [s].
                Keeping the behavior of Lvar case for now. *)
             begin try Ident.Map.find id s with Not_found -> lam end
        end
    | Lconst _ as l -> l
    | Lapply ap ->
        Lapply{ap with ap_func = subst s l ap.ap_func;
                      ap_args = subst_list s l ap.ap_args}
    | Lfunction lf ->
        Lfunction (subst_lfun s l lf)
    | Llet(str, k, id, arg, body) ->
        let id, l' = bind id l in
        Llet(str, k, id, subst s l arg, subst s l' body)
    | Lmutlet(k, id, arg, body) ->
        let id, l' = bind id l in
        Lmutlet(k, id, subst s l arg, subst s l' body)
    | Lletrec(decl, body) ->
        let decl, l' = bind_rec decl l in
        Lletrec(List.map (subst_decl s l') decl, subst s l' body)
    | Lprim(p, args, loc) -> Lprim(p, subst_list s l args, loc)
    | Lswitch(arg, sw, loc) ->
        Lswitch(subst s l arg,
                {sw with sw_consts = List.map (subst_case s l) sw.sw_consts;
                        sw_blocks = List.map (subst_case s l) sw.sw_blocks;
                        sw_failaction = subst_opt s l sw.sw_failaction; },
                loc)
    | Lstringswitch (arg,cases,default,loc) ->
        Lstringswitch
          (subst s l arg,
           List.map (subst_strcase s l) cases,
           subst_opt s l default,
           loc)
    | Lstaticraise (i,args) ->  Lstaticraise (i, subst_list s l args)
    | Lstaticcatch(body, (id, params), handler) ->
        let params, l' = bind_many params l in
        Lstaticcatch(subst s l body, (id, params),
                     subst s l' handler)
    | Ltrywith(body, exn, handler) ->
        let exn, l' = bind exn l in
        Ltrywith(subst s l body, exn, subst s l' handler)
    | Lifthenelse(e1, e2, e3) ->
        Lifthenelse(subst s l e1, subst s l e2, subst s l e3)
    | Lsequence(e1, e2) -> Lsequence(subst s l e1, subst s l e2)
    | Lwhile(e1, e2) -> Lwhile(subst s l e1, subst s l e2)
    | Lfor(v, lo, hi, dir, body) ->
        let v, l' = bind v l in
        Lfor(v, subst s l lo, subst s l hi, dir, subst s l' body)
    | Lassign(id, e) ->
        assert (not (Ident.Map.mem id s));
        let id = try Ident.Map.find id l with Not_found -> id in
        Lassign(id, subst s l e)
    | Lsend (k, met, obj, args, loc) ->
        Lsend (k, subst s l met, subst s l obj, subst_list s l args, loc)
    | Levent (lam, evt) ->
        let old_env = evt.lev_env in
        let env_updates =
          let find_in_old id = Env.find_value (Path.Pident id) old_env in
          let rebind id id' new_env =
            match find_in_old id with
            | exception Not_found -> new_env
            | vd -> Env.add_value id' vd new_env
          in
          let update_free id new_env =
            match find_in_old id with
            | exception Not_found -> new_env
            | vd -> update_env id vd new_env
          in
          Ident.Map.merge (fun id bound free ->
            match bound, free with
            | Some id', _ ->
                if Ident.equal id id' then None else Some (rebind id id')
            | None, Some _ -> Some (update_free id)
            | None, None -> None
          ) l s
        in
        let new_env =
          Ident.Map.fold (fun _id update env -> update env) env_updates old_env
        in
        Levent (subst s l lam, { evt with lev_env = new_env })
    | Lifused (id, e) ->
        let id = try Ident.Map.find id l with Not_found -> id in
        Lifused (id, subst s l e)
  and subst_list s l li = List.map (subst s l) li
  and subst_decl s l decl = { decl with def = subst_lfun s l decl.def }
  and subst_lfun s l lf =
    let params, l' = bind_many lf.params l in
    { lf with params; body = subst s l' lf.body }
  and subst_case s l (key, case) = (key, subst s l case)
  and subst_strcase s l (key, case) = (key, subst s l case)
  and subst_opt s l = function
    | None -> None
    | Some e -> Some (subst s l e)
  in
  { subst_lambda = (fun lam -> subst s Ident.Map.empty lam);
    subst_lfunction = (fun lfun -> subst_lfun s Ident.Map.empty lfun);
  }

let subst update_env ?freshen_bound_variables s =
  (build_substs update_env ?freshen_bound_variables s).subst_lambda

let rename idmap lam =
  let update_env oldid vd env =
    let newid = Ident.Map.find oldid idmap in
    Env.add_value newid vd env
  in
  let s = Ident.Map.map (fun new_id -> Lvar new_id) idmap in
  subst update_env s lam

let duplicate_function =
  (build_substs
     (fun _ _ env -> env)
     ~freshen_bound_variables:true
     Ident.Map.empty).subst_lfunction

let map_lfunction f { kind; params; return; body; attr; loc } =
  let body = f body in
  { kind; params; return; body; attr; loc }

let shallow_map f = function
  | Lvar _
  | Lmutvar _
  | Lconst _ as lam -> lam
  | Lapply { ap_func; ap_args; ap_loc; ap_tailcall;
             ap_inlined; ap_specialised } ->
      Lapply {
        ap_func = f ap_func;
        ap_args = List.map f ap_args;
        ap_loc;
        ap_tailcall;
        ap_inlined;
        ap_specialised;
      }
  | Lfunction lfun ->
      Lfunction (map_lfunction f lfun)
  | Llet (str, k, v, e1, e2) ->
      Llet (str, k, v, f e1, f e2)
  | Lmutlet (k, v, e1, e2) ->
      Lmutlet (k, v, f e1, f e2)
  | Lletrec (idel, e2) ->
      Lletrec
        (List.map (fun rb ->
             { rb with def = map_lfunction f rb.def })
            idel,
         f e2)
  | Lprim (p, el, loc) ->
      Lprim (p, List.map f el, loc)
  | Lswitch (e, sw, loc) ->
      Lswitch (f e,
               { sw_numconsts = sw.sw_numconsts;
                 sw_consts = List.map (fun (n, e) -> (n, f e)) sw.sw_consts;
                 sw_numblocks = sw.sw_numblocks;
                 sw_blocks = List.map (fun (n, e) -> (n, f e)) sw.sw_blocks;
                 sw_failaction = Option.map f sw.sw_failaction;
               },
               loc)
  | Lstringswitch (e, sw, default, loc) ->
      Lstringswitch (
        f e,
        List.map (fun (s, e) -> (s, f e)) sw,
        Option.map f default,
        loc)
  | Lstaticraise (i, args) ->
      Lstaticraise (i, List.map f args)
  | Lstaticcatch (body, id, handler) ->
      Lstaticcatch (f body, id, f handler)
  | Ltrywith (e1, v, e2) ->
      Ltrywith (f e1, v, f e2)
  | Lifthenelse (e1, e2, e3) ->
      Lifthenelse (f e1, f e2, f e3)
  | Lsequence (e1, e2) ->
      Lsequence (f e1, f e2)
  | Lwhile (e1, e2) ->
      Lwhile (f e1, f e2)
  | Lfor (v, e1, e2, dir, e3) ->
      Lfor (v, f e1, f e2, dir, f e3)
  | Lassign (v, e) ->
      Lassign (v, f e)
  | Lsend (k, m, o, el, loc) ->
      Lsend (k, f m, f o, List.map f el, loc)
  | Levent (l, ev) ->
      Levent (f l, ev)
  | Lifused (v, e) ->
      Lifused (v, f e)

let map f =
  let rec g lam = f (shallow_map g lam) in
  g

(* To let-bind expressions to variables *)

let bind_with_value_kind str (var, kind) exp body =
  match exp with
    Lvar var' when Ident.same var var' -> body
  | _ -> Llet(str, kind, var, exp, body)

let bind str var exp body =
  bind_with_value_kind str (var, Pgenval) exp body

let negate_integer_comparison = function
  | Ceq -> Cne
  | Cne -> Ceq
  | Clt -> Cge
  | Cle -> Cgt
  | Cgt -> Cle
  | Cge -> Clt

let swap_integer_comparison = function
  | Ceq -> Ceq
  | Cne -> Cne
  | Clt -> Cgt
  | Cle -> Cge
  | Cgt -> Clt
  | Cge -> Cle

let negate_float_comparison = function
  | CFeq -> CFneq
  | CFneq -> CFeq
  | CFlt -> CFnlt
  | CFnlt -> CFlt
  | CFgt -> CFngt
  | CFngt -> CFgt
  | CFle -> CFnle
  | CFnle -> CFle
  | CFge -> CFnge
  | CFnge -> CFge

let swap_float_comparison = function
  | CFeq -> CFeq
  | CFneq -> CFneq
  | CFlt -> CFgt
  | CFnlt -> CFngt
  | CFle -> CFge
  | CFnle -> CFnge
  | CFgt -> CFlt
  | CFngt -> CFnlt
  | CFge -> CFle
  | CFnge -> CFnle

let raise_kind = function
  | Raise_regular -> "raise"
  | Raise_reraise -> "reraise"
  | Raise_notrace -> "raise_notrace"

let merge_inline_attributes attr1 attr2 =
  match attr1, attr2 with
  | Default_inline, _ -> Some attr2
  | _, Default_inline -> Some attr1
  | _, _ ->
    if attr1 = attr2 then Some attr1
    else None

let function_is_curried func =
  match func.kind with
  | Curried -> true
  | Tupled -> false

let find_exact_application kind ~arity args =
  match kind with
  | Curried ->
      if arity <> List.length args
      then None
      else Some args
  | Tupled ->
      begin match args with
      | [Lprim(Pmakeblock _, tupled_args, _)] ->
          if arity <> List.length tupled_args
          then None
          else Some tupled_args
      | [Lconst(Const_block (_, const_args))] ->
          if arity <> List.length const_args
          then None
          else Some (List.map (fun cst -> Lconst cst) const_args)
      | _ -> None
      end

let reset () =
  raise_count := 0
