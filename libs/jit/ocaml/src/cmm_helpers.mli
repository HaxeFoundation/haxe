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

open Cmm

(** [bind name arg fn] is equivalent to [let name = arg in fn name],
    or simply [fn arg] if [arg] is simple enough *)
val bind :
  string -> expression -> (expression -> expression) -> expression

(** Same as [bind], but also treats loads from a variable as simple *)
val bind_load :
  string -> expression -> (expression -> expression) -> expression

(** Same as [bind], but does not treat variables as simple *)
val bind_nonvar :
  string -> expression -> (expression -> expression) -> expression

(** Headers *)

(** A null header with GC bits set to black *)
val caml_black : nativeint

(** A constant equal to the tag for float arrays *)
val floatarray_tag : Debuginfo.t -> expression

(** [block_header tag size] creates a header with tag [tag] for a
    block of size [size] *)
val block_header : int -> int -> nativeint

(** Same as block_header, but with GC bits set to black *)
val black_block_header : int -> int -> nativeint

(** Closure headers of the given size *)
val white_closure_header : int -> nativeint
val black_closure_header : int -> nativeint

(** Infix header at the given offset *)
val infix_header : int -> nativeint

(** Header for a boxed float value *)
val float_header : nativeint

(** Header for an unboxed float array of the given size *)
val floatarray_header : int -> nativeint

(** Header for a string (or bytes) of the given length *)
val string_header : int -> nativeint

(** Boxed integer headers *)
val boxedint32_header : nativeint
val boxedint64_header : nativeint
val boxedintnat_header : nativeint

(** Closure info for a closure of given arity and distance to environment *)
val closure_info : arity:int -> startenv:int -> nativeint

(** Wrappers *)
val alloc_float_header : Debuginfo.t -> expression
val alloc_floatarray_header : int -> Debuginfo.t -> expression
val alloc_closure_header : int -> Debuginfo.t -> expression
val alloc_infix_header : int -> Debuginfo.t -> expression
val alloc_closure_info :
      arity:int -> startenv:int -> Debuginfo.t -> expression
val alloc_boxedint32_header : Debuginfo.t -> expression
val alloc_boxedint64_header : Debuginfo.t -> expression
val alloc_boxedintnat_header : Debuginfo.t -> expression

(** Integers *)

(** Minimal/maximal OCaml integer values whose backend representation fits
    in a regular OCaml integer *)
val max_repr_int : int
val min_repr_int : int

(** Make an integer constant from the given integer (tags the integer) *)
val int_const : Debuginfo.t -> int -> expression
val cint_const : int -> data_item
val targetint_const : int -> Targetint.t

(** Make a Cmm constant holding the given nativeint value.
    Uses [Cconst_int] instead of [Cconst_nativeint] when possible
    to preserve peephole optimisations. *)
val natint_const_untagged : Debuginfo.t -> Nativeint.t -> expression

(** Add an integer to the given expression *)
val add_const : expression -> int -> Debuginfo.t -> expression

(** Increment/decrement of integers *)
val incr_int : expression -> Debuginfo.t -> expression
val decr_int : expression -> Debuginfo.t -> expression

(** Simplify the given expression knowing its last bit will be
    irrelevant *)
val ignore_low_bit_int : expression -> expression

(** Simplify the given expression knowing its first bit will be
    irrelevant *)
val ignore_high_bit_int : expression -> expression

(** Arithmetical operations on integers *)
val add_int : expression -> expression -> Debuginfo.t -> expression
val sub_int : expression -> expression -> Debuginfo.t -> expression
val lsl_int : expression -> expression -> Debuginfo.t -> expression
val mul_int : expression -> expression -> Debuginfo.t -> expression
val lsr_int : expression -> expression -> Debuginfo.t -> expression
val asr_int : expression -> expression -> Debuginfo.t -> expression
val div_int :
  expression -> expression -> Lambda.is_safe -> Debuginfo.t -> expression
val mod_int :
  expression -> expression -> Lambda.is_safe -> Debuginfo.t -> expression

(** Integer tagging. [tag_int x = (x lsl 1) + 1] *)
val tag_int : expression -> Debuginfo.t -> expression

(** Integer untagging. [untag_int x = (x asr 1)] *)
val untag_int : expression -> Debuginfo.t -> expression

(** Specific division operations for boxed integers *)
val safe_div_bi :
  Lambda.is_safe ->
  expression ->
  expression ->
  Primitive.boxed_integer ->
  Debuginfo.t ->
  expression
val safe_mod_bi :
  Lambda.is_safe ->
  expression ->
  expression ->
  Primitive.boxed_integer ->
  Debuginfo.t ->
  expression

(** If-Then-Else expression
    [mk_if_then_else dbg cond ifso_dbg ifso ifnot_dbg ifnot] associates
    [dbg] to the global if-then-else expression, [ifso_dbg] to the
    then branch [ifso], and [ifnot_dbg] to the else branch [ifnot] *)
val mk_if_then_else :
  Debuginfo.t ->
  expression ->
  Debuginfo.t -> expression ->
  Debuginfo.t -> expression ->
  expression

(** Boolean negation *)
val mk_not : Debuginfo.t -> expression -> expression

(** Integer and float comparison that returns int not bool *)
val mk_compare_ints : Debuginfo.t -> expression -> expression -> expression
val mk_compare_floats : Debuginfo.t -> expression -> expression -> expression

(** Loop construction (while true do expr done).
    Used to be represented as Cloop. *)
val create_loop : expression -> Debuginfo.t -> expression

(** Exception raising *)
val raise_symbol : Debuginfo.t -> string -> expression

(** Convert a tagged integer into a raw integer with boolean meaning *)
val test_bool : Debuginfo.t -> expression -> expression

(** Float boxing and unboxing *)
val box_float : Debuginfo.t -> expression -> expression
val unbox_float : Debuginfo.t -> expression -> expression

(** Conversions for 16-bit floats *)
val float_of_float16 : Debuginfo.t -> expression -> expression
val float16_of_float : Debuginfo.t -> expression -> expression

(** Complex number creation and access *)
val box_complex : Debuginfo.t -> expression -> expression -> expression
val complex_re : expression -> Debuginfo.t -> expression
val complex_im : expression -> Debuginfo.t -> expression

(** Make the given expression return a unit value *)
val return_unit : Debuginfo.t -> expression -> expression

(** Remove a trailing unit return if any *)
val remove_unit : expression -> expression

(** Blocks *)

(** Non-atomic load of a mutable field *)
val mk_load_mut : memory_chunk -> operation

(** Atomic load. All atomic fields are mutable. *)
val mk_load_atomic : memory_chunk -> operation

(** [field_address ptr n dbg] returns an expression for the address of the
    [n]th field of the block pointed to by [ptr] *)
val field_address : expression -> int -> Debuginfo.t -> expression

(** [get_field_gen mut ptr n dbg] returns an expression for the access to the
    [n]th field of the block pointed to by [ptr] *)
val get_field_gen :
  Asttypes.mutable_flag -> expression -> int -> Debuginfo.t -> expression

(** [set_field ptr n newval init dbg] returns an expression for setting the
    [n]th field of the block pointed to by [ptr] to [newval] *)
val set_field :
  expression -> int -> expression -> Lambda.initialization_or_assignment ->
  Debuginfo.t -> expression

(** Load a block's header *)
val get_header : expression -> Debuginfo.t -> expression

(** Same as [get_header], but also clear all reserved bits of the result *)
val get_header_masked : expression -> Debuginfo.t -> expression

(** Load a block's tag *)
val get_tag : expression -> Debuginfo.t -> expression

(** Load a block's size *)
val get_size : expression -> Debuginfo.t -> expression

(** Arrays *)

val wordsize_shift : int
val numfloat_shift : int

(** Check whether the given array is an array of regular OCaml values
    (as opposed to unboxed floats), from its header or pointer *)
val is_addr_array_hdr : expression -> Debuginfo.t -> expression
val is_addr_array_ptr : expression -> Debuginfo.t -> expression

(** Get the length of an array from its header
    Shifts by one bit less than necessary, keeping one of the GC colour bits,
    to save an operation when returning the length as a caml integer or when
    comparing it to a caml integer.
    Assumes that the reserved bits are clear (see get_header_masked) *)
val addr_array_length_shifted : expression -> Debuginfo.t -> expression
val float_array_length_shifted : expression -> Debuginfo.t -> expression

(** For [array_indexing ?typ log2size ptr ofs dbg] :
    Produces a pointer to the element of the array [ptr] on the position [ofs]
    with the given element [log2size] log2 element size. [ofs] is given as a
    tagged int expression.
    The optional ?typ argument is the C-- type of the result.
    By default, it is Addr, meaning we are constructing a derived pointer
    into the heap.  If we know the pointer is outside the heap
    (this is the case for bigarray indexing), we give type Int instead. *)
val array_indexing :
  ?typ:machtype_component -> int -> expression -> expression -> Debuginfo.t ->
  expression

(** Array loads and stores
    [unboxed_float_array_ref] and [float_array_ref] differ in the
    boxing of the result; [float_array_set] takes an unboxed float *)
val addr_array_ref : expression -> expression -> Debuginfo.t -> expression
val int_array_ref : expression -> expression -> Debuginfo.t -> expression
val unboxed_float_array_ref :
  expression -> expression -> Debuginfo.t -> expression
val float_array_ref : expression -> expression -> Debuginfo.t -> expression
val addr_array_set :
  expression -> expression -> expression -> Debuginfo.t -> expression
val addr_array_initialize :
  expression -> expression -> expression -> Debuginfo.t -> expression
val int_array_set :
  expression -> expression -> expression -> Debuginfo.t -> expression
val float_array_set :
  expression -> expression -> expression -> Debuginfo.t -> expression

(** Strings *)

val string_length : expression -> Debuginfo.t -> expression
val bigstring_length : expression -> Debuginfo.t -> expression

(** Objects *)

(** Lookup a method by its hash, using [caml_get_public_method]
    Arguments :
    - obj : the object from which to lookup
    - tag : the hash of the method name, as a tagged integer *)
val lookup_tag : expression -> expression -> Debuginfo.t -> expression

(** Lookup a method by its offset in the method table
    Arguments :
    - obj : the object from which to lookup
    - lab : the position of the required method in the object's
    method array, as a tagged integer *)
val lookup_label : expression -> expression -> Debuginfo.t -> expression

(** Lookup and call a method using the method cache
    Arguments :
    - obj : the object from which to lookup
    - tag : the hash of the method name, as a tagged integer
    - cache : the method cache array
    - pos : the position of the cache entry in the cache array
    - args : the additional arguments to the method call *)
val call_cached_method :
  expression -> expression -> expression -> expression -> expression list ->
  Debuginfo.t -> expression

(** Allocations *)

(** Allocate a block of regular values with the given tag *)
val make_alloc : Debuginfo.t -> int -> expression list -> expression

(** Allocate a block of unboxed floats with the given tag *)
val make_float_alloc : Debuginfo.t -> int -> expression list -> expression

(** Bounds checking *)

(** Generate a [Ccheckbound] term *)
val make_checkbound : Debuginfo.t -> expression list -> expression

(** [check_bound safety access_size dbg length a2 k] prefixes expression [k]
    with a check that reading [access_size] bits starting at position [a2]
    in a string/bytes value of length [length] is within bounds, unless
    [safety] is [Unsafe]. *)
val check_bound :
  Lambda.is_safe -> Clambda_primitives.memory_access_size -> Debuginfo.t ->
  expression -> expression -> expression ->
  expression

(** Sys.opaque_identity *)
val opaque : expression -> Debuginfo.t -> expression

(** Generic application functions *)

(** Get the symbol for the generic application with [n] arguments, and
    ensure its presence in the set of defined symbols *)
val apply_function_sym : int -> string

(** If [n] is positive, get the symbol for the generic currying wrapper with
    [n] arguments, and ensure its presence in the set of defined symbols.
    Otherwise, do the same for the generic tuple wrapper with [-n] arguments. *)
val curry_function_sym : int -> string

(** Bigarrays *)

(** [bigarray_get unsafe kind layout b args dbg]
    - unsafe : if true, do not insert bound checks
    - kind : see [Lambda.bigarray_kind]
    - layout : see [Lambda.bigarray_layout]
    - b : the bigarray to load from
    - args : a list of tagged integer expressions, corresponding to the
    indices in the respective dimensions
    - dbg : debugging information *)
val bigarray_get :
  bool -> Lambda.bigarray_kind -> Lambda.bigarray_layout ->
  expression -> expression list -> Debuginfo.t ->
  expression

(** [bigarray_set unsafe kind layout b args newval dbg]
    Same as [bigarray_get], with [newval] the value being assigned *)
val bigarray_set :
  bool -> Lambda.bigarray_kind -> Lambda.bigarray_layout ->
  expression -> expression list -> expression -> Debuginfo.t ->
  expression

(** Operations on 32-bit integers *)

(** [low_32 _ x] is a value which agrees with x on at least the low 32 bits *)
val low_32 : Debuginfo.t -> expression -> expression

(** Sign extend from 32 bits to the word size *)
val sign_extend_32 : Debuginfo.t -> expression -> expression

(** Zero extend from 32 bits to the word size *)
val zero_extend_32 : Debuginfo.t -> expression -> expression

(** Boxed numbers *)

(** Global symbols for the ops field of boxed integers *)
val caml_nativeint_ops : string
val caml_int32_ops : string
val caml_int64_ops : string

(** Box a given integer, without sharing of constants *)
val box_int_gen :
  Debuginfo.t -> Primitive.boxed_integer -> expression -> expression

(** Unbox a given boxed integer *)
val unbox_int :
  Debuginfo.t -> Primitive.boxed_integer -> expression -> expression

(** Used to prepare 32-bit integers on 64-bit platforms for a lsr operation *)
val make_unsigned_int :
  Primitive.boxed_integer -> expression -> Debuginfo.t -> expression

val unaligned_load_16 : expression -> expression -> Debuginfo.t -> expression
val unaligned_set_16 :
  expression -> expression -> expression -> Debuginfo.t -> expression
val unaligned_load_32 : expression -> expression -> Debuginfo.t -> expression
val unaligned_set_32 :
  expression -> expression -> expression -> Debuginfo.t -> expression
val unaligned_load_64 : expression -> expression -> Debuginfo.t -> expression
val unaligned_set_64 :
  expression -> expression -> expression -> Debuginfo.t -> expression

(** Raw memory accesses *)

(** [unaligned_set size ptr idx newval dbg] *)
val unaligned_set :
  Clambda_primitives.memory_access_size ->
  expression -> expression -> expression -> Debuginfo.t -> expression

(** [unaligned_load size ptr idx dbg] *)
val unaligned_load :
  Clambda_primitives.memory_access_size ->
  expression -> expression -> Debuginfo.t -> expression

(** [box_sized size dbg exp] *)
val box_sized :
  Clambda_primitives.memory_access_size ->
  Debuginfo.t -> expression -> expression

(** Primitives *)

val simplif_primitive :
  Clambda_primitives.primitive -> Clambda_primitives.primitive

type unary_primitive = expression -> Debuginfo.t -> expression

(** Return the n-th field of a float array (or float-only record), as an
    unboxed float *)
val floatfield : int -> unary_primitive

(** Int_as_pointer primitive *)
val int_as_pointer : unary_primitive

(** Raise primitive *)
val raise_prim : Lambda.raise_kind -> unary_primitive

(** Unary negation of an OCaml integer *)
val negint : unary_primitive

(** Add a constant number to an OCaml integer *)
val offsetint : int -> unary_primitive

(** Add a constant number to an OCaml integer reference *)
val offsetref : int -> unary_primitive

(** Return the length of the array argument, as an OCaml integer *)
val arraylength : Lambda.array_kind -> unary_primitive

(** Byte swap primitive
    Operates on Cmm integers (unboxed values) *)
val bbswap : Primitive.boxed_integer -> unary_primitive

(** 16-bit byte swap primitive
    Operates on Cmm integers (untagged integers) *)
val bswap16 : unary_primitive

type binary_primitive = expression -> expression -> Debuginfo.t -> expression

type assignment_kind = Caml_modify | Caml_initialize | Simple

(** [setfield offset value_is_ptr init ptr value dbg] *)
val setfield :
  int -> Lambda.immediate_or_pointer -> Lambda.initialization_or_assignment ->
  binary_primitive

(** [setfloatfield offset init ptr value dbg]
    [value] is expected to be an unboxed floating point number *)
val setfloatfield :
  int -> Lambda.initialization_or_assignment -> binary_primitive

(** Operations on OCaml integers *)
val add_int_caml : binary_primitive
val sub_int_caml : binary_primitive
val mul_int_caml : binary_primitive
val div_int_caml : Lambda.is_safe -> binary_primitive
val mod_int_caml : Lambda.is_safe -> binary_primitive
val and_int_caml : binary_primitive
val or_int_caml : binary_primitive
val xor_int_caml : binary_primitive
val lsl_int_caml : binary_primitive
val lsr_int_caml : binary_primitive
val asr_int_caml : binary_primitive
val int_comp_caml : Lambda.integer_comparison -> binary_primitive

(** Strings, Bytes and Bigstrings *)

(** Regular string/bytes access. Args: string/bytes, index *)
val stringref_unsafe : binary_primitive
val stringref_safe : binary_primitive

(** Load by chunk from string/bytes, bigstring. Args: string, index *)
val string_load :
  Clambda_primitives.memory_access_size -> Lambda.is_safe -> binary_primitive
val bigstring_load :
  Clambda_primitives.memory_access_size -> Lambda.is_safe -> binary_primitive

(** Arrays *)

(** Array access. Args: array, index *)
val arrayref_unsafe : Lambda.array_kind -> binary_primitive
val arrayref_safe : Lambda.array_kind -> binary_primitive

type ternary_primitive =
  expression -> expression -> expression -> Debuginfo.t -> expression

(** Same as setfield, except the offset is one of the arguments.
    Args: pointer (structure/array/...), index, value *)
val setfield_computed :
  Lambda.immediate_or_pointer -> Lambda.initialization_or_assignment ->
  ternary_primitive

(** Set the byte at the given offset to the given value.
    Args: bytes, index, value *)
val bytesset_unsafe : ternary_primitive
val bytesset_safe : ternary_primitive

(** Set the element at the given index in the given array to the given value.
    WARNING: if [kind] is [Pfloatarray], then [value] is expected to be an
    _unboxed_ float. Otherwise, it is expected to be a regular caml value,
    including in the case where the array contains floats.
    Args: array, index, value *)
val arrayset_unsafe : Lambda.array_kind -> ternary_primitive
val arrayset_safe : Lambda.array_kind -> ternary_primitive

(** Set a chunk of data in the given bytes or bigstring structure.
    See also [string_load] and [bigstring_load].
    Note: [value] is expected to be an unboxed number of the given size.
    Args: pointer, index, value *)
val bytes_set :
  Clambda_primitives.memory_access_size -> Lambda.is_safe -> ternary_primitive
val bigstring_set :
  Clambda_primitives.memory_access_size -> Lambda.is_safe -> ternary_primitive

(** Switch *)

(** [transl_isout h arg dbg] *)
val transl_isout : expression -> expression -> Debuginfo.t -> expression

(** [make_switch arg cases actions dbg] : Generate a Cswitch construct,
    or optimize as a static table lookup when possible. *)
val make_switch :
  expression -> int array -> (expression * Debuginfo.t) array -> Debuginfo.t ->
  expression

(** [transl_int_switch loc arg low high cases default] *)
val transl_int_switch :
  Debuginfo.t -> expression -> int -> int ->
  (int * expression) list -> expression -> expression

(** [transl_switch_clambda loc arg index cases] *)
val transl_switch_clambda :
  Debuginfo.t -> expression -> int array -> expression array -> expression

(** [strmatch_compile dbg arg default cases] *)
val strmatch_compile :
  Debuginfo.t -> expression -> expression option ->
  (string * expression) list -> expression

(** Closures and function applications *)

(** Adds a constant offset to a pointer (for infix access) *)
val ptr_offset : expression -> int -> Debuginfo.t -> expression

(** Direct application of a function via a symbol *)
val direct_apply : string -> expression list -> Debuginfo.t -> expression

(** Generic application of a function to one or several arguments.
    The mutable_flag argument annotates the loading of the code pointer
    from the closure. The Cmmgen code uses a mutable load by
    default, with a special case when the load is from (the first function of)
    the currently defined closure. *)
val generic_apply :
  Asttypes.mutable_flag ->
  expression -> expression list -> Debuginfo.t -> expression

(** Method call : [send kind met obj args dbg]
    - [met] is a method identifier, which can be a hashed variant or an index
    in [obj]'s method table, depending on [kind]
    - [obj] is the object whose method is being called
    - [args] is the extra arguments to the method call (Note: I'm not aware
    of any way for the frontend to generate any arguments other than the
    cache and cache position) *)
val send :
  Lambda.meth_kind -> expression -> expression -> expression list ->
  Debuginfo.t -> expression

(** Generic Cmm fragments *)

(** Generate generic functions *)
val generic_functions : bool -> Cmx_format.unit_infos list -> Cmm.phrase list

val placeholder_dbg : unit -> Debuginfo.t
val placeholder_fun_dbg : human_name:string -> Debuginfo.t

(** Entry point *)
val entry_point : string list -> phrase

(** Generate the caml_globals table *)
val global_table: string list -> phrase

(** Add references to the given symbols *)
val reference_symbols: string list -> phrase

(** Generate the caml_globals_map structure, as a marshalled string constant *)
val globals_map:
  (string * Digest.t option * Digest.t option * string list) list -> phrase

(** Generate the caml_frametable table, referencing the frametables
    from the given compilation units *)
val frame_table: string list -> phrase

(** Generate the tables for data and code positions respectively of the given
    compilation units *)
val data_segment_table: string list -> phrase
val code_segment_table: string list -> phrase

(** Generate data for a predefined exception *)
val predef_exception: int -> string -> phrase

val plugin_header: (Cmx_format.unit_infos * Digest.t) list -> phrase

(** Emit constant symbols *)

(** Produce the data_item list corresponding to a symbol definition *)
val cdefine_symbol : (string * Cmmgen_state.is_global) -> data_item list

(** [emit_block symb white_header cont] prepends to [cont] the header and symbol
    for the block.
    [cont] must already contain the fields of the block (and may contain
    additional data items afterwards). *)
val emit_block :
  (string * Cmmgen_state.is_global) -> nativeint -> data_item list ->
  data_item list

(** Emit specific kinds of constant blocks as data items *)
val emit_float_constant :
  (string * Cmmgen_state.is_global) -> float -> data_item list ->
  data_item list
val emit_string_constant :
  (string * Cmmgen_state.is_global) -> string -> data_item list ->
  data_item list
val emit_int32_constant :
  (string * Cmmgen_state.is_global) -> int32 -> data_item list ->
  data_item list
val emit_int64_constant :
  (string * Cmmgen_state.is_global) -> int64 -> data_item list ->
  data_item list
val emit_nativeint_constant :
  (string * Cmmgen_state.is_global) -> nativeint -> data_item list ->
  data_item list
val emit_float_array_constant :
  (string * Cmmgen_state.is_global) -> float list -> data_item list ->
  data_item list

val fundecls_size : Clambda.ufunction list -> int

val emit_constant_closure :
  (string * Cmmgen_state.is_global) -> Clambda.ufunction list ->
  data_item list -> data_item list -> data_item list

val emit_preallocated_blocks :
  Clambda.preallocated_block list -> phrase list -> phrase list
