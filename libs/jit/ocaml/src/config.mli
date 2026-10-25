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

(** System configuration

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

val version: string
(** The current version number of the system *)

val bindir: string
(** The directory containing the binary programs *)

val standard_library: string
(** The directory containing the standard libraries *)

val ccomp_type: string
(** The "kind" of the C compiler, assembler and linker used: one of
    "cc" (for Unix-style C compilers)
    "msvc" (for Microsoft Visual C++ and MASM) *)

val c_compiler: string
(** The compiler to use for compiling C files *)

val c_output_obj: string
(** Name of the option of the C compiler for specifying the output
    file *)

val c_has_debug_prefix_map : bool
(** Whether the C compiler supports -fdebug-prefix-map *)

val as_has_debug_prefix_map : bool
(** Whether the assembler supports --debug-prefix-map *)

val bytecode_cflags : string
(** The flags ocamlc should pass to the C compiler *)

val bytecode_cppflags : string
(** The flags ocamlc should pass to the C preprocessor *)

val native_cflags : string
(** The flags ocamlopt should pass to the C compiler *)

val native_cppflags : string
(** The flags ocamlopt should pass to the C preprocessor *)

val bytecomp_c_libraries: string
(** The C libraries to link with custom runtimes *)

val native_c_libraries: string
(** The C libraries to link with native-code programs *)

val native_ldflags : string
(* Flags to pass to the system linker *)

val native_pack_linker: string
(** The linker to use for packaging (ocamlopt -pack) and for partial
    links (ocamlopt -output-obj). *)

val mkdll: string
(** The linker command line to build dynamic libraries. *)

val mkexe: string
(** The linker command line to build executables. *)

val mkmaindll: string
(** The linker command line to build main programs as dlls. *)

val default_rpath: string
(** Option to add a directory to be searched for libraries at runtime
    (used by ocamlmklib) *)

val mksharedlibrpath: string
(** Option to add a directory to be searched for shared libraries at runtime
    (used by ocamlmklib) *)

val ar: string
(** Name of the ar command, or "" if not needed  (MSVC) *)

val interface_suffix: string ref
(** Suffix for interface file names *)

val exec_magic_number: string
(** Magic number for bytecode executable files *)

val cmi_magic_number: string
(** Magic number for compiled interface files *)

val cmo_magic_number: string
(** Magic number for object bytecode files *)

val cma_magic_number: string
(** Magic number for archive files *)

val cmx_magic_number: string
(** Magic number for compilation unit descriptions *)

val cmxa_magic_number: string
(** Magic number for libraries of compilation unit descriptions *)

val ast_intf_magic_number: string
(** Magic number for file holding an interface syntax tree *)

val ast_impl_magic_number: string
(** Magic number for file holding an implementation syntax tree *)

val cmxs_magic_number: string
(** Magic number for dynamically-loadable plugins *)

val cmt_magic_number: string
(** Magic number for compiled interface files *)

val linear_magic_number: string
(** Magic number for Linear internal representation files *)

val max_tag: int
(** Biggest tag that can be stored in the header of a regular block. *)

val lazy_tag : int
(** Normally the same as Obj.lazy_tag.  Separate definition because
    of technical reasons for bootstrapping. *)

val max_young_wosize: int
(** Maximal size of arrays that are directly allocated in the
    minor heap *)

val stack_threshold: int
(** Size in words of safe area at bottom of VM stack,
    see runtime/caml/config.h *)

val stack_safety_margin: int
(** Size in words of the safety margin between the bottom of
    the stack and the stack pointer. This margin can be used by
    intermediate computations of some instructions, or the event
    handler. *)

val native_compiler: bool
(** Whether the native compiler is available or not

    @since 5.1 *)

val architecture: string
(** Name of processor type for the native-code compiler *)

val model: string
(** Name of processor submodel for the native-code compiler *)

val system: string
(** Name of operating system for the native-code compiler *)

val asm: string
(** The assembler (and flags) to use for assembling
    ocamlopt-generated code. *)

val asm_cfi_supported: bool
(** Whether assembler understands CFI directives *)

val with_frame_pointers : bool
(** Whether assembler should maintain frame pointers *)

val ext_obj: string
(** Extension for object files, e.g. [.o] under Unix. *)

val ext_asm: string
(** Extension for assembler files, e.g. [.s] under Unix. *)

val ext_lib: string
(** Extension for library files, e.g. [.a] under Unix. *)

val ext_dll: string
(** Extension for dynamically-loaded libraries, e.g. [.so] under Unix.*)

val ext_exe: string
(** Extension for executable programs, e.g. [.exe] under Windows.

    @since 4.12 *)

val default_executable_name: string
(** Name of executable produced by linking if none is given with -o,
    e.g. [a.out] under Unix. *)

val systhread_supported : bool
(** Whether the system thread library is implemented *)

val flexdll_dirs : string list
(** Directories needed for the FlexDLL objects *)

val host : string
(** Whether the compiler is a cross-compiler *)

val target : string
(** Whether the compiler is a cross-compiler *)

val flambda : bool
(** Whether the compiler was configured for flambda *)

val with_flambda_invariants : bool
(** Whether the invariants checks for flambda are enabled *)

val with_cmm_invariants : bool
(** Whether the invariants checks for Cmm are enabled *)

val reserved_header_bits : int
(** How many bits of a block's header are reserved *)

val flat_float_array : bool
(** Whether the compiler and runtime automagically flatten float
    arrays *)

val function_sections : bool
(** Whether the compiler was configured to generate
    each function in a separate section *)

val windows_unicode: bool
(** Whether Windows Unicode runtime is enabled *)

val naked_pointers : bool
(** Whether the runtime supports naked pointers

    @since 4.14 *)

val supports_shared_libraries: bool
(** Whether shared libraries are supported

    @since 4.08 *)

val native_dynlink: bool
(** Whether native shared libraries are supported

    @since 5.1 *)

val afl_instrument : bool
(** Whether afl-fuzz instrumentation is generated by default *)

val ar_supports_response_files: bool
(** Whether ar supports @FILE arguments. *)

val tsan : bool
(** Whether ThreadSanitizer instrumentation is enabled *)

(** Access to configuration values *)
val print_config : out_channel -> unit

val config_var : string -> string option
(** the configuration value of a variable, if it exists *)

(**/**)

val merlin : bool

(**/**)
