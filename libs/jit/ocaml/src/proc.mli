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

(* Processor descriptions *)

(* Registers available for register allocation *)
val num_register_classes: int
val register_class: Reg.t -> int
val num_available_registers: int array
val first_available_register: int array
val register_name: int -> string
val phys_reg: int -> Reg.t
val rotate_registers: bool

(* Calling conventions *)
val loc_arguments: Cmm.machtype -> Reg.t array * int
val loc_results: Cmm.machtype -> Reg.t array
val loc_parameters: Cmm.machtype -> Reg.t array
(* For argument number [n] split across multiple registers, the target-specific
   implementation of [loc_external_arguments] must return [regs] such that
   [regs.(n).(0)] is to hold the part of the value at the lowest address. *)
val loc_external_arguments: Cmm.exttype list -> Reg.t array array * int
val loc_external_results: Cmm.machtype -> Reg.t array
val loc_exn_bucket: Reg.t

(* The maximum number of arguments of an OCaml to OCaml function call for
   which it is guaranteed there will be no arguments passed on the stack.
   (Above this limit, tail call optimization may be disabled.)
   N.B. The values for this parameter in the backends currently assume
   that no unboxed floats are passed using the OCaml calling conventions.
*)
val max_arguments_for_tailcalls : int

(* Maximal register pressures for pre-spilling *)
val safe_register_pressure: Mach.operation -> int
val max_register_pressure: Mach.operation -> int array

(* Registers destroyed by operations *)
val destroyed_at_oper: Mach.instruction_desc -> Reg.t array
val destroyed_at_raise: Reg.t array
val destroyed_at_reloadretaddr : Reg.t array

(** For a given register class, the DWARF register numbering for that class.
    Given an allocated register with location [Reg n] and class [reg_class], the
    returned array contains the corresponding DWARF register number at index
    [n - first_available_register.(reg_class)]. *)
val dwarf_register_numbers : reg_class:int -> int array

(** The DWARF register number corresponding to the stack pointer. *)
val stack_ptr_dwarf_register_number : int

(* Calling the assembler *)
val assemble_file: string -> string -> int

(* Called before translating a fundecl. *)
val init : unit -> unit
