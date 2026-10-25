(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                    Marcell Fischbach, University of Siegen             *)
(*                     Benedikt Meurer, University of Siegen              *)
(*                                                                        *)
(*   Copyright 2011 Lehrstuhl für Compilerbau und Softwareanalyse,        *)
(*     Universität Siegen.                                                *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Linear scan register allocation. *)

open Interval

module IntervalSet = Set.Make (struct
    type t = Interval.t
    let compare i j =
      let c = Int.compare i.iend j.iend in
      if c = 0 then Int.compare i.reg.stamp j.reg.stamp else c
  end)

module SlotSet = Set.Make(Int)

(* Live intervals per register class *)

type class_intervals =
  {
    mutable ci_fixed: IntervalSet.t;
    mutable ci_active: IntervalSet.t;
    mutable ci_inactive: IntervalSet.t;
    mutable ci_spilled:
      (* spilled stack slots (reg.loc = Stack (Local n)) still in use *)
      IntervalSet.t;
    mutable ci_free_slots:
      (* expired stack slots available for reuse *)
      SlotSet.t;
  }

let active = Array.init Proc.num_register_classes (fun _ -> {
  ci_fixed = IntervalSet.empty;
  ci_active = IntervalSet.empty;
  ci_inactive = IntervalSet.empty;
  ci_spilled = IntervalSet.empty;
  ci_free_slots = SlotSet.empty;
})

let slot_of_spilled i =
  match i.reg.loc with
  | Stack(Local ss) -> ss
  | _ -> invalid_arg "Linscan.slot_of_spilled"


let split_by_pos intervals pos =
  let divider =
    (* this interval is strictly above intervals [i] with [i.iend < pos] and
       strictly below [i] with [i.iend >= pos]. We use a dummy register with a
       non-existent [stamp] to make sure that it is not "equal" to any of the
       intervals in the set (according to the equality function of [IntervalSet]
       above). *)
    {Interval.reg = {Reg.dummy with stamp = -1};
     ibegin = pos;
     iend = pos;
     ranges = []}
  in
  let (before, divider_in_set, after) = IntervalSet.split divider intervals in
  assert (not divider_in_set);
  (before, after)

let remove_expired_ranges intervals pos =
  IntervalSet.iter (fun i -> Interval.remove_expired_ranges i pos) intervals

let release_expired_spilled ci pos =
  let (expired, rest) = split_by_pos ci.ci_spilled pos in
  ci.ci_free_slots <-
    IntervalSet.fold (fun i free -> SlotSet.add (slot_of_spilled i) free)
      expired ci.ci_free_slots;
  ci.ci_spilled <- rest

let release_expired_fixed ci pos =
  let (_expired, rest) = split_by_pos ci.ci_fixed pos in
  remove_expired_ranges rest pos;
  ci.ci_fixed <- rest

let partition_live intervals pos =
  IntervalSet.partition (fun i -> Interval.is_live i pos) intervals

let release_expired_active ci pos =
  let (_expired, rest) = split_by_pos ci.ci_active pos in
  remove_expired_ranges rest pos;
  let active, inactive = partition_live rest pos in
  ci.ci_active <- active;
  ci.ci_inactive <- IntervalSet.union inactive ci.ci_inactive

let release_expired_inactive ci pos =
  let (_expired, rest) = split_by_pos ci.ci_inactive pos in
  remove_expired_ranges rest pos;
  let active, inactive = partition_live rest pos in
  ci.ci_inactive <- inactive;
  ci.ci_active <- IntervalSet.union active ci.ci_active

(* Allocate a new stack slot to the interval. *)

let allocate_stack_slot num_stack_slots i =
  let cl = Proc.register_class i.reg in
  let ci = active.(cl) in
  let ss =
    match SlotSet.min_elt_opt ci.ci_free_slots with
    | Some ss ->
        ci.ci_free_slots <- SlotSet.remove ss ci.ci_free_slots;
        ss
    | None ->
        let ss = num_stack_slots.(cl) in
        num_stack_slots.(cl) <- succ ss;
        ss
  in
  i.reg.loc <- Stack(Local ss);
  i.reg.spill <- true;
  ci.ci_spilled <- IntervalSet.add i ci.ci_spilled

(* Find a register for the given interval and assigns this register.
   The interval is added to active. Raises Not_found if no free registers
   left. *)

let allocate_free_register num_stack_slots i =
  begin match i.reg.loc, i.reg.spill with
    Unknown, true ->
      (* Allocate a stack slot for the already spilled interval *)
      allocate_stack_slot num_stack_slots i
  | Unknown, _ ->
      (* We need to allocate a register to this interval somehow *)
      let cl = Proc.register_class i.reg in
      begin match Proc.num_available_registers.(cl) with
        0 ->
          (* There are no registers available for this class *)
          raise Not_found
      | rn ->
          let ci = active.(cl) in
          let r0 = Proc.first_available_register.(cl) in
          (* Create register mask for this class
             note: if frame pointers are enabled then some registers may have
                   indexes that are off-bounds; we hence protect write accesses
                   below (given that the assign function will not consider such
                   registers) *)
          let regmask = Array.make rn true in
          (* Remove all assigned registers from the register mask *)
          IntervalSet.iter
            (function
              {reg = {loc = Reg r}} ->
                if r - r0 < rn then regmask.(r - r0) <- false
            | _ -> ())
            ci.ci_active;
          (* Remove all overlapping registers from the register mask *)
          let remove_bound_overlapping = function
              {reg = {loc = Reg r}} as j ->
                if (r - r0 < rn) && regmask.(r - r0)
                   && Interval.overlap j i then
                regmask.(r - r0) <- false
            | _ -> () in
          IntervalSet.iter remove_bound_overlapping ci.ci_inactive;
          IntervalSet.iter remove_bound_overlapping ci.ci_fixed;
          (* Assign the first free register (if any) *)
          let rec assign r =
            if r = rn then
              raise Not_found
            else if regmask.(r) then begin
              (* Assign the free register and insert the
                 current interval into the active list *)
              i.reg.loc <- Reg (r0 + r);
              i.reg.spill <- false;
              ci.ci_active <- IntervalSet.add i ci.ci_active
            end else
              assign (succ r) in
          assign 0
      end
  | _ -> ()
  end

let allocate_blocked_register num_stack_slots i =
  let cl = Proc.register_class i.reg in
  let ci = active.(cl) in
  match IntervalSet.max_elt_opt ci.ci_active with
  | Some ilast when
      ilast.iend > i.iend &&
      (* Last interval in active is the last interval, so spill it. *)
      let chk r = r.reg.loc = ilast.reg.loc && Interval.overlap r i in
      (* But only if its physical register is admissible for the current
         interval. *)
      not (IntervalSet.exists chk ci.ci_fixed ||
           IntervalSet.exists chk ci.ci_inactive)
    ->
      let il = IntervalSet.remove ilast ci.ci_active in
      begin match ilast.reg.loc with Reg _ -> () | _ -> assert false end;
      (* Use register from last interval for current interval *)
      i.reg.loc <- ilast.reg.loc;
      (* Remove the last interval from active and insert the current *)
      ci.ci_active <- IntervalSet.add i il;
      (* Now get a new stack slot for the spilled register *)
      allocate_stack_slot num_stack_slots ilast
  | _ ->
      (* Either the current interval is last and we have to spill it,
         or there are no registers at all in the register class (i.e.
         floating point class on i386). *)
      allocate_stack_slot num_stack_slots i

let walk_interval num_stack_slots i =
  let pos = i.ibegin land (lnot 0x01) in
  (* Release all intervals that have been expired at the current position *)
  Array.iter
    (fun ci ->
      release_expired_fixed ci pos;
      release_expired_active ci pos;
      release_expired_inactive ci pos;
      release_expired_spilled ci pos)
    active;
  try
    (* Allocate free register (if any) *)
    allocate_free_register num_stack_slots i
  with
    Not_found ->
      (* No free register, need to decide which interval to spill *)
      allocate_blocked_register num_stack_slots i

let allocate_registers (intervals : Interval.result) =
  (* Initialize the stack slots and interval lists *)
  for cl = 0 to Proc.num_register_classes - 1 do
    (* Start with empty interval lists *)
    active.(cl) <- {
      ci_fixed = IntervalSet.empty;
      ci_active = IntervalSet.empty;
      ci_inactive = IntervalSet.empty;
      ci_spilled = IntervalSet.empty;
      ci_free_slots = SlotSet.empty;
    };
  done;
  (* Reset the stack slot counts *)
  let num_stack_slots = Array.make Proc.num_register_classes 0 in
  (* Add all fixed intervals (sorted by end position) *)
  List.iter
    (fun i ->
      let ci = active.(Proc.register_class i.reg) in
      ci.ci_fixed <- IntervalSet.add i ci.ci_fixed)
    intervals.fixed_intervals;
  (* Walk all the intervals within the list *)
  List.iter (walk_interval num_stack_slots) intervals.intervals;
  num_stack_slots
