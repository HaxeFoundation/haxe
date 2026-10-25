(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Luc Maranget, projet Moscova, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2000 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* see high-level comments in switch.mli *)

type 'a shared = Shared of 'a | Single of 'a

type ('a, 'ctx) t_store =
  {act_get : unit -> 'a array ;
   act_get_shared : unit -> 'a shared array ;
   act_store : 'ctx -> 'a -> int ;
   act_store_shared : 'ctx -> 'a -> int ; }

module type Stored = sig
  type t
  type key
  val compare_key : key -> key -> int
  val make_key : t -> key option
end

module type CtxStored = sig
  include Stored
  type context
  val make_key : context -> t -> key option
end

module CtxStore(A:CtxStored) = struct
  module AMap =
    Map.Make(struct type t = A.key let compare = A.compare_key end)

  type intern =
    { mutable map : (bool * int)  AMap.t ;
      mutable next : int ;
      mutable acts : (bool * A.t) list; }

  let mk_store () =
    let st =
      { map = AMap.empty ;
        next = 0 ;
        acts = [] ; } in

    let add mustshare act =
      let i = st.next in
      st.acts <- (mustshare,act) :: st.acts ;
      st.next <- i+1 ;
      i in

    let store mustshare ctx act = match A.make_key ctx act with
      | Some key ->
          begin try
            let (shared,i) = AMap.find key st.map in
            if not shared then st.map <- AMap.add key (true,i) st.map ;
            i
          with Not_found ->
            let i = add mustshare act in
            st.map <- AMap.add key (mustshare,i) st.map ;
            i
          end
      | None ->
          add mustshare act

    and get () = Array.of_list (List.rev_map (fun (_,act) -> act) st.acts)

    and get_shared () =
      let acts =
        Array.of_list
          (List.rev_map
             (fun (shared,act) ->
                if shared then Shared act else Single act)
             st.acts) in
      AMap.iter
        (fun _ (shared,i) ->
           if shared then match acts.(i) with
             | Single act -> acts.(i) <- Shared act
             | Shared _ -> ())
        st.map ;
      acts in
    {act_store = store false ; act_store_shared = store true ;
     act_get = get; act_get_shared = get_shared; }
end

module Store(A:Stored) = struct
  module Me =
    CtxStore
      (struct
        include A
        type context = unit
        let make_key () = A.make_key
      end)

  let mk_store = Me.mk_store
end



module type S =
sig
  type primitive
  val eqint : primitive
  val neint : primitive
  val leint : primitive
  val ltint : primitive
  val geint : primitive
  val gtint : primitive

  type loc
  type arg
  type test
  type act

  val bind : arg -> (arg -> act) -> act
  val make_const : int -> arg
  val make_offset : arg -> int -> arg
  val make_prim : primitive -> arg list -> test
  val make_isout : arg -> arg -> test
  val make_isin : arg -> arg -> test
  val make_is_nonzero : arg -> test
  val arg_as_test : arg -> test

  val make_if : test -> act -> act -> act
  val make_switch : loc -> arg -> int array -> act array -> act

  val make_catch : act -> int * (act -> act)
  val make_exit : int -> act
end

(* The module will ``produce good code for the case statement''

  Adaptation of
   Robert L. Berstein
   ``Producing good code for the case statement''
   Software Practice and Experience, 15(10) (1985)
  and
   Sampath Kannan and Todd A. Proebsting
   ``Correction to ``Producing good code for the case statement'' ''
   Software Practice and Experience, 24(2) (1993)
  and
   David L. Spuler
    ``Two-Way Comparison Search Trees, a Generalisation of Binary Search Trees
      and Split Trees''
    ``Compiler Code Generation for Multiway Branch Statement as
      a Static Search Problem''
   Technical Reports, James Cook University

 The article of Bernstein considers how to compile C-style switches:
 arrays of actions indexed over non-negative integers with some "missing"
 cases that are sent to a default action.

 The strategy proposed, which is followed in our implementation below,
 is as follows:
 1. Compute a "clustering" of the cases as a disjoint union of smaller intervals
    with a high enough "density" (few default cases on the interval).
 2. Generate "dense switch" code for each cluster, typically using a jump table.
 3. Generate a sequence of tests for the whole switch, whose leaves
    are the dense switches generated for each cluster.

 Berstein believes that computing the optimal clustering
 (smaller number of clusters) is NP-complete, and only proposes
 a suboptimal heuristic method. Kannan and Proebsting remark that it
 can be solved by a quadratic dynamic algorithm, which is also used in
 our implementation.

 The article of Spuler explains how to generate good test sequences
 (optimal in worst-case number of tests) for a two-way tests instead
 of three-way tests: traditional dichotomic search assumes that we
 check at each step whether the key is (1) equal to the pivot, (2)
 strictly less or (3) strictly more, but the test instructions in our
 intermediate representations typically only let us test for (1)
 lesser or equal or (2) strictly bigger (or: (1) strictly less, (2)
 bigger or equal, which is symmetric.). Spuler proves that, even in
 this two-way setting, dichotomic search generates optimal test
 sequences.

 The code below uses two additional ideas from Luc Maranget.

 1. The code to compute an optimal sequence of tests also makes use of
    an interval check (is the input in the range [m; n]), which
    (as remarked by Bernstein) can be implemented efficiently as
    a subtraction and an unsigned comparison. We don't know of an
    efficient algorithm to compute optimal test sequences using both
    comparison and interval checks, so instead:
    a. on large input intervals, we use the dichotomy
    b. on medium-sized input intervals, we use the best of
       the dichotomy and an interval check carving out
       exactly the lowest and highest cases
    c. on small input intervals, we use optimal exhaustive search.

 2. The works of Bernstein and Kannan-Proebsting compute clusters of
    sufficient density, where density is defined naturally as the
    proportion of non-default cases. Maranget instead computes density
    as the height of the test sequence divided by interval size
    (note that the number of non-default cases is an upperbound on the
    test sequence height, as the length of the linear test sequence).
    As a result, sub-intervals that can be efficiently decided by
    tests get a lower density, so they are more likely to be merged
    into the toplevel test sequence instead of generating a less
    compact jump table.
*)
module Make (Arg : S) =
struct

  (* A representation of switches over intervals rather than discrete
     values the [cases] array stores triples [(low, high, act)], where
     [low] is the lowest input value of the interval, [high] is the
     highest input value, and [act] is an index into the [actions]
     array.

     (There can be substantially less actions than intervals if many
     actions are shared.)
  *)
  type 'a inter = {
    cases : (int * int * int) array ;
    actions : 'a array
  }

  let small_size_limit = 8

  and medium_size_limit = 16

(*
let pint chan i =
  if i = min_int then Printf.fprintf chan "-oo"
  else if i=max_int then Printf.fprintf chan "oo"
  else Printf.fprintf chan "%d" i

let pcases chan cases =
  for i =0 to Array.length cases-1 do
    let l,h,act = cases.(i) in
    if l=h then
      Printf.fprintf chan "%d:%d " l act
    else
      Printf.fprintf chan "%a..%a:%d " pint l pint h act
  done

let prerr_inter i = Printf.fprintf stderr
        "cases=%a" pcases i.cases
*)

  let get_act cases i =
    let _,_,r = cases.(i) in
    r
  and get_low cases i =
    let r,_,_ = cases.(i) in
    r
  and get_high cases i =
    let _,r,_ = cases.(i) in
    r

  (* a "cost" as a number of tests in the worst case;
     [n] is the total number of tests
     [ni] is the number of interval tests

     If two choices have the same total number of tests, we will prefer
     the one with less interval tests as they cost slightly more.
  *)
  type ctests = {
    mutable n : int ;
    mutable ni : int ;
  }

  let too_much = {n=max_int ; ni=max_int}

(*
let ptests chan {n=n ; ni=ni} =
  Printf.fprintf chan "{n=%d ; ni=%d}" n ni

let pta chan t =
  for i =0 to Array.length t-1 do
    Printf.fprintf chan "%d: %a\n" i ptests t.(i)
  done
*)

  let less_tests c1 c2 =
    if c1.n < c2.n then
      true
    else if c1.n = c2.n then begin
      if c1.ni < c2.ni then
        true
      else
        false
    end else
      false

  and eq_tests c1 c2 = c1.n = c2.n && c1.ni=c2.ni

  let less2tests (c1,d1) (c2,d2) =
    if eq_tests c1 c2 then
      less_tests d1 d2
    else
      less_tests c1 c2

  let add_test t1 t2 =
    t1.n <- t1.n + t2.n ;
    t1.ni <- t1.ni + t2.ni ;

  (* Represents tests in a test sequence
     [Inter (low, high)] is an interval test
     [Sep bound] is [fun x -> x < bound]
     [No] is when no tests are necessary. *)
  type t_ret = Inter of int * int  | Sep of int | No

(*
let pret chan = function
  | Inter (i,j)-> Printf.fprintf chan "Inter %d %d" i j
  | Sep i -> Printf.fprintf chan "Sep %d" i
  | No -> Printf.fprintf chan "No"
*)

  let coupe cases i =
    let l,_,_ = cases.(i) in
    l,
    Array.sub cases 0 i,
    Array.sub cases i (Array.length cases-i)


  let case_append c1 c2 =
    let len1 = Array.length c1
    and len2 = Array.length c2 in
    match len1,len2 with
    | 0,_ -> c2
    | _,0 -> c1
    | _,_ ->
        let l1,h1,act1 = c1.(Array.length c1-1)
        and l2,h2,act2 = c2.(0) in
        if act1 = act2 then
          let r = Array.make (len1+len2-1) c1.(0) in
          for i = 0 to len1-2 do
            r.(i) <- c1.(i)
          done ;
          let l =
            if len1 < 2 then l1
            else begin (* 0 <= len1 - 2 < len1 *)
              let _,h,_ = r.(len1-2) in
              min (h + 1) l1
            end
          and h =
            if len2 < 2 then h2
            else begin (* 0 <= 1 < len2 *)
              let l,_,_ = c2.(1) in
              max h2 (l - 1)
            end
          in
          r.(len1-1) <- (l,h,act1) ;
          for i=1 to len2-1  do
            r.(len1-1+i) <- c2.(i)
          done ;
          r
        else if h1 > l1 then
          let r = Array.make (len1+len2) c1.(0) in
          for i = 0 to len1-2 do
            r.(i) <- c1.(i)
          done ;
          r.(len1-1) <- (l1,l2-1,act1) ;
          for i=0 to len2-1  do
            r.(len1+i) <- c2.(i)
          done ;
          r
        else if h2 > l2 then
          let r = Array.make (len1+len2) c1.(0) in
          for i = 0 to len1-1 do
            r.(i) <- c1.(i)
          done ;
          r.(len1) <- (h1+1,h2,act2) ;
          for i=1 to len2-1  do
            r.(len1+i) <- c2.(i)
          done ;
          r
        else
          Array.append c1 c2


  let coupe_inter i j cases =
    let lcases = Array.length cases in
    let low,_,_ = cases.(i)
    and _,high,_ = cases.(j) in
    low,high,
    Array.sub cases i (j-i+1),
    case_append (Array.sub cases 0 i) (Array.sub cases (j+1) (lcases-(j+1)))

  type kind = Kvalue of int | Kinter of int | Kempty

(*
let pkind chan = function
  | Kvalue i ->Printf.fprintf chan "V%d" i
  | Kinter i -> Printf.fprintf chan "I%d" i
  | Kempty -> Printf.fprintf chan "E"

let rec pkey chan  = function
  | [] -> ()
  | [k] -> pkind chan k
  | k::rem ->
      Printf.fprintf chan "%a %a" pkey rem pkind k
*)

  let t = Hashtbl.create 17

  let make_key  cases =
    let seen = ref []
    and count = ref 0 in
    let rec got_it act = function
      | [] ->
          seen := (act,!count):: !seen ;
          let r = !count in
          incr count ;
          r
      | (act0,index) :: rem ->
          if act0 = act then
            index
          else
            got_it act rem in

    let make_one l h act =
      if l=h then
        Kvalue (got_it act !seen)
      else
        Kinter (got_it act !seen) in

    let rec make_rec i pl =
      if i < 0 then
        []
      else
        let l,h,act = cases.(i) in
        if pl = h+1 then
          make_one l h act::make_rec (i-1) l
        else
          Kempty::make_one l h act::make_rec (i-1) l in

    let l,h,act = cases.(Array.length cases-1) in
    make_one l h act::make_rec (Array.length cases-2) l


  let same_act t =
    let len = Array.length t in
    let a = get_act t (len-1) in
    let rec do_rec i =
      if i < 0 then true
      else
        let b = get_act t i in
        b=a && do_rec (i-1) in
    do_rec (len-2)


(*
  Interval test x in [l,h] works by checking x-l in [0,h-l]
   * This may be false for arithmetic modulo 2^31
   * Subtracting l may change the relative ordering of values
     and invalid the invariant that matched values are given in
     increasing order

   To avoid this, interval check is allowed only when the
   integers indeed present in the whole case interval are
   in [-2^16 ; 2^16]

   This condition is checked by zyva
*)

  let inter_limit = 1 lsl 16

  let ok_inter = ref false

  (* Compute a good test sequence. *)
  let rec opt_count cases =
    let key = make_key cases in
    try
      Hashtbl.find t key
    with
    | Not_found ->
        let r =
          let lcases = Array.length cases in
          match lcases with
          | 0 -> assert false
          | _ when same_act cases -> No, ({n=0; ni=0},{n=0; ni=0})
          | _ ->
              if lcases < small_size_limit then
                enum cases
              else if lcases < medium_size_limit then
                heuristic cases
              else
                divide cases in
        Hashtbl.add t key r ;
        r

  (* Large inputs: dichotomic sequence. *)
  and divide cases =
    let lcases = Array.length cases in
    let m = lcases/2 in
    let _,left,right = coupe cases m in
    let ci = {n=1 ; ni=0}
    and cm = {n=1 ; ni=0}
    and _,(cml,cleft) = opt_count left
    and _,(cmr,cright) = opt_count right in
    add_test ci cleft ;
    add_test ci cright ;
    (* To compute a worst-case cost, we add the more costly of the
       left/right branches to the running total. *)
    if less_tests cml cmr then
      add_test cm cmr
    else
      add_test cm cml ;
    Sep m,(cm, ci)

  (* Medium-size inputs: dichotomy or interval tests. *)
  and heuristic cases =
    let lcases = Array.length cases in

    let sep,csep = divide cases

    and inter,cinter =
      if !ok_inter then begin
        let _,_,act0 = cases.(0)
        and _,_,act1 = cases.(lcases-1) in
        if act0 = act1 then begin
          let low, high, inside, outside = coupe_inter 1 (lcases-2) cases in
          let _,(cmi,cinside) = opt_count inside
          and _,(cmo,coutside) = opt_count outside
          and cmij = {n=1 ; ni=(if low=high then 0 else 1)}
          and cij = {n=1 ; ni=(if low=high then 0 else 1)} in
          add_test cij cinside ;
          add_test cij coutside ;
          if less_tests cmi cmo then
            add_test cmij cmo
          else
            add_test cmij cmi ;
          Inter (1,lcases-2),(cmij,cij)
        end else
          Inter (-1,-1),(too_much, too_much)
      end else
        Inter (-1,-1),(too_much, too_much) in
    if less2tests csep cinter then
      sep,csep
    else
      inter,cinter

  (* Small inputs: exhaustive search for optimal sequence. *)
  and enum cases =
    let lcases = Array.length cases in
    let lim, with_sep =
      let best = ref (-1) and best_cost = ref (too_much,too_much) in

      for i = 1 to lcases-(1) do
        let _,left,right = coupe cases i in
        let ci = {n=1 ; ni=0}
        and cm = {n=1 ; ni=0}
        and _,(cml,cleft) = opt_count left
        and _,(cmr,cright) = opt_count right in
        add_test ci cleft ;
        add_test ci cright ;
        if less_tests cml cmr then
          add_test cm cmr
        else
          add_test cm cml ;

        if
          less2tests (cm,ci) !best_cost
        then begin
          best := i ;
          best_cost := (cm,ci)
        end
      done ;
      !best, !best_cost in

    let ilow, ihigh, with_inter =
      if not !ok_inter then
        let rlow = ref (-1) and rhigh = ref (-1)
        and best_cost= ref (too_much,too_much) in
        for i=1 to lcases-2 do
          let low, high, inside, outside = coupe_inter i i cases in
          if low=high then begin
            let _,(cmi,cinside) = opt_count inside
            and _,(cmo,coutside) = opt_count outside
            and cmij = {n=1 ; ni=0}
            and cij = {n=1 ; ni=0} in
            add_test cij cinside ;
            add_test cij coutside ;
            if less_tests cmi cmo then
              add_test cmij cmo
            else
              add_test cmij cmi ;
            if less2tests (cmij,cij) !best_cost then begin
              rlow := i ;
              rhigh := i ;
              best_cost := (cmij,cij)
            end
          end
        done ;
        !rlow, !rhigh, !best_cost
      else
        let rlow = ref (-1) and rhigh = ref (-1)
        and best_cost= ref (too_much,too_much) in
        for i=1 to lcases-2 do
          for j=i to lcases-2 do
            let low, high, inside, outside = coupe_inter i j cases in
            let _,(cmi,cinside) = opt_count inside
            and _,(cmo,coutside) = opt_count outside
            and cmij = {n=1 ; ni=(if low=high then 0 else 1)}
            and cij = {n=1 ; ni=(if low=high then 0 else 1)} in
            add_test cij cinside ;
            add_test cij coutside ;
            if less_tests cmi cmo then
              add_test cmij cmo
            else
              add_test cmij cmi ;
            if less2tests (cmij,cij) !best_cost then begin
              rlow := i ;
              rhigh := j ;
              best_cost := (cmij,cij)
            end
          done
        done ;
        !rlow, !rhigh, !best_cost in
    let r = ref (Inter (ilow,ihigh)) and rc = ref with_inter in
    if less2tests with_sep !rc then begin
      r := Sep lim ; rc := with_sep
    end ;
    !r, !rc

  (* Consider the following sequence of interval tests:

       if a in [2; 10] then
         if a in [2; 4] then act24
         else if a in [5; 8] then act58
         else act810
       else act_default

     Our interval check works by subtracting the interval lower
     bound, then checking a range [0; n] using an unsigned
     comparison. Naively we would generate code with one subtraction
     to [a] before each comparison:

       let tmp1 = a - 2 in
       if tmp1 <=u 8 then
         let tmp2 = a - 2 in
         if tmp2 <=u 2 then act24
         else
           let tmp3 = a - 5 in
           if tmp3 <=u 3 then act58
           else act810
       else act_default

     but we can avoid some substractions by working with the result
     of the first subtraction, instead of the original index [a],
     inside the interval.

       let a2 = a - 2 in
       if a2 <=u 8 then
         if a2 in <=u 2 then act24
         else
           let a5 = a2 - 3 in
           if a5 <=u 3 then act58
           else act810
       else act_default

     The type [t_ctx] represents an input argument "shifted" by a certain
     (negative) offset by repeated substractions.

     In the example above, [a5] would be represented with [off = -5].
  *)
  type 'a t_ctx =  {off : int ; arg : 'a}

  let make_if_test test arg i ifso ifnot =
    Arg.make_if
      (Arg.make_prim test [arg ; Arg.make_const i])
      ifso ifnot

  let make_if_lt arg i  ifso ifnot = match i with
    | 1 ->
        make_if_test Arg.leint arg 0 ifso ifnot
    | _ ->
        make_if_test Arg.ltint arg i ifso ifnot

  and make_if_ge arg i  ifso ifnot = match i with
    | 1 ->
        make_if_test Arg.gtint arg 0 ifso ifnot
    | _ ->
        make_if_test Arg.geint arg i ifso ifnot

  and make_if_eq  arg i ifso ifnot =
    make_if_test Arg.eqint arg i ifso ifnot

  and make_if_ne  arg i ifso ifnot =
    make_if_test Arg.neint arg i ifso ifnot

  let make_if_nonzero arg ifso ifnot =
    Arg.make_if (Arg.make_is_nonzero arg) ifso ifnot

  let make_if_bool arg ifso ifnot =
    Arg.make_if (Arg.arg_as_test arg) ifso ifnot

  let do_make_if_out h arg ifso ifno =
    Arg.make_if (Arg.make_isout h arg) ifso ifno

  let make_if_out ctx l d mk_ifso mk_ifno = match l with
    | 0 ->
        do_make_if_out
          (Arg.make_const d) ctx.arg (mk_ifso ctx) (mk_ifno ctx)
    | _ ->
        Arg.bind
          (Arg.make_offset ctx.arg (-l))
          (fun arg ->
             let ctx = {off= (-l+ctx.off) ; arg=arg} in
             do_make_if_out
               (Arg.make_const d) arg (mk_ifso ctx) (mk_ifno ctx))

  let do_make_if_in h arg ifso ifno =
    Arg.make_if (Arg.make_isin h arg) ifso ifno

  let make_if_in ctx l d mk_ifso mk_ifno = match l with
    | 0 ->
        do_make_if_in
          (Arg.make_const d) ctx.arg (mk_ifso ctx) (mk_ifno ctx)
    | _ ->
        Arg.bind
          (Arg.make_offset ctx.arg (-l))
          (fun arg ->
             let ctx = {off= (-l+ctx.off) ; arg=arg} in
             do_make_if_in
               (Arg.make_const d) arg (mk_ifso ctx) (mk_ifno ctx))

  (* Generate the code for a good test sequence. *)
  let rec c_test ctx ({cases=cases ; actions=actions} as s) =
    let lcases = Array.length cases in
    assert(lcases > 0) ;
    if lcases = 1 then
      actions.(get_act cases 0) ctx

    else begin

      let w,_c = opt_count cases in
(*
  Printf.fprintf stderr
  "off=%d tactic=%a for %a\n"
  ctx.off pret w pcases cases ;
  *)
      match w with
      | No -> actions.(get_act cases 0) ctx
      | Inter (i,j) ->
          let low,high,inside, outside = coupe_inter i j cases in
          let _,(cinside,_) = opt_count inside
          and _,(coutside,_) = opt_count outside in
          (* Costs are retrieved to put the code with more remaining tests
             in the privileged (positive) branch of ``if'' *)
          if low=high then begin
            if less_tests coutside cinside then
              make_if_eq
                ctx.arg
                (low+ctx.off)
                (c_test ctx {s with cases=inside})
                (c_test ctx {s with cases=outside})
            else
              make_if_ne
                ctx.arg
                (low+ctx.off)
                (c_test ctx {s with cases=outside})
                (c_test ctx {s with cases=inside})
          end else begin
            if less_tests coutside cinside then
              make_if_in
                ctx
                (low+ctx.off)
                (high-low)
                (fun ctx -> c_test ctx {s with cases=inside})
                (fun ctx -> c_test ctx {s with cases=outside})
            else
              make_if_out
                ctx
                (low+ctx.off)
                (high-low)
                (fun ctx -> c_test ctx {s with cases=outside})
                (fun ctx -> c_test ctx {s with cases=inside})
          end
      | Sep i ->
          let lim,left,right = coupe cases i in
          let _,(cleft,_) = opt_count left
          and _,(cright,_) = opt_count right in
          let left = {s with cases=left}
          and right = {s with cases=right} in

          if i=1 && (lim+ctx.off)=1 && get_low cases 0+ctx.off=0 then
            if lcases = 2 && get_high cases 1+ctx.off = 1 then
              make_if_bool
                ctx.arg
                (c_test ctx right) (c_test ctx left)
            else
              make_if_nonzero
                ctx.arg
                (c_test ctx right) (c_test ctx left)
          else if less_tests cright cleft then
            make_if_lt
              ctx.arg (lim+ctx.off)
              (c_test ctx left) (c_test ctx right)
          else
            make_if_ge
              ctx.arg (lim+ctx.off)
              (c_test ctx right) (c_test ctx left)

    end


  (* Minimal density of dense switches. *)
  let theta = 0.33333

  (* Minimal number of tests to make a dense switch. *)
  let switch_min = 3

  (* Particular case 0, 1, 2. *)
  let particular_case cases i j =
    j-i = 2 &&
    (let l1,_h1,act1 = cases.(i)
     and  l2,_h2,_act2 = cases.(i+1)
     and  l3,h3,act3 = cases.(i+2) in
     l1+1=l2 && l2+1=l3 && l3=h3 &&
     act1 <> act3)

  (* Approximation of the test sequence height,
     used to determine cluster density. *)
  let approx_count cases i j =
    let l = j-i+1 in
    if l < small_size_limit then
      (* on small input intervals, use test sequence height *)
      let _,(_,{n=ntests}) = opt_count (Array.sub cases i l) in
      ntests
    else
      (* otherwise use the standard notion of density
         (number of non-default cases) *)
      l-1

  (* Sends back a boolean that says whether it is worth making a jump table. *)
  let dense {cases} i j =
    if i=j then true
    else
      let l,_,_ = cases.(i)
      and _,h,_ = cases.(j) in
      let ntests = approx_count cases i j in
(*
  (ntests+1) >= theta * (h-l+1)
*)
      particular_case cases i j ||
      ((* The switch_min test guarantees that we don't use jump tables
          for very small switches. *)
       ntests >= switch_min &&
       float_of_int ntests +. 1.0 >=
       theta *. (float_of_int h -. float_of_int l +. 1.0))

  (* Compute an optimal clustering by dynamic programming. *)
  let comp_clusters s =
    let len = Array.length s.cases in
    let min_clusters = Array.make len max_int
    and k = Array.make len 0 in
    let get_min i = if i < 0 then 0 else min_clusters.(i) in

    for i = 0 to len-1 do
      for j = 0 to i do
        if
          dense s j i &&
          get_min (j-1) + 1 < min_clusters.(i)
        then begin
          k.(i) <- j ;
          min_clusters.(i) <- get_min (j-1) + 1
        end
      done ;
    done ;
    min_clusters.(len-1),k

  (* The code to generate a dense switch is provided
     by the functor parameter as Arg.make_switch
     (which will typically use a jump table) *)
  let make_switch loc {cases=cases ; actions=actions} i j =
    (* Assume j > i *)
    let ll,_,_ = cases.(i)
    and _,hh,_ = cases.(j) in
    let tbl = Array.make (hh-ll+1) 0
    and t = Hashtbl.create 17
    and index = ref 0 in
    let get_index act =
      try
        Hashtbl.find t act
      with
      | Not_found ->
          let i = !index in
          incr index ;
          Hashtbl.add t act i ;
          i in

    for k=i to j do
      let l,h,act = cases.(k) in
      let index = get_index act in
      for kk=l-ll to h-ll do
        tbl.(kk) <- index
      done
    done ;
    let acts = Array.make !index actions.(0) in
    Hashtbl.iter
      (fun act i -> acts.(i) <- actions.(act))
      t ;
    (fun ctx ->
       match -ll-ctx.off with
       | 0 -> Arg.make_switch loc ctx.arg tbl acts
       | _ ->
           Arg.bind
             (Arg.make_offset ctx.arg (-ll-ctx.off))
             (fun arg -> Arg.make_switch loc arg tbl acts))

  (* Generate code from a clustering choice. *)
  let make_clusters loc ({cases=cases ; actions=actions} as s) n_clusters k =
    let len = Array.length cases in
    let r = Array.make n_clusters (0,0,0)
    and t = Hashtbl.create 17
    and index = ref 0
    and bidon = ref (Array.length actions) in
    let get_index act =
      try
        let i,_ = Hashtbl.find t act in
        i
      with
      | Not_found ->
          let i = !index in
          incr index ;
          Hashtbl.add
            t act
            (i,(fun _ -> actions.(act))) ;
          i
    and add_index act =
      let i = !index in
      incr index ;
      incr bidon ;
      Hashtbl.add t !bidon (i,act) ;
      i in

    let rec zyva j ir =
      let i = k.(j) in
      begin if i=j then
          let l,h,act = cases.(i) in
          r.(ir) <- (l,h,get_index act)
        else (* assert i < j *)
          let l,_,_ = cases.(i)
          and _,h,_ = cases.(j) in
          r.(ir) <- (l,h,add_index (make_switch loc s i j))
      end ;
      if i > 0 then zyva (i-1) (ir-1) in

    zyva (len-1) (n_clusters-1) ;
    let acts = Array.make !index (fun _ -> assert false) in
    Hashtbl.iter (fun _ (i,act) -> acts.(i) <- act) t ;
    {cases = r ; actions = acts}


  let do_zyva loc (low,high) arg cases actions =
    let old_ok = !ok_inter in
    ok_inter := (abs low <= inter_limit && abs high <= inter_limit) ;
    if !ok_inter <> old_ok then Hashtbl.clear t ;

    let s = {cases=cases ; actions=actions} in

(*
  Printf.eprintf "ZYVA: %B [low=%i,high=%i]\n" !ok_inter low high ;
  pcases stderr cases ;
  prerr_endline "" ;
*)
    let n_clusters,k = comp_clusters s in
    let clusters = make_clusters loc s n_clusters k in
    c_test {arg=arg ; off=0} clusters

  let abstract_shared actions =
    let handlers = ref (fun x -> x) in
    let actions =
      Array.map
        (fun act -> match  act with
           | Single act -> act
           | Shared act ->
               let i,h = Arg.make_catch act in
               let oh = !handlers in
               handlers := (fun act -> h (oh act)) ;
               Arg.make_exit i)
        actions in
    !handlers,actions

  (* Standard entry point. *)
  let zyva loc lh arg cases actions =
    assert (Array.length cases > 0) ;
    let actions = actions.act_get_shared () in
    let hs,actions = abstract_shared actions in
    hs (do_zyva loc lh arg cases actions)

  (* Generate code using test sequences only, not Arg.make_switch *)
  and test_sequence arg cases actions =
    assert (Array.length cases > 0) ;
    let actions = actions.act_get_shared () in
    let hs,actions = abstract_shared actions in
    let old_ok = !ok_inter in
    ok_inter := false ;
    if !ok_inter <> old_ok then Hashtbl.clear t ;
    let s =
      {cases=cases ;
       actions=Array.map (fun act -> (fun _ -> act)) actions} in
(*
  Printf.eprintf "SEQUENCE: %B\n" !ok_inter ;
  pcases stderr cases ;
  prerr_endline "" ;
*)
    hs (c_test {arg=arg ; off=0} s)

end
