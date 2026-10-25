(* Step 0 spike of the native JIT (docs/macro-jit-native.md): Cmm written by
   hand, compiled by the OCaml backend in process, assembled by
   [X86_encode], loaded by [Jit_link], and called from OCaml, under GC stress
   and from several domains.

   Run with a small minor heap to stress the GC:
     OCAMLRUNPARAM=s=4k dune exec libs/jit/test/spike.exe *)

open Jit
open Ocaml_backend
open Cmm

let dbg = Debuginfo.none

let var name =
  let v = Backend_var.create_local name in
  v, Backend_var.With_provenance.create v

let int n = Cconst_int (n, dbg)
let tagged n = int ((2 * n) + 1)
let op o args = Cop (o, args, dbg)
let ifte c a b = Cifthenelse (c, dbg, a, dbg, b, dbg)
let field e i =
  op (Cload { memory_chunk = Word_val; mutability = Mutable; is_atomic = false })
    [ if i = 0 then e else op Cadda [ e; int (8 * i) ] ]

let fundecl name args body =
  Cfunction {
    fun_name = name;
    fun_args = List.map (fun wp -> wp, typ_val) args;
    fun_body = body;
    fun_codegen_options = [];
    fun_poll = Lambda.Default_poll;
    fun_dbg = dbg;
  }

(* Static closure of an arity-1 function *)
let closure ~clos ~code =
  Cdata [
    Cint (Cmm_helpers.black_closure_header 2);
    Cglobal_symbol clos;
    Cdefine_symbol clos;
    Csymbol_address code;
    Cint (Cmm_helpers.closure_info ~arity:1 ~startenv:2);
  ]

(* Application of the closure [f] to one argument *)
let apply1 f x =
  op (Capply typ_val) [ field f 0; x; f ]

let program sym =
  let fn name = sym name and clos name = sym (name ^ "_closure") in
  let fib =
    let (n, nw) = var "n" and (_, ew) = var "env" in
    let call e = op (Capply typ_val) [ Cconst_symbol (fn "fib", dbg); e; Cconst_symbol (clos "fib", dbg) ] in
    fundecl (fn "fib") [ nw; ew ]
      (ifte (op (Ccmpi Clt) [ Cvar n; tagged 2 ])
         (Cvar n)
         (op Csubi [
             op Caddi [ call (op Csubi [ Cvar n; int 2 ]); call (op Csubi [ Cvar n; int 4 ]) ];
             int 1 ]))
  in
  (* mklist n = [(n, n); (n-1, n-1); ...; (1, 1)] *)
  let mklist =
    let (n, nw) = var "n" and (_, ew) = var "env" in
    let (t, tw) = var "t" in
    fundecl (fn "mklist") [ nw; ew ]
      (ifte (op (Ccmpi Ceq) [ Cvar n; tagged 0 ])
         (tagged 0)
         (Clet (tw, Cmm_helpers.make_alloc dbg 0 [ Cvar n; Cvar n ],
                Cmm_helpers.make_alloc dbg 0 [
                  Cvar t;
                  op (Capply typ_val) [
                    Cconst_symbol (fn "mklist", dbg);
                    op Csubi [ Cvar n; int 2 ];
                    Cconst_symbol (clos "mklist", dbg) ] ])))
  in
  (* callcb (f, x) = f x *)
  let callcb =
    let (p, pw) = var "p" and (_, ew) = var "env" in
    fundecl (fn "callcb") [ pw; ew ] (apply1 (field (Cvar p) 0) (field (Cvar p) 1))
  in
  (* trycb (f, x) = try f x with _ -> -1 *)
  let trycb =
    let (p, pw) = var "p" and (_, ew) = var "env" and (_, xw) = var "exn" in
    fundecl (fn "trycb") [ pw; ew ]
      (Ctrywith (apply1 (field (Cvar p) 0) (field (Cvar p) 1), xw, tagged (-1), dbg))
  in
  (* raisenf _ = raise Not_found *)
  let raisenf =
    let (x, xw) = var "x" and (_, ew) = var "env" in
    fundecl (fn "raisenf") [ xw; ew ]
      (op (Craise Lambda.Raise_regular) [ Cconst_symbol ("caml_exn_Not_found", dbg) ])
  in
  (* usehelper x = helpers.(0) x, the helper being set from OCaml *)
  let usehelper =
    let (x, xw) = var "x" and (_, ew) = var "env" in
    fundecl (fn "usehelper") [ xw; ew ]
      (apply1 (field (Cconst_symbol (sym "helpers", dbg)) 0) (Cvar x))
  in
  let roots = Cdata [
      Cint (Cmm_helpers.black_block_header 0 1);
      Cdefine_symbol (sym "helpers");
      Cint 1n;
      Calign 8;
      Cdefine_symbol (sym "gc_roots");
      Csymbol_address (sym "helpers");
      Cint 0n;
    ]
  in
  let fns = [ "fib", fib; "mklist", mklist; "callcb", callcb; "trycb", trycb;
              "raisenf", raisenf; "usehelper", usehelper ] in
  roots
  :: List.concat_map (fun (name, f) -> [ f; closure ~clos:(clos name) ~code:(fn name) ]) fns

let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

let () =
  let t0 = Unix.gettimeofday () in
  let c = Jit_backend.compile program in
  let t1 = Unix.gettimeofday () in
  let a, l = Jit_backend.load ~gc_roots:"gc_roots" c in
  let t2 = Unix.gettimeofday () in
  Printf.printf "compile %.2f ms, assemble+load %.2f ms, text %d bytes, data %d bytes\n%!"
    ((t1 -. t0) *. 1000.) ((t2 -. t1) *. 1000.)
    (Bytes.length a.text.s_bytes) (Bytes.length a.data.s_bytes);
  let get : 'a. string -> 'a = fun name ->
    Obj.obj (Jit_link.value_at (l.lookup (X86_proc.string_of_symbol "" (c.symbol (name ^ "_closure")))))
  in
  let nfib : int -> int = get "fib" in
  let mklist : int -> (int * int) list = get "mklist" in
  let callcb : (int -> int) * int -> int = get "callcb" in
  let trycb : (int -> int) * int -> int = get "trycb" in
  let raisenf : int -> int = get "raisenf" in
  let usehelper : int -> int = get "usehelper" in
  let helpers : Obj.t = Jit_link.value_at (l.lookup (X86_proc.string_of_symbol "" (c.symbol "helpers"))) in
  let check name b = if not b then (Printf.printf "FAIL %s\n%!" name; exit 1) in
  check "fib" (nfib 25 = fib 25);
  let lst = mklist 1000 in
  check "mklist" (List.length lst = 1000 && List.for_all (fun (a, b) -> a = b) lst
                  && fst (List.hd lst) = 1000);
  check "callcb" (callcb ((fun x -> x * 3), 14) = 42);
  check "trycb ok" (trycb ((fun x -> x + 1), 1) = 2);
  check "trycb exn" (trycb ((fun _ -> failwith "boom"), 1) = -1);
  check "raise" (match raisenf 0 with _ -> false | exception Not_found -> true);
  check "callback exn"
    (match callcb ((fun _ -> raise Exit), 0) with _ -> false | exception Exit -> true);
  (* a heap closure stored in the static helper block: it must survive and
     follow the GC *)
  let k = ref 7 in
  Obj.set_field helpers 0 (Obj.repr (fun x -> x + !k));
  Gc.full_major ();
  check "helper" (usehelper 35 = 42);
  print_endline "basic checks ok";
  (* timing *)
  let time name f =
    let t = Unix.gettimeofday () in
    let r = f () in
    Printf.printf "%s: %.1f ms\n%!" name ((Unix.gettimeofday () -. t) *. 1000.);
    r
  in
  let a = time "fib 32 native (JIT)" (fun () -> nfib 32) in
  let b = time "fib 32 ocamlopt" (fun () -> fib 32) in
  check "fib 32" (a = b);
  (* GC stress, several domains *)
  let work d () =
    for i = 1 to 200 do
      let n = 100 + ((i * 37 + d) mod 900) in
      let l = mklist n in
      if i mod 50 = 0 then Gc.full_major ();
      if i mod 70 = 0 then Gc.compact ();
      let s = List.fold_left (fun acc (a, b) -> acc + a + b) 0 l in
      if s <> n * (n + 1) then (Printf.printf "FAIL domain %d: %d\n%!" d s; exit 1);
      if trycb ((fun x -> if x mod 3 = 0 then raise Exit else x), i) <> (if i mod 3 = 0 then -1 else i)
      then (Printf.printf "FAIL trycb domain %d\n%!" d; exit 1);
      if usehelper i <> i + 7 then (Printf.printf "FAIL helper domain %d\n%!" d; exit 1);
      ignore (nfib 15)
    done
  in
  time "4 domains, GC stress" (fun () ->
      let ds = List.init 4 (fun d -> Domain.spawn (work d)) in
      List.iter Domain.join ds);
  print_endline "spike ok"
