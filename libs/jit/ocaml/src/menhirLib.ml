module General = struct
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(* --------------------------------------------------------------------------- *)

(* Lists. *)

let rec take n xs =
  match n, xs with
  | 0, _
  | _, [] ->
      []
  | _, (x :: xs as input) ->
     let xs' = take (n - 1) xs in
     if xs == xs' then
       input
     else
       x :: xs'

let rec drop n xs =
  match n, xs with
  | 0, _ ->
      xs
  | _, [] ->
      []
  | _, _ :: xs ->
      drop (n - 1) xs

let rec uniq1 cmp x ys =
  match ys with
  | [] ->
      []
  | y :: ys ->
      if cmp x y = 0 then
        uniq1 cmp x ys
      else
        y :: uniq1 cmp y ys

let uniq cmp xs =
  match xs with
  | [] ->
      []
  | x :: xs ->
      x :: uniq1 cmp x xs

let weed cmp xs =
  uniq cmp (List.sort cmp xs)

(* --------------------------------------------------------------------------- *)

(* Streams. *)

type 'a stream =
    'a head Lazy.t

and 'a head =
  | Nil
  | Cons of 'a * 'a stream

(* The length of a stream. *)

let rec length xs =
  match Lazy.force xs with
  | Nil ->
      0
  | Cons (_, xs) ->
      1 + length xs

(* Folding over a stream. *)

let rec foldr f xs accu =
  match Lazy.force xs with
  | Nil ->
      accu
  | Cons (x, xs) ->
      f x (foldr f xs accu)
end
module Convert = struct
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(* An ocamlyacc-style, or Menhir-style, parser requires access to
   the lexer, which must be parameterized with a lexing buffer, and
   to the lexing buffer itself, where it reads position information. *)

(* This traditional API is convenient when used with ocamllex, but
   inelegant when used with other lexer generators. *)

type ('token, 'semantic_value) traditional =
    (Lexing.lexbuf -> 'token) -> Lexing.lexbuf -> 'semantic_value

(* This revised API is independent of any lexer generator. Here, the
   parser only requires access to the lexer, and the lexer takes no
   parameters. The tokens returned by the lexer may contain position
   information. *)

type ('token, 'semantic_value) revised =
    (unit -> 'token) -> 'semantic_value

(* --------------------------------------------------------------------------- *)

(* Converting a traditional parser, produced by ocamlyacc or Menhir,
   into a revised parser. *)

(* A token of the revised lexer is essentially a triple of a token
   of the traditional lexer (or raw token), a start position, and
   and end position. The three [get] functions are accessors. *)

(* We do not require the type ['token] to actually be a triple type.
   This enables complex applications where it is a record type with
   more than three fields. It also enables simple applications where
   positions are of no interest, so ['token] is just ['raw_token]
   and [get_startp] and [get_endp] return dummy positions. *)

let traditional2revised
  (get_raw_token : 'token -> 'raw_token)
  (get_startp    : 'token -> Lexing.position)
  (get_endp      : 'token -> Lexing.position)
  (parser : ('raw_token, 'semantic_value) traditional)
: ('token, 'semantic_value) revised =

  (* Accept a revised lexer. *)

  fun (lexer : unit -> 'token) ->

    (* Create a dummy lexing buffer. *)

    let lexbuf : Lexing.lexbuf =
      Lexing.from_string ""
    in

    (* Wrap the revised lexer as a traditional lexer. A traditional
       lexer returns a raw token and updates the fields of the lexing
       buffer with new positions, which will be read by the parser. *)

    let lexer (lexbuf : Lexing.lexbuf) : 'raw_token =
      let token : 'token = lexer() in
      lexbuf.Lexing.lex_start_p <- get_startp token;
      lexbuf.Lexing.lex_curr_p <- get_endp token;
      get_raw_token token
    in

    (* Invoke the traditional parser. *)

    parser lexer lexbuf

(* --------------------------------------------------------------------------- *)

(* Converting a revised parser back to a traditional parser. *)

let revised2traditional
  (make_token : 'raw_token -> Lexing.position -> Lexing.position -> 'token)
  (parser : ('token, 'semantic_value) revised)
: ('raw_token, 'semantic_value) traditional =

  (* Accept a traditional lexer and a lexing buffer. *)

  fun (lexer : Lexing.lexbuf -> 'raw_token) (lexbuf : Lexing.lexbuf) ->

    (* Wrap the traditional lexer as a revised lexer. *)

    let lexer () : 'token =
      let token : 'raw_token = lexer lexbuf in
      make_token token lexbuf.Lexing.lex_start_p lexbuf.Lexing.lex_curr_p
    in

    (* Invoke the revised parser. *)

    parser lexer

(* --------------------------------------------------------------------------- *)

(* Simplified versions of the above, where concrete triples are used. *)

module Simplified = struct

  let traditional2revised parser =
    traditional2revised
      (fun (token, _, _)  -> token)
      (fun (_, startp, _) -> startp)
      (fun (_, _, endp)   -> endp)
      parser

  let revised2traditional parser =
    revised2traditional
      (fun token startp endp -> (token, startp, endp))
      parser

end
end
module IncrementalEngine = struct
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

type position = Lexing.position

open General

(* This signature describes the incremental LR engine. *)

(* In this mode, the user controls the lexer, and the parser suspends
   itself when it needs to read a new token. *)

module type INCREMENTAL_ENGINE = sig

  type token

  (* A value of type [production] is (an index for) a production. The start
     productions (which do not exist in an \mly file, but are constructed by
     Menhir internally) are not part of this type. *)

  type production

  (* The type ['a checkpoint] represents an intermediate or final state of the
     parser. An intermediate checkpoint is a suspension: it records the parser's
     current state, and allows parsing to be resumed. The parameter ['a] is
     the type of the semantic value that will eventually be produced if the
     parser succeeds. *)

  (* [Accepted] and [Rejected] are final checkpoints. [Accepted] carries a
     semantic value. *)

  (* [InputNeeded] is an intermediate checkpoint. It means that the parser wishes
     to read one token before continuing. *)

  (* [Shifting] is an intermediate checkpoint. It means that the parser is taking
     a shift transition. It exposes the state of the parser before and after
     the transition. The Boolean parameter tells whether the parser intends to
     request a new token after this transition. (It always does, except when
     it is about to accept.) *)

  (* [AboutToReduce] is an intermediate checkpoint. It means that the parser is
     about to perform a reduction step. It exposes the parser's current
     state as well as the production that is about to be reduced. *)

  (* [HandlingError] is an intermediate checkpoint. It means that the parser has
     detected an error and is currently handling it, in several steps. *)

  (* A value of type ['a env] represents a configuration of the automaton:
     current state, stack, lookahead token, etc. The parameter ['a] is the
     type of the semantic value that will eventually be produced if the parser
     succeeds. *)

  (* In normal operation, the parser works with checkpoints: see the functions
     [offer] and [resume]. However, it is also possible to work directly with
     environments (see the functions [pop], [force_reduction], and [feed]) and
     to reconstruct a checkpoint out of an environment (see [input_needed]).
     This is considered advanced functionality; its purpose is to allow error
     recovery strategies to be programmed by the user. *)

  type 'a env

  type 'a checkpoint = private
    | InputNeeded of 'a env
    | Shifting of 'a env * 'a env * bool
    | AboutToReduce of 'a env * production
    | HandlingError of 'a env
    | Accepted of 'a
    | Rejected

  (* [offer] allows the user to resume the parser after it has suspended
     itself with a checkpoint of the form [InputNeeded env]. [offer] expects
     the old checkpoint as well as a new token and produces a new checkpoint.
     It does not raise any exception. *)

  val offer:
    'a checkpoint ->
    token * position * position ->
    'a checkpoint

  (* [resume] allows the user to resume the parser after it has suspended
     itself with a checkpoint of the form [Shifting _], [AboutToReduce _], or
     [HandlingError _]. [resume] expects the old checkpoint and produces a
     new checkpoint. It does not raise any exception. *)

  (* The optional argument [strategy] influences the manner in which [resume]
     deals with checkpoints of the form [HandlingError _]. Its default value
     is [`Legacy]. It can be briefly described as follows:

     - If the [error] token is used only to report errors (that is, if the
       [error] token appears only at the end of a production, whose semantic
       action raises an exception) then the simplified strategy should be
       preferred. (This includes the case where the [error] token does not
       appear at all in the grammar.)

     - If the [error] token is used to recover after an error, or if
       perfect backward compatibility is required, the legacy strategy
       should be selected.

     More details on these strategies appear in the file [Engine.ml]. *)

  type strategy =
    [ `Legacy | `Simplified ]

  val resume:
    ?strategy:strategy ->
    'a checkpoint ->
    'a checkpoint

  (* A token supplier is a function of no arguments which delivers a new token
     (together with its start and end positions) every time it is called. *)

  type supplier =
    unit -> token * position * position

  (* A pair of a lexer and a lexing buffer can be easily turned into a
     supplier. *)

  val lexer_lexbuf_to_supplier:
    (Lexing.lexbuf -> token) ->
    Lexing.lexbuf ->
    supplier

  (* The functions [offer] and [resume] are sufficient to write a parser loop.
     One can imagine many variations (which is why we expose these functions
     in the first place!). Here, we expose a few variations of the main loop,
     ready for use. *)

  (* [loop supplier checkpoint] begins parsing from [checkpoint], reading
     tokens from [supplier]. It continues parsing until it reaches a
     checkpoint of the form [Accepted v] or [Rejected]. In the former case, it
     returns [v]. In the latter case, it raises the exception [Error].
     The optional argument [strategy], whose default value is [Legacy],
     is passed to [resume] and influences the error-handling strategy. *)

  val loop: ?strategy:strategy -> supplier -> 'a checkpoint -> 'a

  (* [loop_handle succeed fail supplier checkpoint] begins parsing from
     [checkpoint], reading tokens from [supplier]. It continues parsing until
     it reaches a checkpoint of the form [Accepted v] or [HandlingError env]
     (or [Rejected], but that should not happen, as [HandlingError _] will be
     observed first). In the former case, it calls [succeed v]. In the latter
     case, it calls [fail] with this checkpoint. It cannot raise [Error].

     This means that Menhir's error-handling procedure does not get a chance
     to run. For this reason, there is no [strategy] parameter. Instead, the
     user can implement her own error handling code, in the [fail]
     continuation. *)

  val loop_handle:
    ('a -> 'answer) ->
    ('a checkpoint -> 'answer) ->
    supplier -> 'a checkpoint -> 'answer

  (* [loop_handle_undo] is analogous to [loop_handle], except it passes a pair
     of checkpoints to the failure continuation.

     The first (and oldest) checkpoint is the last [InputNeeded] checkpoint that
     was encountered before the error was detected. The second (and newest)
     checkpoint is where the error was detected, as in [loop_handle]. Going back
     to the first checkpoint can be thought of as undoing any reductions that
     were performed after seeing the problematic token. (These reductions must
     be default reductions or spurious reductions.)

     [loop_handle_undo] must initially be applied to an [InputNeeded] checkpoint.
     The parser's initial checkpoints satisfy this constraint. *)

  val loop_handle_undo:
    ('a -> 'answer) ->
    ('a checkpoint -> 'a checkpoint -> 'answer) ->
    supplier -> 'a checkpoint -> 'answer

  (* [shifts checkpoint] assumes that [checkpoint] has been obtained by
     submitting a token to the parser. It runs the parser from [checkpoint],
     through an arbitrary number of reductions, until the parser either
     accepts this token (i.e., shifts) or rejects it (i.e., signals an error).
     If the parser decides to shift, then [Some env] is returned, where [env]
     is the parser's state just before shifting. Otherwise, [None] is
     returned. *)

  (* It is desirable that the semantic actions be side-effect free, or that
     their side-effects be harmless (replayable). *)

  val shifts: 'a checkpoint -> 'a env option

  (* The function [acceptable] allows testing, after an error has been
     detected, which tokens would have been accepted at this point. It is
     implemented using [shifts]. Its argument should be an [InputNeeded]
     checkpoint. *)

  (* For completeness, one must undo any spurious reductions before carrying out
     this test -- that is, one must apply [acceptable] to the FIRST checkpoint
     that is passed by [loop_handle_undo] to its failure continuation. *)

  (* This test causes some semantic actions to be run! The semantic actions
     should be side-effect free, or their side-effects should be harmless. *)

  (* The position [pos] is used as the start and end positions of the
     hypothetical token, and may be picked up by the semantic actions. We
     suggest using the position where the error was detected. *)

  val acceptable: 'a checkpoint -> token -> position -> bool

  (* The abstract type ['a lr1state] describes the non-initial states of the
     LR(1) automaton. The index ['a] represents the type of the semantic value
     associated with this state's incoming symbol. *)

  type 'a lr1state

  (* The states of the LR(1) automaton are numbered (from 0 and up). *)

  val number: _ lr1state -> int

  (* Productions are numbered. *)

  (* [find_production i] requires the index [i] to be valid. Use with care. *)

  val production_index: production -> int
  val find_production: int -> production

  (* An element is a pair of a non-initial state [s] and a semantic value [v]
     associated with the incoming symbol of this state. The idea is, the value
     [v] was pushed onto the stack just before the state [s] was entered. Thus,
     for some type ['a], the state [s] has type ['a lr1state] and the value [v]
     has type ['a]. In other words, the type [element] is an existential type. *)

  type element =
    | Element: 'a lr1state * 'a * position * position -> element

  (* The parser's stack is (or, more precisely, can be viewed as) a stream of
     elements. The type [stream] is defined by the module [General]. *)

  (* As of 2017/03/31, the types [stream] and [stack] and the function [stack]
     are DEPRECATED. They might be removed in the future. An alternative way
     of inspecting the stack is via the functions [top] and [pop]. *)

  type stack = (* DEPRECATED *)
    element stream

  (* This is the parser's stack, a stream of elements. This stream is empty if
     the parser is in an initial state; otherwise, it is non-empty.  The LR(1)
     automaton's current state is the one found in the top element of the
     stack. *)

  val stack: 'a env -> stack (* DEPRECATED *)

  (* [top env] returns the parser's top stack element. The state contained in
     this stack element is the current state of the automaton. If the stack is
     empty, [None] is returned. In that case, the current state of the
     automaton must be an initial state. *)

  val top: 'a env -> element option

  (* [pop_many i env] pops [i] cells off the automaton's stack. This is done
     via [i] successive invocations of [pop]. Thus, [pop_many 1] is [pop]. The
     index [i] must be nonnegative. The time complexity is O(i). *)

  val pop_many: int -> 'a env -> 'a env option

  (* [get i env] returns the parser's [i]-th stack element. The index [i] is
     0-based: thus, [get 0] is [top]. If [i] is greater than or equal to the
     number of elements in the stack, [None] is returned. The time complexity
     is O(i). *)

  val get: int -> 'a env -> element option

  (* [current_state_number env] is (the integer number of) the automaton's
     current state. This works even if the automaton's stack is empty, in
     which case the current state is an initial state. This number can be
     passed as an argument to a [message] function generated by [menhir
     --compile-errors]. *)

  val current_state_number: 'a env -> int

  (* [equal env1 env2] tells whether the parser configurations [env1] and
     [env2] are equal in the sense that the automaton's current state is the
     same in [env1] and [env2] and the stack is *physically* the same in
     [env1] and [env2]. If [equal env1 env2] is [true], then the sequence of
     the stack elements, as observed via [pop] and [top], must be the same in
     [env1] and [env2]. Also, if [equal env1 env2] holds, then the checkpoints
     [input_needed env1] and [input_needed env2] must be equivalent. The
     function [equal] has time complexity O(1). *)

  val equal: 'a env -> 'a env -> bool

  (* These are the start and end positions of the current lookahead token. If
     invoked in an initial state, this function returns a pair of twice the
     initial position. *)

  val positions: 'a env -> position * position

  (* When applied to an environment taken from a checkpoint of the form
     [AboutToReduce (env, prod)], the function [env_has_default_reduction]
     tells whether the reduction that is about to take place is a default
     reduction. *)

  val env_has_default_reduction: 'a env -> bool

  (* [state_has_default_reduction s] tells whether the state [s] has a default
     reduction. This includes the case where [s] is an accepting state. *)

  val state_has_default_reduction: _ lr1state -> bool

  (* [pop env] returns a new environment, where the parser's top stack cell
     has been popped off. (If the stack is empty, [None] is returned.) This
     amounts to pretending that the (terminal or nonterminal) symbol that
     corresponds to this stack cell has not been read. *)

  val pop: 'a env -> 'a env option

  (* [force_reduction prod env] should be called only if in the state [env]
     the parser is capable of reducing the production [prod]. If this
     condition is satisfied, then this production is reduced, which means that
     its semantic action is executed (this can have side effects!) and the
     automaton makes a goto (nonterminal) transition. If this condition is not
     satisfied, [Invalid_argument _] is raised. *)

  val force_reduction: production -> 'a env -> 'a env

  (* [input_needed env] returns [InputNeeded env]. That is, out of an [env]
     that might have been obtained via a series of calls to the functions
     [pop], [force_reduction], [feed], etc., it produces a checkpoint, which
     can be used to resume normal parsing, by supplying this checkpoint as an
     argument to [offer]. *)

  (* This function should be used with some care. It could "mess up the
     lookahead" in the sense that it allows parsing to resume in an arbitrary
     state [s] with an arbitrary lookahead symbol [t], even though Menhir's
     reachability analysis (menhir --list-errors) might well think that it is
     impossible to reach this particular configuration. If one is using
     Menhir's new error reporting facility, this could cause the parser to
     reach an error state for which no error message has been prepared. *)

  val input_needed: 'a env -> 'a checkpoint

end

(* This signature is a fragment of the inspection API that is made available
   to the user when [--inspection] is used. This fragment contains type
   definitions for symbols. *)

module type SYMBOLS = sig

  (* The type ['a terminal] represents a terminal symbol. The type ['a
     nonterminal] represents a nonterminal symbol. In both cases, the index
     ['a] represents the type of the semantic values associated with this
     symbol. The concrete definitions of these types are generated. *)

  type 'a terminal
  type 'a nonterminal

  (* The type ['a symbol] represents a terminal or nonterminal symbol. It is
     the disjoint union of the types ['a terminal] and ['a nonterminal]. *)

  type 'a symbol =
    | T : 'a terminal -> 'a symbol
    | N : 'a nonterminal -> 'a symbol

  (* The type [xsymbol] is an existentially quantified version of the type
     ['a symbol]. This type is useful in situations where the index ['a]
     is not statically known. *)

  type xsymbol =
    | X : 'a symbol -> xsymbol

end

(* This signature describes the inspection API that is made available to the
   user when [--inspection] is used. *)

module type INSPECTION = sig

  (* The types of symbols are described above. *)

  include SYMBOLS

  (* The type ['a lr1state] is meant to be the same as in [INCREMENTAL_ENGINE]. *)

  type 'a lr1state

  (* The type [production] is meant to be the same as in [INCREMENTAL_ENGINE].
     It represents a production of the grammar. A production can be examined
     via the functions [lhs] and [rhs] below. *)

  type production

  (* An LR(0) item is a pair of a production [prod] and a valid index [i] into
     this production. That is, if the length of [rhs prod] is [n], then [i] is
     comprised between 0 and [n], inclusive. *)

  type item =
      production * int

  (* Ordering functions. *)

  val compare_terminals: _ terminal -> _ terminal -> int
  val compare_nonterminals: _ nonterminal -> _ nonterminal -> int
  val compare_symbols: xsymbol -> xsymbol -> int
  val compare_productions: production -> production -> int
  val compare_items: item -> item -> int

  (* [incoming_symbol s] is the incoming symbol of the state [s], that is,
     the symbol that the parser must recognize before (has recognized when)
     it enters the state [s]. This function gives access to the semantic
     value [v] stored in a stack element [Element (s, v, _, _)]. Indeed,
     by case analysis on the symbol [incoming_symbol s], one discovers the
     type ['a] of the value [v]. *)

  val incoming_symbol: 'a lr1state -> 'a symbol

  (* [items s] is the set of the LR(0) items in the LR(0) core of the LR(1)
     state [s]. This set is not epsilon-closed. This set is presented as a
     list, in an arbitrary order. *)

  val items: _ lr1state -> item list

  (* [lhs prod] is the left-hand side of the production [prod]. This is
     always a non-terminal symbol. *)

  val lhs: production -> xsymbol

  (* [rhs prod] is the right-hand side of the production [prod]. This is
     a (possibly empty) sequence of (terminal or nonterminal) symbols. *)

  val rhs: production -> xsymbol list

  (* [nullable nt] tells whether the non-terminal symbol [nt] is nullable.
     That is, it is true if and only if this symbol produces the empty
     word [epsilon]. *)

  val nullable: _ nonterminal -> bool

  (* [first nt t] tells whether the FIRST set of the nonterminal symbol [nt]
     contains the terminal symbol [t]. That is, it is true if and only if
     [nt] produces a word that begins with [t]. *)

  val first: _ nonterminal -> _ terminal -> bool

  (* [xfirst] is analogous to [first], but expects a first argument of type
     [xsymbol] instead of [_ terminal]. *)

  val xfirst: xsymbol -> _ terminal -> bool

  (* [foreach_terminal] enumerates the terminal symbols, including [error].
     [foreach_terminal_but_error] enumerates the terminal symbols, excluding
     [error]. *)

  val foreach_terminal:           (xsymbol -> 'a -> 'a) -> 'a -> 'a
  val foreach_terminal_but_error: (xsymbol -> 'a -> 'a) -> 'a -> 'a

  (* The type [env] is meant to be the same as in [INCREMENTAL_ENGINE]. *)

  type 'a env

  (* [feed symbol startp semv endp env] causes the parser to consume the
     (terminal or nonterminal) symbol [symbol], accompanied with the semantic
     value [semv] and with the start and end positions [startp] and [endp].
     Thus, the automaton makes a transition, and reaches a new state. The
     stack grows by one cell. This operation is permitted only if the current
     state (as determined by [env]) has an outgoing transition labeled with
     [symbol]. Otherwise, [Invalid_argument _] is raised. *)

  val feed: 'a symbol -> position -> 'a -> position -> 'b env -> 'b env

end

(* This signature combines the incremental API and the inspection API. *)

module type EVERYTHING = sig

  include INCREMENTAL_ENGINE

  include INSPECTION
    with type 'a lr1state := 'a lr1state
    with type production := production
    with type 'a env := 'a env

end
end
module EngineTypes = struct
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(* This file defines several types and module types that are used in the
   specification of module [Engine]. *)

(* --------------------------------------------------------------------------- *)

(* It would be nice if we could keep the structure of stacks and environments
   hidden. However, stacks and environments must be accessible to semantic
   actions, so the following data structure definitions must be public. *)

(* --------------------------------------------------------------------------- *)

(* A stack is a linked list of cells. A sentinel cell -- which is its own
   successor -- is used to mark the bottom of the stack. The sentinel cell
   itself is not significant -- it contains dummy values. *)

type ('state, 'semantic_value) stack = {

  (* The state that we should go back to if we pop this stack cell. *)

  (* This convention means that the state contained in the top stack cell is
     not the current state [env.current]. It also means that the state found
     within the sentinel is a dummy -- it is never consulted. This convention
     is the same as that adopted by the code-based back-end. *)

  state: 'state;

  (* The semantic value associated with the chunk of input that this cell
     represents. *)

  semv: 'semantic_value;

  (* The start and end positions of the chunk of input that this cell
     represents. *)

  startp: Lexing.position;
  endp: Lexing.position;

  (* The next cell down in the stack. If this is a self-pointer, then this
     cell is the sentinel, and the stack is conceptually empty. *)

  next: ('state, 'semantic_value) stack;

}

(* --------------------------------------------------------------------------- *)

(* A parsing environment contains all of the parser's state (except for the
   current program point). *)

type ('state, 'semantic_value, 'token) env = {

  (* If this flag is true, then the first component of [env.triple] should
     be ignored, as it has been logically overwritten with the [error]
     pseudo-token. *)

  error: bool;

  (* The last token that was obtained from the lexer, together with its start
     and end positions. Warning: before the first call to the lexer has taken
     place, a dummy (and possibly invalid) token is stored here. *)

  triple: 'token * Lexing.position * Lexing.position;

  (* The stack. In [CodeBackend], it is passed around on its own,
     whereas, here, it is accessed via the environment. *)

  stack: ('state, 'semantic_value) stack;

  (* The current state. In [CodeBackend], it is passed around on its
     own, whereas, here, it is accessed via the environment. *)

  current: 'state;

}

(* --------------------------------------------------------------------------- *)

(* A number of logging hooks are used to (optionally) emit logging messages. *)

(* The comments indicate the conventional messages that correspond
   to these hooks in the code-based back-end; see [CodeBackend]. *)

module type LOG = sig

  type state
  type terminal
  type production

  (* State %d: *)

  val state: state -> unit

  (* Shifting (<terminal>) to state <state> *)

  val shift: terminal -> state -> unit

  (* Reducing a production should be logged either as a reduction
     event (for regular productions) or as an acceptance event (for
     start productions). *)

  (* Reducing production <production> / Accepting *)

  val reduce_or_accept: production -> unit

  (* Lookahead token is now <terminal> (<pos>-<pos>) *)

  val lookahead_token: terminal -> Lexing.position -> Lexing.position -> unit

  (* Initiating error handling *)

  val initiating_error_handling: unit -> unit

  (* Resuming error handling *)

  val resuming_error_handling: unit -> unit

  (* Handling error in state <state> *)

  val handling_error: state -> unit

end

(* --------------------------------------------------------------------------- *)

(* This signature describes the parameters that must be supplied to the LR
   engine. *)

module type TABLE = sig

  (* The type of automaton states. *)

  type state

  (* States are numbered. *)

  val number: state -> int

  (* The type of tokens. These can be thought of as real tokens, that is,
     tokens returned by the lexer. They carry a semantic value. This type
     does not include the [error] pseudo-token. *)

  type token

  (* The type of terminal symbols. These can be thought of as integer codes.
     They do not carry a semantic value. This type does include the [error]
     pseudo-token. *)

  type terminal

  (* The type of nonterminal symbols. *)

  type nonterminal

  (* The type of semantic values. *)

  type semantic_value

  (* A token is conceptually a pair of a (non-[error]) terminal symbol and
     a semantic value. The following two functions are the pair projections. *)

  val token2terminal: token -> terminal
  val token2value: token -> semantic_value

  (* Even though the [error] pseudo-token is not a real token, it is a
     terminal symbol. Furthermore, for regularity, it must have a semantic
     value. *)

  val error_terminal: terminal
  val error_value: semantic_value

  (* [foreach_terminal] allows iterating over all terminal symbols. *)

  val foreach_terminal: (terminal -> 'a -> 'a) -> 'a -> 'a

  (* The type of productions. *)

  type production

  val production_index: production -> int
  val find_production: int -> production

  (* If a state [s] has a default reduction on production [prod], then, upon
     entering [s], the automaton should reduce [prod] without consulting the
     lookahead token. The following function allows determining which states
     have default reductions. *)

  (* Instead of returning a value of a sum type -- either [DefRed prod], or
     [NoDefRed] -- it accepts two continuations, and invokes just one of
     them. This mechanism allows avoiding a memory allocation. *)

  val default_reduction:
    state ->
    ('env -> production -> 'answer) ->
    ('env -> 'answer) ->
    'env -> 'answer

  (* An LR automaton can normally take three kinds of actions: shift, reduce,
     or fail. (Acceptance is a particular case of reduction: it consists in
     reducing a start production.) *)

  (* There are two variants of the shift action. [shift/discard s] instructs
     the automaton to discard the current token, request a new one from the
     lexer, and move to state [s]. [shift/nodiscard s] instructs it to move to
     state [s] without requesting a new token. This instruction should be used
     when [s] has a default reduction on [#]. See [CodeBackend.gettoken] for
     details. *)

  (* This is the automaton's action table. It maps a pair of a state and a
     terminal symbol to an action. *)

  (* Instead of returning a value of a sum type -- one of shift/discard,
     shift/nodiscard, reduce, or fail -- this function accepts three
     continuations, and invokes just one them. This mechanism allows avoiding
     a memory allocation. *)

  (* In summary, the parameters to [action] are as follows:

     - the first two parameters, a state and a terminal symbol, are used to
       look up the action table;

     - the next parameter is the semantic value associated with the above
       terminal symbol; it is not used, only passed along to the shift
       continuation, as explained below;

     - the shift continuation expects an environment; a flag that tells
       whether to discard the current token; the terminal symbol that
       is being shifted; its semantic value; and the target state of
       the transition;

     - the reduce continuation expects an environment and a production;

     - the fail continuation expects an environment;

     - the last parameter is the environment; it is not used, only passed
       along to the selected continuation. *)

  val action:
    state ->
    terminal ->
    semantic_value ->
    ('env -> bool -> terminal -> semantic_value -> state -> 'answer) ->
    ('env -> production -> 'answer) ->
    ('env -> 'answer) ->
    'env -> 'answer

  (**[maybe_shift_t s t] determines whether there exists a transition out of
     the state [s], labeled with the terminal symbol [t], to some state
     [s']. If so, it returns [Some s']. Otherwise, it returns [None]. *)
  val maybe_shift_t : state -> terminal -> state option

  (**[may_reduce_prod s t prod] determines whether in the state [s], with
     lookahead symbol [t], the automaton reduces production [prod]. This test
     accounts for the possible existence of a default reduction. *)
  val may_reduce_prod : state -> terminal -> production -> bool

  (* This is the automaton's goto table. This table maps a pair of a state
     and a nonterminal symbol to a new state. By extension, it also maps a
     pair of a state and a production to a new state. *)

  (* The function [goto_nt] can be applied to [s] and [nt] ONLY if the state
     [s] has an outgoing transition labeled [nt]. Otherwise, its result is
     undefined. Similarly, the call [goto_prod prod s] is permitted ONLY if
     the state [s] has an outgoing transition labeled with the nonterminal
     symbol [lhs prod]. The function [maybe_goto_nt] involves an additional
     dynamic check and CAN be called even if there is no outgoing transition. *)

  val       goto_nt  : state -> nonterminal -> state
  val       goto_prod: state -> production  -> state
  val maybe_goto_nt:   state -> nonterminal -> state option

  (* [lhs prod] returns the left-hand side of production [prod],
     a nonterminal symbol. *)

  val lhs: production -> nonterminal

  (* [is_start prod] tells whether the production [prod] is a start production. *)

  val is_start: production -> bool

  (* By convention, a semantic action is responsible for:

     1. fetching whatever semantic values and positions it needs off the stack;

     2. popping an appropriate number of cells off the stack, as dictated
        by the length of the right-hand side of the production;

     3. computing a new semantic value, as well as new start and end positions;

     4. pushing a new stack cell, which contains the three values
        computed in step 3;

     5. returning the new stack computed in steps 2 and 4.

     Point 1 is essentially forced upon us: if semantic values were fetched
     off the stack by this interpreter, then the calling convention for
     semantic actions would be variadic: not all semantic actions would have
     the same number of arguments. The rest follows rather naturally. *)

  (* Semantic actions are allowed to raise [Error]. *)

  exception Error

  type semantic_action =
      (state, semantic_value, token) env -> (state, semantic_value) stack

  val semantic_action: production -> semantic_action

  (* [may_reduce state prod] tests whether the state [state] is capable of
     reducing the production [prod]. This function is currently costly and
     is not used by the core LR engine. It is used in the implementation
     of certain functions, such as [force_reduction], which allow the engine
     to be driven programmatically. *)

  val may_reduce: state -> production -> bool

  (* If the flag [log] is false, then the logging functions are not called.
     If it is [true], then they are called. *)

  val log : bool

  (* The logging hooks required by the LR engine. *)

  module Log : LOG
    with type state := state
     and type terminal := terminal
     and type production := production

end

(* --------------------------------------------------------------------------- *)

(* This signature describes the monolithic (traditional) LR engine. *)

(* In this interface, the parser controls the lexer. *)

module type MONOLITHIC_ENGINE = sig

  type state

  type token

  type semantic_value

  (* An entry point to the engine requires a start state, a lexer, and a lexing
     buffer. It either succeeds and produces a semantic value, or fails and
     raises [Error]. *)

  exception Error

  val entry:
    (* strategy: *) [ `Legacy | `Simplified ] -> (* see [IncrementalEngine] *)
    state ->
    (Lexing.lexbuf -> token) ->
    Lexing.lexbuf ->
    semantic_value

end

(* --------------------------------------------------------------------------- *)

(* The following signatures describe the incremental LR engine. *)

(* First, see [INCREMENTAL_ENGINE] in the file [IncrementalEngine.ml]. *)

(* The [start] function is set apart because we do not wish to publish
   it as part of the generated [parser.mli] file. Instead, the table
   back-end will publish specialized versions of it, with a suitable
   type cast. *)

module type INCREMENTAL_ENGINE_START = sig

  (* [start] is an entry point. It requires a start state and a start position
     and begins the parsing process. If the lexer is based on an OCaml lexing
     buffer, the start position should be [lexbuf.lex_curr_p]. [start] produces
     a checkpoint, which usually will be an [InputNeeded] checkpoint. (It could
     be [Accepted] if this starting state accepts only the empty word. It could
     be [Rejected] if this starting state accepts no word at all.) It does not
     raise any exception. *)

  (* [start s pos] should really produce a checkpoint of type ['a checkpoint],
     for a fixed ['a] that depends on the state [s]. We cannot express this, so
     we use [semantic_value checkpoint], which is safe. The table back-end uses
     [Obj.magic] to produce safe specialized versions of [start]. *)

  type state
  type semantic_value
  type 'a checkpoint

  val start:
    state ->
    Lexing.position ->
    semantic_value checkpoint

end

(* --------------------------------------------------------------------------- *)

(* This signature describes the LR engine, which combines the monolithic
   and incremental interfaces. *)

module type ENGINE = sig

  include MONOLITHIC_ENGINE

  include IncrementalEngine.INCREMENTAL_ENGINE
    with type token := token
     and type 'a lr1state = state (* useful for us; hidden from the end user *)

  include INCREMENTAL_ENGINE_START
    with type state := state
     and type semantic_value := semantic_value
     and type 'a checkpoint := 'a checkpoint

end
end
module Engine = struct
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

type position = Lexing.position
open EngineTypes

(* The LR parsing engine. *)

(* This module is used:

   - at compile time, if so requested by the user, via the --interpret options;
   - at run time, in the table-based back-end. *)

module Make (T : TABLE) = struct

  (* This propagates type and exception definitions. The functions [number],
     [production_index], [find_production], too, are defined by this [include]
     declaration. *)

  include T

  type 'a env =
      (state, semantic_value, token) EngineTypes.env

  (* ------------------------------------------------------------------------ *)

  (* The type [checkpoint] represents an intermediate or final result of the
     parser. See [EngineTypes]. *)

  (* The type [checkpoint] is presented to the user as a private type (see
     [IncrementalEngine]). This prevents the user from manufacturing
     checkpoints (i.e., continuations) that do not make sense. (Such
     continuations could potentially violate the LR invariant and lead to
     crashes.) *)

  (* 2017/03/29 Although [checkpoint] is a private type, we now expose a
     constructor function, [input_needed]. This function allows manufacturing
     a checkpoint out of an environment. For this reason, the type [env] must
     also be parameterized with ['a]. *)

  type 'a checkpoint =
    | InputNeeded of 'a env
    | Shifting of 'a env * 'a env * bool
    | AboutToReduce of 'a env * production
    | HandlingError of 'a env
    | Accepted of 'a
    | Rejected

  (* ------------------------------------------------------------------------ *)

  (* As of 2020/12/16, we introduce a choice between multiple error handling
     strategies. *)

  (* Regardless of the strategy, when a syntax error is encountered, the
     function [initiate] is called, a [HandlingError] checkpoint is produced,
     and (after resuming) the function [error] is called. This function checks
     whether the current state allows shifting, reducing, or neither, when the
     lookahead token is [error]. Its behavior, then, depends on the strategy,
     as follows. *)

  (* In the legacy strategy, which until now was the only strategy,

     - If shifting is possible, then a [Shifting] checkpoint is produced,
       whose field [please_discard] is [true], so (after resuming) an
       [InputNeeded] checkpoint is produced, and (after a new token
       has been provided) the parser leaves error-handling mode and
       returns to normal mode.

     - If reducing is possible, then one or more reductions are performed.
       Default reductions are announced via [AboutToReduce] checkpoints,
       whereas ordinary reductions are performed silently. (It is unclear
       why this is so.) The parser remains in error-handling mode, so
       another [HandlingError] checkpoint is produced, and the function
       [error] is called again.

     - If neither action is possible and if the stack is nonempty, then a
       cell is popped off the stack, then a [HandlingError] checkpoint is
       produced, and the function [error] is called again.

     - If neither action is possible and if the stack is empty, then the
       parse dies with a [Reject] checkpoint. *)

  (* The simplified strategy differs from the legacy strategy as follows:

     - When shifting, a [Shifting] checkpoint is produced, whose field
       [please_discard] is [false], so the parser does not request another
       token, and the parser remains in error-handling mode. (If the
       destination state of this shift transition has a default reduction,
       then the parser will perform this reduction as its next step.)

     - When reducing, all reductions are announced by [AboutToReduce]
       checkpoints.

     - If neither shifting [error] nor reducing on [error] is possible,
       then the parser dies with a [Reject] checkpoint. (The parser does
       not attempt to pop cells off the stack one by one.)

     This simplified strategy is appropriate when the grammar uses the [error]
     token in a limited way, where the [error] token always appears at the end
     of a production whose semantic action raises an exception (whose purpose
     is to signal a syntax error and perhaps produce a custom message). Then,
     the parser must not request one token past the syntax error. (In a REPL,
     that would be undesirable.) It must perform as many reductions on [error]
     as possible, then (if possible) shift the [error] token and move to a new
     state where a default reduction will be possible. (Because the [error]
     token always appears at the end of a production, no other action can
     exist in that state, so a default reduction must exist.) The semantic
     action raises an exception, and that is it. *)

  (* Let us note that it is also possible to perform no error handling at
     all, or to perform customized error handling, by stopping as soon as
     the first [ErrorHandling] checkpoint appears. *)

  type strategy =
    [ `Legacy | `Simplified ]

  (* ------------------------------------------------------------------------ *)

  (* In the code-based back-end, the [run] function is sometimes responsible
     for pushing a new cell on the stack. This is motivated by code sharing
     concerns. In this interpreter, there is no such concern; [run]'s caller
     is always responsible for updating the stack. *)

  (* In the code-based back-end, there is a [run] function for each state
     [s]. This function can behave in two slightly different ways, depending
     on when it is invoked, or (equivalently) depending on [s].

     If [run] is invoked after shifting a terminal symbol (or, equivalently,
     if [s] has a terminal incoming symbol), then [run] discards a token,
     unless [s] has a default reduction on [#]. (Indeed, in that case,
     requesting the next token might drive the lexer off the end of the input
     stream.)

     If, on the other hand, [run] is invoked after performing a goto
     transition, or invoked directly by an entry point, then there is nothing
     to discard.

     These two cases are reflected in [CodeBackend.gettoken].

     Here, the code is structured in a slightly different way. It is up to the
     caller of [run] to indicate whether to discard a token, via the parameter
     [please_discard]. This flag is set when [s] is being entered by shifting
     a terminal symbol and [s] does not have a default reduction on [#]. *)

  (* The following recursive group of functions are tail recursive, produce a
     checkpoint of type [semantic_value checkpoint], and cannot raise an
     exception. *)

  let rec run env please_discard : semantic_value checkpoint =

    (* Log the fact that we just entered this state. *)

    if log then
      Log.state env.current;

    (* If [please_discard] is set, we discard the current lookahead token and
       fetch the next one. In order to request a token from the user, we
       return an [InputNeeded] continuation, which, when invoked by the user,
       will take us to [discard]. If [please_discard] is not set, we skip this
       step and jump directly to [check_for_default_reduction]. *)

    if please_discard then
      InputNeeded env
    else
      check_for_default_reduction env

  (* [discard env triple] stores [triple] into [env], overwriting the previous
     token. It is invoked by [offer], which itself is invoked by the user in
     response to an [InputNeeded] checkpoint. *)

  and discard env triple =
    if log then begin
      let (token, startp, endp) = triple in
      Log.lookahead_token (T.token2terminal token) startp endp
    end;
    let env = { env with error = false; triple } in
    check_for_default_reduction env

  and check_for_default_reduction env =

    (* Examine what situation we are in. This case analysis is analogous to
       that performed in [CodeBackend.gettoken], in the sub-case where we do
       not have a terminal incoming symbol. *)

    T.default_reduction
      env.current
      announce_reduce       (* there is a default reduction; perform it *)
      check_for_error_token (* there is none; continue below *)
      env

  and check_for_error_token env =

    (* There is no default reduction. Consult the current lookahead token
       so as to determine which action should be taken. *)

    (* Peeking at the first input token, without taking it off the input
       stream, is done by reading [env.triple]. We are careful to first
       check [env.error]. *)

    (* Note that, if [please_discard] was true, then we have just called
       [discard], so the lookahead token cannot be [error]. *)

    (* Returning [HandlingError env] is like calling [error ~strategy env]
       directly, except it allows the user to regain control and choose an
       error-handling strategy. *)

    if env.error then begin
      if log then
        Log.resuming_error_handling();
      HandlingError env
    end
    else
      let (token, _, _) = env.triple in

      (* We consult the two-dimensional action table, indexed by the
         current state and the current lookahead token, in order to
         determine which action should be taken. *)

      T.action
        env.current                    (* determines a row *)
        (T.token2terminal token)       (* determines a column *)
        (T.token2value token)
        shift                          (* shift continuation *)
        announce_reduce                (* reduce continuation *)
        initiate                       (* failure continuation *)
        env

  (* ------------------------------------------------------------------------ *)

  (* This function takes care of shift transitions along a terminal symbol.
     (Goto transitions are taken care of within [reduce] below.) The symbol
     can be either an actual token or the [error] pseudo-token. *)

  (* Here, the lookahead token CAN be [error]. *)

  and shift env
      (please_discard : bool)
      (terminal : terminal)
      (value : semantic_value)
      (s' : state) =

    (* Log the transition. *)

    if log then
      Log.shift terminal s';

    (* Push a new cell onto the stack, containing the identity of the
       state that we are leaving. *)

    let (_, startp, endp) = env.triple in
    let stack = {
      state = env.current;
      semv = value;
      startp;
      endp;
      next = env.stack;
    } in

    (* Switch to state [s']. *)

    let new_env = { env with stack; current = s' } in

    (* Expose the transition to the user. (In principle, we have a choice
       between exposing the transition before we take it, after we take
       it, or at some point in between. This affects the number and type
       of the parameters carried by [Shifting]. Here, we choose to expose
       the transition after we take it; this allows [Shifting] to carry
       only three parameters, whose meaning is simple.) *)

    Shifting (env, new_env, please_discard)

  (* ------------------------------------------------------------------------ *)

  (* The function [announce_reduce] stops the parser and returns a checkpoint
     which allows the parser to be resumed by calling [reduce]. *)

  (* Only ordinary productions are exposed to the user. Start productions
     are not exposed to the user. Reducing a start production simply leads
     to the successful termination of the parser. *)

  and announce_reduce env (prod : production) =
    if T.is_start prod then
      accept env prod
    else
      AboutToReduce (env, prod)

  (* The function [reduce] takes care of reductions. It is invoked by
     [resume] after an [AboutToReduce] event has been produced. *)

  (* Here, the lookahead token CAN be [error]. *)

  (* The production [prod] CANNOT be a start production. *)

  and reduce env (prod : production) =

    (* Log a reduction event. *)

    if log then
      Log.reduce_or_accept prod;

    (* Invoke the semantic action. The semantic action is responsible for
       truncating the stack and pushing a new cell onto the stack, which
       contains a new semantic value. The semantic action returns a new stack,
       which becomes the current stack. *)

    let stack = T.semantic_action prod env in

    (* By our convention, the semantic action has produced an updated
       stack. The state now found in the top stack cell is the return
       state. *)

    (* Perform a goto transition. The target state is determined
       by consulting the goto table at the return state and at
       production [prod]. *)

    let current = T.goto_prod stack.state prod in
    let env = { env with stack; current } in
    run env false

  and accept env prod =
    (* Log an accept event. *)
    if log then
      Log.reduce_or_accept prod;
    (* Extract the semantic value out of the stack. *)
    let v = env.stack.semv in
    (* Finish. *)
    Accepted v

  (* ------------------------------------------------------------------------ *)

  (* The following functions deal with errors. *)

  (* [initiate] initiates or resumes error handling. *)

  (* Here, the lookahead token CAN be [error]. *)

  and initiate env =
    if log then
      Log.initiating_error_handling();
    let env = { env with error = true } in
    HandlingError env

  (* [error] handles errors. *)

  and error ~strategy env =
    assert env.error;

    (* Consult the column associated with the [error] pseudo-token in the
       action table. *)

    T.action
      env.current                    (* determines a row *)
      T.error_terminal               (* determines a column *)
      T.error_value
      (error_shift ~strategy)        (* shift continuation *)
      (error_reduce ~strategy)       (* reduce continuation *)
      (error_fail ~strategy)         (* failure continuation *)
      env

  and error_shift ~strategy env please_discard terminal value s' =
    assert (terminal = T.error_terminal && value = T.error_value);

    (* This state is capable of shifting the [error] token. *)

    if log then
      Log.handling_error env.current;

    (* In the simplified strategy, we change [please_discard] to [false],
       which means that we won't request the next token and (therefore)
       we will remain in error-handling mode after shifting the [error]
       token. *)

    let please_discard =
      match strategy with `Legacy -> please_discard | `Simplified -> false
    in

    shift env please_discard terminal value s'

  and error_reduce ~strategy env prod =

    (* This state is capable of performing a reduction on [error]. *)

    if log then
      Log.handling_error env.current;

    (* In the legacy strategy, we call [reduce] instead of [announce_reduce],
       apparently in an attempt to hide the reduction steps performed during
       error handling. In the simplified strategy, all reductions steps are
       announced. *)

    match strategy with
    | `Legacy ->
        reduce env prod
    | `Simplified ->
        announce_reduce env prod

  and error_fail ~strategy env =

    (* This state is unable to handle errors. In the simplified strategy, we
       die immediately. In the legacy strategy, we attempt to pop a stack
       cell. (This amounts to forgetting part of what we have just read, in
       the hope of reaching a state where we can shift the [error] token and
       resume parsing in normal mode. Forgetting past input is not appropriate
       when the goal is merely to produce a good syntax error message.) *)

    match strategy with
    | `Simplified ->
        Rejected
    | `Legacy ->

    (* Attempt to pop a stack cell. *)

    let cell = env.stack in
    let next = cell.next in
    if next == cell then

      (* The stack is empty. Die. *)

      Rejected

    else begin

      (* The stack is nonempty. Pop a cell, updating the current state
         to the state [cell.state] found in the popped cell, and continue
         error handling there. *)

      (* I note that if the new state [cell.state] has a default reduction,
         then it is ignored. It is unclear whether this is intentional. It
         could be a good thing, as it avoids a scenario where the parser
         diverges by repeatedly popping, performing a default reduction of
         an epsilon production, popping, etc. Still, the question of whether
         to obey default reductions while error handling seems obscure. *)

      let env = { env with
        stack = next;
        current = cell.state
      } in
      HandlingError env

    end

  (* End of the nest of tail recursive functions. *)

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  (* The incremental interface. See [EngineTypes]. *)

  (* [start s] begins the parsing process. *)

  let start (s : state) (initial : position) : semantic_value checkpoint =

    (* Build an empty stack. This is a dummy cell, which is its own successor.
       Its [next] field WILL be accessed by [error_fail] if an error occurs and
       is propagated all the way until the stack is empty. Its [endp] field WILL
       be accessed (by a semantic action) if an epsilon production is reduced
       when the stack is empty. *)

    let rec empty = {
      state = s;                          (* dummy *)
      semv = T.error_value;               (* dummy *)
      startp = initial;                   (* dummy *)
      endp = initial;
      next = empty;
    } in

    (* Build an initial environment. *)

    (* Unfortunately, there is no type-safe way of constructing a
       dummy token. Tokens carry semantic values, which in general
       we cannot manufacture. This instance of [Obj.magic] could
       be avoided by adopting a different representation (e.g., no
       [env.error] field, and an option in the first component of
       [env.triple]), but I like this representation better. *)

    let dummy_token = Obj.magic () in
    let env = {
      error = false;
      triple = (dummy_token, initial, initial); (* dummy *)
      stack = empty;
      current = s;
    } in

    (* Begin parsing. *)

    (* The parameter [please_discard] here is [true], which means we know
       that we must read at least one token. This claim relies on the fact
       that we have ruled out the two special cases where a start symbol
       recognizes the empty language or the singleton language {epsilon}. *)

    run env true

  (* [offer checkpoint triple] is invoked by the user in response to a
     checkpoint of the form [InputNeeded env]. It checks that [checkpoint] is
     indeed of this form, and invokes [discard]. *)

  (* [resume checkpoint] is invoked by the user in response to a checkpoint
     of the form [Shifting _], [AboutToReduce _], or [HandlingError env]. It
     checks that [checkpoint] is indeed of this form, and invokes [reduce]
     or [error], as appropriate. *)

  (* In reality, [offer] and [resume] accept an argument of type
     [semantic_value checkpoint] and produce a checkpoint of the same type.
     The choice of [semantic_value] is forced by the fact that this is the
     parameter of the checkpoint [Accepted]. *)

  (* We change this as follows. *)

  (* We change the argument and result type of [offer] and [resume] from
     [semantic_value checkpoint] to ['a checkpoint]. This is safe, in this
     case, because we give the user access to values of type [t checkpoint]
     only if [t] is indeed the type of the eventual semantic value for this
     run. (More precisely, by examining the signatures [INCREMENTAL_ENGINE]
     and [INCREMENTAL_ENGINE_START], one finds that the user can build a value
     of type ['a checkpoint] only if ['a] is [semantic_value]. The table
     back-end goes further than this and produces versions of [start] composed
     with a suitable cast, which give the user access to a value of type
     [t checkpoint] where [t] is the type of the start symbol.) *)

  let offer : 'a . 'a checkpoint ->
                   token * position * position ->
                   'a checkpoint
  = function
    | InputNeeded env ->
        Obj.magic discard env
    | _ ->
        invalid_arg "offer expects InputNeeded"

  let resume : 'a . ?strategy:strategy -> 'a checkpoint -> 'a checkpoint =
  fun ?(strategy=`Legacy) checkpoint ->
    match checkpoint with
    | HandlingError env ->
        Obj.magic error ~strategy env
    | Shifting (_, env, please_discard) ->
        Obj.magic run env please_discard
    | AboutToReduce (env, prod) ->
        Obj.magic reduce env prod
    | _ ->
        invalid_arg "resume expects HandlingError | Shifting | AboutToReduce"

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  (* The traditional interface. See [EngineTypes]. *)

  (* ------------------------------------------------------------------------ *)

  (* Wrapping a lexer and lexbuf as a token supplier. *)

  type supplier =
    unit -> token * position * position

  let lexer_lexbuf_to_supplier
      (lexer : Lexing.lexbuf -> token)
      (lexbuf : Lexing.lexbuf)
  : supplier =
    fun () ->
      let token = lexer lexbuf in
      let startp = lexbuf.Lexing.lex_start_p
      and endp = lexbuf.Lexing.lex_curr_p in
      token, startp, endp

  (* ------------------------------------------------------------------------ *)

  (* The main loop repeatedly handles intermediate checkpoints, until a final
     checkpoint is obtained. This allows implementing the monolithic interface
     ([entry]) in terms of the incremental interface ([start], [offer],
     [handle], [reduce]). *)

  (* By convention, acceptance is reported by returning a semantic value,
     whereas rejection is reported by raising [Error]. *)

  (* [loop] is polymorphic in ['a]. No cheating is involved in achieving this.
     All of the cheating resides in the types assigned to [offer] and [handle]
     above. *)

  let rec loop : 'a . ?strategy:strategy -> supplier -> 'a checkpoint -> 'a =
    fun ?(strategy=`Legacy) read checkpoint ->
    match checkpoint with
    | InputNeeded _ ->
        (* The parser needs a token. Request one from the lexer,
           and offer it to the parser, which will produce a new
           checkpoint. Then, repeat. *)
        let triple = read() in
        let checkpoint = offer checkpoint triple in
        loop ~strategy read checkpoint
    | Shifting _
    | AboutToReduce _
    | HandlingError _ ->
        (* The parser has suspended itself, but does not need
           new input. Just resume the parser. Then, repeat. *)
        let checkpoint = resume ~strategy checkpoint in
        loop ~strategy read checkpoint
    | Accepted v ->
        (* The parser has succeeded and produced a semantic value.
           Return this semantic value to the user. *)
        v
    | Rejected ->
        (* The parser rejects this input. Raise an exception. *)
        raise Error

  let entry strategy (s : state) lexer lexbuf : semantic_value =
    let initial = lexbuf.Lexing.lex_curr_p in
    loop ~strategy (lexer_lexbuf_to_supplier lexer lexbuf) (start s initial)

  (* ------------------------------------------------------------------------ *)

  (* [loop_handle] stops if it encounters an error, and at this point, invokes
     its failure continuation, without letting Menhir do its own traditional
     error-handling (which involves popping the stack, etc.). *)

  let rec loop_handle succeed fail read checkpoint =
    match checkpoint with
    | InputNeeded _ ->
        let triple = read() in
        let checkpoint = offer checkpoint triple in
        loop_handle succeed fail read checkpoint
    | Shifting _
    | AboutToReduce _ ->
        (* Which strategy is passed to [resume] here is irrelevant,
           since this checkpoint is not [HandlingError _]. *)
        let checkpoint = resume checkpoint in
        loop_handle succeed fail read checkpoint
    | HandlingError _
    | Rejected ->
        (* The parser has detected an error. Invoke the failure continuation. *)
        fail checkpoint
    | Accepted v ->
        (* The parser has succeeded and produced a semantic value. Invoke the
           success continuation. *)
        succeed v

  (* ------------------------------------------------------------------------ *)

  (* [loop_handle_undo] is analogous to [loop_handle], except it passes a pair
     of checkpoints to the failure continuation.

     The first (and oldest) checkpoint is the last [InputNeeded] checkpoint
     that was encountered before the error was detected. The second (and
     newest) checkpoint is where the error was detected, as in [loop_handle].
     Going back to the first checkpoint can be thought of as undoing any
     reductions that were performed after seeing the problematic token. (These
     reductions must be default reductions or spurious reductions.) *)

  let rec loop_handle_undo succeed fail read (inputneeded, checkpoint) =
    match checkpoint with
    | InputNeeded _ ->
        (* Update the last recorded [InputNeeded] checkpoint. *)
        let inputneeded = checkpoint in
        let triple = read() in
        let checkpoint = offer checkpoint triple in
        loop_handle_undo succeed fail read (inputneeded, checkpoint)
    | Shifting _
    | AboutToReduce _ ->
        (* Which strategy is passed to [resume] here is irrelevant,
           since this checkpoint is not [HandlingError _]. *)
        let checkpoint = resume checkpoint in
        loop_handle_undo succeed fail read (inputneeded, checkpoint)
    | HandlingError _
    | Rejected ->
        fail inputneeded checkpoint
    | Accepted v ->
        succeed v

  (* For simplicity, we publish a version of [loop_handle_undo] that takes a
     single checkpoint as an argument, instead of a pair of checkpoints. We
     check that the argument is [InputNeeded _], and duplicate it. *)

  (* The parser cannot accept or reject before it asks for the very first
     character of input. (Indeed, we statically reject a symbol that
     generates the empty language or the singleton language {epsilon}.)
     So, the [start] checkpoint must match [InputNeeded _]. Hence, it is
     permitted to call [loop_handle_undo] with a [start] checkpoint. *)

  let loop_handle_undo succeed fail read checkpoint =
    assert (match checkpoint with InputNeeded _ -> true | _ -> false);
    loop_handle_undo succeed fail read (checkpoint, checkpoint)

  (* ------------------------------------------------------------------------ *)

  let rec shifts checkpoint =
    match checkpoint with
    | Shifting (env, _, _) ->
        (* The parser is about to shift, which means it is willing to
           consume the terminal symbol that we have fed it. Return the
           state just before this transition. *)
        Some env
    | AboutToReduce _ ->
        (* The parser wishes to reduce. Just follow. *)
        (* Which strategy is passed to [resume] here is irrelevant,
           since this checkpoint is not [HandlingError _]. *)
        shifts (resume checkpoint)
    | HandlingError _ ->
        (* The parser fails, which means it rejects the terminal symbol
           that we have fed it. *)
        None
    | InputNeeded _
    | Accepted _
    | Rejected ->
        (* None of these cases can arise. Indeed, after a token is submitted
           to it, the parser must shift, reduce, or signal an error, before
           it can request another token or terminate. *)
        assert false

  let acceptable checkpoint token pos =
    let triple = (token, pos, pos) in
    let checkpoint = offer checkpoint triple in
    match shifts checkpoint with
    | None      -> false
    | Some _env -> true

  (* ------------------------------------------------------------------------ *)

  (* The type ['a lr1state] describes the (non-initial) states of the LR(1)
     automaton. The index ['a] represents the type of the semantic value
     associated with the state's incoming symbol. *)

  (* The type ['a lr1state] is defined as an alias for [state], which itself
     is usually defined as [int] (see [TableInterpreter]). So, ['a lr1state]
     is technically a phantom type, but should really be thought of as a GADT
     whose data constructors happen to be represented as integers. It is
     presented to the user as an abstract type (see [IncrementalEngine]). *)

  type 'a lr1state =
      state

  (* ------------------------------------------------------------------------ *)

  (* Stack inspection. *)

  (* We offer a read-only view of the parser's state as a stream of elements.
     Each element contains a pair of a (non-initial) state and a semantic
     value associated with (the incoming symbol of) this state. Note that the
     type [element] is an existential type. *)

  (* As of 2017/03/31, the type [stack] and the function [stack] are DEPRECATED.
     If desired, they could now be implemented outside Menhir, by relying on
     the functions [top] and [pop]. *)

  type element =
    | Element: 'a lr1state * 'a * position * position -> element

  open General

  type stack =
    element stream

  (* If [current] is the current state and [cell] is the top stack cell,
     then [stack cell current] is a view of the parser's state as a stream
     of elements. *)

  let rec stack cell current : element stream =
    lazy (
      (* The stack is empty iff the top stack cell is its own successor. In
         that case, the current state [current] should be an initial state
         (which has no incoming symbol).
         We do not allow the user to inspect this state. *)
      let next = cell.next in
      if next == cell then
        Nil
      else
        (* Construct an element containing the current state [current] as well
           as the semantic value contained in the top stack cell. This semantic
           value is associated with the incoming symbol of this state, so it
           makes sense to pair them together. The state has type ['a state] and
           the semantic value has type ['a], for some type ['a]. Here, the OCaml
           type-checker thinks ['a] is [semantic_value] and considers this code
           well-typed. Outside, we will use magic to provide the user with a way
           of inspecting states and recovering the value of ['a]. *)
        let element = Element (
          current,
          cell.semv,
          cell.startp,
          cell.endp
        ) in
        Cons (element, stack next cell.state)
    )

  let stack env : element stream =
    stack env.stack env.current

  (* As explained above, the function [top] allows access to the top stack
     element only if the stack is nonempty, i.e., only if the current state
     is not an initial state. *)

  let top env : element option =
    let cell = env.stack in
    let next = cell.next in
    if next == cell then
      None
    else
      Some (Element (env.current, cell.semv, cell.startp, cell.endp))

  (* [equal] compares the stacks for physical equality, and compares the
     current states via their numbers (this seems cleaner than using OCaml's
     polymorphic equality). *)

  (* The two fields that are not compared by [equal], namely [error] and
     [triple], are overwritten by the function [discard], which handles
     [InputNeeded] checkpoints. Thus, if [equal env1 env2] holds, then the
     checkpoints [input_needed env1] and [input_needed env2] are
     equivalent: they lead the parser to behave in the same way. *)

  let equal env1 env2 =
    env1.stack == env2.stack &&
    number env1.current = number env2.current

  let current_state_number env =
    number env.current

  (* ------------------------------------------------------------------------ *)

  (* Access to the position of the lookahead token. *)

  let positions { triple = (_, startp, endp); _ } =
    startp, endp

  (* ------------------------------------------------------------------------ *)

  (* Access to information about default reductions. *)

  (* This can be a function of states, or a function of environments.
     We offer both. *)

  (* Instead of a Boolean result, we could return a [production option].
     However, we would have to explicitly test whether [prod] is a start
     production, and in that case, return [None], I suppose. Indeed, we
     have decided not to expose the start productions. *)

  let state_has_default_reduction (state : _ lr1state) : bool =
    T.default_reduction state
      (fun _env _prod -> true)
      (fun _env -> false)
      ()

  let env_has_default_reduction env =
    state_has_default_reduction env.current

  (* ------------------------------------------------------------------------ *)

  (* The following functions work at the level of environments (as opposed to
     checkpoints). The function [pop] causes the automaton to go back into the
     past, pretending that the last input symbol has never been read. The
     function [force_reduction] causes the automaton to re-interpret the past,
     by recognizing the right-hand side of a production and reducing this
     production. The function [feed] causes the automaton to progress into the
     future by pretending that a (terminal or nonterminal) symbol has been
     read. *)

  (* The function [feed] would ideally be defined here. However, for this
     function to be type-safe, the GADT ['a symbol] is needed. For this
     reason, we move its definition to [InspectionTableInterpreter], where
     the inspection API is available. *)

  (* [pop] pops one stack cell. It cannot go wrong. *)

  let pop (env : 'a env) : 'a env option =
    let cell = env.stack in
    let next = cell.next in
    if next == cell then
      (* The stack is empty. *)
      None
    else
      (* The stack is nonempty. Pop off one cell. *)
      Some { env with stack = next; current = cell.state }

  (* [force_reduction] is analogous to [reduce], except that it does not
     continue by calling [run env] or [initiate env]. Instead, it returns
     [env] to the user. *)

  (* [force_reduction] is dangerous insofar as it executes a semantic action.
     This semantic action could have side effects: nontermination, state,
     exceptions, input/output, etc. *)

  let force_reduction prod (env : 'a env) : 'a env =
    (* Check if this reduction is permitted. This check is REALLY important.
       The stack must have the correct shape: that is, it must be sufficiently
       high, and must contain semantic values of appropriate types, otherwise
       the semantic action will crash and burn. *)
    (* We currently check whether the current state is WILLING to reduce this
       production (i.e., there is a reduction action in the action table row
       associated with this state), whereas it would be more liberal to check
       whether this state is CAPABLE of reducing this production (i.e., the
       stack has an appropriate shape). We currently have no means of
       performing such a check. *)
    if not (T.may_reduce env.current prod) then
      invalid_arg "force_reduction: this reduction is not permitted in this state"
    else begin
      (* We do not expose the start productions to the user, so this cannot be
         a start production. Hence, it has a semantic action. *)
      assert (not (T.is_start prod));
      (* Invoke the semantic action. *)
      let stack = T.semantic_action prod env in
      (* Perform a goto transition. *)
      let current = T.goto_prod stack.state prod in
      { env with stack; current }
    end

  (* The environment manipulation functions -- [pop] and [force_reduction]
     above, plus [feed] -- manipulate the automaton's stack and current state,
     but do not affect the automaton's lookahead symbol. When the function
     [input_needed] is used to go back from an environment to a checkpoint
     (and therefore, resume normal parsing), the lookahead symbol is clobbered
     anyway, since the only action that the user can take is to call [offer].
     So far, so good. One problem, though, is that this call to [offer] may
     well place the automaton in a configuration of a state [s] and a
     lookahead symbol [t] that is normally unreachable. Also, perhaps the
     state [s] is a state where an input symbol normally is never demanded, so
     this [InputNeeded] checkpoint is fishy. There does not seem to be a deep
     problem here, but, when programming an error recovery strategy, one
     should pay some attention to this issue. Ideally, perhaps, one should use
     [input_needed] only in a state [s] where an input symbol is normally
     demanded, that is, a state [s] whose incoming symbol is a terminal symbol
     and which does not have a default reduction on [#]. *)

  let input_needed (env : 'a env) : 'a checkpoint =
    InputNeeded env

  (* The following functions are compositions of [top] and [pop]. *)

  let rec pop_many i env =
    if i = 0 then
      Some env
    else match pop env with
    | None ->
        None
    | Some env ->
        pop_many (i - 1) env

  let get i env =
    match pop_many i env with
    | None ->
        None
    | Some env ->
        top env

end
end
module ErrorReports = struct
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(* -------------------------------------------------------------------------- *)

(* A two-place buffer stores zero, one, or two elements. *)

type 'a content =
| Zero
| One of 'a
| Two of 'a * (* most recent: *) 'a

type 'a buffer =
  'a content ref

(* [update buffer x] pushes [x] into [buffer], causing the buffer to slide. *)

let update buffer x =
  buffer :=
    match !buffer, x with
    | Zero, _ ->
        One x
    | One x1, x2
    | Two (_, x1), x2 ->
        Two (x1, x2)

let show f buffer : string =
  match !buffer with
  | Zero ->
      (* The buffer cannot be empty. If we have read no tokens,
         we cannot have detected a syntax error. *)
      assert false
  | One invalid ->
      (* It is unlikely, but possible, that we have read just one token. *)
      Printf.sprintf "before '%s'" (f invalid)
  | Two (valid, invalid) ->
      (* In the most likely case, we have read two tokens. *)
      Printf.sprintf "after '%s' and before '%s'" (f valid) (f invalid)

let last buffer =
  match !buffer with
  | Zero ->
      (* The buffer cannot be empty. If we have read no tokens,
         we cannot have detected a syntax error. *)
      assert false
  | One invalid
  | Two (_, invalid) ->
      invalid

open Lexing

let wrap lexer =
  let buffer = ref Zero in
  buffer,
  fun lexbuf ->
    let token = lexer lexbuf in
    update buffer (lexbuf.lex_start_p, lexbuf.lex_curr_p);
    token

let wrap_supplier supplier =
  let buffer = ref Zero in
  buffer,
  fun () ->
    let (_token, pos1, pos2) as triple = supplier() in
    update buffer (pos1, pos2);
    triple

(* -------------------------------------------------------------------------- *)

let extract text (pos1, pos2) : string =
  let ofs1 = pos1.pos_cnum
  and ofs2 = pos2.pos_cnum in
  let len = ofs2 - ofs1 in
  try
    String.sub text ofs1 len
  with Invalid_argument _ ->
    (* In principle, this should not happen, but if it does, let's make this
       a non-fatal error. *)
    "???"

let sanitize text =
  String.map (fun c ->
    if Char.code c < 32 then ' ' else c
  ) text

(* If we were willing to depend on [Str], we could implement [compress] as
   follows:

   let compress text =
     Str.global_replace (Str.regexp "[ \t\n\r]+") " " text

 *)

let rec compress n b i j skipping =
  if j < n then
    let c, j = Bytes.get b j, j + 1 in
    match c with
    | ' ' | '\t' | '\n' | '\r' ->
        let i = if not skipping then (Bytes.set b i ' '; i + 1) else i in
        let skipping = true in
        compress n b i j skipping
    | _ ->
        let i = Bytes.set b i c; i + 1 in
        let skipping = false in
        compress n b i j skipping
  else
    Bytes.sub_string b 0 i

let compress text =
  let b = Bytes.of_string text in
  let n = Bytes.length b in
  compress n b 0 0 false

let shorten k text =
  let n = String.length text in
  if n <= 2 * k + 3 then
    text
  else
    String.sub text 0 k ^
    "..." ^
    String.sub text (n - k) k

let is_digit c =
  let c = Char.code c in
  Char.code '0' <= c && c <= Char.code '9'

exception Copy

let expand f text =
  let n = String.length text in
  let b = Buffer.create n in
  let rec loop i =
    if i < n then begin
      let c, i = text.[i], i + 1 in
      loop (
        try
          if c <> '$' then raise Copy;
          let j = ref i in
          while !j < n && is_digit text.[!j] do incr j done;
          if i = !j then raise Copy;
          let k = int_of_string (String.sub text i (!j - i)) in
          Buffer.add_string b (f k);
          !j
        with Copy ->
          (* We reach this point if either [c] is not '$' or [c] is '$'
             but is not followed by an integer literal. *)
          Buffer.add_char b c;
          i
      )
    end
    else
      Buffer.contents b
  in
  loop 0
end
module LexerUtil = struct
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

open Lexing
open Printf

let init filename lexbuf =
  lexbuf.lex_curr_p <- {
    pos_fname = filename;
    pos_lnum  = 1;
    pos_bol   = 0;
    pos_cnum  = 0
  };
  lexbuf

let read filename =
  let c = open_in filename in
  let text = really_input_string c (in_channel_length c) in
  close_in c;
  let lexbuf = Lexing.from_string text in
  text, init filename lexbuf

let newline lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = pos.pos_cnum;
  }

let is_dummy (pos1, pos2) =
  pos1 == dummy_pos || pos2 == dummy_pos

let range ((pos1, pos2) as range) =
  if is_dummy range then
    sprintf "At an unknown location:\n"
  else
    let file = pos1.pos_fname in
    let line = pos1.pos_lnum in
    let char1 = pos1.pos_cnum - pos1.pos_bol in
    let char2 = pos2.pos_cnum - pos1.pos_bol in (* yes, [pos1.pos_bol] *)
    sprintf "File \"%s\", line %d, characters %d-%d:\n"
      file line char1 char2
      (* use [char1 + 1] and [char2 + 1] if *not* using Caml mode *)

let tabulate (type a) (is_eof : a -> bool) (lexer : unit -> a) : unit -> a =
  (* Read tokens from the lexer until we hit an EOF token. *)
  let rec read tokens =
    let token = lexer() in
    let tokens = token :: tokens in
    if is_eof token then
      (* Once done, reverse the list and convert it to an array. *)
      tokens |> List.rev |> Array.of_list
    else
      read tokens
  in
  (* We now have an array of tokens. *)
  let tokens = read [] in
  (* Define a pseudo-lexer that reads from this array. *)
  let i = ref 0 in
  let lexer () =
    (* If this assertion is violated, then the parser is trying to read
       past an EOF token. This should not happen. *)
    assert (!i < Array.length tokens);
    let token = Array.unsafe_get tokens !i in
    i := !i + 1;
    token
  in
  lexer
end
module Printers = struct
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

module Make
  (I : IncrementalEngine.EVERYTHING)
  (User : sig
    val print: string -> unit
    val print_symbol: I.xsymbol -> unit
    val print_element: (I.element -> unit) option
  end)
= struct

  let arrow = " -> "
  let dot = "."
  let space = " "
  let newline = "\n"

  open User
  open I

  (* Printing a list of symbols. An optional dot is printed at offset
     [i] into the list [symbols], if this offset lies between [0] and
     the length of the list (included). *)

  let rec print_symbols i symbols =
    if i = 0 then begin
      print dot;
      print space;
      print_symbols (-1) symbols
    end
    else begin
      match symbols with
      | [] ->
          ()
      | symbol :: symbols ->
          print_symbol symbol;
          print space;
          print_symbols (i - 1) symbols
    end

  (* Printing an element as a symbol. *)

  let print_element_as_symbol element =
    match element with
    | Element (s, _, _, _) ->
        print_symbol (X (incoming_symbol s))

  (* Some of the functions that follow need an element printer. They use
     [print_element] if provided by the user; otherwise they use
     [print_element_as_symbol]. *)

  let print_element =
    match print_element with
    | Some print_element ->
        print_element
    | None ->
        print_element_as_symbol

  (* Printing a stack as a list of symbols. Stack bottom on the left,
     stack top on the right. *)

  let rec print_stack env =
    match top env, pop env with
    | Some element, Some env ->
        print_stack env;
        print space;
        print_element element
    | _, _ ->
        ()

  let print_stack env =
    print_stack env;
    print newline

  (* Printing an item. *)

  let print_item (prod, i) =
    print_symbol (lhs prod);
    print arrow;
    print_symbols i (rhs prod);
    print newline

  (* Printing a list of symbols (public version). *)

  let print_symbols symbols =
    print_symbols (-1) symbols

  (* Printing a production (without a dot). *)

  let print_production prod =
    print_item (prod, -1)

  (* Printing the current LR(1) state. *)

  let print_current_state env =
    print "Current LR(1) state: ";
    match top env with
    | None ->
        print "<some initial state>"; (* TEMPORARY unsatisfactory *)
        print newline
    | Some (Element (current, _, _, _)) ->
        print (string_of_int (number current));
        print newline;
        List.iter print_item (items current)

  let print_env env =
    print_stack env;
    print_current_state env;
    print newline

end
end
module InfiniteArray = struct
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(** This module implements infinite arrays, that is, arrays that grow
    transparently upon demand. *)

type 'a t = {
    default: 'a;
    mutable table: 'a array;
    mutable extent: int; (* the index of the greatest [set] ever, plus one *)
  }

let default_size =
  16384 (* must be non-zero *)

let make x = {
  default = x;
  table = Array.make default_size x;
  extent = 0;
}

let rec new_length length i =
  if i < length then
    length
  else
    new_length (2 * length) i

let ensure a i =
  assert (0 <= i);
  let table = a.table in
  let length = Array.length table in
  if i >= length then begin
    let table' = Array.make (new_length (2 * length) i) a.default in
    Array.blit table 0 table' 0 length;
    a.table <- table'
  end

let get a i =
  ensure a i;
  Array.unsafe_get a.table (i)

let set a i x =
  ensure a i;
  Array.unsafe_set a.table (i) x;
  if a.extent <= i then
    a.extent <- i + 1

let extent a =
  a.extent

let domain a =
  Array.sub a.table 0 a.extent

end
module PackedIntArray = struct
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(* A packed integer array is represented as a pair of an integer [k] and
   a string [s]. The integer [k] is the number of bits per integer that we
   use. The string [s] is just an array of bits, which is read in 8-bit
   chunks. *)

(* The ocaml programming language treats string literals and array literals
   in slightly different ways: the former are statically allocated, while
   the latter are dynamically allocated. (This is rather arbitrary.) In the
   context of Menhir's table-based back-end, where compact, immutable
   integer arrays are needed, ocaml strings are preferable to ocaml arrays. *)

type t =
  int * string

(* The magnitude [k] of an integer [v] is the number of bits required
   to represent [v]. It is rounded up to the nearest power of two, so
   that [k] divides [Sys.word_size]. *)

let magnitude (v : int) =
  if v < 0 then
    Sys.word_size
  else
    let rec check k max = (* [max] equals [2^k] *)
      if (max <= 0) || (v < max) then
        k
          (* if [max] just overflew, then [v] requires a full ocaml
             integer, and [k] is the number of bits in an ocaml integer
             plus one, that is, [Sys.word_size]. *)
      else
        check (2 * k) (max * max)
    in
    check 1 2

(* [pack a] turns an array of integers into a packed integer array. *)

(* Because the sign bit is the most significant bit, the magnitude of
   any negative number is the word size. In other words, [pack] does
   not achieve any space savings as soon as [a] contains any negative
   numbers, even if they are ``small''. *)

let pack (a : int array) : t =

  let m = Array.length a in

  (* Compute the maximum magnitude of the array elements. This tells
     us how many bits per element we are going to use. *)

  let k =
    Array.fold_left (fun k v ->
      max k (magnitude v)
    ) 1 a
  in

  (* Because access to ocaml strings is performed on an 8-bit basis,
     two cases arise. If [k] is less than 8, then we can pack multiple
     array entries into a single character. If [k] is greater than 8,
     then we must use multiple characters to represent a single array
     entry. *)

  if k <= 8 then begin

    (* [w] is the number of array entries that we pack in a character. *)

    assert (8 mod k = 0);
    let w = 8 / k in

    (* [n] is the length of the string that we allocate. *)

    let n =
      if m mod w = 0 then
        m / w
      else
        m / w + 1
    in

    let s =
      Bytes.create n
    in

    (* Define a reader for the source array. The reader might run off
       the end if [w] does not divide [m]. *)

    let i = ref 0 in
    let next () =
      let ii = !i in
      if ii = m then
        0 (* ran off the end, pad with zeroes *)
      else
        let v = a.(ii) in
        i := ii + 1;
        v
    in

    (* Fill up the string. *)

    for j = 0 to n - 1 do
      let c = ref 0 in
      for _x = 1 to w do
        c := (!c lsl k) lor next()
      done;
      Bytes.set s j (Char.chr !c)
    done;

    (* Done. *)

    k, Bytes.unsafe_to_string s

  end
  else begin (* k > 8 *)

    (* [w] is the number of characters that we use to encode an array entry. *)

    assert (k mod 8 = 0);
    let w = k / 8 in

    (* [n] is the length of the string that we allocate. *)

    let n =
      m * w
    in

    let s =
      Bytes.create n
    in

    (* Fill up the string. *)

    for i = 0 to m - 1 do
      let v = ref a.(i) in
      for x = 1 to w do
        Bytes.set s ((i + 1) * w - x) (Char.chr (!v land 255));
        v := !v lsr 8
      done
    done;

    (* Done. *)

    k, Bytes.unsafe_to_string s

  end

(* Access to a string. *)

let read (s : string) (i : int) : int =
  Char.code (String.unsafe_get s i)

(* [get1 t i] returns the integer stored in the packed array [t] at index [i].
   It assumes (and does not check) that the array's bit width is [1]. The
   parameter [t] is just a string. *)

let get1 (s : string) (i : int) : int =
  let c = read s (i lsr 3) in
  let c = c lsr ((lnot i) land 0b111) in
  let c = c land 0b1 in
  c

(* [get t i] returns the integer stored in the packed array [t] at index [i]. *)

(* Together, [pack] and [get] satisfy the following property: if the index [i]
   is within bounds, then [get (pack a) i] equals [a.(i)]. *)

let get ((k, s) : t) (i : int) : int =
  match k with
  | 1 ->
      get1 s i
  | 2 ->
      let c = read s (i lsr 2) in
      let c = c lsr (2 * ((lnot i) land 0b11)) in
      let c = c land 0b11 in
      c
  | 4 ->
      let c = read s (i lsr 1) in
      let c = c lsr (4 * ((lnot i) land 0b1)) in
      let c = c land 0b1111 in
      c
  | 8 ->
      read s i
  | 16 ->
      let j = 2 * i in
      (read s j) lsl 8 + read s (j + 1)
  | _ ->
      assert (k = 32); (* 64 bits unlikely, not supported *)
      let j = 4 * i in
      (((read s j lsl 8) + read s (j + 1)) lsl 8 + read s (j + 2)) lsl 8 + read s (j + 3)

(* [unflatten1 (n, data) i j] accesses the two-dimensional bitmap
   represented by [(n, data)] at indices [i] and [j]. The integer
   [n] is the width of the bitmap; the string [data] is the second
   component of the packed array obtained by encoding the table as
   a one-dimensional array. *)

let unflatten1 (n, data) i j =
   get1 data (n * i + j)

end
module RowDisplacement = struct
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(* This module compresses a two-dimensional table, where some values
   are considered insignificant, via row displacement. *)

(* This idea reportedly appears in Aho and Ullman's ``Principles
   of Compiler Design'' (1977). It is evaluated in Tarjan and Yao's
   ``Storing a Sparse Table'' (1979) and in Dencker, Dürre, and Heuft's
   ``Optimization of Parser Tables for Portable Compilers'' (1984). *)

(* A compressed table is represented as a pair of arrays. The
   displacement array is an array of offsets into the data array. *)

type 'a table =
    int array * (* displacement *)
     'a array   (* data *)

(* In a natural version of this algorithm, displacements would be greater
   than (or equal to) [-n]. However, in the particular setting of Menhir,
   both arrays are intended to be compressed with [PackedIntArray], which
   does not efficiently support negative numbers. For this reason, we are
   careful not to produce negative displacements. *)

(* In order to avoid producing negative displacements, we simply use the
   least significant bit as the sign bit. This is implemented by [encode]
   and [decode] below. *)

(* One could also think, say, of adding [n] to every displacement, so as
   to ensure that all displacements are nonnegative. This would work, but
   would require [n] to be published, for use by the decoder. *)

let encode (displacement : int) : int =
  if displacement >= 0 then
    displacement lsl 1
  else
    (-displacement) lsl 1 + 1

let decode (displacement : int) : int =
  if displacement land 1 = 0 then
    displacement lsr 1
  else
    -(displacement lsr 1)

(* It is reasonable to assume that, as matrices grow large, their
   density becomes low, i.e., they have many insignificant entries.
   As a result, it is important to work with a sparse data structure
   for rows. We internally represent a row as a list of its
   significant entries, where each entry is a pair of a [j] index and
   an element. *)

type 'a row =
    (int * 'a) list

(* [compress equal insignificant dummy m n t] turns the two-dimensional table
   [t] into a compressed table. The parameter [equal] is equality of data
   values. The parameter [wildcard] tells which data values are insignificant,
   and can thus be overwritten with other values. The parameter [dummy] is
   used to fill holes in the data array. [m] and [n] are the integer
   dimensions of the table [t]. *)

let compress
    (equal : 'a -> 'a -> bool)
    (insignificant : 'a -> bool)
    (dummy : 'a)
    (m : int) (n : int)
    (t : 'a array array)
    : 'a table =

  (* Be defensive. *)

  assert (Array.length t = m);
  assert begin
    for i = 0 to m - 1 do
      assert (Array.length t.(i) = n)
    done;
    true
  end;

  (* This turns a row-as-array into a row-as-sparse-list. The row is
     accompanied by its index [i] and by its rank (the number of its
     significant entries, that is, the length of the row-as-a-list. *)

  let sparse (i : int) (line : 'a array) : int * int * 'a row (* index, rank, row *) =

    let rec loop (j : int) (rank : int) (row : 'a row) =
      if j < 0 then
        i, rank, row
      else
        let x = line.(j) in
        if insignificant x then
          loop (j - 1) rank row
        else
          loop (j - 1) (1 + rank) ((j, x) :: row)
    in

    loop (n - 1) 0 []

  in

  (* Construct an array of all rows, together with their index and rank. *)

  let rows : (int * int * 'a row) array = (* index, rank, row *)
    Array.mapi sparse t
  in

  (* Sort this array by decreasing rank. This does not have any impact
     on correctness, but reportedly improves compression. The
     intuitive idea is that rows with few significant elements are
     easy to fit, so they should be inserted last, after the problem
     has become quite constrained by fitting the heavier rows. This
     heuristic is attributed to Ziegler. *)

  Array.fast_sort (fun (_, rank1, _) (_, rank2, _) ->
    compare rank2 rank1
  ) rows;

  (* Allocate a one-dimensional array of displacements. *)

  let displacement : int array =
    Array.make m 0
  in

  (* Allocate a one-dimensional, infinite array of values. Indices
     into this array are written [k]. *)

  let data : 'a InfiniteArray.t =
    InfiniteArray.make dummy
  in

  (* Determine whether [row] fits at offset [k] within the current [data]
     array, up to extension of this array. *)

  (* Note that this check always succeeds when [k] equals the length of
     the [data] array. Indeed, the loop is then skipped. This property
     guarantees the termination of the recursive function [fit] below. *)

  let fits k (row : 'a row) : bool =

    let d = InfiniteArray.extent data in

    let rec loop = function
      | [] ->
          true
      | (j, x) :: row ->

          (* [x] is a significant element. *)

          (* By hypothesis, [k + j] is nonnegative. If it is greater than or
             equal to the current length of the data array, stop -- the row
             fits. *)

          assert (k + j >= 0);

          if k + j >= d then
            true

          (* We now know that [k + j] is within bounds of the data
             array. Check whether it is compatible with the element [y] found
             there. If it is, continue. If it isn't, stop -- the row does not
             fit. *)

          else
            let y = InfiniteArray.get data (k + j) in
            if insignificant y || equal x y then
              loop row
            else
              false

    in
    loop row

  in

  (* Find the leftmost position where a row fits. *)

  (* If the leftmost significant element in this row is at offset [j],
     then we can hope to fit as far left as [-j] -- so this element
     lands at offset [0] in the data array. *)

  (* Note that displacements may be negative. This means that, for
     insignificant elements, accesses to the data array could fail: they could
     be out of bounds, either towards the left or towards the right. This is
     not a problem, as long as [get] is invoked only at significant
     elements. *)

  let rec fit k row : int =
    if fits k row then
      k
    else
      fit (k + 1) row
  in

  let fit row =
    match row with
    | [] ->
        0 (* irrelevant *)
    | (j, _) :: _ ->
        fit (-j) row
  in

  (* Write [row] at (compatible) offset [k]. *)

  let rec write k = function
    | [] ->
        ()
    | (j, x) :: row ->
        InfiniteArray.set data (k + j) x;
        write k row
  in

  (* Iterate over the sorted array of rows. Fit and write each row at
     the leftmost compatible offset. Update the displacement table. *)

  Array.iter (fun (i, _, row) ->
    let k = fit row in (* if [row] has leading insignificant elements, then [k] can be negative *)
    write k row;
    displacement.(i) <- encode k
  ) rows;

  (* Return the compressed tables. *)

  displacement, InfiniteArray.domain data

(* [get ct i j] returns the value found at indices [i] and [j] in the
   compressed table [ct]. This function call is permitted only if the
   value found at indices [i] and [j] in the original table is
   significant -- otherwise, it could fail abruptly. *)

(* Together, [compress] and [get] have the property that, if the value
   found at indices [i] and [j] in an uncompressed table [t] is
   significant, then [get (compress t) i j] is equal to that value. *)

let get (displacement, data) i j =
  assert (0 <= i && i < Array.length displacement);
  let k = decode displacement.(i) in
  assert (0 <= k + j && k + j < Array.length data);
    (* failure of this assertion indicates an attempt to access an
       insignificant element that happens to be mapped out of the bounds
       of the [data] array. *)
  data.(k + j)

(* [getget] is a variant of [get] which only requires read access,
   via accessors, to the two components of the table. *)

let getget get_displacement get_data (displacement, data) i j =
  let k = decode (get_displacement displacement i) in
  get_data data (k + j)
end
module LinearizedArray = struct
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(* The [entry] array contains offsets into the [data] array. It has [n+1]
   elements if the original (unencoded) array has [n] elements. The value
   of [entry.(n)] is the length of the [data] array. This convention is
   natural and allows avoiding a special case. *)

type 'a t =
  (* data: *)   'a array *
  (* entry: *) int array

let make (a : 'a array array) : 'a t =
  let n = Array.length a in
  (* Build the entry array. *)
  let size = ref 0 in
  let entry = Array.init (n + 1) (fun i ->
    let s = !size in
    if i < n then
      size := s + Array.length a.(i);
    s
  ) in
  assert (entry.(n) = !size);
  (* Build the data array. *)
  let i = ref 0
  and j = ref 0 in
  let data = Array.init !size (fun _ ->
    while !j = Array.length a.(!i) do
      i := !i + 1;
      j := 0;
    done;
    let x = a.(!i).(!j) in
    j := !j + 1;
    x
  ) in
  data, entry

let length ((_, entry) : 'a t) : int =
  Array.length entry

let row_length ((_, entry) : 'a t) i : int =
  entry.(i + 1) - entry.(i)

let row_length_via get_entry i =
  get_entry (i + 1) - get_entry i

let read ((data, entry) as la : 'a t) i j : 'a =
  assert (0 <= j && j < row_length la i);
  data.(entry.(i) + j)

let read_via get_data get_entry i j =
  assert (0 <= j && j < row_length_via get_entry i);
  get_data (get_entry i + j)

let write ((data, entry) as la : 'a t) i j (v : 'a) : unit =
  assert (0 <= j && j < row_length la i);
  data.(entry.(i) + j) <- v

let rec read_interval_via get_data i j =
  if i = j then
    []
  else
    get_data i :: read_interval_via get_data (i + 1) j

let read_row_via get_data get_entry i =
  read_interval_via get_data (get_entry i) (get_entry (i + 1))

let read_row ((data, entry) : 'a t) i : 'a list =
  read_row_via (Array.get data) (Array.get entry) i

end
module TableFormat = struct
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(* This signature defines the format of the parse tables. It is used as
   an argument to [TableInterpreter.Make]. *)

module type TABLES = sig

  (* This is the parser's type of tokens. *)

  type token

  (* This maps a token to its internal (generation-time) integer code. *)

  val token2terminal: token -> int

  (* This is the integer code for the error pseudo-token. *)

  val error_terminal: int

  (* This maps a token to its semantic value. *)

  val token2value: token -> Obj.t

  (* Traditionally, an LR automaton is described by two tables, namely, an
     action table and a goto table. See, for instance, the Dragon book.

     The action table is a two-dimensional matrix that maps a state and a
     lookahead token to an action. An action is one of: shift to a certain
     state, reduce a certain production, accept, or fail.

     The goto table is a two-dimensional matrix that maps a state and a
     non-terminal symbol to either a state or undefined. By construction, this
     table is sparse: its undefined entries are never looked up. A compression
     technique is free to overlap them with other entries.

     In Menhir, things are slightly different. If a state has a default
     reduction on token [#], then that reduction must be performed without
     consulting the lookahead token. As a result, we must first determine
     whether that is the case, before we can obtain a lookahead token and use it
     as an index in the action table.

     Thus, Menhir's tables are as follows.

     A one-dimensional default reduction table maps a state to either ``no
     default reduction'' (encoded as: 0) or ``by default, reduce prod''
     (encoded as: 1 + prod). The action table is looked up only when there
     is no default reduction. *)

  val default_reduction: PackedIntArray.t

  (* Menhir follows Dencker, Dürre and Heuft, who point out that, although the
     action table is not sparse by nature (i.e., the error entries are
     significant), it can be made sparse by first factoring out a binary error
     matrix, then replacing the error entries in the action table with undefined
     entries. Thus:

     A two-dimensional error bitmap maps a state and a terminal to either
     ``fail'' (encoded as: 0) or ``do not fail'' (encoded as: 1). The action
     table, which is now sparse, is looked up only in the latter case. *)

  (* The error bitmap is flattened into a one-dimensional table; its width is
     recorded so as to allow indexing. The table is then compressed via
     [PackedIntArray]. The bit width of the resulting packed array must be
     [1], so it is not explicitly recorded. *)

  (* The error bitmap does not contain a column for the [#] pseudo-terminal.
     Thus, its width is [Terminal.n - 1]. We exploit the fact that the integer
     code assigned to [#] is greatest: the fact that the right-most column
     in the bitmap is missing does not affect the code for accessing it. *)

  val error: int (* width of the bitmap *) * string (* second component of [PackedIntArray.t] *)

  (* A two-dimensional action table maps a state and a terminal to one of
     ``shift to state s and discard the current token'' (encoded as: s | 10),
     ``shift to state s without discarding the current token'' (encoded as: s |
     11), or ``reduce prod'' (encoded as: prod | 01). *)

  (* The action table is first compressed via [RowDisplacement], then packed
     via [PackedIntArray]. *)

  (* Like the error bitmap, the action table does not contain a column for the
     [#] pseudo-terminal. *)

  val action: PackedIntArray.t * PackedIntArray.t

  (* A one-dimensional lhs table maps a production to its left-hand side (a
     non-terminal symbol). *)

  val lhs: PackedIntArray.t

  (* A two-dimensional goto table maps a state and a non-terminal symbol to
     either undefined (encoded as: 0) or a new state s (encoded as: 1 + s). *)

  (* The goto table is first compressed via [RowDisplacement], then packed
     via [PackedIntArray]. *)

  val goto: PackedIntArray.t * PackedIntArray.t

  (* The number of start productions. A production [prod] is a start
     production if and only if [prod < start] holds. This is also the
     number of start symbols. A nonterminal symbol [nt] is a start
     symbol if and only if [nt < start] holds. *)

  val start: int

  (* A one-dimensional semantic action table maps productions to semantic
     actions. The calling convention for semantic actions is described in
     [EngineTypes]. This table contains ONLY NON-START PRODUCTIONS, so the
     indexing is off by [start]. Be careful. *)

  val semantic_action: ((int, Obj.t, token) EngineTypes.env ->
                        (int, Obj.t)        EngineTypes.stack) array

  (* The parser defines its own [Error] exception. This exception can be
     raised by semantic actions and caught by the engine, and raised by the
     engine towards the final user. *)

  exception Error

  (* The parser indicates whether to generate a trace. Generating a
     trace requires two extra tables, which respectively map a
     terminal symbol and a production to a string. *)

  val trace: (string array * string array) option

end
end
module InspectionTableFormat = struct
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(* This signature defines the format of the tables that are produced (in
   addition to the tables described in [TableFormat]) when the command line
   switch [--inspection] is enabled. It is used as an argument to
   [InspectionTableInterpreter.Make]. *)

module type TABLES = sig

  (* The types of symbols. *)

  include IncrementalEngine.SYMBOLS

  (* The type ['a lr1state] describes an LR(1) state. The generated parser defines
     it internally as [int]. *)

  type 'a lr1state

  (* Some of the tables that follow use encodings of (terminal and
     nonterminal) symbols as integers. So, we need functions that
     map the integer encoding of a symbol to its algebraic encoding. *)

  val    terminal: int -> xsymbol
  val nonterminal: int -> xsymbol

  (* The left-hand side of every production already appears in the
     signature [TableFormat.TABLES], so we need not repeat it here. *)

  (* The right-hand side of every production. This a linearized array
     of arrays of integers, whose [data] and [entry] components have
     been packed. The encoding of symbols as integers in described in
     [TableBackend]. *)

  val rhs: PackedIntArray.t * PackedIntArray.t

  (* A mapping of every (non-initial) state to its LR(0) core. *)

  val lr0_core: PackedIntArray.t

  (* A mapping of every LR(0) state to its set of LR(0) items. Each item is
     represented in its packed form (see [Item]) as an integer. Thus the
     mapping is an array of arrays of integers, which is linearized and
     packed, like [rhs]. *)

  val lr0_items: PackedIntArray.t * PackedIntArray.t

  (* A mapping of every LR(0) state to its incoming symbol, if it has one. *)

  val lr0_incoming: PackedIntArray.t

  (* A table that tells which non-terminal symbols are nullable. *)

  val nullable: string
    (* This is a packed int array of bit width 1. It can be read
       using [PackedIntArray.get1]. *)

  (* A two-table dimensional table, indexed by a nonterminal symbol and
     by a terminal symbol (other than [#]), encodes the FIRST sets. *)

  val first: int (* width of the bitmap *) * string (* second component of [PackedIntArray.t] *)

end

end
module InspectionTableInterpreter = struct
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(* -------------------------------------------------------------------------- *)

(* The type functor. *)

module Symbols (T : sig

  type 'a terminal
  type 'a nonterminal

end) = struct

  open T

  (* This should be the only place in the whole library (and generator!)
     where these types are defined. *)

  type 'a symbol =
    | T : 'a terminal -> 'a symbol
    | N : 'a nonterminal -> 'a symbol

  type xsymbol =
    | X : 'a symbol -> xsymbol

end

(* -------------------------------------------------------------------------- *)

(* The code functor. *)

module Make
  (TT : TableFormat.TABLES)
  (IT : InspectionTableFormat.TABLES
        with type 'a lr1state = int)
  (ET : EngineTypes.TABLE
        with type terminal = int
         and type nonterminal = int
         and type semantic_value = Obj.t)
  (E : sig
     type 'a env = (ET.state, ET.semantic_value, ET.token) EngineTypes.env
   end)
= struct

  (* Including [IT] is an easy way of inheriting the definitions of the types
     [symbol] and [xsymbol]. *)

  include IT

  (* This auxiliary function decodes a packed linearized array, as created by
     [TableBackend.linearize_and_marshal1]. Here, we read a row all at once. *)

  let read_packed_linearized
    (data, entry : PackedIntArray.t * PackedIntArray.t) (i : int) : int list
  =
    LinearizedArray.read_row_via
      (PackedIntArray.get data)
      (PackedIntArray.get entry)
      i

  (* This auxiliary function decodes a symbol. The encoding was done by
     [encode_symbol] or [encode_symbol_option] in the table back-end. *)

  let decode_symbol (symbol : int) : IT.xsymbol =
    (* If [symbol] is 0, then we have no symbol. This could mean e.g.
       that the function [incoming_symbol] has been applied to an
       initial state. In principle, this cannot happen. *)
    assert (symbol > 0);
    (* The low-order bit distinguishes terminal and nonterminal symbols. *)
    let kind = symbol land 1 in
    let symbol = symbol lsr 1 in
    if kind = 0 then
      IT.terminal (symbol - 1)
    else
      IT.nonterminal symbol

  (* These auxiliary functions convert a symbol to its integer code. For speed
     and for convenience, we use an unsafe type cast. This relies on the fact
     that the data constructors of the [terminal] and [nonterminal] GADTs are
     declared in an order that reflects their internal code. In the case of
     nonterminal symbols, we add [start] to account for the presence of the
     start symbols. *)

  let n2i (nt : 'a IT.nonterminal) : int =
    let answer = TT.start + Obj.magic nt in
    (* For safety, check that the above cast produced a correct result. *)
    assert (IT.nonterminal answer = X (N nt));
    answer

  let t2i (t : 'a IT.terminal) : int =
    let answer = Obj.magic t in
    (* For safety, check that the above cast produced a correct result. *)
    assert (IT.terminal answer = X (T t));
    answer

  (* Ordering functions. *)

  let compare_terminals t1 t2 =
    (* Subtraction is safe because overflow is impossible. *)
    t2i t1 - t2i t2

  let compare_nonterminals nt1 nt2 =
    (* Subtraction is safe because overflow is impossible. *)
    n2i nt1 - n2i nt2

  let compare_symbols symbol1 symbol2 =
    match symbol1, symbol2 with
    | X (T _), X (N _) ->
        -1
    | X (N _), X (T _) ->
        1
    | X (T t1), X (T t2) ->
        compare_terminals t1 t2
    | X (N nt1), X (N nt2) ->
        compare_nonterminals nt1 nt2

  let compare_productions prod1 prod2 =
    (* Subtraction is safe because overflow is impossible. *)
    prod1 - prod2

  let compare_items (prod1, index1) (prod2, index2) =
    let c = compare_productions prod1 prod2 in
    (* Subtraction is safe because overflow is impossible. *)
    if c <> 0 then c else index1 - index2

  (* The function [incoming_symbol] goes through the tables [IT.lr0_core] and
     [IT.lr0_incoming]. This yields a representation of type [xsymbol], out of
     which we strip the [X] quantifier, so as to get a naked symbol. This last
     step is ill-typed and potentially dangerous. It is safe only because this
     function is used at type ['a lr1state -> 'a symbol], which forces an
     appropriate choice of ['a]. *)

  let incoming_symbol (s : 'a IT.lr1state) : 'a IT.symbol =
    let core = PackedIntArray.get IT.lr0_core s in
    let symbol = decode_symbol (PackedIntArray.get IT.lr0_incoming core) in
    match symbol with
    | IT.X symbol ->
        Obj.magic symbol

  (* The function [lhs] reads the table [TT.lhs] and uses [IT.nonterminal]
     to decode the symbol. *)

  let lhs prod =
    IT.nonterminal (PackedIntArray.get TT.lhs prod)

  (* The function [rhs] reads the table [IT.rhs] and uses [decode_symbol]
     to decode the symbol. *)

  let rhs prod =
    List.map decode_symbol (read_packed_linearized IT.rhs prod)

  (* The function [items] maps the LR(1) state [s] to its LR(0) core,
     then uses [core] as an index into the table [IT.lr0_items]. The
     items are then decoded by the function [export] below, which is
     essentially a copy of [Item.export]. *)

  type item =
      int * int

  let low_bits =
    10

  let low_limit =
    1 lsl low_bits

  let export t : item =
    (t lsr low_bits, t mod low_limit)

  let items s =
    (* Map [s] to its LR(0) core. *)
    let core = PackedIntArray.get IT.lr0_core s in
    (* Now use [core] to look up the table [IT.lr0_items]. *)
    List.map export (read_packed_linearized IT.lr0_items core)

  (* The function [nullable] maps the nonterminal symbol [nt] to its
     integer code, which it uses to look up the array [IT.nullable].
     This yields 0 or 1, which we map back to a Boolean result. *)

  let decode_bool i =
    assert (i = 0 || i = 1);
    i = 1

  let nullable nt =
    decode_bool (PackedIntArray.get1 IT.nullable (n2i nt))

  (* The function [first] maps the symbols [nt] and [t] to their integer
     codes, which it uses to look up the matrix [IT.first]. *)

  let first nt t =
    decode_bool (PackedIntArray.unflatten1 IT.first (n2i nt) (t2i t))

  let xfirst symbol t =
    match symbol with
    | X (T t') ->
        compare_terminals t t' = 0
    | X (N nt) ->
        first nt t

  (* The function [foreach_terminal] exploits the fact that the
     first component of [TT.error] is [Terminal.n - 1], i.e., the
     number of terminal symbols, including [error] but not [#]. *)

  let rec foldij i j f accu =
    if i = j then
      accu
    else
      foldij (i + 1) j f (f i accu)

  let foreach_terminal f accu =
    let n, _ = TT.error in
    foldij 0 n (fun i accu ->
      f (IT.terminal i) accu
    ) accu

  let foreach_terminal_but_error f accu =
    let n, _ = TT.error in
    foldij 0 n (fun i accu ->
      if i = TT.error_terminal then
        accu
      else
        f (IT.terminal i) accu
    ) accu

  (* ------------------------------------------------------------------------ *)

  (* The following is the implementation of the function [feed]. This function
     is logically part of the LR engine, so it would be nice if it were placed
     in the module [Engine], but it must be placed here because, to ensure
     type safety, its arguments must be a symbol of type ['a symbol] and a
     semantic value of type ['a]. The type ['a symbol] is not available in
     [Engine]. It is available here. *)

  open EngineTypes
  open ET
  open E

  (* [feed] fails if the current state does not have an outgoing transition
     labeled with the desired symbol. This check is carried out at runtime. *)

  let feed_failure () =
    invalid_arg "feed: outgoing transition does not exist"

  (* Feeding a nonterminal symbol [nt]. Here, [nt] has type [nonterminal],
     which is a synonym for [int], and [semv] has type [semantic_value],
     which is a synonym for [Obj.t]. This type is unsafe, because pushing
     a semantic value of arbitrary type into the stack can later cause a
     semantic action to crash and burn. The function [feed] is given a safe
     type below. *)

  let feed_nonterminal
        (nt : nonterminal) startp (semv : semantic_value) endp (env : 'b env)
      : 'b env
  =
    (* Check if the source state has an outgoing transition labeled [nt].
       This is done by consulting the [goto] table. *)
    let source = env.current in
    match ET.maybe_goto_nt source nt with
    | None ->
        feed_failure()
    | Some target ->
        (* Push a new cell onto the stack, containing the identity of the state
           that we are leaving. The semantic value [semv] and positions [startp]
           and [endp] contained in the new cell are provided by the caller. *)
        let stack = { state = source; semv; startp; endp; next = env.stack } in
        (* Move to the target state. *)
        { env with stack; current = target }

  let reduce   _env _prod = feed_failure()
  let initiate _env       = feed_failure()

  let feed_terminal
        (terminal : terminal) startp (semv : semantic_value) endp (env : 'b env)
      : 'b env
  =
    (* Check if the source state has an outgoing transition labeled [terminal].
       This is done by consulting the [action] table. *)
    let source = env.current in
    ET.action source terminal semv
      (fun env _please_discard _terminal semv target ->
        (* There is indeed a transition toward the state [target].
           Push a new cell onto the stack and move to the target state. *)
        let stack = { state = source; semv; startp; endp; next = env.stack } in
        { env with stack; current = target }
      ) reduce initiate env

  (* The type assigned to [feed] ensures that the type of the semantic value
     [semv] is appropriate: it must be the semantic-value type of the symbol
     [symbol]. *)

  let feed (symbol : 'a symbol) startp (semv : 'a) endp env =
    let semv : semantic_value = Obj.repr semv in
    match symbol with
    | N nt ->
        feed_nonterminal (n2i nt) startp semv endp env
    | T terminal ->
        feed_terminal (t2i terminal) startp semv endp env

end
end
module TableInterpreter = struct
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

module MakeEngineTable (T : TableFormat.TABLES) = struct

  type state =
      int

  let number s = s

  type token =
      T.token

  type terminal =
      int

  type nonterminal =
      int

  type semantic_value =
      Obj.t

  let token2terminal =
    T.token2terminal

  let token2value =
    T.token2value

  let error_terminal =
    T.error_terminal

  let error_value =
    Obj.repr ()

  (* The function [foreach_terminal] exploits the fact that the
     first component of [T.error] is [Terminal.n - 1], i.e., the
     number of terminal symbols, including [error] but not [#]. *)

  (* There is similar code in [InspectionTableInterpreter]. The
     code there contains an additional conversion of the type
     [terminal] to the type [xsymbol]. *)

  let rec foldij i j f accu =
    if i = j then
      accu
    else
      foldij (i + 1) j f (f i accu)

  let foreach_terminal f accu =
    let n, _ = T.error in
    foldij 0 n (fun i accu ->
      f i accu
    ) accu

  type production =
      int

  (* In principle, only non-start productions are exposed to the user,
     at type [production] or at type [int]. This is checked dynamically. *)
  let non_start_production i =
    assert (T.start <= i && i - T.start < Array.length T.semantic_action)

  let production_index i =
    non_start_production i;
    i

  let find_production i =
    non_start_production i;
    i

  let default_reduction state defred nodefred env =
    let code = PackedIntArray.get T.default_reduction state in
    if code = 0 then
      (* no default reduction *)
      nodefred env
    else
      (* default reduction *)
      let prod = code - 1 in
      defred env prod

  let is_start prod =
    prod < T.start

  (* This auxiliary function helps access a compressed, two-dimensional
     matrix, like the action and goto tables. *)

  let unmarshal2 table i j =
    RowDisplacement.getget
      PackedIntArray.get
      PackedIntArray.get
      table
      i j

  let action state terminal value shift reduce fail env =
    match PackedIntArray.unflatten1 T.error state terminal with
    | 1 ->
        let action = unmarshal2 T.action state terminal in
        let opcode = action land 0b11
        and param = action lsr 2 in
        if opcode >= 0b10 then
          (* 0b10 : shift/discard *)
          (* 0b11 : shift/nodiscard *)
          let please_discard = (opcode = 0b10) in
          shift env please_discard terminal value param
        else
          (* 0b01 : reduce *)
          (* 0b00 : cannot happen *)
          reduce env param
    | c ->
        assert (c = 0);
        fail env

  let maybe_shift_t state terminal =
    match PackedIntArray.unflatten1 T.error state terminal with
    | 1 ->
        let action = unmarshal2 T.action state terminal in
        let opcode = action land 0b11 in
        if opcode >= 0b10 then
          (* 0b10 : shift/discard *)
          (* 0b11 : shift/nodiscard *)
          let state' = action lsr 2 in
          Some state'
        else
          (* 0b01 : reduce *)
          (* 0b00 : cannot happen *)
          None
    | c ->
        assert (c = 0);
        None

  let may_reduce_prod state terminal prod =
    let code = PackedIntArray.get T.default_reduction state in
    if code = 0 then
      (* no default reduction *)
      match PackedIntArray.unflatten1 T.error state terminal with
      | 1 ->
          let action = unmarshal2 T.action state terminal in
          let opcode = action land 0b11 in
          if opcode >= 0b10 then
            (* 0b10 : shift/discard *)
            (* 0b11 : shift/nodiscard *)
            false
          else
            (* 0b01 : reduce *)
            (* 0b00 : cannot happen *)
            let prod' = action lsr 2 in
            prod = prod'
      | c ->
          assert (c = 0);
          false
    else
      (* default reduction *)
      let prod' = code - 1 in
      prod = prod'

  let goto_nt state nt =
    let code = unmarshal2 T.goto state nt in
    (* code = 1 + state *)
    code - 1

  let[@inline] lhs prod =
    PackedIntArray.get T.lhs prod

  let goto_prod state prod =
    goto_nt state (lhs prod)

  let maybe_goto_nt state nt =
    let code = unmarshal2 T.goto state nt in
    (* If [code] is 0, there is no outgoing transition.
       If [code] is [1 + state], there is a transition towards [state]. *)
    assert (0 <= code);
    if code = 0 then None else Some (code - 1)

  exception Error =
        T.Error

  type semantic_action =
      (state, semantic_value, token) EngineTypes.env ->
      (state, semantic_value)        EngineTypes.stack

  let semantic_action prod =
    (* Indexing into the array [T.semantic_action] is off by [T.start],
       because the start productions do not have entries in this array. *)
    T.semantic_action.(prod - T.start)

  (* [may_reduce state prod] tests whether the state [state] is capable of
     reducing the production [prod]. This information could be determined
     in constant time if we were willing to create a bitmap for it, but
     that would take up a lot of space. Instead, we obtain this information
     by iterating over a line in the action table. This is costly, but this
     function is not normally used by the LR engine anyway; it is supposed
     to be used only by programmers who wish to develop error recovery
     strategies. *)

  (* In the future, if desired, we could memoize this function, so as
     to pay the cost in (memory) space only if and where this function
     is actually used. We could also replace [foreach_terminal] with a
     function [exists_terminal] which stops as soon as the accumulator
     is [true]. *)

  let may_reduce state prod =
    (* Test if there is a default reduction of [prod]. *)
    default_reduction state
      (fun () prod' -> prod = prod')
      (fun () ->
        (* If not, then for each terminal [t], ... *)
        foreach_terminal (fun t accu ->
          accu ||
          (* ... test if there is a reduction of [prod] on [t]. *)
          action state t ()
            (* shift:  *) (fun () _ _ () _ -> false)
            (* reduce: *) (fun () prod' -> prod = prod')
            (* fail:   *) (fun () -> false)
            ()
        ) false
      )
      ()

  (* If [T.trace] is [None], then the logging functions do nothing. *)

  let log =
    match T.trace with Some _ -> true | None -> false

  module Log = struct

    open Printf

    let state state =
      match T.trace with
      | Some _ ->
          fprintf stderr "State %d:\n%!" state
      | None ->
          ()

    let shift terminal state =
      match T.trace with
      | Some (terminals, _) ->
          fprintf stderr "Shifting (%s) to state %d\n%!" terminals.(terminal) state
      | None ->
          ()

    let reduce_or_accept prod =
      match T.trace with
      | Some (_, productions) ->
          fprintf stderr "%s\n%!" productions.(prod)
      | None ->
          ()

    let lookahead_token token startp endp =
      match T.trace with
      | Some (terminals, _) ->
          fprintf stderr "Lookahead token is now %s (%d-%d)\n%!"
            terminals.(token)
            startp.Lexing.pos_cnum
            endp.Lexing.pos_cnum
      | None ->
          ()

    let initiating_error_handling () =
      match T.trace with
      | Some _ ->
          fprintf stderr "Initiating error handling\n%!"
      | None ->
          ()

    let resuming_error_handling () =
      match T.trace with
      | Some _ ->
          fprintf stderr "Resuming error handling\n%!"
      | None ->
          ()

    let handling_error state =
      match T.trace with
      | Some _ ->
          fprintf stderr "Handling error in state %d\n%!" state
      | None ->
          ()

  end

end
end
module StaticVersion = struct
let require_20231231 = ()
end
