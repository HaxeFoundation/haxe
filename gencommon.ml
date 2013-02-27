(*
 * Copyright (C)2005-2013 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *)

(*
  Gen Common API

  This module intends to be a common set of utilities common to all targets.

  It's intended to provide a set of tools to be able to make targets in haXe more easily, and to
  allow the programmer to have more control of how the target language will handle the program.

  For example, as of now, the hxcpp target, while greatly done, relies heavily on cpp's own operator
  overloading, and implicit conversions, which make it very hard to deliver a similar solution for languages
  that lack these features.

  So this little framework is here so you can manipulate the HaXe AST and start bringing the AST closer
  to how it's intenteded to be in your host language.

  Rules

  Design goals

  Naming convention

  Weaknesses and TODO's
*)

open Ast
open Type
open Common
open Option
open Printf

let debug_type_ctor = function
  | TMono _ -> "TMono"
  | TEnum _ -> "TEnum"
  | TInst _ -> "TInst"
  | TType _ -> "TType"
  | TFun _ -> "TFun"
  | TAnon _ -> "TAnon"
  | TDynamic _ -> "TDynamic"
  | TLazy _ -> "TLazy"
  | TAbstract _ -> "TAbstract"

let debug_type = (s_type (print_context()))

let debug_expr = s_expr debug_type

let rec like_float t =
  match follow t with
    | TAbstract({ a_path = ([], "Float") },[])
    | TAbstract({ a_path = ([], "Int") },[]) -> true
    | TAbstract(a, _) -> List.exists (fun (t,_) -> like_float t) a.a_from || List.exists (fun (t,_) -> like_float t) a.a_to
    | _ -> false

let rec like_int t =
  match follow t with
    | TAbstract({ a_path = ([], "Int") },[]) -> true
    | TAbstract(a, _) -> List.exists (fun (t,_) -> like_int t) a.a_from || List.exists (fun (t,_) -> like_float t) a.a_to
    | _ -> false


let follow_once t =
  match t with
	| TMono r ->
		(match !r with
		| Some t -> t
		| _ -> t_dynamic (* avoid infinite loop / should be the same in this context *))
	| TLazy f ->
		!f()
	| TType (t,tl) ->
		apply_params t.t_types tl t.t_type
	| _ -> t

let t_empty = TAnon({ a_fields = PMap.empty; a_status = ref (Closed) })

(* the undefined is a special var that works like null, but can have special meaning *)
let v_undefined = alloc_var "__undefined__" t_dynamic

let undefined pos = { eexpr = TLocal(v_undefined); etype = t_dynamic; epos = pos }

module ExprHashtblHelper =
struct
  type hash_texpr_t =
  {
    hepos : pos;
    heexpr : int;
    hetype : int;
  }

  let mk_heexpr = function
    | TConst _ -> 0 | TLocal _ -> 1 | TArray _ -> 3 | TBinop _ -> 4 | TField _ -> 5 | TTypeExpr _ -> 7 | TParenthesis _ -> 8 | TObjectDecl _ -> 9
    | TArrayDecl _ -> 10 | TCall _ -> 11 | TNew _ -> 12 | TUnop _ -> 13 | TFunction _ -> 14 | TVars _ -> 15 | TBlock _ -> 16 | TFor _ -> 17 | TIf _ -> 18 | TWhile _ -> 19
    | TSwitch _ -> 20 | TMatch _ -> 21 | TTry _ -> 22 | TReturn _ -> 23 | TBreak -> 24 | TContinue -> 25 | TThrow _ -> 26 | TCast _ -> 27

  let mk_heetype = function
    | TMono _ -> 0 | TEnum _ -> 1 | TInst _ -> 2 | TType _ -> 3 | TFun _ -> 4
    | TAnon _ -> 5 | TDynamic _ -> 6 | TLazy _ -> 7 | TAbstract _ -> 8

  let mk_type e =
    {
      hepos = e.epos;
      heexpr = mk_heexpr e.eexpr;
      hetype = mk_heetype e.etype;
    }
end;;

open ExprHashtblHelper;;
(* Expression Hashtbl. This shouldn't be kept indefinately as it's not a weak Hashtbl. *)
module ExprHashtbl = Hashtbl.Make(
    struct
      type t = Type.texpr

      let equal = (==)
      let hash t = Hashtbl.hash (mk_type t)
    end
);;

(* ******************************************* *)
(*  Gen Common

This is the key module for generation of Java and C# sources
In order for both modules to share as much code as possible, some
rules were devised:

- every feature has its own submodule, and may contain the following methods:
  - configure
    sets all the configuration variables for the module to run. If a module has this method,
    it *should* be called once before running any filter
  - run_filter ->
    runs the filter immediately on the context
  - add_filter ->
    adds the filter to an expr->expr list. Most filter modules will provide this option so the filter
    function can only run once.
- most submodules will have side-effects so the order of operations will matter.
  When running configure / add_filter this might be taken care of with the rule-based dispatch system working
  underneath, but still there might be some incompatibilities. There will be an effort to document it.
  The modules can hint on the order by suffixing their functions with _first or _last.
- any of those methods might have different parameters, that configure how the filter will run.
  For example, a simple filter that maps switch() expressions to if () .. else if... might receive
  a function that filters what content should be mapped
- Other targets can use those filters on their own code. In order to do that,
  a simple configuration step is needed: you need to initialize a generator_ctx type with
  Gencommon.new_gen (context:Common.context)
  with a generator_ctx context you will be able to add filters to your code, and execute them with
  Gencommon.run_filters (gen_context:Gencommon.generator_ctx)

  After running the filters, you can run your own generator normally.

  (* , or you can run
  Gencommon.generate_modules (gen_context:Gencommon.generator_ctx) (extension:string) (module_gen:module_type list->bool)
  where module_gen will take a whole module (can be *)

*)

(* ******************************************* *)
(* common helpers *)
(* ******************************************* *)

let assertions = false (* when assertions == true, many assertions will be made to guarantee the quality of the data input *)
let debug_mode = ref false
let trace s = if !debug_mode then print_endline s else ()
let timer name = if !debug_mode then Common.timer name else fun () -> ()

let is_string t = match follow t with | TInst({ cl_path = ([], "String") }, []) -> true | _ -> false

(* helper function for creating Anon types of class / enum modules *)

let anon_of_classtype cl =
  TAnon {
    a_fields = cl.cl_statics;
    a_status = ref (Statics cl)
  }

let anon_of_enum e =
  TAnon {
    a_fields = PMap.empty;
    a_status = ref (EnumStatics e)
  }

let anon_of_abstract a =
  TAnon {
    a_fields = PMap.empty;
    a_status = ref (AbstractStatics a)
  }

let anon_of_mt mt = match mt with
  | TClassDecl cl -> anon_of_classtype cl
  | TEnumDecl e -> anon_of_enum e
  | TAbstractDecl a -> anon_of_abstract a
  | _ -> assert false

let anon_class t =
    match follow t with
      | TAnon anon ->
        (match !(anon.a_status) with
          | Statics (cl) -> Some(TClassDecl(cl))
          | EnumStatics (e) -> Some(TEnumDecl(e))
          | AbstractStatics (a) -> Some(TAbstractDecl(a))
          | _ -> None)
      | _ -> None

let path_s path =
  match path with | ([], s) -> s | (p, s) -> (String.concat "." (fst path)) ^ "." ^ (snd path)

 let rec t_to_md t = match t with
  | TInst (cl,_) -> TClassDecl cl
  | TEnum (e,_) -> TEnumDecl e
  | TType (t,_) -> TTypeDecl t
  | TAbstract (a,_) -> TAbstractDecl a
  | TAnon anon ->
    (match !(anon.a_status) with
      | EnumStatics e -> TEnumDecl e
      | Statics cl -> TClassDecl cl
      | AbstractStatics a -> TAbstractDecl a
      | _ -> assert false)
  | TLazy f -> t_to_md (!f())
  | TMono r -> (match !r with | Some t -> t_to_md t | None -> assert false)
  | _ -> assert false

let get_cl mt = match mt with | TClassDecl cl -> cl | _ -> failwith ("Unexpected module type of '" ^ path_s (t_path mt) ^ "'")

let get_tdef mt = match mt with | TTypeDecl t -> t | _ -> assert false

let mk_mt_access mt pos = { eexpr = TTypeExpr(mt); etype = anon_of_mt mt; epos = pos }

let is_void t = match follow t with
  | TEnum({ e_path = ([], "Void") }, [])
  | TAbstract ({ a_path = ([], "Void") },[]) ->
      true
  | _ -> false

let mk_local var pos = { eexpr = TLocal(var); etype = var.v_type; epos = pos }

(* this function is used by CastDetection module *)
let get_fun t =
  match follow t with | TFun(r1,r2) -> (r1,r2) | _ -> (trace (s_type (print_context()) (follow t) )); assert false

let mk_cast t e =
  { eexpr = TCast(e, None); etype = t; epos = e.epos }

let mk_classtype_access cl pos =
  { eexpr = TTypeExpr(TClassDecl(cl)); etype = anon_of_classtype cl; epos = pos }

let mk_static_field_access_infer cl field pos params =
  try
    let cf = (PMap.find field cl.cl_statics) in
    { eexpr = TField(mk_classtype_access cl pos, FStatic(cl, cf)); etype = (if params = [] then cf.cf_type else apply_params cf.cf_params params cf.cf_type); epos = pos }
  with | Not_found -> failwith ("Cannot find field " ^ field ^ " in type " ^ (path_s cl.cl_path))

let mk_static_field_access cl field fieldt pos =
  { (mk_static_field_access_infer cl field pos []) with etype = fieldt }

(* stolen from Hugh's sources ;-) *)
(* this used to be a class, but there was something in there that crashed ocaml native compiler in windows *)
module SourceWriter =
struct

  type source_writer =
  {
    sw_buf : Buffer.t;
    mutable sw_has_content : bool;
    mutable sw_indent : string;
    mutable sw_indents : string list;
  }

  let new_source_writer () =
    {
      sw_buf = Buffer.create 0;
      sw_has_content = false;
      sw_indent = "";
      sw_indents = [];
    }

  let add_writer w_write w_read = Buffer.add_buffer w_read.sw_buf w_write.sw_buf

  let contents w = Buffer.contents w.sw_buf

  let len w = Buffer.length w.sw_buf

  let write w x =
    (if not w.sw_has_content then begin w.sw_has_content <- true; Buffer.add_string w.sw_buf w.sw_indent; Buffer.add_string w.sw_buf x; end else Buffer.add_string w.sw_buf x);
    let len = (String.length x)-1 in
    if len >= 0 && String.get x len = '\n' then begin w.sw_has_content <- false end else w.sw_has_content <- true

  let push_indent w = w.sw_indents <- "\t"::w.sw_indents; w.sw_indent <- String.concat "" w.sw_indents

  let pop_indent w =
    match w.sw_indents with
      | h::tail -> w.sw_indents <- tail; w.sw_indent <- String.concat "" w.sw_indents
      | [] -> w.sw_indent <- "/*?*/"

  let newline w = write w "\n"

  let begin_block w = (if w.sw_has_content then newline w); write w "{"; push_indent w; newline w

  let end_block w = pop_indent w; (if w.sw_has_content then newline w); write w "}"; newline w

  let print w =
    (if not w.sw_has_content then begin w.sw_has_content <- true; Buffer.add_string w.sw_buf w.sw_indent end);
    bprintf w.sw_buf;

end;;

(* rule_dispatcher's priority *)
type priority =
  | PFirst
  | PLast
  | PZero
  | PCustom of float

exception DuplicateName of string
exception NoRulesApplied

let indent = ref []

(* the rule dispatcher is the primary way to deal with distributed "plugins" *)
(* we will define rules that will form a distributed / extensible match system *)
class ['tp, 'ret] rule_dispatcher name ignore_not_found =
  object(self)
  val tbl = Hashtbl.create 16
  val mutable keys = []
  val names = Hashtbl.create 16
  val mutable temp = 0

  method add ?(name : string option) (* name helps debugging *) ?(priority : priority = PZero) (rule : 'tp->'ret option) =
    let p = match priority with
      | PFirst -> infinity
      | PLast -> neg_infinity
      | PZero -> 0.0
      | PCustom i -> i
    in

    let q = if not( Hashtbl.mem tbl p ) then begin
      let q = Stack.create() in
      Hashtbl.add tbl p q;
      keys <- p :: keys;
      keys <- List.sort (fun x y -> - (compare x y)) keys;
      q
    end else Hashtbl.find tbl p in
    let name = match name with
      | None -> temp <- temp + 1; "$_" ^ (string_of_int temp)
      | Some s -> s
    in
    (if Hashtbl.mem names name then raise (DuplicateName(name)));
    Hashtbl.add names name q;

    Stack.push (name, rule) q

  method describe =
    Hashtbl.iter (fun s _ -> (trace s)) names;

  method remove (name : string) =
    if Hashtbl.mem names name then begin
      let q = Hashtbl.find names name in
      let q_temp = Stack.create () in
      Stack.iter (function
        | (n, _) when n = name -> ()
        | _ as r -> Stack.push r q_temp
      ) q;

      Stack.clear q;
      Stack.iter (fun r -> Stack.push r q) q_temp;

      Hashtbl.remove names name;
      true
    end else false

  method run_f tp = get (self#run tp)

  method did_run tp = is_some (self#run tp)

  method get_list =
    let ret = ref [] in
    List.iter (fun key ->
      let q = Hashtbl.find tbl key in
      Stack.iter (fun (_, rule) -> ret := rule :: !ret) q
    ) keys;

    List.rev !ret

  method run_from (priority:float) (tp:'tp) : 'ret option =
    let ok = ref ignore_not_found in
    let ret = ref None in
    indent := "\t" :: !indent;

    (try begin
      List.iter (fun key ->
        if key < priority then begin
          let q = Hashtbl.find tbl key in
          Stack.iter (fun (n, rule) ->
            let t = if !debug_mode then Common.timer ("rule dispatcher rule: " ^ n) else fun () -> () in
            let r = rule(tp) in
            t();
            if is_some r then begin ret := r; raise Exit end
          ) q
        end
      ) keys

    end with Exit -> ok := true);

    (match !indent with
      | [] -> ()
      | h::t -> indent := t);

    (if not (!ok) then raise NoRulesApplied);
    !ret

  method run (tp:'tp) : 'ret option =
    self#run_from infinity tp

end;;

(* this is a special case where tp = tret and you stack their output as the next's input *)
class ['tp] rule_map_dispatcher name =
  object(self)
  inherit ['tp, 'tp] rule_dispatcher name true as super

  method run_f tp = get (self#run tp)

  method run_from (priority:float) (tp:'tp) : 'ret option =
    let cur = ref tp in
    (try begin
      List.iter (fun key ->

        if key < priority then begin
          let q = Hashtbl.find tbl key in
          Stack.iter (fun (n, rule) ->
            trace ("running rule " ^ n);
            let t = if !debug_mode then Common.timer ("rule map dispatcher rule: " ^ n) else fun () -> () in
            let r = rule(!cur) in
            t();
            if is_some r then begin cur := get r end
          ) q
        end
      ) keys

    end with Exit -> ());
    Some (!cur)

end;;


type generator_ctx =
{
  (* these are the basic context fields. If another target is using this context, *)
  (* this is all you need to care about *)
  mutable gcon : Common.context;

  gclasses : gen_classes;

  gtools : gen_tools;

  (*
    configurable function that receives a desired name and makes it "internal", doing the best
    to ensure that it will not be called from outside.
    To avoid name clashes between internal names, user must specify two strings: a "namespace" and the name itself
   *)
  mutable gmk_internal_name : string->string->string;

  (*
    module filters run before module filters and they should generate valid haxe syntax as a result.
    Module filters shouldn't go through the expressions as it adds an unnecessary burden to the GC,
    and it can all be done in a single step with gexpr_filters and proper priority selection.

    As a convention, Module filters should end their name with Modf, so they aren't mistaken with expression filters
  *)
  gmodule_filters : (module_type) rule_map_dispatcher;

  (*
    expression filters are the most common filters to be applied.
    They should also generate only valid haxe expressions, so e.g. calls to non-existant methods
    should be avoided, although there are some ways around them (like gspecial_methods)
  *)
  gexpr_filters : (texpr) rule_map_dispatcher;
  (*
    syntax filters are also expression filters but they no longer require
    that the resulting expressions be valid haxe expressions.
    They then have no guarantee that either the input expressions or the output one follow the same
    rules as normal haxe code.
  *)
  gsyntax_filters : (texpr) rule_map_dispatcher;

  (* these are more advanced features, but they would require a rewrite of targets *)
  (* they are just helpers to ditribute functions like "follow" or "type to string" *)
  (* so adding a module will already take care of correctly following a certain type of *)
  (* variable, for example *)

  (* follows the type through typedefs, lazy typing, etc. *)
  (* it's the place to put specific rules to handle typedefs, like *)
  (* other basic types like UInt *)
  gfollow : (t, t) rule_dispatcher;

  gtypes : (path, module_type) Hashtbl.t;

  (* cast detection helpers / settings *)
  (* this is a cache for all field access types *)
  greal_field_types : (path * string, (tclass_field (* does the cf exist *) * t (*cf's type in relation to current class type params *) * tclass (* declared class *) ) option) Hashtbl.t;
  (* this function allows any code to handle casts as if it were inside the cast_detect module *)
  mutable ghandle_cast : t->t->texpr->texpr;
  (* when an unsafe cast is made, we can warn the user *)
  mutable gon_unsafe_cast : t->t->pos->unit;
  (* does this type needs to be boxed? Normally always false, unless special type handling must be made *)
  mutable gneeds_box : t->bool;
  (* does this 'special type' needs cast to this other type? *)
  (* this is here so we can implement custom behavior for "opaque" typedefs *)
  mutable gspecial_needs_cast : t->t->bool;
  (* sometimes we may want to support unrelated conversions on cast detection *)
  (* for example, haxe.lang.Null<T> -> T on C# *)
  (* every time an unrelated conversion is found, each to/from path is searched on this hashtbl *)
  (* if found, the function will be executed with from_type, to_type. If returns true, it means that *)
  (* it is a supported conversion, and the unsafe cast routine changes to a simple cast *)
  gsupported_conversions : (path, t->t->bool) Hashtbl.t;

  (* API for filters *)
  (* add type can be called at any time, and will add a new module_def that may or may not be filtered *)
  (* module_type -> should_filter *)
  mutable gadd_type : module_type -> bool -> unit;
  (* during expr filters, add_to_module will be available so module_types can be added to current module_def. we must pass the priority argument so the filters can be resumed  *)
  mutable gadd_to_module : module_type -> float -> unit;
  (* during expr filters, shows the current class path *)
  mutable gcurrent_path : path;
  (* current class *)
  mutable gcurrent_class : tclass option;
  (* current class field, if any *)
  mutable gcurrent_classfield : tclass_field option;

  (* events *)
  (* is executed once every new classfield *)
  mutable gon_classfield_start : (unit -> unit) list;
  (* is executed once every new module type *)
  mutable gon_new_module_type : (unit -> unit) list;
  (* after expression filters ended *)
  mutable gafter_expr_filters_ended : (unit -> unit) list;
  (* after all filters are run *)
  mutable gafter_filters_ended : (unit -> unit) list;

  mutable gbase_class_fields : (string, tclass_field) PMap.t;

  (* real type is the type as it is read by the target. *)
  (* This function is here because most targets don't have *)
  (* a 1:1 translation between haxe types and its native types *)
  (* But types aren't changed to this representation as we might lose *)
  (* some valuable type information in the process *)
  mutable greal_type : t -> t;
  (*
    the same as greal_type but for type parameters.
  *)
  mutable greal_type_param : module_type -> tparams -> tparams;
  (*
    is the type a value type?
    This may be used in some optimizations where reference types and value types
    are handled differently. At first the default is very good to use, and if tweaks are needed,
    it's best to be done by adding @:struct meta to the value types
  *
  mutable gis_value_type : t -> bool;*)

  (* misc configuration *)
  (*
    Should the target allow type parameter dynamic conversion,
    or should we add a cast to those cases as well?
  *)
  mutable gallow_tp_dynamic_conversion : bool;

  (*
    Does the target support type parameter constraints?
    If not, they will be ignored when detecting casts
  *)
  mutable guse_tp_constraints : bool;

  (* internal apis *)
  (* param_func_call : used by TypeParams and CastDetection *)
  mutable gparam_func_call : texpr->texpr->tparams->texpr list->texpr;
  (* does it already have a type parameter cast handler? This is used by CastDetect to know if it should handle type parameter casts *)
  mutable ghas_tparam_cast_handler : bool;
  (* type parameter casts - special cases *)
  (* function cast_from, cast_to -> texpr *)
  gtparam_cast : (path, (texpr->t->texpr)) Hashtbl.t;

  (*
    special vars are used for adding special behavior to
  *)
  gspecial_vars : (string, bool) Hashtbl.t;
}

and gen_classes =
{
  cl_reflect : tclass;
  cl_type : tclass;
  cl_dyn : tclass;

  t_iterator : tdef;
}

(* add here all reflection transformation additions *)
and gen_tools =
{
  (* (klass : texpr, t : t) : texpr *)
  mutable r_create_empty : texpr->t->texpr;
  (* Reflect.fields(). The bool is if we are iterating in a read-only manner. If it is read-only we might not need to allocate a new array *)
  mutable r_fields : bool->texpr->texpr;
  (* (first argument = return type. should be void in most cases) Reflect.setField(obj, field, val) *)
  mutable r_set_field : t->texpr->texpr->texpr->texpr;
  (* Reflect.field. bool indicates if is safe (no error throwing) or unsafe; t is the expected return type true = safe *)
  mutable r_field : bool->t->texpr->texpr->texpr;

  (*
    these are now the functions that will later be used when creating the reflection classes
  *)

  (* on the default implementation (at OverloadingCtors), it will be new SomeClass<params>(EmptyInstance) *)
  mutable rf_create_empty : tclass->tparams->pos->texpr;
}

let get_type types path =
  List.find (fun md -> match md with
    | TClassDecl cl when cl.cl_path = path -> true
    | TEnumDecl e when e.e_path = path -> true
    | TTypeDecl t when t.t_path = path -> true
    | TAbstractDecl a when a.a_path = path -> true
    | _ -> false
  ) types

let new_ctx con =
  let types = Hashtbl.create (List.length con.types) in
  List.iter (fun mt ->
    match mt with
      | TClassDecl cl -> Hashtbl.add types cl.cl_path mt
      | TEnumDecl e -> Hashtbl.add types e.e_path mt
      | TTypeDecl t -> Hashtbl.add types t.t_path mt
      | TAbstractDecl a -> Hashtbl.add types a.a_path mt
  ) con.types;

  let cl_dyn = match get_type con.types ([], "Dynamic") with
    | TClassDecl c -> c
    | TAbstractDecl a ->
        mk_class a.a_module ([], "Dynamic") a.a_pos
    | _ -> assert false
  in

  let rec gen = {
    gcon = con;
    gclasses = {
      cl_reflect = get_cl (get_type con.types ([], "Reflect"));
      cl_type = get_cl (get_type con.types ([], "Type"));
      cl_dyn = cl_dyn;

      t_iterator = get_tdef (get_type con.types ([], "Iterator"));
    };
    gtools = {
      r_create_empty = (fun eclass t ->
        let fieldcall = mk_static_field_access_infer gen.gclasses.cl_type "createEmptyInstance" eclass.epos [t] in
        { eexpr = TCall(fieldcall, [eclass]); etype = t; epos = eclass.epos }
      );
      r_fields = (fun is_used_only_by_iteration expr ->
        let fieldcall = mk_static_field_access_infer gen.gclasses.cl_reflect "fields" expr.epos [] in
        { eexpr = TCall(fieldcall, [expr]); etype = gen.gcon.basic.tarray gen.gcon.basic.tstring; epos = expr.epos }
      );
      (* Reflect.setField(obj, field, val). t by now is ignored. FIXME : fix this implementation *)
      r_set_field = (fun t obj field v ->
        let fieldcall = mk_static_field_access_infer gen.gclasses.cl_reflect "setField" v.epos [] in
        { eexpr = TCall(fieldcall, [obj; field; v]); etype = t_dynamic; epos = v.epos }
      );
      (* Reflect.field. bool indicates if is safe (no error throwing) or unsafe. true = safe *)
      r_field = (fun is_safe t obj field ->
        let fieldcall = mk_static_field_access_infer gen.gclasses.cl_reflect "field" obj.epos [] in
        (* FIXME: should we see if needs to cast? *)
        mk_cast t { eexpr = TCall(fieldcall, [obj; field]); etype = t_dynamic; epos = obj.epos }
      );

      rf_create_empty = (fun cl p pos ->
        gen.gtools.r_create_empty { eexpr = TTypeExpr(TClassDecl cl); epos = pos; etype = t_dynamic } (TInst(cl,p))
      ); (* TODO: Maybe implement using normal reflection? Type.createEmpty(MyClass) *)
    };
    gmk_internal_name = (fun ns s -> sprintf "__%s_%s" ns s);
    gexpr_filters = new rule_map_dispatcher "gexpr_filters";
    gmodule_filters = new rule_map_dispatcher "gmodule_filters";
    gsyntax_filters = new rule_map_dispatcher "gsyntax_filters";
    gfollow = new rule_dispatcher "gfollow" false;
    gtypes = types;

    greal_field_types = Hashtbl.create 0;
    ghandle_cast = (fun to_t from_t e -> mk_cast to_t e);
    gon_unsafe_cast = (fun t t2 pos -> (gen.gcon.warning ("Type " ^ (debug_type t2) ^ " is being cast to the unrelated type " ^ (s_type (print_context()) t)) pos));
    gneeds_box = (fun t -> false);
    gspecial_needs_cast = (fun to_t from_t -> true);
    gsupported_conversions = Hashtbl.create 0;

    gadd_type = (fun md should_filter ->
      if should_filter then begin
        con.types <- md :: con.types;
        con.modules <- { m_id = alloc_mid(); m_path = (t_path md); m_types = [md]; m_extra = module_extra "" "" 0. MFake } :: con.modules
      end else gen.gafter_filters_ended <- (fun () ->
        con.types <- md :: con.types;
        con.modules <- { m_id = alloc_mid(); m_path = (t_path md); m_types = [md]; m_extra = module_extra "" "" 0. MFake } :: con.modules
      ) :: gen.gafter_filters_ended;
    );
    gadd_to_module = (fun md pr -> failwith "module added outside expr filters");
    gcurrent_path = ([],"");
    gcurrent_class = None;
    gcurrent_classfield = None;

    gon_classfield_start = [];
    gon_new_module_type = [];
    gafter_expr_filters_ended = [];
    gafter_filters_ended = [];

    gbase_class_fields = PMap.empty;

    greal_type = (fun t -> t);
    greal_type_param = (fun _ t -> t);

   gallow_tp_dynamic_conversion = false;

    guse_tp_constraints = false;

    (* as a default, ignore the params *)
    gparam_func_call = (fun ecall efield params elist -> { ecall with eexpr = TCall(efield, elist) });
    ghas_tparam_cast_handler = false;
    gtparam_cast = Hashtbl.create 0;

    gspecial_vars = Hashtbl.create 0;
  } in

  (*gen.gtools.r_create_empty <-
  gen.gtools.r_get_class <-
  gen.gtools.r_fields <- *)

  gen

let init_ctx gen =
  (* ultimately add a follow once handler as the last follow handler *)
  let follow_f = gen.gfollow#run in
  let follow t =
    match t with
    | TMono r ->
      (match !r with
      | Some t -> follow_f t
      | _ -> Some t)
    | TLazy f ->
      follow_f (!f())
    | TType (t,tl) ->
      follow_f (apply_params t.t_types tl t.t_type)
    | _ -> Some t
  in
  gen.gfollow#add ~name:"final" ~priority:PLast follow

(* run_follow (gen:generator_ctx) (t:t) *)
let run_follow gen = gen.gfollow#run_f

let reorder_modules gen =
  let modules = Hashtbl.create 20 in
  List.iter (fun md ->
    Hashtbl.add modules ( (t_infos md).mt_module ).m_path md
  ) gen.gcon.types;

  let con = gen.gcon in
  con.modules <- [];
  let processed = Hashtbl.create 20 in
  Hashtbl.iter (fun md_path _ ->
    if not (Hashtbl.mem processed md_path) then begin
      Hashtbl.add processed md_path true;
      con.modules <- { m_id = alloc_mid(); m_path = md_path; m_types = List.rev ( Hashtbl.find_all modules md_path ); m_extra = module_extra "" "" 0. MFake } :: con.modules
    end
  ) modules

let run_filters_from gen t filters =
  match t with
      | TClassDecl c ->
        trace (snd c.cl_path);
        gen.gcurrent_path <- c.cl_path;
        gen.gcurrent_class <- Some(c);

        List.iter (fun fn -> fn()) gen.gon_new_module_type;

        gen.gcurrent_classfield <- None;
        let process_field f =
          gen.gcurrent_classfield <- Some(f);
          List.iter (fun fn -> fn()) gen.gon_classfield_start;

          trace f.cf_name;
          match f.cf_expr with
          | None -> ()
          | Some e ->
            f.cf_expr <- Some (List.fold_left (fun e f -> f e) e filters)
        in
        List.iter process_field c.cl_ordered_fields;
        List.iter process_field c.cl_ordered_statics;

        gen.gcurrent_classfield <- None;
        (match c.cl_constructor with
        | None -> ()
        | Some f -> process_field f);
        (match c.cl_init with
        | None -> ()
        | Some e ->
          c.cl_init <- Some (List.fold_left (fun e f -> f e) e filters));
      | TEnumDecl _ -> ()
      | TTypeDecl _ -> ()
      | TAbstractDecl _ -> ()

let run_filters gen =
  (* first of all, we have to make sure that the filters won't trigger a major Gc collection *)
  let t = Common.timer "gencommon_filters" in
  (if Common.defined gen.gcon Define.GencommonDebug then debug_mode := true);
  let run_filters filter =
    let rec loop acc mds =
      match mds with
        | [] -> acc
        | md :: tl ->
          let filters = [ filter#run_f ] in
          let added_types = ref [] in
          gen.gadd_to_module <- (fun md_type priority ->
            gen.gcon.types <- md_type :: gen.gcon.types;
            added_types := (md_type, priority) :: !added_types
          );

          run_filters_from gen md filters;

          let added_types = List.map (fun (t,p) ->
            run_filters_from gen t [ fun e -> get (filter#run_from p e) ];
            if Hashtbl.mem gen.gtypes (t_path t) then begin
              let rec loop i =
                let p = t_path t in
                let new_p = (fst p, snd p ^ "_" ^ (string_of_int i)) in
                if Hashtbl.mem gen.gtypes new_p then
                  loop (i+1)
                else
                  match t with
                    | TClassDecl cl -> cl.cl_path <- new_p
                    | TEnumDecl e -> e.e_path <- new_p
                    | TTypeDecl _ | TAbstractDecl _ -> ()
              in
              loop 0
            end;
            Hashtbl.add gen.gtypes (t_path t) t;
            t
          ) !added_types in

          loop (added_types @ (md :: acc)) tl
    in
    List.rev (loop [] gen.gcon.types)
  in

  let run_mod_filter filter =
    let last_add_to_module = gen.gadd_to_module in
    let added_types = ref [] in
    gen.gadd_to_module <- (fun md_type priority ->
      Hashtbl.add gen.gtypes (t_path md_type) md_type;
      added_types := (md_type, priority) :: !added_types
    );

    let rec loop processed not_processed =
      match not_processed with
        | hd :: tl ->
          let new_hd = filter#run_f hd in

          let added_types_new = !added_types in
          added_types := [];
          let added_types = List.map (fun (t,p) ->
            get (filter#run_from p t)
          ) added_types_new in

          loop ( added_types @ (new_hd :: processed) ) tl
        | [] ->
          processed
    in

    let filtered = loop [] gen.gcon.types in
    gen.gadd_to_module <- last_add_to_module;
    gen.gcon.types <- List.rev (filtered)
  in

  run_mod_filter gen.gmodule_filters;

  let last_add_to_module = gen.gadd_to_module in
  gen.gcon.types <- run_filters gen.gexpr_filters;
  gen.gadd_to_module <- last_add_to_module;

  List.iter (fun fn -> fn()) gen.gafter_expr_filters_ended;
  (* Codegen.post_process gen.gcon.types [gen.gexpr_filters#run_f]; *)
  gen.gcon.types <- run_filters gen.gsyntax_filters;
  List.iter (fun fn -> fn()) gen.gafter_filters_ended;

  reorder_modules gen;
  t()

(* ******************************************* *)
(* basic generation module that source code compilation implementations can use *)
(* ******************************************* *)

let write_file gen w source_dir path extension =
  let t = timer "write file" in
  let s_path = gen.gcon.file ^ "/" ^  source_dir  ^ "/" ^ (String.concat "/" (fst path)) ^ "/" ^ (snd path) ^ "." ^ (extension) in
  (* create the folders if they don't exist *)
  let rec create acc = function
    | [] -> ()
    | d :: l ->
        let dir = String.concat "/" (List.rev (d :: acc)) in
        if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
        create (d :: acc) l
  in
  let p = gen.gcon.file :: source_dir :: fst path in
  create [] p;

  let contents = SourceWriter.contents w in
  let should_write = if not (Common.defined gen.gcon Define.ReplaceFiles) && Sys.file_exists s_path then begin
    let in_file = open_in s_path in
    let old_contents = Std.input_all in_file in
    close_in in_file;
    contents <> old_contents
  end else true in

  if should_write then begin
    let f = open_out s_path in
    output_string f contents;
    close_out f
  end;
  t()

let dump_descriptor gen name path_s =
  let w = SourceWriter.new_source_writer () in
  (* dump called path *)
  SourceWriter.write w (Sys.getcwd());
  SourceWriter.newline w;
  (* dump all defines. deprecated *)
  SourceWriter.write w "begin defines";
  SourceWriter.newline w;
  PMap.iter (fun name _ ->
    SourceWriter.write w name;
    SourceWriter.newline w
  ) gen.gcon.defines;
  SourceWriter.write w "end defines";
  SourceWriter.newline w;
  (* dump all defines with their values; keeping the old defines for compatibility *)
  SourceWriter.write w "begin defines_data";
  SourceWriter.newline w;
  PMap.iter (fun name v ->
    SourceWriter.write w name;
    SourceWriter.write w "=";
    SourceWriter.write w v;
    SourceWriter.newline w
  ) gen.gcon.defines;
  SourceWriter.write w "end defines_data";
  SourceWriter.newline w;
  (* dump all generated types *)
  SourceWriter.write w "begin modules";
  SourceWriter.newline w;
  List.iter (fun md_def ->
    SourceWriter.write w "M ";
    SourceWriter.write w (path_s md_def.m_path);
    SourceWriter.newline w;
    List.iter (fun m ->
      match m with
        | TClassDecl cl when not cl.cl_extern ->
          SourceWriter.write w "C ";
          SourceWriter.write w (path_s cl.cl_path);
          SourceWriter.newline w
        | TEnumDecl e when not e.e_extern ->
          SourceWriter.write w "E ";
          SourceWriter.write w (path_s e.e_path);
          SourceWriter.newline w
        | _ -> () (* still no typedef or abstract is generated *)
    ) md_def.m_types
  ) gen.gcon.modules;
  SourceWriter.write w "end modules";
  SourceWriter.newline w;
  (* dump all resources *)
  (match gen.gcon.main_class with
    | Some path ->
      SourceWriter.write w "begin main";
      SourceWriter.newline w;
      SourceWriter.write w (path_s path);
      SourceWriter.newline w;
      SourceWriter.write w "end main";
      SourceWriter.newline w
	| _ -> ()
  );
  SourceWriter.write w "begin resources";
  SourceWriter.newline w;
  Hashtbl.iter (fun name _ ->
    SourceWriter.write w name;
    SourceWriter.newline w
  ) gen.gcon.resources;
  SourceWriter.write w "end resources";
  SourceWriter.newline w;
  SourceWriter.write w "begin libs";
  SourceWriter.newline w;
  if Common.platform gen.gcon Java then
    List.iter (fun (s,_,_,_) ->
      SourceWriter.write w s;
      SourceWriter.newline w
    ) gen.gcon.java_libs;
  SourceWriter.write w "end libs";

  let contents = SourceWriter.contents w in
  let f = open_out (gen.gcon.file ^ "/" ^ name) in
  output_string f contents;
  close_out f

(*
  helper function to create the source structure. Will send each module_def to the function passed.
  If received true, it means that module_gen has generated this content, so the file must be saved.
  See that it will write a whole module
*)
let generate_modules gen extension source_dir (module_gen : SourceWriter.source_writer->module_def->bool) =
  List.iter (fun md_def ->
    let w = SourceWriter.new_source_writer () in
    (*let should_write = List.fold_left (fun should md -> module_gen w md or should) false md_def.m_types in*)
    let should_write = module_gen w md_def in
    if should_write then begin
      let path = md_def.m_path in
      write_file gen w source_dir path extension;


    end
  ) gen.gcon.modules

let generate_modules_t gen extension source_dir change_path (module_gen : SourceWriter.source_writer->module_type->bool) =
  List.iter (fun md ->
    let w = SourceWriter.new_source_writer () in
    (*let should_write = List.fold_left (fun should md -> module_gen w md or should) false md_def.m_types in*)
    let should_write = module_gen w md in
    if should_write then begin
      let path = change_path (t_path md) in
      write_file gen w source_dir path extension;
    end
  ) gen.gcon.types

(*
  various helper functions
*)

let mk_paren e =
  match e.eexpr with | TParenthesis _ -> e | _ -> { e with eexpr=TParenthesis(e) }

(* private *)
let tmp_count = ref 0

let mk_int gen i pos = { eexpr = TConst(TInt ( Int32.of_int i)); etype = gen.gcon.basic.tint; epos = pos }

let mk_return e = { eexpr = TReturn (Some e); etype = e.etype; epos = e.epos }

let mk_temp gen name t =
    incr tmp_count;
    let name = gen.gmk_internal_name "temp" (name ^ (string_of_int !tmp_count)) in
    alloc_var name t

let ensure_local gen block name e =
  match e.eexpr with
    | TLocal _ -> e
    | _ ->
      let var = mk_temp gen name e.etype in
      block := { e with eexpr = TVars([ var, Some e ]); etype = gen.gcon.basic.tvoid; } :: !block;
      { e with eexpr = TLocal var }

let reset_temps () = tmp_count := 0

let follow_module follow_func md = match md with
  | TClassDecl _
  | TEnumDecl _
  | TAbstractDecl _ -> md
  | TTypeDecl tdecl -> match (follow_func (TType(tdecl, List.map snd tdecl.t_types))) with
    | TInst(cl,_) -> TClassDecl cl
    | TEnum(e,_) -> TEnumDecl e
    | TType(t,_) -> TTypeDecl t
    | TAbstract(a,_) -> TAbstractDecl a
    | _ -> assert false

(*
  hxgen means if the type was generated by haxe. If a type was generated by haxe, it means
  it will contain special constructs for speedy reflection, for example

  @see SetHXGen module
 *)
let rec is_hxgen md =
  match md with
    | TClassDecl cl -> Meta.has Meta.HxGen cl.cl_meta
    | TEnumDecl e -> Meta.has Meta.HxGen e.e_meta
    | TTypeDecl t -> Meta.has Meta.HxGen t.t_meta || ( match follow t.t_type with | TInst(cl,_) -> is_hxgen (TClassDecl cl) | TEnum(e,_) -> is_hxgen (TEnumDecl e) | _ -> false )
    | TAbstractDecl a -> Meta.has Meta.HxGen a.a_meta

let is_hxgen_t t =
  match t with
    | TInst (cl, _) -> Meta.has Meta.HxGen cl.cl_meta
    | TEnum (e, _) -> Meta.has Meta.HxGen e.e_meta
    | TAbstract (a, _) -> Meta.has Meta.HxGen a.a_meta
    | TType (t, _) -> Meta.has Meta.HxGen t.t_meta
    | _ -> false

let mt_to_t_dyn md =
  match md with
    | TClassDecl cl -> TInst(cl, List.map (fun _ -> t_dynamic) cl.cl_types)
    | TEnumDecl e -> TEnum(e, List.map (fun _ -> t_dynamic) e.e_types)
    | TAbstractDecl a -> TAbstract(a, List.map (fun _ -> t_dynamic) a.a_types)
    | TTypeDecl t -> TType(t, List.map (fun _ -> t_dynamic) t.t_types)

let mt_to_t mt params =
  match mt with
    | TClassDecl (cl) -> TInst(cl, params)
    | TEnumDecl (e) -> TEnum(e, params)
    | TAbstractDecl a -> TAbstract(a, params)
    | _ -> assert false

let t_to_mt t =
  match follow t with
    | TInst(cl, _) -> TClassDecl(cl)
    | TEnum(e, _) -> TEnumDecl(e)
    | TAbstract(a, _) -> TAbstractDecl a
    | _ -> assert false

let mk_paren e =
  match e.eexpr with
    | TParenthesis _ -> e
    | _ -> { e with eexpr = TParenthesis(e) }

let rec get_last_ctor cl =
  Option.map_default (fun (super,_) -> if is_some super.cl_constructor then Some(get super.cl_constructor) else get_last_ctor super) None cl.cl_super

let add_constructor cl cf =
  match cl.cl_constructor with
  | None -> cl.cl_constructor <- Some cf
  | Some ctor ->
      if ctor != cf && not (List.memq cf ctor.cf_overloads) then
        ctor.cf_overloads <- cf :: ctor.cf_overloads

(* replace open TMonos with TDynamic *)
let rec replace_mono t =
  match follow t with
  | TMono t -> t := Some t_dynamic
  | TEnum (_,p) | TInst (_,p) | TType (_,p) | TAbstract (_,p) ->
      List.iter replace_mono p
  | TFun (args,ret) ->
      List.iter (fun (_,_,t) -> replace_mono t) args;
      replace_mono ret
  | TAnon a ->
      PMap.iter (fun _ f -> replace_mono f.cf_type) a.a_fields
  | TDynamic _ -> ()
  | _ -> assert false


(* helper *)
let mk_class_field name t public pos kind params =
  {
    cf_name = name;
    cf_type = t;
    cf_public = public;
    cf_pos = pos;
    cf_doc = None;
    cf_meta = [ Meta.CompilerGenerated, [], Ast.null_pos ]; (* annotate that this class field was generated by the compiler *)
    cf_kind = kind;
    cf_params = params;
    cf_expr = None;
    cf_overloads = [];
  }

(* this helper just duplicates the type parameter class, which is assumed that cl is. *)
(* This is so we can use class parameters on function parameters, without running the risk of name clash *)
(* between both *)
let map_param cl =
  let ret = mk_class cl.cl_module cl.cl_path cl.cl_pos in
  ret.cl_implements <- cl.cl_implements;
  ret.cl_kind <- cl.cl_kind;
  ret

let get_cl_t t =
  match follow t with | TInst (cl,_) -> cl | _ -> assert false

let mk_class m path pos =
  let cl = Type.mk_class m path pos in
  cl.cl_meta <- [ Meta.CompilerGenerated, [], Ast.null_pos ];
  cl

type tfield_access =
  | FClassField of tclass * tparams * tclass (* declared class *) * tclass_field * bool (* is static? *) * t (* the actual cf type, in relation to the class type params *)
  | FEnumField of tenum * tenum_field * bool (* is parameterized enum ? *)
  | FAnonField of tclass_field
  | FDynamicField of t
  | FNotFound

let field_access gen (t:t) (field:string) : (tfield_access) =
  (*
    t can be either an haxe-type as a real-type;
    'follow' should be applied here since we can generalize that a TType will be accessible as its
    underlying type.
  *)

  match follow t with
    | TInst(cl, params) ->
      let orig_cl = cl in
      let orig_params = params in
      let rec not_found cl params =
        match cl.cl_dynamic with
          | Some t ->
            let t = apply_params cl.cl_types params t in
            FDynamicField t
          | None ->
            match cl.cl_super with
              | None -> FNotFound
              | Some (super,p) ->  not_found super p
      in

      let not_found () =
        try
          let cf = PMap.find field gen.gbase_class_fields in
          FClassField (orig_cl, orig_params, gen.gclasses.cl_dyn, cf, false, cf.cf_type)
        with
          | Not_found -> not_found cl params
      in

      (* this is a hack for C#'s different generic types with same path *)
      let hashtbl_field = (String.concat "" (List.map (fun _ -> "]") cl.cl_types)) ^ field in
      (try
        match Hashtbl.find gen.greal_field_types (orig_cl.cl_path, hashtbl_field) with
          | None -> not_found()
          | Some (cf, actual_t, declared_cl) ->
            FClassField(orig_cl, orig_params, declared_cl, cf, false, actual_t)
      with | Not_found ->
        let rec flatten_hierarchy cl acc =
          match cl.cl_super with
            | None -> acc
            | Some (cl,super) -> flatten_hierarchy cl ((cl,super) :: acc)
        in

        let hierarchy = flatten_hierarchy orig_cl [orig_cl, List.map snd orig_cl.cl_types] in

        let rec loop_find_cf acc =
          match acc with
            | [] ->
              not_found()
            | (cl,params) :: tl ->
              (try
                let cf = PMap.find field cl.cl_fields in
                (* found *)
                (* get actual type *)
                let get_real_t = match cf.cf_kind with
                  | Var _ -> (fun t -> gen.greal_type t)
                  | _ -> (fun t ->
                    let args, ret = get_fun t in
                    TFun(List.map (fun (n,o,t) -> (n,o,gen.greal_type t)) args, gen.greal_type ret)
                  )
                in
                let actual_t = List.fold_left (fun t (cl,params) -> apply_params cl.cl_types (gen.greal_type_param (TClassDecl cl) params) (get_real_t t)) cf.cf_type acc in
                Hashtbl.add gen.greal_field_types (orig_cl.cl_path, hashtbl_field) (Some (cf, actual_t, cl));
                FClassField(orig_cl, orig_params, cl, cf, false, actual_t)
              with | Not_found ->
                loop_find_cf tl
              )
        in
        loop_find_cf hierarchy
      )
    | TEnum _ | TAbstract _ ->
      (* enums have no field *) FNotFound
    | TAnon anon ->
      (try match !(anon.a_status) with
        | Statics cl ->
          let cf = PMap.find field cl.cl_statics in
          FClassField(cl, List.map (fun _ -> t_dynamic) cl.cl_types, cl, cf, true, cf.cf_type)
        | EnumStatics e ->
          let f = PMap.find field e.e_constrs in
          let is_param = match follow f.ef_type with | TFun _ -> true | _ -> false in
          FEnumField(e, f, is_param)
        | _ when PMap.mem field gen.gbase_class_fields ->
          let cf = PMap.find field gen.gbase_class_fields in
          FClassField(gen.gclasses.cl_dyn, [t_dynamic], gen.gclasses.cl_dyn, cf, false, cf.cf_type)
        | _ ->
          FAnonField(PMap.find field anon.a_fields)
      with | Not_found -> FNotFound)
    | _ when PMap.mem field gen.gbase_class_fields ->
      let cf = PMap.find field gen.gbase_class_fields in
      FClassField(gen.gclasses.cl_dyn, [t_dynamic], gen.gclasses.cl_dyn, cf, false, cf.cf_type)
    | TDynamic t -> FDynamicField t
    | TMono _ -> FDynamicField t_dynamic
    | _ -> FNotFound

let mk_field_access gen expr field pos =
  match field_access gen expr.etype field with
    | FClassField(c,p,dc,cf,false,at) ->
        { eexpr = TField(expr, FInstance(dc,cf)); etype = apply_params c.cl_types p at; epos = pos }
    | FClassField(c,p,dc,cf,true,at) ->
        { eexpr = TField(expr, FStatic(dc,cf)); etype = at; epos = pos }
    | FAnonField cf ->
        { eexpr = TField(expr, FAnon cf); etype = cf.cf_type; epos = pos }
    | FDynamicField t ->
        { eexpr = TField(expr, FDynamic field); etype = t; epos = pos }
    | FNotFound ->
        { eexpr = TField(expr, FDynamic field); etype = t_dynamic; epos = pos }
    | FEnumField _ -> assert false

let mk_iterator_access gen t expr =
  let pos = expr.epos in
  let itf = mk_field_access gen expr "iterator" pos in
  { eexpr = TCall(itf, []); epos = pos; etype = snd (get_fun itf.etype) }

(* ******************************************* *)
(* Module dependency resolution *)
(* ******************************************* *)

type t_dependency =
  | DAfter of float
  | DBefore of float

exception ImpossibleDependency of string

let max_dep = 10000.0
let min_dep = - (10000.0)

let solve_deps name (deps:t_dependency list) =
  let vmin = min_dep -. 1.0 in
  let vmax = max_dep +. 1.0 in
  let rec loop dep vmin vmax =
    match dep with
      | [] ->
        (if vmin >= vmax then raise (ImpossibleDependency name));
        (vmin +. vmax) /. 2.0
      | head :: tail ->
        match head with
          | DBefore f ->
            loop tail (max vmin f) vmax
          | DAfter f ->
            loop tail vmin (min vmax f)
  in
  loop deps vmin vmax

(* type resolution *)

exception TypeNotFound of path

let get_type gen path =
  try Hashtbl.find gen.gtypes path with | Not_found -> raise (TypeNotFound path)

(* ******************************************* *)
(* follow all module *)
(* ******************************************* *)

(*
  this module will follow each and every type using the rules defined in
  gen.gfollow. This is a minor helper module, so we don't end up
  having to follow the same time multiple times in the many filter iterations
  because of this, it will be one of the first modules to run.
*)
module FollowAll =
struct

  let follow gen e =
    let follow_func = gen.gfollow#run_f in
    Some (Type.map_expr_type (fun e->e) (follow_func) (fun tvar-> tvar.v_type <- (follow_func tvar.v_type); tvar) e)

  let priority = max_dep

  (* will add an expression filter as the first filter *)
  let configure gen =
    gen.gexpr_filters#add ~name:"follow_all" ~priority:(PCustom(priority)) (follow gen)

end;;

(* ******************************************* *)
(* set hxgen module *)
(* ******************************************* *)

(*
  goes through all module types and sets the :hxgen meta on all which
  then is_hxgen_func returns true. There is a default is_hxgen_func implementation also
*)

module SetHXGen =
struct

  (*
    basically, everything that is extern is assumed to not be hxgen, unless meta :hxgen is set, and
    everything that is not extern is assumed to be hxgen, unless meta :nativegen is set
  *)
  let default_hxgen_func md =
    match md with
      | TClassDecl cl ->
        let rec is_hxgen_class c =
          if c.cl_extern then begin
            if Meta.has Meta.HxGen c.cl_meta then true else Option.map_default (fun (c,_) -> is_hxgen_class c) false c.cl_super
          end else begin
            if Meta.has Meta.NativeGen c.cl_meta then Option.map_default (fun (c, _) -> is_hxgen_class c) false c.cl_super else true
          end
        in

        is_hxgen_class cl
      | TEnumDecl e -> if e.e_extern then Meta.has Meta.HxGen e.e_meta else not (Meta.has Meta.NativeGen e.e_meta)
      | TAbstractDecl a -> not (Meta.has Meta.NativeGen a.a_meta)
      | TTypeDecl t -> (* TODO see when would we use this *)
        false

  (*
    by now the only option is to run it eagerly, because it must be one of the first filters to run,
    since many others depend of it
  *)
  let run_filter gen is_hxgen_func =
    let filter md =
      if is_hxgen_func md then begin
        match md with
          | TClassDecl cl -> cl.cl_meta <- (Meta.HxGen, [], cl.cl_pos) :: cl.cl_meta
          | TEnumDecl e -> e.e_meta <- (Meta.HxGen, [], e.e_pos) :: e.e_meta
          | TTypeDecl t -> t.t_meta <- (Meta.HxGen, [], t.t_pos) :: t.t_meta
          | TAbstractDecl a -> a.a_meta <- (Meta.HxGen, [], a.a_pos) :: a.a_meta
      end
    in
    List.iter filter gen.gcon.types

end;;

(* ******************************************* *)
(* overloading reflection constructors *)
(* ******************************************* *)

(*
  this module works on languages that support function overloading and
  enable function hiding via static functions.
  it takes the constructor body out of the constructor and adds it to a special ctor
  static function. The static function will receive the same parameters as the constructor,
  plus the special "me" var, which will replace "this"

  Then it always adds two constructors to the function: one that receives a special class,
  indicating that it should be constructed without any parameters, and one that receives its normal constructor.
  Both will only include a super() call to the superclasses' emtpy constructor.


  This enables two things:
    empty construction without the need of incompatibility with the platform's native construction method
    the ability to call super() constructor in any place in the constructor

  This will insert itself in the default reflection-related module filter
  TODO: cleanup
*)
module OverloadingConstructor =
struct

  let priority = 0.0

  let name = "overloading_constructor"

  let set_new_create_empty gen empty_ctor_expr =
    let old = gen.gtools.rf_create_empty in
    gen.gtools.rf_create_empty <- (fun cl params pos ->
      if is_hxgen (TClassDecl cl) then
        { eexpr = TNew(cl,params,[empty_ctor_expr]); etype = TInst(cl,params); epos = pos }
      else
        old cl params pos
    )

  let configure gen (empty_ctor_type : t) (empty_ctor_expr : texpr) supports_ctor_inheritance =

    set_new_create_empty gen empty_ctor_expr;

    let basic = gen.gcon.basic in

    let should_change cl = not cl.cl_interface && is_hxgen (TClassDecl cl) in

    let static_ctor_name = gen.gmk_internal_name "hx" "ctor" in

    let processed = Hashtbl.create (List.length gen.gcon.types) in

    let rec change cl =
      Hashtbl.add processed cl.cl_path true;

      (match cl.cl_super with
        | Some (super,_) when should_change super && not (Hashtbl.mem processed super.cl_path) ->
          change super
        | _ -> ()
      );

      let rec get_last_static_ctor cl params mayt =
        match cl.cl_super with
          | None -> None
          | Some (super,tl) ->
            let params = List.map (apply_params cl.cl_types params) tl in
            try
              let cf = PMap.find static_ctor_name super.cl_statics in
              (match mayt with
                | None -> Some (cf, super, tl)
                | Some argst ->
                    let chosen_cf = List.find (fun cf -> try unify (apply_params cf.cf_params tl cf.cf_type) argst; true with | Unify_error _ -> false) (cf :: cf.cf_overloads) in
                    Some(chosen_cf, super, tl))
            with | Not_found ->
              get_last_static_ctor super params mayt
      in

      let rec prev_ctor cl =
        match cl.cl_super with
          | None -> None
          | Some(cl,_) ->
            match cl.cl_constructor with
              | None -> prev_ctor cl
              | Some ctor -> Some ctor
      in

      let is_super_hxgen cl =
        match cl.cl_super with
          | None -> false
          | Some(cl, _) -> is_hxgen (TClassDecl cl)
      in

      (* check if we have a constructor right now *)
      let do_empty_only and_no_args_too =
        let super = match get_last_static_ctor cl (List.map snd cl.cl_types) None with
          | None ->
            { eexpr = TCall({ eexpr = TConst(TSuper); etype = TInst(cl, List.map snd cl.cl_types); epos = cl.cl_pos }, []); etype = basic.tvoid; epos = cl.cl_pos }
          | Some _ ->
            { eexpr = TCall({ eexpr = TConst(TSuper); etype = TInst(cl, List.map snd cl.cl_types); epos = cl.cl_pos }, [ empty_ctor_expr ]); etype = basic.tvoid; epos = cl.cl_pos }
        in
        let empty_ctor = mk_class_field "new" (TFun(["empty",false,empty_ctor_type],basic.tvoid)) false cl.cl_pos (Method MethNormal) [] in
        empty_ctor.cf_expr <- Some {
          eexpr = TFunction {
            tf_type = basic.tvoid;
            tf_args = [alloc_var "empty" empty_ctor_type, None];
            tf_expr = mk_block super
          };
          etype = empty_ctor.cf_type;
          epos = empty_ctor.cf_pos
        };
        empty_ctor.cf_meta <- [Meta.SkipCtor, [], empty_ctor.cf_pos];

        if and_no_args_too then begin
          let noargs_ctor = mk_class_field "new" (TFun([],basic.tvoid)) false cl.cl_pos (Method MethNormal) [] in
          noargs_ctor.cf_expr <- Some {
          eexpr = TFunction {
            tf_type = basic.tvoid;
            tf_args = [];
            tf_expr = mk_block super
          };
          etype = noargs_ctor.cf_type;
          epos = noargs_ctor.cf_pos
        };
        add_constructor cl noargs_ctor
        end;

        add_constructor cl empty_ctor
      in

      let cur_ctor =
        match cl.cl_constructor with
          | Some ctor when Meta.has Meta.SkipCtor cl.cl_meta ->
            if not supports_ctor_inheritance then begin
              do_empty_only false;
            end;
            None
          | Some ctor -> Some ctor
          | None ->
            (* if we don't, check if there are any previous constructors *)
            match prev_ctor cl with
              | Some ctor when not supports_ctor_inheritance ->
                (* if there are and not supports_ctor_inheritance, we need to create the constructors anyway *)
                (* create a constructor that only receives its arguments and calls super with them *)
                List.iter (function
                  | ctor when not (type_iseq (TFun(["empty",false,empty_ctor_type], gen.gcon.basic.tvoid)) ctor.cf_type) ->
                  let new_ctor = mk_class_field "new" ctor.cf_type ctor.cf_public cl.cl_pos (Method MethNormal) [] in
                  let args, _ = get_fun ctor.cf_type in
                  let tf_args = List.map (fun (name,_,t) ->
                    (* the constructor will have no optional arguments, as presumably this will be handled by the underlying expr *)
                    (alloc_var name t, None)
                  ) args in
                  let super_call =
                  {
                    eexpr = TCall(
                      { eexpr = TConst(TSuper); etype = TInst(cl, List.map snd cl.cl_types); epos = ctor.cf_pos },
                      List.map (fun (v,_) -> mk_local v ctor.cf_pos) tf_args);
                    etype = basic.tvoid;
                    epos = ctor.cf_pos
                  } in
                  new_ctor.cf_expr <- Some ({
                    eexpr = TFunction({
                      tf_args = tf_args;
                      tf_type = basic.tvoid;
                      tf_expr = mk_block super_call
                    });
                    etype = ctor.cf_type;
                    epos = ctor.cf_pos
                  });
                  add_constructor cl new_ctor;
                | _ -> ()) (ctor :: ctor.cf_overloads);
                cl.cl_constructor
              | _ ->
                do_empty_only true;
                None
      in

      let rec create_static_ctor cur_ctor is_overload =
      match cur_ctor with
        | None -> ()
        | Some ctor when Meta.has Meta.SkipCtor ctor.cf_meta -> ()
        | Some ctor ->
          (* now that we are sure to have a constructor:
              change its contents to reference 'me' var whenever 'this' is referenced
              extract a super call, if there's one. Change the super call to either call the static function,
                or if it can't (super not hxgen), make sure it's the first call. If it's not, error.
          *)
          let ctor_types = List.map (fun (s,t) -> (s, TInst (map_param (get_cl_t t), []))) cl.cl_types in
          let me = mk_temp gen "me" (TInst(cl, List.map snd ctor_types)) in
          (*let me = alloc_var "me" (TInst(cl, List.map snd ctor_types)) in*)
          me.v_capture <- true;

          let fn_args, _ = get_fun ctor.cf_type in
          let ctor_params = List.map snd ctor_types in
          let fn_type = TFun([me.v_name, false, me.v_type] @ (List.map (fun (n,b,t) -> (n,b,apply_params cl.cl_types ctor_params t)) fn_args), basic.tvoid) in
          let cur_tf_args = match ctor.cf_expr with
            | Some({ eexpr = TFunction(tf) }) -> tf.tf_args
            | _ -> assert false
          in

          let changed_tf_args = List.map (fun (v,_) -> (v, None)) cur_tf_args in

          let local_map = Hashtbl.create (List.length cur_tf_args) in
          let static_tf_args = [ me, None ] @ List.map (fun (v,b) ->
            let new_v = alloc_var v.v_name (apply_params cl.cl_types ctor_params v.v_type) in
            Hashtbl.add local_map v.v_id new_v;
            (new_v, b)
          ) cur_tf_args in

          let static_ctor = mk_class_field static_ctor_name fn_type false ctor.cf_pos (Method MethNormal) ctor_types in

          let is_super_first =
            let rec loop e =
              match e.eexpr with
                | TBlock(hd :: tl) -> loop hd
                | TCall({ eexpr = TConst(TSuper) }, _) -> true
                | _ -> false
            in
            match ctor.cf_expr with
              | Some({ eexpr = TFunction(tf) }) ->
                loop tf.tf_expr
              | _ -> assert false
          in

          let super_call = ref None in
          let change_super_to, mk_supers =
            let change_super_to scall params =
              let argst = TFun(("me",false,me.v_type) :: List.map (fun e -> replace_mono e.etype; "arg",false,e.etype) params, gen.gcon.basic.tvoid) in
              let last_static_ctor = get_last_static_ctor cl (List.map snd ctor_types) (Some argst) in
              super_call := Some scall;
              match last_static_ctor with
                | None ->
                  if is_super_first then
                    { eexpr = TBlock []; etype = t_dynamic; epos = scall.epos }
                  else
                    ( gen.gcon.error "Super call must be the first call when extending native types." scall.epos; assert false )
                | Some (chosen_cf, csup, tlsup) ->
                    { scall with eexpr = TCall(
                      { eexpr = TField(mk_classtype_access csup scall.epos, FStatic(csup, chosen_cf)); etype = apply_params csup.cl_types tlsup chosen_cf.cf_type; epos = scall.epos },
                      (mk_local me scall.epos) :: params
                    )}
            in

            (*
              with this information, create the static hx_ctor with the mapped contents, and create two constructors:
                one with the actual arguments and either the actual super call(if super not hxgen), or the super to
              create empty (if available), or just to empty super (if first)
                the other with either the mapped arguments of the actual super call, mapped to null, or the super to
              create empty, or just to empty super
            *)
            let mk_supers () =
              match is_super_hxgen cl with
                | true ->
                  (* can call super empty *)
                  let ret_empty = {
                    eexpr = TCall({ eexpr = TConst(TSuper); etype = me.v_type; epos = cl.cl_pos }, [ empty_ctor_expr ]);
                    etype = basic.tvoid;
                    epos = cl.cl_pos
                  } in

                  let ret = match get_last_static_ctor cl (List.map snd cl.cl_types) None, !super_call with
                    | None, Some super ->
                      (* it has an empty constructor, but we cannot call an out of placed super *)
                      super
                    | _ -> ret_empty
                  in

                  ret, ret_empty
                | false ->
                  match prev_ctor cl with
                    | None ->
                      let ret = {
                        eexpr = TCall({ eexpr = TConst(TSuper); etype = me.v_type; epos = cl.cl_pos }, []);
                        etype = basic.tvoid;
                        epos = cl.cl_pos
                      } in
                      ret, ret
                    | Some _ ->
                      let super = get (!super_call) in
                      super, match super with
                        | { eexpr = TCall(super, args) } ->
                          { super with eexpr = TCall(super, List.map (fun e -> mk_cast e.etype { e with eexpr = TConst(TNull) }) args) }
                        | _ -> assert false
            in
            change_super_to, mk_supers
          in

          let rec map_expr e = match e.eexpr with
            | TCall( { eexpr = TConst(TSuper) }, params ) ->
              change_super_to e (List.map map_expr params)
            | TLocal(v) ->
              (try let new_v = Hashtbl.find local_map v.v_id in { e with eexpr = TLocal(new_v); etype = new_v.v_type }
              with | Not_found -> e)
            | TConst(TThis) ->
              mk_local me e.epos
            | TNew(ncl,nparams,eparams) ->
              let cl, params = match apply_params cl.cl_types ctor_params (TInst(ncl,nparams)) with
                | TInst(cl,p) -> cl,p
                | _ -> assert false
              in
              { e with eexpr = TNew(cl, params, List.map map_expr eparams); etype = TInst(cl, params) }
            | _ -> Type.map_expr map_expr { e with etype = apply_params cl.cl_types ctor_params e.etype }
          in

          let mapped = match ctor.cf_expr with
            | Some({ eexpr = TFunction(tf) }) ->
              { tf with tf_args = static_tf_args; tf_expr = map_expr tf.tf_expr }
            | _ -> assert false
          in

          static_ctor.cf_expr <- Some { eexpr = TFunction(mapped); etype = static_ctor.cf_type; epos = ctor.cf_pos };
          let normal_super, empty_super = mk_supers () in

          (try
            let sc = PMap.find static_ctor.cf_name cl.cl_statics in
            sc.cf_overloads <- static_ctor :: sc.cf_overloads
          with | Not_found ->
            cl.cl_ordered_statics <- static_ctor :: cl.cl_ordered_statics;
            cl.cl_statics <- PMap.add static_ctor_name static_ctor cl.cl_statics);

          let normal_super =
          {
            eexpr = TBlock([
              normal_super;
              {
                eexpr = TCall(
                  { eexpr = TField(mk_classtype_access cl ctor.cf_pos, FStatic(cl,static_ctor)); etype = apply_params ctor_types (List.map snd cl.cl_types) fn_type; epos = ctor.cf_pos },
                  [ { eexpr = TConst(TThis); etype = TInst(cl, List.map snd cl.cl_types); epos = cl.cl_pos } ] @ List.map (fun (v,_) -> mk_local v ctor.cf_pos) changed_tf_args
                );
                etype = basic.tvoid;
                epos = ctor.cf_pos
              }
            ]);
            etype = basic.tvoid;
            epos = ctor.cf_pos
          } in

          ctor.cf_expr <- Some {
            eexpr = TFunction { tf_type = basic.tvoid; tf_args = changed_tf_args; tf_expr = normal_super };
            etype = ctor.cf_type;
            epos = ctor.cf_pos;
          };

          List.iter (fun cf -> create_static_ctor (Some cf) true) ctor.cf_overloads;
          if not is_overload then begin
            let empty_ctor = mk_class_field "new" (TFun(["empty",false,empty_ctor_type],basic.tvoid)) false cl.cl_pos (Method MethNormal) [] in
            empty_ctor.cf_meta <- [Meta.SkipCtor,[],empty_ctor.cf_pos];
            empty_ctor.cf_expr <- Some {
              eexpr = TFunction {
                tf_type = basic.tvoid;
                tf_args = [alloc_var "empty" empty_ctor_type, None];
                tf_expr = mk_block empty_super
              };
              etype = empty_ctor.cf_type;
              epos = empty_ctor.cf_pos
            };

            add_constructor cl empty_ctor
          end;

          ctor.cf_meta <- (Meta.SkipCtor,[],ctor.cf_pos) :: ctor.cf_meta;
          (match cl.cl_constructor with
          | None -> ()
          | Some cf ->
              (* since all constructors are overloaded, make sure no TMonos are left open *)
              List.iter (fun cf -> replace_mono cf.cf_type) (cf :: cf.cf_overloads))
      in
      create_static_ctor cur_ctor false
    in

    let module_filter md = match md with
      | TClassDecl cl when should_change cl && not (Hashtbl.mem processed cl.cl_path) ->
        change cl;
        None
      | _ -> None
    in
    gen.gmodule_filters#add ~name:name ~priority:(PCustom priority) module_filter

end;;

(* ******************************************* *)
(* init function module *)
(* ******************************************* *)

(*
  This module will take proper care of the init function, by taking off all expressions from static vars and putting them
  in order in the init function.
  It will also initialize dynamic functions, both by putting them in the constructor and in the init function

  depends on:
    (syntax) must run before ExprStatement module
    (ok) must run before OverloadingCtor module so the constructor can be in the correct place
    (syntax) must run before FunctionToClass module
*)

module InitFunction =
struct

  let name = "init_funcs"

  let priority = solve_deps name [DBefore OverloadingConstructor.priority]

  let configure gen should_handle_dynamic_functions =
    let handle_override_dynfun acc e this field =
      let add_expr = ref None in
      let v = mk_temp gen ("super_" ^ field) e.etype in
      v.v_capture <- true;

      let rec loop e =
        match e.eexpr with
          | TField({ eexpr = TConst(TSuper) }, f) ->
            let n = field_name f in
            (if n <> field then assert false);
            let local = mk_local v e.epos in
            (match !add_expr with
              | None ->
                add_expr := Some { e with eexpr = TVars([v, Some this]) }
              | Some _ -> ());
            local
          | TConst TSuper -> assert false
          | _ -> Type.map_expr loop e
      in
      let e = loop e in

      match !add_expr with
        | None -> e :: acc
        | Some add_expr -> add_expr :: e :: acc
    in

    let handle_class cl =
      let init = match cl.cl_init with
        | None -> []
        | Some i -> [i]
      in
      let init = List.fold_left (fun acc cf ->
        match cf.cf_kind, should_handle_dynamic_functions with
          | (Var _, _)
          | (Method (MethDynamic), true) ->
            (match cf.cf_expr with
              | Some e ->
                (match cf.cf_params with
                  | [] ->
                    let var = { eexpr = TField(mk_classtype_access cl cf.cf_pos, FStatic(cl,cf)); etype = cf.cf_type; epos = cf.cf_pos } in
                    let ret = ({ eexpr = TBinop(Ast.OpAssign, var, e); etype = cf.cf_type; epos = cf.cf_pos; }) in
                    cf.cf_expr <- None;

                    ret :: acc
                  | _ ->
                    let params = List.map (fun _ -> t_dynamic) cf.cf_params in
                    let fn = apply_params cf.cf_params params in
                    let var = { eexpr = TField(mk_classtype_access cl cf.cf_pos, FStatic(cl,cf)); etype = fn cf.cf_type; epos = cf.cf_pos } in
                    let rec change_expr e =
                      Type.map_expr_type (change_expr) fn (fun v -> v.v_type <- fn v.v_type; v) e
                    in

                    let ret = ({ eexpr = TBinop(Ast.OpAssign, var, change_expr e); etype = fn cf.cf_type; epos = cf.cf_pos; }) in
                    cf.cf_expr <- None;
                    ret :: acc
                )
              | None -> acc)
          | _ -> acc
      ) init cl.cl_ordered_statics
      in
      let init = List.rev init in
      (match init with
        | [] -> cl.cl_init <- None
        | _ -> cl.cl_init <- Some { eexpr = TBlock(init); epos = cl.cl_pos; etype = gen.gcon.basic.tvoid; });

      (* FIXME: find a way to tell OverloadingCtors to execute this code even with empty constructors *)
      if should_handle_dynamic_functions then begin
        let funs = List.fold_left (fun acc cf ->
          match cf.cf_kind with
            | Var _
            | Method(MethDynamic) ->
              (match cf.cf_expr, cf.cf_params with
                | Some e, [] ->
                  let var = { eexpr = TField({ eexpr = TConst(TThis); epos = cf.cf_pos; etype = TInst(cl, List.map snd cl.cl_types); }, FInstance(cl, cf)); etype = cf.cf_type; epos = cf.cf_pos } in
                  let ret = ({ eexpr = TBinop(Ast.OpAssign, var, e); etype = cf.cf_type; epos = cf.cf_pos; }) in
                  cf.cf_expr <- None;
                  let is_override = List.memq cf cl.cl_overrides in

                  if is_override then begin
                    cl.cl_ordered_fields <- List.filter (fun f -> f.cf_name <> cf.cf_name) cl.cl_ordered_fields;
                    cl.cl_fields <- PMap.remove cf.cf_name cl.cl_fields;
                    handle_override_dynfun acc ret var cf.cf_name
                  end else ret :: acc
                | Some e, _ ->
                  let params = List.map (fun _ -> t_dynamic) cf.cf_params in
                  let fn = apply_params cf.cf_params params in
                  let var = { eexpr = TField({ eexpr = TConst(TThis); epos = cf.cf_pos; etype = TInst(cl, List.map snd cl.cl_types); }, FInstance(cl, cf)); etype = cf.cf_type; epos = cf.cf_pos } in
                  let rec change_expr e =
                    Type.map_expr_type (change_expr) fn (fun v -> v.v_type <- fn v.v_type; v) e
                  in

                  let ret = ({ eexpr = TBinop(Ast.OpAssign, var, change_expr e); etype = fn cf.cf_type; epos = cf.cf_pos; }) in
                  cf.cf_expr <- None;
                  let is_override = List.memq cf cl.cl_overrides in

                  if is_override then begin
                    cl.cl_ordered_fields <- List.filter (fun f -> f.cf_name <> cf.cf_name) cl.cl_ordered_fields;
                    cl.cl_fields <- PMap.remove cf.cf_name cl.cl_fields;
                    handle_override_dynfun acc ret var cf.cf_name
                  end else ret :: acc
                | None, _ -> acc)
            | _ -> acc
        ) [] cl.cl_ordered_fields
        in
        (* see if there is any *)
        (match funs with
          | [] -> ()
          | _ ->
            (* if there is, we need to find the constructor *)
            match cl.cl_constructor with
              | None ->
                (* no constructor, create one by replicating the last arguments *)
                let last_ctor = get_last_ctor cl in
                (* if there is no ctor, create a standard one *)
                (match last_ctor with
                  | None ->
                    let ft = TFun([], gen.gcon.basic.tvoid) in
                    let ctor = mk_class_field "new" ft true cl.cl_pos (Method(MethNormal)) [] in
                    let func =
                    {
                      eexpr = TFunction({
                        tf_args = [];
                        tf_type = gen.gcon.basic.tvoid;
                        tf_expr = { eexpr = TBlock(funs); etype = gen.gcon.basic.tvoid; epos = cl.cl_pos };
                      });
                      epos = cl.cl_pos;
                      etype = ft;
                    } in
                    ctor.cf_expr <- Some(func);

                    cl.cl_constructor <- Some(ctor)
                  | Some (ctor) ->
                    let ft = ctor.cf_type in
                    let ctor = mk_class_field "new" ft true cl.cl_pos (Method(MethNormal)) [] in
                    let args, ret = match ft with
                      | TFun (args, ret) -> args, ret
                      | _ -> assert false
                    in
                    let tf_args = List.map (fun (s,_,t) ->
                      let v = alloc_var s t in
                      (v, None)
                    ) args in

                    let block =
                    {
                      eexpr = TCall({ eexpr = TConst(TSuper); etype = TInst(cl, List.map snd cl.cl_types); epos = cl.cl_pos },
                        List.map (fun (v, _) -> {eexpr = TLocal(v); etype = v.v_type; epos = cl.cl_pos;}) tf_args
                      );
                      etype = gen.gcon.basic.tvoid;
                      epos = cl.cl_pos;
                    } :: funs in

                    let func =
                    {
                      eexpr = TFunction({
                        tf_args = tf_args;
                        tf_type = gen.gcon.basic.tvoid;
                        tf_expr = { eexpr = TBlock(block); etype = gen.gcon.basic.tvoid; epos = cl.cl_pos };
                      });
                      epos = cl.cl_pos;
                      etype = ft;
                    } in
                    ctor.cf_expr <- Some(func);

                    cl.cl_constructor <- Some ctor
                )
              | Some ctor ->
                (* FIXME search for super() call here to not interfere with native extension *)
                let func = match ctor.cf_expr with
                  | Some({eexpr = TFunction(tf)} as e) ->

                    let block = match tf.tf_expr.eexpr with
                      | TBlock(bl) -> bl
                      | _ -> [tf.tf_expr]
                    in

                    let found = ref false in
                    let rec add_fn block acc =
                      match block with
                        | ({ eexpr = TCall({ eexpr = TConst(TSuper) }, _) } as hd) :: tl ->
                          found := true;
                          (List.rev acc) @ ((hd :: funs) @ tl)
                        | ({ eexpr = TBlock bl } as hd) :: tl ->
                          add_fn tl ( ({ hd with eexpr = TBlock (add_fn bl []) }) :: acc )
                        | hd :: tl ->
                          add_fn tl ( hd :: acc )
                        | [] -> List.rev acc
                    in

                    let block = add_fn block [] in
                    let block = if !found then
                      block
                    else
                      funs @ block
                    in

                    { e with eexpr = TFunction({
                      tf with tf_expr = {tf.tf_expr with eexpr = TBlock(block)}
                    })}
                  | _ -> assert false
                in
                ctor.cf_expr <- Some(func)
              )
      end

    in

    let mod_filter = function
      | TClassDecl cl -> (if not cl.cl_extern then handle_class cl); None
      | _ -> None in

    gen.gmodule_filters#add ~name:"init_funcs" ~priority:(PCustom priority) mod_filter

end;;

(* ******************************************* *)
(* Dynamic Binop/Unop handler *)
(* ******************************************* *)

(*
  On some languages there is limited support for operations on
  dynamic variables, so those operations must be changed.

  There are 5 types of binary operators:
    1 - can take any variable and returns a bool (== and !=)
    2 - can take either a string, or a number and returns either a bool or the underlying type ( >, < for bool and + for returning its type)
    3 - take numbers and return a number ( *, /, ...)
    4 - take ints and return an int (bit manipulation)
    5 - take a bool and returns a bool ( &&, || ...)

  On the default implementation, type 1 and the plus function will be handled with a function call;
  Type 2 will be handled with the parameter "compare_handler", which will do something like Reflect.compare(x1, x2);
  Types 3, 4 and 5 will perform a cast to double, int and bool, which will then be handled normally by the platform

  Unary operators are the most difficult to handle correctly.
  With unary operators, there are 2 types:

    1 - can take a number, changes and returns the result (++, --, ~)
    2 - can take a number (-) or bool (!), and returns the result

  The first case is much trickier, because it doesn't seem a good idea to change any variable to double just because it is dynamic,
  but this is how we will handle right now.
  something like that:

  var x:Dynamic = 10;
  x++;

  will be:
  object x = 10;
  x = ((IConvertible)x).ToDouble(null) + 1;

  depends on:
    (syntax) must run before expression/statment normalization because it may generate complex expressions
    must run before OverloadingCtor due to later priority conflicts. Since ExpressionUnwrap is only
    defined afterwards, we will set this value with absolute values
*)

module DynamicOperators =
struct

  let name = "dyn_ops"

  let priority = 0.0

  let priority_as_synf = 100.0 (*solve_deps name [DBefore ExpressionUnwrap.priority]*)

  let abstract_implementation gen ?(handle_strings = true) (should_change:texpr->bool) (equals_handler:texpr->texpr->texpr) (dyn_plus_handler:texpr->texpr->texpr->texpr) (compare_handler:texpr->texpr->texpr) =


    let get_etype_one e =
      if like_int e.etype then
        (gen.gcon.basic.tint, { eexpr = TConst(TInt(Int32.one)); etype = gen.gcon.basic.tint; epos = e.epos })
      else
        (gen.gcon.basic.tfloat, { eexpr = TConst(TFloat("1.0")); etype = gen.gcon.basic.tfloat; epos = e.epos })
    in

    let basic = gen.gcon.basic in

    let rec run e =
      match e.eexpr with
        | TBinop (OpAssignOp op, e1, e2) when should_change e -> (* e1 will never contain another TBinop *)
          (match e1.eexpr with
            | TLocal _ ->
              mk_paren { e with eexpr = TBinop(OpAssign, e1, run { e with eexpr = TBinop(op, e1, e2) }) }
            | TField _ | TArray _ ->
              let eleft, rest = match e1.eexpr with
                | TField(ef, f) ->
                  let v = mk_temp gen "dynop" ef.etype in
                  { e1 with eexpr = TField(mk_local v ef.epos, f) }, [ { eexpr = TVars([v,Some (run ef)]); etype = basic.tvoid; epos = ef.epos } ]
                | TArray(e1a, e2a) ->
                  let v = mk_temp gen "dynop" e1a.etype in
                  let v2 = mk_temp gen "dynopi" e2a.etype in
                  { e1 with eexpr = TArray(mk_local v e1a.epos, mk_local v2 e2a.epos) }, [ { eexpr = TVars([v,Some (run e1a); v2, Some (run e2a)]); etype = basic.tvoid; epos = e1.epos } ]
                | _ -> assert false
              in
              { e with
                eexpr = TBlock (rest @ [ { e with eexpr = TBinop(OpAssign, eleft, run { e with eexpr = TBinop(op, eleft, e2) }) } ]);
              }
            | _ ->
              assert false
          )

        | TBinop (OpAssign, e1, e2)
        | TBinop (OpInterval, e1, e2) -> Type.map_expr run e
        | TBinop (op, e1, e2) when should_change e->
          (match op with
            | OpEq -> (* type 1 *)
              equals_handler (run e1) (run e2)
            | OpNotEq -> (* != -> !equals() *)
              mk_paren { eexpr = TUnop(Ast.Not, Prefix, (equals_handler (run e1) (run e2))); etype = gen.gcon.basic.tbool; epos = e.epos }
            | OpAdd  ->
              if handle_strings && (is_string e.etype or is_string e1.etype or is_string e2.etype) then
                { e with eexpr = TBinop(op, mk_cast gen.gcon.basic.tstring (run e1), mk_cast gen.gcon.basic.tstring (run e2)) }
              else
                dyn_plus_handler e (run e1) (run e2)
            | OpGt | OpGte | OpLt | OpLte  -> (* type 2 *)
              { eexpr = TBinop(op, compare_handler (run e1) (run e2), { eexpr = TConst(TInt(Int32.zero)); etype = gen.gcon.basic.tint; epos = e.epos} ); etype = gen.gcon.basic.tbool; epos = e.epos }
            | OpMult | OpDiv | OpSub -> (* always cast everything to double *)
              let etype, _ = get_etype_one e in
              { e with eexpr = TBinop(op, mk_cast etype (run e1), mk_cast etype (run e2)) }
            | OpBoolAnd | OpBoolOr ->
              { e with eexpr = TBinop(op, mk_cast gen.gcon.basic.tbool (run e1), mk_cast gen.gcon.basic.tbool (run e2)) }
            | OpAnd | OpOr | OpXor | OpShl | OpShr | OpUShr | OpMod ->
              { e with eexpr = TBinop(op, mk_cast gen.gcon.basic.tint (run e1), mk_cast gen.gcon.basic.tint (run e2)) }
            | OpAssign | OpAssignOp _ | OpInterval | OpArrow -> assert false)
        | TUnop (Increment as op, flag, e1)
        | TUnop (Decrement as op, flag, e1) when should_change e ->
          (*
            some naming definitions:
             * ret => the returning variable
             * _g => the get body
             * getvar => the get variable expr

            This will work like this:
              - if e1 is a TField, set _g = get body, getvar = (get body).varname
              - if Prefix, return getvar = getvar + 1.0
              - if Postfix, set ret = getvar; getvar = getvar + 1.0; ret;
          *)
          let etype, one = get_etype_one e in
          let op = (match op with Increment -> OpAdd | Decrement -> OpSub | _ -> assert false) in

          let tvars, getvar =
            match e1.eexpr with
              | TField(fexpr, field) ->
                let tmp = mk_temp gen "getvar" fexpr.etype in
                let tvars = [tmp, Some(run fexpr)] in
                (tvars, { eexpr = TField( { fexpr with eexpr = TLocal(tmp) }, field); etype = etype; epos = e1.epos })
              | _ ->
                ([], e1)
          in

          (match flag with
            | Prefix ->
              let tvars = match tvars with
                | [] -> []
                | _ -> [{ eexpr = TVars(tvars); etype = gen.gcon.basic.tvoid; epos = e.epos }]
              in
              let block = tvars @
              [
                mk_cast etype { e with eexpr = TBinop(OpAssign, getvar,{ eexpr = TBinop(op, mk_cast etype getvar, one); etype = etype; epos = e.epos }); etype = getvar.etype; }
              ] in
              { eexpr = TBlock(block); etype = etype; epos = e.epos }
            | Postfix ->
              let ret = mk_temp gen "ret" etype in
              let tvars = { eexpr = TVars(tvars @ [ret, Some (mk_cast etype getvar)]); etype = gen.gcon.basic.tvoid; epos = e.epos } in
              let retlocal = { eexpr = TLocal(ret); etype = etype; epos = e.epos } in
              let block = tvars ::
              [
                { e with eexpr = TBinop(OpAssign, getvar, { eexpr = TBinop(op, retlocal, one); etype = getvar.etype; epos = e.epos }) };
                retlocal
              ] in
              { eexpr = TBlock(block); etype = etype; epos = e.epos }
          )
        | TUnop (op, flag, e1) when should_change e ->
          let etype = match op with | Not -> gen.gcon.basic.tbool | _ -> gen.gcon.basic.tint in
          mk_paren { eexpr = TUnop(op, flag, mk_cast etype (run e1)); etype = etype; epos = e.epos }
        | _ -> Type.map_expr run e
    in
    run

  let configure gen (mapping_func:texpr->texpr) =
    let map e = Some(mapping_func e) in
    gen.gexpr_filters#add ~name:"dyn_ops" ~priority:(PCustom priority) map

  let configure_as_synf gen (mapping_func:texpr->texpr) =
    let map e = Some(mapping_func e) in
    gen.gexpr_filters#add ~name:"dyn_ops" ~priority:(PCustom priority_as_synf) map

end;;

(* ******************************************* *)
(* Closure Detection *)
(* ******************************************* *)

(*

  Just a small utility filter that detects when a closure must be created.
  On the default implementation, this means when a function field is being accessed
  not via reflection and not to be called instantly

*)

module FilterClosures =
struct

  let priority = 0.0

  let traverse gen (should_change:texpr->string->bool) (filter:texpr->texpr->string->bool->texpr) =
    let rec run e =
      match e.eexpr with
        (*(* this is precisely the only case where we won't even ask if we should change, because it is a direct use of TClosure *)
        | TCall ( {eexpr = TClosure(e1,s)} as clos, args ) ->
          { e with eexpr = TCall({ clos with eexpr = TClosure(run e1, s) }, List.map run args ) }
        | TCall ( clos, args ) ->
          let rec loop clos = match clos.eexpr with
            | TClosure(e1,s) -> Some (clos, e1, s)
            | TParenthesis p -> loop p
            | _ -> None
          in
          let clos = loop clos in
          (match clos with
            | Some (clos, e1, s) -> { e with eexpr = TCall({ clos with eexpr = TClosure(run e1, s) }, List.map run args ) }
            | None -> Type.map_expr run e)*)
          | TCall(({ eexpr = TField(_, _) } as ef), params) ->
            { e with eexpr = TCall(Type.map_expr run ef, List.map run params) }
          | TField(ef, FEnum(en, field)) ->
              (* FIXME replace t_dynamic with actual enum Anon field *)
              let ef = run ef in
              (match follow field.ef_type with
                | TFun _ when should_change ef field.ef_name ->
                  filter e ef field.ef_name true
                | _ ->
                    { e with eexpr = TField(ef, FEnum(en,field)) }
              )
          | TField(({ eexpr = TTypeExpr _ } as tf), f) ->
            (match field_access gen tf.etype (field_name f) with
              | FClassField(_,_,_,cf,_,_) ->
                (match cf.cf_kind with
                  | Method(MethDynamic)
                  | Var _ ->
                    e
                  | _ when should_change tf cf.cf_name ->
                    filter e tf cf.cf_name true
                  | _ ->
                    e
               )
              | _ -> e)
          | TField(e1, FClosure (Some _, cf)) when should_change e1 cf.cf_name ->
            filter e (run e1) cf.cf_name false
          | _ -> Type.map_expr run e
    in
    run

  let configure gen (mapping_func:texpr->texpr) =
    let map e = Some(mapping_func e) in
    gen.gexpr_filters#add ~name:"closures_filter" ~priority:(PCustom priority) map

end;;

(* ******************************************* *)
(* Dynamic Field Access *)
(* ******************************************* *)

(*
  This module will filter every dynamic field access in haxe.

  On platforms that do not support dynamic access, it is with this that you should
  replace dynamic calls with x.field / Reflect.setField calls, and guess what -
  this is the default implemenation!
  Actually there is a problem with Reflect.setField because it returns void, which is a bad thing for us,
  so even in the default implementation, the function call should be specified to a Reflect.setField version that returns
  the value that was set

  (TODO: should it be separated?)
  As a plus, the default implementation adds something that doesn't hurt anybody, it looks for
  TAnon with Statics / EnumStatics field accesses and transforms them into real static calls.
  This means it will take this

  var m = Math;
  for (i in 0...1000) m.cos(10);

  which is an optimization in dynamic platforms, but performs horribly on strongly typed platforms
  and transform into:

  var m = Math;
  for (i in 0...1000) Math.cos(10);

  (addendum:)
  configure_generate_classes will already take care of generating the reflection-enabled class fields and calling abstract_implementation
  with the right arguments.

  Also

  depends on:
    (ok) must run AFTER Binop/Unop handler - so Unops / Binops are already unrolled
*)

module DynamicFieldAccess =
struct

  let name = "dynamic_field_access"

  let priority = solve_deps name [DAfter DynamicOperators.priority]

  let priority_as_synf = solve_deps name [DAfter DynamicOperators.priority_as_synf]

  (*
    is_dynamic (expr) (field_access_expr) (field) : a function that indicates if the field access should be changed

    change_expr (expr) (field_access_expr) (field) (setting expr) (is_unsafe) : changes the expression
    call_expr (expr) (field_access_expr) (field) (call_params) : changes a call expression
  *)
  let abstract_implementation gen (is_dynamic:texpr->texpr->string->bool) (change_expr:texpr->texpr->string->texpr option->bool->texpr) (call_expr:texpr->texpr->string->texpr list->texpr) =
    let rec run e =
      match e.eexpr with
        (* class types *)
        | TField(fexpr, f) when is_some (anon_class fexpr.etype) ->
          let decl = get (anon_class fexpr.etype) in
          let name = field_name f in
          (try
            match decl with
              | TClassDecl cl ->
                  let cf = PMap.find name cl.cl_statics in
                  { e with eexpr = TField({ fexpr with eexpr = TTypeExpr decl }, FStatic(cl, cf)) }
              | TEnumDecl en ->
                  let ef = PMap.find name en.e_constrs in
                  { e with eexpr = TField({ fexpr with eexpr = TTypeExpr decl }, FEnum(en, ef)) }
              | TAbstractDecl _ -> (* abstracts don't have TFields *) assert false
              | TTypeDecl _ -> (* anon_class doesn't return TTypeDecl *) assert false
            with
              | Not_found ->
                  change_expr e { fexpr with eexpr = TTypeExpr decl } (field_name f) None true
          )
        | TField(fexpr, f) when is_dynamic e fexpr (field_name f) ->
          change_expr e (run fexpr) (field_name f) None true
        | TCall(
          { eexpr = TField(_, FStatic({ cl_path = ([], "Reflect") }, { cf_name = "field" })) } ,
            [obj; { eexpr = TConst(TString(field)) }]
          ) ->
          change_expr (mk_field_access gen obj field obj.epos) (run obj) field None false
        | TCall(
          { eexpr = TField(_, FStatic({ cl_path = ([], "Reflect") }, { cf_name = "setField" } )) },
            [obj; { eexpr = TConst(TString(field)) }; evalue]
          ) ->
          change_expr (mk_field_access gen obj field obj.epos) (run obj) field (Some (run evalue)) false
        | TBinop(OpAssign, ({eexpr = TField(fexpr, f)}), evalue) when is_dynamic e fexpr (field_name f) ->
          change_expr e (run fexpr) (field_name f) (Some (run evalue)) true
        | TBinop(OpAssign, { eexpr = TField(fexpr, f) }, evalue) ->
            (match field_access gen fexpr.etype (field_name f) with
              | FClassField(_,_,_,cf,false,t) when (try PMap.find cf.cf_name gen.gbase_class_fields == cf with Not_found -> false) ->
                  change_expr e (run fexpr) (field_name f) (Some (run evalue)) true
              | _ -> Type.map_expr run e
            )
(* #if debug *)
        | TBinop(OpAssignOp op, ({eexpr = TField(fexpr, f)}), evalue) when is_dynamic e fexpr (field_name f) -> assert false (* this case shouldn't happen *)
        | TUnop(Increment, _, ({eexpr = TField( ( { eexpr=TLocal(local) } as fexpr ), f)}))
        | TUnop(Decrement, _, ({eexpr = TField( ( { eexpr=TLocal(local) } as fexpr ), f)})) when is_dynamic e fexpr (field_name f) -> assert false (* this case shouldn't happen *)
(* #end *)
        | TCall( ({ eexpr = TField(fexpr, f) }), params ) when is_dynamic e fexpr (field_name f) ->
          call_expr e (run fexpr) (field_name f) (List.map run params)
        | _ -> Type.map_expr run e
    in run

  (*
    this function will already configure with the abstract implementation, and also will create the needed class fields to
    enable reflection on platforms that don't support reflection.

    this means it will create the following class methods:
      - getField(field, isStatic) - gets the value of the field. isStatic
      - setField -
      -
  *)
  let configure_generate_classes gen optimize (runtime_getset_field:texpr->texpr->string->texpr option->texpr) (runtime_call_expr:texpr->texpr->string->texpr list->texpr) =
    ()

  let configure gen (mapping_func:texpr->texpr) =
    let map e = Some(mapping_func e) in
    gen.gexpr_filters#add ~name:"dynamic_field_access" ~priority:(PCustom(priority)) map

  let configure_as_synf gen (mapping_func:texpr->texpr) =
    let map e = Some(mapping_func e) in
    gen.gexpr_filters#add ~name:"dynamic_field_access" ~priority:(PCustom(priority_as_synf)) map

end;;

(* ******************************************* *)
(* Dynamic TArray Handling *)
(* ******************************************* *)

(*
  In some languages you cannot overload the [] operator,
  so we need to decide what is kept as TArray and what gets mapped.

  - in order to do this you must ensure that

  depends on:
    (syntax) must run before expression/statment normalization because it may generate complex expressions
    (ok) must run before binop transformations because it may generate some untreated binop ops
    (ok) must run before dynamic field access is transformed into reflection
*)

module TArrayTransform =
struct

  let name = "dyn_tarray"

  let priority = solve_deps name [DBefore DynamicOperators.priority; DBefore DynamicFieldAccess.priority]

  let priority_as_synf = solve_deps name [DBefore DynamicOperators.priority_as_synf; DBefore DynamicFieldAccess.priority_as_synf]

  let default_implementation gen (should_change:texpr->bool) (get_fun:string) (set_fun:string) =
    let basic = gen.gcon.basic in
    let mk_get e e1 e2 =
      let efield = mk_field_access gen e1 get_fun e.epos in
      { e with eexpr = TCall(efield, [e2]) }
    in
    let mk_set e e1 e2 evalue =
      let efield = mk_field_access gen e1 set_fun e.epos in
      { e with eexpr = TCall(efield, [e2; evalue]) }
    in
    let rec run e =
      match e.eexpr with
        | TArray(e1, e2) ->
          (* e1 should always be a var; no need to map there *)
          if should_change e then mk_get e (run e1) (run e2) else Type.map_expr run e
        | TBinop (Ast.OpAssign, ({ eexpr = TArray(e1a,e2a) } as earray), evalue) when should_change earray ->
          mk_set e (run e1a) (run e2a) (run evalue)
        | TBinop (Ast.OpAssignOp op,({ eexpr = TArray(e1a,e2a) } as earray) , evalue) when should_change earray ->
          (* cache all arguments in vars so they don't get executed twice *)
          (* let ensure_local gen block name e = *)
          let block = ref [] in

          let arr_local = ensure_local gen block "array" (run e1a) in
          let idx_local = ensure_local gen block "index" (run e2a) in
          block := (mk_set e arr_local idx_local ( { e with eexpr=TBinop(op, mk_get earray arr_local idx_local, run evalue) } )) :: !block;

          { e with eexpr = TBlock (List.rev !block) }
        | TUnop(op, flag, ({ eexpr = TArray(e1a, e2a) } as earray)) ->
          if should_change earray && match op with | Not | Neg -> false | _ -> true then begin

            let block = ref [] in

            let actual_t = match op with
              | Ast.Increment | Ast.Decrement -> (match follow earray.etype with
                | TInst _ | TAbstract _ | TEnum _ -> earray.etype
                | _ -> basic.tfloat)
              | Ast.Not -> basic.tbool
              | _ -> basic.tint
            in

            let val_v = mk_temp gen "arrVal" actual_t in
            let ret_v = mk_temp gen "arrRet" actual_t in

            let arr_local = ensure_local gen block "arr" (run e1a) in
            let idx_local = ensure_local gen block "arrIndex" (run e2a) in

            let val_local = { earray with eexpr = TLocal(val_v) } in
            let ret_local = { earray with eexpr = TLocal(ret_v) } in
            (* var idx = 1; var val = x._get(idx); var ret = val++; x._set(idx, val); ret; *)
            block := { eexpr = TVars(
                [
                  val_v, Some(mk_get earray arr_local idx_local); (* var val = x._get(idx) *)
                  ret_v, Some { e with eexpr = TUnop(op, flag, val_local) } (* var ret = val++ *)
                ]);
                etype = gen.gcon.basic.tvoid;
                epos = e2a.epos
              } :: !block;
            block := (mk_set e arr_local idx_local val_local) (*x._set(idx,val)*) :: !block;
            block := ret_local :: !block;
            { e with eexpr = TBlock (List.rev !block) }
          end else
            Type.map_expr run e
        | _ -> Type.map_expr run e

    in run

  let configure gen (mapping_func:texpr->texpr) =
    let map e = Some(mapping_func e) in
    gen.gexpr_filters#add ~name:"dyn_tarray" ~priority:(PCustom priority) map

  let configure_as_synf gen (mapping_func:texpr->texpr) =
    let map e = Some(mapping_func e) in
    gen.gexpr_filters#add ~name:"dyn_tarray" ~priority:(PCustom priority_as_synf) map

end;;

(* ******************************************* *)
(* Try / Catch + throw native types handling *)
(* ******************************************* *)

(*

  Some languages/vm's do not support throwing any kind of value. For them, only
  special kinds of objects can be thrown. Because of this, we must wrap some throw
  statements with an expression, and also we must unwrap it on the catch() phase, and
  maybe manually test with Std.is()

  dependencies:
    must run before dynamic field access (?) TODO review
    It's a syntax filter, as it alters types (throw wrapper)

*)

module TryCatchWrapper =
struct

  let priority = solve_deps "try_catch" [DBefore DynamicFieldAccess.priority]

  (*
    should_wrap : does the type should be wrapped? This of course works on the reverse way, so it tells us if the type should be unwrapped as well
    wrap_throw : the wrapper for throw (throw expr->expr inside throw->returning wrapped expression)
    unwrap_expr : the other way around : given the catch var (maybe will need casting to wrapper_type) , return the unwrap expr
    rethrow_expr : how to rethrow ane exception in the platform
    catchall_type : the class used for catchall (e:Dynamic)
    wrapper_type : the wrapper type, so we can test if exception is of type 'wrapper'
    catch_map : maps the catch expression to include some intialization code (e.g. setting up Stack.exceptionStack)
  *)
  let traverse gen (should_wrap:t->bool) (wrap_throw:texpr->texpr->texpr) (unwrap_expr:tvar->pos->texpr) (rethrow_expr:texpr->texpr) (catchall_type:t) (wrapper_type:t) (catch_map:tvar->texpr->texpr) =
    let rec run e =
      match e.eexpr with
          | TThrow texpr when should_wrap texpr.etype -> wrap_throw e (run texpr)
          | TTry (ttry, catches) ->
            let nowrap_catches, must_wrap_catches, catchall = List.fold_left (fun (nowrap_catches, must_wrap_catches, catchall) (v, catch) ->
              (* first we'll see if the type is Dynamic (catchall) *)
              match follow v.v_type with
                | TDynamic _ ->
                  assert (is_none catchall);
                  (nowrap_catches, must_wrap_catches, Some(v,catch_map v (run catch)))
                (* see if we should unwrap it *)
                | _ when should_wrap (follow v.v_type) ->
                  (nowrap_catches, (v,catch_map v (run catch)) :: must_wrap_catches, catchall)
                | _ ->
                  ( (v,catch_map v (run catch)) :: nowrap_catches, must_wrap_catches, catchall )
            ) ([], [], None) catches
            in
            (*
              1st catch all nowrap "the easy way"
              2nd see if there are any must_wrap or catchall. If there is,
                do a catchall first with a temp var.
                then get catchall var (as dynamic) (or create one), and declare it = catchall exception
                then test if it is of type wrapper_type. If it is, unwrap it
                then start doing Std.is() tests for each catch type
                if there is a catchall in the end, end with it. If there isn't, rethrow
            *)
            let dyn_catch = match (catchall, must_wrap_catches) with
              | Some (v,c), _
              | _, (v, c) :: _ ->
                let pos = c.epos in
                let temp_var = mk_temp gen "catchallException" catchall_type in
                let temp_local = { eexpr=TLocal(temp_var); etype = temp_var.v_type; epos = pos } in
                let catchall_var = (*match catchall with
                  | None -> *) mk_temp gen "catchall" t_dynamic
                  (*| Some (v,_) -> v*)
                in
                let catchall_decl = { eexpr = TVars([catchall_var, Some(temp_local)]); etype=gen.gcon.basic.tvoid; epos = pos } in
                let catchall_local = { eexpr = TLocal(catchall_var); etype = t_dynamic; epos = pos } in
                (* if it is of type wrapper_type, unwrap it *)
                let std_is = mk_static_field_access (get_cl (get_type gen ([],"Std"))) "is" (TFun(["v",false,t_dynamic;"cl",false,mt_to_t (get_type gen ([], "Class")) [t_dynamic]],gen.gcon.basic.tbool)) pos in
                let mk_std_is t pos = { eexpr = TCall(std_is, [catchall_local; mk_mt_access (t_to_mt t) pos]); etype = gen.gcon.basic.tbool; epos = pos } in

                let if_is_wrapper_expr = { eexpr = TIf(mk_std_is wrapper_type pos,
                  { eexpr = TBinop(OpAssign, catchall_local, unwrap_expr temp_var pos); etype = t_dynamic; epos = pos }
                , None); etype = gen.gcon.basic.tvoid; epos = pos } in
                let rec loop must_wrap_catches = match must_wrap_catches with
                  | (vcatch,catch) :: tl ->
                    { eexpr = TIf(mk_std_is vcatch.v_type catch.epos,
                      { eexpr = TBlock({ eexpr=TVars([vcatch, Some(mk_cast vcatch.v_type catchall_local)]); etype=gen.gcon.basic.tvoid; epos=catch.epos } :: [catch] ); etype = gen.gcon.basic.tvoid; epos = catch.epos },
                      Some (loop tl));
                    etype = gen.gcon.basic.tvoid; epos = catch.epos }
                  | [] ->
                    match catchall with
                      | Some (v,s) ->
                        Codegen.concat { eexpr = TVars([v, Some(catchall_local)]); etype = gen.gcon.basic.tvoid; epos = pos } s
                      | None ->
                        mk_block (rethrow_expr temp_local)
                in
                [ ( temp_var, { e with eexpr = TBlock([ catchall_decl; if_is_wrapper_expr; loop must_wrap_catches ]) } ) ]
              | _ ->
                []
            in
            { e with eexpr = TTry(run ttry, (List.rev nowrap_catches) @ dyn_catch) }
          | _ -> Type.map_expr run e
    in
    run

  let configure gen (mapping_func:texpr->texpr) =
    let map e = Some(mapping_func e) in
    gen.gsyntax_filters#add ~name:"try_catch" ~priority:(PCustom priority) map

end;;

let fun_args = List.map (function | (v,s) -> (v.v_name, (match s with | None -> false | Some _ -> true), v.v_type))

(* ******************************************* *)
(* Closures To Class *)
(* ******************************************* *)

(*

  This is a very important filter. It will take all anonymous functions from the AST, will search for all captured variables, and will create a class
  that implements an abstract interface for calling functions. This is very important for targets that don't support anonymous functions to work correctly.
  Also it is possible to implement some strategies to avoid value type boxing, such as NaN tagging or double/object arguments. All this will be abstracted away
  from this interface.


  dependencies:
    must run after dynamic field access, because of conflicting ways to deal with invokeField
    (module filter) must run after OverloadingCtor so we can also change the dynamic function expressions

    uses TArray expressions for array. TODO see interaction
    uses TThrow expressions.
*)

module ClosuresToClass =
struct

  let name = "closures_to_class"

  let priority = solve_deps name [ DAfter DynamicFieldAccess.priority ]

  let priority_as_synf = solve_deps name [ DAfter DynamicFieldAccess.priority_as_synf ]

  type closures_ctx =
  {
    fgen : generator_ctx;

    mutable func_class : tclass;

    (*
      this is what will actually turn the function into class field.
      The standard implementation by default will already take care of creating the class, and setting the captured variables.

      It will also return the super arguments to be called
    *)
    mutable closure_to_classfield : tfunc->t->pos->tclass_field * (texpr list);

    (*
      when a dynamic function call is made, we need to convert it as if it were calling the dynamic function interface.

      TCall expr -> new TCall expr
    *)
    mutable dynamic_fun_call : texpr->texpr;

    (*
      called once so the implementation can make one of a time initializations in the base class
      for all functions
    *)
    mutable initialize_base_class : tclass->unit;

    (*
      Base classfields are the class fields for the abstract implementation of either the Function implementation,
      or the invokeField implementation for the classes
      They will either try to call the right function or will fail with

      (tclass - subject (so we know the type of this)) -> is_function_base -> additional arguments for each function (at the beginning) -> list of the abstract implementation class fields
    *)
    mutable get_base_classfields_for : tclass->bool->(unit->(tvar * tconstant option) list)->tclass_field list;

    (*
      This is a more complex version of get_base_classfields_for.
      It's meant to provide a toolchain so we can easily create classes that extend Function
      and add more functionality on top of it.

      arguments:
        tclass -> subject (so we know the type of this)
        bool -> is it a function type
        ( int -> (int->t->tconstant option->texpr) -> ( (tvar * tconstant option) list * texpr) )
          int -> current arity of the function whose member will be mapped; -1 for dynamic function. It is guaranteed that dynamic function will be called last
          t -> the return type of the function
          (int->t->tconstant option->texpr) -> api to get exprs that unwrap arguments correctly
            int -> argument wanted to unwrap
            t -> solicited type
            tconstant option -> map to this default value if null
            returns a texpr that tells how the default
          should return a list with additional arguments (only works if is_function_base = true)
          and the underlying function expression
    *)
    mutable map_base_classfields : tclass->bool->( int -> t -> (tvar list) -> (int->t->tconstant option->texpr) -> ( (tvar * tconstant option) list * texpr) )->tclass_field list;

    mutable transform_closure : texpr->texpr->string->texpr;

  }

  (*
    the default implementation will take 3 transformation functions:
      * one that will transform closures that are not called immediately (instance.myFunc).
        normally on this case it's best to have a runtime handler that will take the instance, the function and call its invokeField when invoked
      * one that will actually handle the anonymous functions themselves.
      * one that will transform calling a dynamic function. So for example, dynFunc(arg1, arg2) might turn into dynFunc.apply2(arg1, arg2);
      ( suspended ) * an option to match papplied functions
  *)

  let traverse gen (transform_closure:texpr->texpr->string->texpr) (handle_anon_func:texpr->tfunc->texpr) (dynamic_func_call:texpr->texpr) e =
    let rec run e =
      match e.eexpr with
        | TCall( { eexpr = TField(_, FEnum _) }, _ ) ->
          Type.map_expr run e
        (* if a TClosure is being call immediately, there's no need to convert it to a TClosure *)
        | TCall(( { eexpr = TField(ecl,f) } as e1), params) ->
          (* check to see if called field is known and if it is a MethNormal (only MethNormal fields can be called directly) *)
          let name = field_name f in
          (match field_access gen (gen.greal_type ecl.etype) name with
            | FClassField(_,_,_,cf,_,_) ->
              (match cf.cf_kind with
                | Method MethNormal
                | Method MethInline ->
                  { e with eexpr = TCall({ e1 with eexpr = TField(run ecl, f) }, List.map run params) }
                | _ ->
                  match gen.gfollow#run_f e1.etype with
                    | TFun _ ->
                      dynamic_func_call { e with eexpr = TCall(run e1, List.map run params) }
                    | _ ->
                      let i = ref 0 in
                      let t = TFun(List.map (fun e -> incr i; "arg" ^ (string_of_int !i), false, e.etype) params, e.etype) in
                      dynamic_func_call { e with eexpr = TCall( mk_cast t (run e1), List.map run params ) }
              )
            (* | FNotFound ->
              { e with eexpr = TCall({ e1 with eexpr = TField(run ecl, f) }, List.map run params) }
                (* expressions by now may have generated invalid expressions *) *)
            | _ ->
              match gen.gfollow#run_f e1.etype with
                | TFun _ ->
                  dynamic_func_call { e with eexpr = TCall(run e1, List.map run params) }
                | _ ->
                  let i = ref 0 in
                  let t = TFun(List.map (fun e -> incr i; "arg" ^ (string_of_int !i), false, e.etype) params, e.etype) in
                  dynamic_func_call { e with eexpr = TCall( mk_cast t (run e1), List.map run params ) }
          )
        | TField(ecl, FClosure (_,cf)) ->
          transform_closure e (run ecl) cf.cf_name
        | TFunction tf ->
          handle_anon_func e { tf with tf_expr = run tf.tf_expr }
        | TCall({ eexpr = TConst(TSuper) }, _) ->
          Type.map_expr run e
        | TCall({ eexpr = TLocal(v) }, args) when String.get v.v_name 0 = '_' && Hashtbl.mem gen.gspecial_vars v.v_name ->
          Type.map_expr run e
        | TCall(tc,params) ->
          let i = ref 0 in
          let may_cast = match gen.gfollow#run_f tc.etype with
            | TFun _ -> fun e -> e
            | _ ->
              let t = TFun(List.map (fun e ->
                  incr i;
                  ("p" ^ (string_of_int !i), false, e.etype)
                ) params, e.etype)
              in
              fun e -> mk_cast t e
          in
          dynamic_func_call { e with eexpr = TCall(run (may_cast tc), List.map run params) }
        | _ -> Type.map_expr run e
    in

    (match e.eexpr with
      | TFunction(tf) -> Type.map_expr run e
      | _ -> run e)

  let rec get_type_params acc t =
    match follow t with
      | TInst(( { cl_kind = KTypeParameter _ } as cl), []) ->
        if List.exists (fun c -> c == cl) acc then acc else cl :: acc
      | TFun _
      | TDynamic _
      | TAnon _
      | TMono _
      | TAbstract(_, [])
      | TInst(_, [])
      | TEnum(_, []) -> acc
      | TAbstract(_, params)
      | TEnum(_, params)
      | TInst(_, params) ->
        List.fold_left get_type_params acc params
      | _ -> assert false

  let get_captured expr =
    let ret = Hashtbl.create 1 in
    let ignored = Hashtbl.create 0 in

    let params = ref [] in
    let check_params t = params := get_type_params !params t in
    let rec traverse expr =
      match expr.eexpr with
        | TFor (v, _, _) ->
          Hashtbl.add ignored v.v_id v;
          check_params v.v_type;
          Type.map_expr traverse expr
        | TFunction(tf) ->
          List.iter (fun (v,_) -> check_params v.v_type; Hashtbl.add ignored v.v_id v) tf.tf_args;
          Type.map_expr traverse expr
        | TVars (vars) ->
          List.iter (fun (v, opt) -> check_params v.v_type; Hashtbl.add ignored v.v_id v; ignore(Option.map traverse opt)) vars;
          expr
        | TLocal(( { v_capture = true } ) as v) ->
          (if not (Hashtbl.mem ignored v.v_id || Hashtbl.mem ret v.v_id) then begin check_params v.v_type; Hashtbl.replace ret v.v_id expr end);
          expr
        | _ -> Type.map_expr traverse expr
    in ignore (traverse expr);
    ret, !params

  (*
    OPTIMIZEME:

    Take off from Codegen the code that wraps captured variables,

    traverse through all variables, looking for their use (just like local_usage)
    three possible outcomes for captured variables:
      - become a function member variable <- best performance.
        Will not work on functions that can be created more than once (functions inside a loop or functions inside functions)
        The function will have to be created on top of the block, so its variables can be filled in instead of being declared
      - single-element array - the most compatible way, though also creates a slight overhead.
    - we'll have some labels for captured variables:
      - used in loop
  *)

  (*
    The default implementation will impose a naming convention:
      invoke(arity)_(o for returning object/d for returning double) when arity < max_arity
      invoke_dynamic_(o/d) when arity > max_arity

    This means that it also imposes that the dynamic function return types may only be Dynamic or Float, and all other basic types must be converted to/from it.
  *)

  let default_implementation ft parent_func_class (* e.g. new haxe.lang.ClassClosure *) =
    let gen = ft.fgen in
    ft.initialize_base_class parent_func_class;
    let cfs = ft.get_base_classfields_for parent_func_class true (fun () -> []) in
    List.iter (fun cf ->
      (if cf.cf_name = "new" then parent_func_class.cl_constructor <- Some cf else
        parent_func_class.cl_fields <- PMap.add cf.cf_name cf parent_func_class.cl_fields
      )
    ) cfs;

    parent_func_class.cl_ordered_fields <- (List.filter (fun cf -> cf.cf_name <> "new") cfs) @ parent_func_class.cl_ordered_fields;

    ft.func_class <- parent_func_class;

    traverse
      ft.fgen
      (* (transform_closure:texpr->texpr->string->texpr) (handle_anon_func:texpr->tfunc->texpr) (dynamic_func_call:texpr->texpr->texpr list->texpr) *)
      ft.transform_closure
      (fun fexpr tfunc -> (* (handle_anon_func:texpr->tfunc->texpr) *)
        (* get all captured variables it uses *)
        let captured_ht, tparams = get_captured fexpr in
        let captured = Hashtbl.fold (fun _ e acc -> e :: acc) captured_ht [] in

        (*let cltypes = List.map (fun cl -> (snd cl.cl_path, TInst(map_param cl, []) )) tparams in*)
        let cltypes = List.map (fun cl -> (snd cl.cl_path, TInst(cl, []) )) tparams in

        (* create a new class that extends abstract function class, with a ctor implementation that will setup all captured variables *)
        let buf = Buffer.create 72 in
        ignore (Type.map_expr (fun e ->
          Buffer.add_string buf (Marshal.to_string (ExprHashtblHelper.mk_type e) [Marshal.Closures]);
          e
        ) tfunc.tf_expr);
        let digest = Digest.to_hex (Digest.string (Buffer.contents buf)) in
        let path = (fst ft.fgen.gcurrent_path, "Fun_" ^ (String.sub digest 0 8)) in
        let cls = mk_class (get ft.fgen.gcurrent_class).cl_module path tfunc.tf_expr.epos in
        cls.cl_module <- (get ft.fgen.gcurrent_class).cl_module;
        cls.cl_types <- cltypes;

        let mk_this v pos =
          {
            (mk_field_access gen { eexpr = TConst TThis; etype = TInst(cls, List.map snd cls.cl_types); epos = pos } v.v_name pos)
            with etype = v.v_type
          }
        in

        let mk_this_assign v pos =
        {
          eexpr = TBinop(OpAssign, mk_this v pos, { eexpr = TLocal(v); etype = v.v_type; epos = pos });
          etype = v.v_type;
          epos = pos
        } in

        (* mk_class_field name t public pos kind params *)
        let ctor_args, ctor_sig, ctor_exprs = List.fold_left (fun (ctor_args, ctor_sig, ctor_exprs) lexpr ->
          match lexpr.eexpr with
            | TLocal(v) ->
              let cf = mk_class_field v.v_name v.v_type false lexpr.epos (Var({ v_read = AccNormal; v_write = AccNormal; })) [] in
              cls.cl_fields <- PMap.add v.v_name cf cls.cl_fields;
              cls.cl_ordered_fields <- cf :: cls.cl_ordered_fields;

              let ctor_v = alloc_var v.v_name v.v_type in
              ((ctor_v, None) :: ctor_args, (v.v_name, false, v.v_type) :: ctor_sig, (mk_this_assign v cls.cl_pos) :: ctor_exprs)
            | _ -> assert false
        ) ([],[],[]) captured in

        (* change all captured variables to this.capturedVariable *)
        let rec change_captured e =
          match e.eexpr with
            | TLocal( ({ v_capture = true }) as v ) when Hashtbl.mem captured_ht v.v_id ->
              mk_this v e.epos
            | _ -> Type.map_expr change_captured e
        in
        let func_expr = change_captured tfunc.tf_expr in

        let invoke_field, super_args = ft.closure_to_classfield { tfunc with tf_expr = func_expr } fexpr.etype fexpr.epos in


        (* create the constructor *)
        (* todo properly abstract how type var is set *)

        cls.cl_super <- Some(parent_func_class, []);
        let pos = cls.cl_pos in
        let super_call =
        {
          eexpr = TCall({ eexpr = TConst(TSuper); etype = TInst(parent_func_class,[]); epos = pos }, super_args);
          etype = ft.fgen.gcon.basic.tvoid;
          epos = pos;
        } in

        let ctor_type = (TFun(ctor_sig, ft.fgen.gcon.basic.tvoid)) in
        let ctor = mk_class_field "new" ctor_type true cls.cl_pos (Method(MethNormal)) [] in
        ctor.cf_expr <- Some(
        {
          eexpr = TFunction(
          {
            tf_args = ctor_args;
            tf_type = ft.fgen.gcon.basic.tvoid;
            tf_expr = { eexpr = TBlock(super_call :: ctor_exprs); etype = ft.fgen.gcon.basic.tvoid; epos = cls.cl_pos }
          });
          etype = ctor_type;
          epos = cls.cl_pos;
        });
        cls.cl_constructor <- Some(ctor);

        (* add invoke function to the class *)
        cls.cl_ordered_fields <- invoke_field :: cls.cl_ordered_fields;
        cls.cl_fields <- PMap.add invoke_field.cf_name invoke_field cls.cl_fields;
        cls.cl_overrides <- invoke_field :: cls.cl_overrides;

        (* add this class to the module with gadd_to_module *)
        ft.fgen.gadd_to_module (TClassDecl(cls)) priority;

    (* if there are no captured variables, we can create a cache so subsequent calls don't need to create a new function *)
    match captured, tparams with
      | [], [] ->
        let cache_var = ft.fgen.gmk_internal_name "hx" "current" in
        let cache_cf = mk_class_field cache_var (TInst(cls,[])) false func_expr.epos (Var({ v_read = AccNormal; v_write = AccNormal })) [] in
        cls.cl_ordered_statics <- cache_cf :: cls.cl_ordered_statics;
        cls.cl_statics <- PMap.add cache_var cache_cf cls.cl_statics;

        (* if (FuncClass.hx_current != null) FuncClass.hx_current; else (FuncClass.hx_current = new FuncClass()); *)

        (* let mk_static_field_access cl field fieldt pos = *)
        let hx_current = mk_static_field_access cls cache_var (TInst(cls,[])) func_expr.epos in

        let pos = func_expr.epos in
        {
          fexpr with

          eexpr = TIf(
          {
            eexpr = TBinop(OpNotEq, hx_current, null (TInst(cls,[])) pos);
            etype = ft.fgen.gcon.basic.tbool;
            epos = pos;
          },

          hx_current,

          Some(
          {
            eexpr = TBinop(OpAssign, hx_current, { fexpr with eexpr = TNew(cls, [], captured) });
            etype = (TInst(cls,[]));
            epos = pos;
          }))

        }

      | _ ->
        (* change the expression so it will be a new "added class" ( captured variables arguments ) *)
        { fexpr with eexpr = TNew(cls, List.map (fun cl -> TInst(cl,[])) tparams, List.rev captured) }


      )
      ft.dynamic_fun_call
      (* (dynamic_func_call:texpr->texpr->texpr list->texpr) *)


  let configure gen (mapping_func:texpr->texpr) =
    let map e = Some(mapping_func e) in
    gen.gexpr_filters#add ~name:name ~priority:(PCustom priority) map

  let configure_as_synf gen (mapping_func:texpr->texpr) =
    let map e = Some(mapping_func e) in
    gen.gsyntax_filters#add ~name:name ~priority:(PCustom priority_as_synf) map


  (*
    this submodule will provide the default implementation for the C# and Java targets.

    it will have two return types: double and dynamic, and
  *)
  module DoubleAndDynamicClosureImpl =
  struct

    let get_ctx gen max_arity =

      let basic = gen.gcon.basic in

      let func_args_i i =

        let rec loop i (acc1,acc2) =
          if i = 0 then (acc1,acc2) else begin
            let vfloat = alloc_var (gen.gmk_internal_name "fn" ("float" ^ string_of_int i)) basic.tfloat in
            let vdyn = alloc_var (gen.gmk_internal_name "fn" ("dyn" ^ string_of_int i)) t_dynamic in

            loop (i - 1) ((vfloat, None) :: acc1 , (vdyn, None) :: acc2)
          end
        in
        let acc1, acc2 = loop i ([],[]) in
        acc1 @ acc2

      in

      let args_real_to_func args =

        let arity = List.length args in
        if arity >= max_arity then
          [ alloc_var (gen.gmk_internal_name "fn" "dynargs") (basic.tarray t_dynamic), None ]
        else func_args_i arity
      in

      let func_sig_i i =

        let rec loop i (acc1,acc2) =
          if i = 0 then (acc1,acc2) else begin
            let vfloat = gen.gmk_internal_name "fn" ("float" ^ string_of_int i) in
            let vdyn = gen.gmk_internal_name "fn" ("dyn" ^ string_of_int i) in

            loop (i - 1) ((vfloat, false, basic.tfloat) :: acc1 , (vdyn, false, t_dynamic) :: acc2)
          end
        in
        let acc1, acc2 = loop i ([],[]) in
        acc1 @ acc2

      in

      let args_real_to_func_sig args =

        let arity = List.length args in
        if arity >= max_arity then
          [gen.gmk_internal_name "fn" "dynargs", false, basic.tarray t_dynamic]
        else begin
          func_sig_i arity
        end

      in

      let rettype_real_to_func t =
        if like_float t then
          (1, basic.tfloat)
        else
          (0, t_dynamic)
      in

      let args_real_to_func_call el (pos:Ast.pos) =
        if List.length el >= max_arity then
          [{ eexpr = TArrayDecl el; etype = basic.tarray t_dynamic; epos = pos }]
        else begin
          let acc1,acc2 = List.fold_left (fun (acc_f,acc_d) e ->
                                            if like_float (gen.greal_type e.etype) then
                                              ( e :: acc_f, undefined e.epos :: acc_d )
                                            else
                                              ( null basic.tfloat e.epos :: acc_f, e :: acc_d )
          ) ([],[]) (List.rev el) in
          acc1 @ acc2
        end
      in

      let const_type c def =
        match c with
          | TString _ -> basic.tstring | TInt _ -> basic.tint
          | TFloat _ -> basic.tfloat | TBool _ -> basic.tbool
          | _ -> def
      in

      let get_args_func args changed_args pos =
        let arity = List.length args in
        let mk_const const elocal t =
          match const with
            | None -> mk_cast t elocal
            | Some const ->
              { eexpr = TIf(
                { elocal with eexpr = TBinop(Ast.OpEq, elocal, null elocal.etype elocal.epos); etype = basic.tbool },
                { elocal with eexpr = TConst(const); etype = const_type const t },
                Some ( mk_cast t elocal )
              ); etype = t; epos = elocal.epos }
        in

        if arity >= max_arity then begin
          let varray = match changed_args with | [v,_] -> v | _ -> assert false in
          let varray_local = mk_local varray pos in
          let mk_varray i = { eexpr = TArray(varray_local, { eexpr = TConst(TInt(Int32.of_int i)); etype = basic.tint; epos = pos }); etype = t_dynamic; epos = pos } in

          snd (List.fold_left (fun (count,acc) (v,const) ->
            (count + 1,
              {
                eexpr = TVars([v, Some(mk_const const ( mk_varray count ) v.v_type)]);
                etype = basic.tvoid;
                epos = pos;
              } :: acc)
          ) (0,[]) args)
        end else begin
          let _, dyn_args, float_args = List.fold_left (fun (count,fargs, dargs) arg ->
            if count > arity then
              (count + 1, fargs, arg :: dargs)
            else
              (count + 1, arg :: fargs, dargs)
          ) (1,[],[]) (List.rev changed_args) in

          let rec loop acc args fargs dargs =
            match args, fargs, dargs with
              | [], [], [] -> acc
              | (v,const) :: args, (vf,_) :: fargs, (vd,_) :: dargs ->
                let acc = { eexpr = TVars([ v, Some(
                  {
                    eexpr = TIf(
                      { eexpr = TBinop(Ast.OpEq, mk_local vd pos, undefined pos); etype = basic.tbool; epos = pos },
                      mk_cast v.v_type (mk_local vf pos),
                      Some ( mk_const const (mk_local vd pos) v.v_type )
                    );
                    etype = v.v_type;
                    epos = pos
                  } ) ]); etype = basic.tvoid; epos = pos } :: acc in
                loop acc args fargs dargs
              | _ -> assert false
          in

          loop [] args float_args dyn_args
        end
      in

      let closure_to_classfield tfunc old_sig pos =
        (* change function signature *)
        let old_args = tfunc.tf_args in
        let changed_args = args_real_to_func old_args in

        (*
          FIXME properly handle int64 cases, which will break here (because of inference to int)
          UPDATE: the fix will be that Int64 won't be a typedef to Float/Int
        *)
        let changed_sig, arity, type_number, changed_sig_ret, is_void, is_dynamic_func = match follow old_sig with
          | TFun(_sig, ret) ->
            let type_n, ret_t = rettype_real_to_func ret in
            let arity = List.length _sig in
            let is_dynamic_func = arity >= max_arity in
            let ret_t = if is_dynamic_func then t_dynamic else ret_t in

            (TFun(args_real_to_func_sig _sig, ret_t), arity, type_n, ret_t, is_void ret, is_dynamic_func)
          | _ -> (trace (s_type (print_context()) (follow old_sig) )); assert false
        in

        let tf_expr = if is_void then begin
          let rec map e =
            match e.eexpr with
              | TReturn None -> { e with eexpr = TReturn (Some (null t_dynamic e.epos)) }
              | _ -> Type.map_expr map e
          in
          let e = mk_block (map tfunc.tf_expr) in
          match e.eexpr with
            | TBlock(bl) ->
              { e with eexpr = TBlock(bl @ [{ eexpr = TReturn (Some (null t_dynamic e.epos)); etype = t_dynamic; epos = e.epos }]) }
            | _ -> assert false
        end else tfunc.tf_expr in

        let changed_sig_ret = if is_dynamic_func then t_dynamic else changed_sig_ret in

        (* get real arguments on top of function body *)
        let get_args = get_args_func tfunc.tf_args changed_args pos in
        (*
          FIXME HACK: in order to be able to run the filters that have already ran for this piece of code,
          we will cheat and run it as if it was the whole code
          We could just make ClosuresToClass run before TArrayTransform, but we cannot because of the
          dependency between ClosuresToClass (after DynamicFieldAccess, and before TArrayTransform)

          maybe a way to solve this would be to add an "until" field to run_from
        *)
        let real_get_args = gen.gexpr_filters#run_f { eexpr = TBlock(get_args); etype = basic.tvoid; epos = pos } in

        let func_expr = Codegen.concat real_get_args tf_expr in

        (* set invoke function *)
        (* todo properly abstract how naming for invoke is made *)
        let invoke_name = if is_dynamic_func then "invokeDynamic" else ("invoke" ^ (string_of_int arity) ^ (if type_number = 0 then "_o" else "_f")) in
        let invoke_name = gen.gmk_internal_name "hx" invoke_name in
        let invoke_field = mk_class_field invoke_name changed_sig false func_expr.epos (Method(MethNormal)) [] in
        let invoke_fun =
        {
          eexpr = TFunction(
          {
            tf_args = changed_args;
            tf_type = changed_sig_ret;
            tf_expr = func_expr;
          });
          etype = changed_sig;
          epos = func_expr.epos;
        } in
        invoke_field.cf_expr <- Some(invoke_fun);

        (invoke_field, [
          { eexpr = TConst(TInt( Int32.of_int arity )); etype = gen.gcon.basic.tint; epos = pos };
          { eexpr = TConst(TInt( Int32.of_int type_number )); etype = gen.gcon.basic.tint; epos = pos };
        ])
      in

      let dynamic_fun_call call_expr =
        let tc, params = match call_expr.eexpr with
          | TCall(tc, params) -> (tc, params)
          | _ -> assert false
        in
          let postfix, ret_t =
            if like_float (gen.greal_type call_expr.etype) then
                "_f", gen.gcon.basic.tfloat
            else
              "_o", t_dynamic
          in
          let params_len = List.length params in
          let ret_t = if params_len >= max_arity then t_dynamic else ret_t in

          let invoke_fun = if params_len >= max_arity then "invokeDynamic" else "invoke" ^ (string_of_int params_len) ^ postfix in
          let invoke_fun = gen.gmk_internal_name "hx" invoke_fun in
          let fun_t = match follow tc.etype with
            | TFun(_sig, _) ->
              TFun(args_real_to_func_sig _sig, ret_t)
            | _ ->
              let i = ref 0 in
              let _sig = List.map (fun p -> let name = "arg" ^ (string_of_int !i) in incr i; (name,false,p.etype) ) params in
              TFun(args_real_to_func_sig _sig, ret_t)
          in

          let may_cast = match follow call_expr.etype with
            | TEnum({ e_path = ([], "Void")}, [])
            | TAbstract ({ a_path = ([], "Void") },[]) -> (fun e -> e)
            | _ -> mk_cast call_expr.etype
          in

          may_cast
          {
            eexpr = TCall(
              { (mk_field_access gen { tc with etype = gen.greal_type tc.etype } invoke_fun tc.epos) with etype = fun_t },
              args_real_to_func_call params call_expr.epos
            );
            etype = ret_t;
            epos = call_expr.epos
          }
      in

      let iname is_function i is_float =
        let postfix = if is_float then "_f" else "_o" in
        gen.gmk_internal_name "hx" ("invoke" ^ (if not is_function then "Field" else "") ^ string_of_int i) ^ postfix
      in

      let map_base_classfields cl is_function map_fn =

        let pos = cl.cl_pos in
        let this_t = TInst(cl,List.map snd cl.cl_types) in
        let this = { eexpr = TConst(TThis); etype = this_t; epos = pos } in
        let mk_this field t = { (mk_field_access gen this field pos) with etype = t } in

        let mk_invoke_i i is_float =
          let cf = mk_class_field (iname is_function i is_float) (TFun(func_sig_i i, if is_float then basic.tfloat else t_dynamic)) false pos (Method MethNormal) [] in
          cf
        in

        let type_name = gen.gmk_internal_name "fn" "type" in

        let dynamic_arg = alloc_var (gen.gmk_internal_name "fn" "dynargs") (basic.tarray t_dynamic) in

        let mk_invoke_complete_i i is_float =

          let arity = i in
          let args = func_args_i i in

          (* api fn *)

          (* only cast if needed *)
          let mk_cast tto efrom = gen.ghandle_cast (gen.greal_type tto) (gen.greal_type efrom.etype) efrom in
          let api i t const =
            let vf, _ = List.nth args i in
            let vo, _ = List.nth args (i + arity) in

            let needs_cast, is_float = match t, like_float t with
              | TInst({ cl_path = ([], "Float") }, []), _
              | TAbstract({ a_path = ([], "Float") },[]), _ -> false, true
              | _, true -> true, true
              | _ -> false,false
            in

            let olocal = mk_local vo pos in
            let flocal = mk_local vf pos in

            let get_from_obj e = match const with
              | None -> mk_cast t e
              | Some tc ->
                {
                  eexpr = TIf(
                    { eexpr = TBinop(Ast.OpEq, olocal, null t_dynamic pos); etype = basic.tbool; epos = pos } ,
                    { eexpr = TConst(tc); etype = t; epos = pos },
                    Some (mk_cast t e)
                  );
                  etype = t;
                  epos = pos;
                }
            in

            {
              eexpr = TIf(
                { eexpr = TBinop(Ast.OpEq, olocal, undefined pos); etype = basic.tbool; epos = pos },
                (if needs_cast then mk_cast t flocal else flocal),
                Some ( get_from_obj olocal )
              );
              etype = t;
              epos = pos
            }
          in
          (* end of api fn *)

          let ret = if is_float then basic.tfloat else t_dynamic in

          let added_args, fn_expr = map_fn i ret (List.map fst args) api in
          let args = added_args @ args in

          let t = TFun(fun_args args, ret) in

          let tfunction =
            {
              eexpr = TFunction({
                tf_args = args;
                tf_type = ret;
                tf_expr =
                mk_block fn_expr
              });
              etype = t;
              epos = pos;
            }
          in

          let cf = mk_invoke_i i is_float in
          cf.cf_expr <- Some tfunction;
          cf
        in

        let rec loop i cfs =
          if i < 0 then cfs else begin
            (*let mk_invoke_complete_i i is_float =*)
            (mk_invoke_complete_i i false) :: (mk_invoke_complete_i i true) :: (loop (i-1) cfs)
          end
        in

        let cfs = loop max_arity [] in

        let added_s_args, switch =
          let api i t const =
            match i with
              | -1 ->
                mk_local dynamic_arg pos
              | _ ->
                mk_cast t {
                  eexpr = TArray(
                    mk_local dynamic_arg pos,
                    { eexpr = TConst(TInt(Int32.of_int i)); etype = basic.tint; epos = pos });
                  etype = t;
                  epos = pos;
                }
          in
          map_fn (-1) t_dynamic [dynamic_arg] api
        in

        let args = added_s_args @ [dynamic_arg, None] in
        let dyn_t = TFun(fun_args args, t_dynamic) in
        let dyn_cf = mk_class_field (gen.gmk_internal_name "hx" "invokeDynamic") dyn_t false pos (Method MethNormal) [] in

        dyn_cf.cf_expr <-
        Some {
          eexpr = TFunction({
            tf_args = args;
            tf_type = t_dynamic;
            tf_expr = mk_block switch
          });
          etype = dyn_t;
          epos = pos;
        };

        let additional_cfs = if is_function then begin
          let new_t = TFun(["arity", false, basic.tint; "type", false, basic.tint],basic.tvoid) in
          let new_cf = mk_class_field "new" (new_t) true pos (Method MethNormal) [] in
          let v_arity, v_type = alloc_var "arity" basic.tint, alloc_var "type" basic.tint in
          let mk_assign v field = { eexpr = TBinop(Ast.OpAssign, mk_this field v.v_type, mk_local v pos); etype = v.v_type; epos = pos } in

          let arity_name = gen.gmk_internal_name "hx" "arity" in
          new_cf.cf_expr <-
            Some {
              eexpr = TFunction({
                tf_args = [v_arity, None; v_type, None];
                tf_type = basic.tvoid;
                tf_expr =
                {
                  eexpr = TBlock([
                    mk_assign v_type type_name;
                    mk_assign v_arity arity_name
                  ]);
                  etype = basic.tvoid;
                  epos = pos;
                }
              });
              etype = new_t;
              epos = pos;
            }
          ;

          [
            new_cf;
            mk_class_field type_name basic.tint true pos (Var { v_read = AccNormal; v_write = AccNormal }) [];
            mk_class_field arity_name basic.tint true pos (Var { v_read = AccNormal; v_write = AccNormal }) [];
          ]
        end else [] in

        dyn_cf :: (additional_cfs @ cfs)
      in

      (* maybe another param for prefix *)
      let get_base_classfields_for cl is_function mk_additional_args =
        let pos = cl.cl_pos in

        let this_t = TInst(cl,List.map snd cl.cl_types) in
        let this = { eexpr = TConst(TThis); etype = this_t; epos = pos } in
        let mk_this field t = { (mk_field_access gen this field pos) with etype = t } in

        let rec mk_dyn_call arity api =
          let zero = { eexpr = TConst(TFloat("0.0")); etype = basic.tfloat; epos = pos } in
          let rec loop i (acc1,acc2) =
            if i = 0 then (acc1,acc2) else begin
              let arr = api (i-1) t_dynamic None in
              loop (i - 1) (zero :: acc1, arr :: acc2)
            end
          in
          let acc1, acc2 = loop arity ([],[]) in
          acc1 @ acc2
        in

        let mk_invoke_switch i (api:(int->t->tconstant option->texpr)) =

          let t = TFun(func_sig_i i,t_dynamic) in

          (* case i: return this.invokeX_o(0, 0, 0, 0, 0, ... arg[0], args[1]....); *)
          ( [{ eexpr = TConst(TInt(Int32.of_int i)); etype = basic.tint; epos = pos }],
          {
            eexpr = TReturn(Some( {
              eexpr = TCall(mk_this (iname is_function i false) t, mk_dyn_call i api);
              etype = t_dynamic;
              epos = pos;
            } ));
            etype = t_dynamic;
            epos = pos;
          } )
        in

        let cl_t = TInst(cl,List.map snd cl.cl_types) in
        let this = { eexpr = TConst(TThis); etype = cl_t; epos = pos } in
        let mk_this field t = { (mk_field_access gen this field pos) with etype = t } in
        let mk_int i = { eexpr = TConst(TInt ( Int32.of_int i)); etype = basic.tint; epos = pos } in
        let mk_string s = { eexpr = TConst(TString s); etype = basic.tstring; epos = pos } in

        (*
          if it is the Function class, the base class fields will be
            * hx::invokeX_d|o (where X is from 0 to max_arity) (args)
            {
              if (this.type == 0|1) return invokeX_o|d(args); else throw "Invalid number of arguments."
            }

            hx::invokeDynamic, which will work in the same way

            new(arity, type)
            {
              if (type != 0 && type != 1) throw "Invalid type";
              this.arity = arity;
              this.type = type;
            }
        *)
        let type_name = gen.gmk_internal_name "fn" "type" in

        let mk_expr i is_float vars =

          let name = if is_function then "invoke" else "invokeField" in

          let look_ahead = alloc_var "lookAhead" basic.tbool in
          let add_args = if not is_function then mk_additional_args() else [] in
          let vars = if not is_function then (List.map fst add_args) @ (look_ahead :: vars) else vars in

          let call_expr =

            let call_t = TFun(List.map (fun v -> (v.v_name, false, v.v_type)) vars, if is_float then t_dynamic else basic.tfloat) in
            {
              eexpr = TCall(mk_this (gen.gmk_internal_name "hx" (name ^ (string_of_int i) ^ (if is_float then "_o" else "_f"))) call_t, List.map (fun v ->  if v.v_id = look_ahead.v_id then ( { eexpr = TConst(TBool false); etype = basic.tbool; epos = pos } ) else mk_local v pos) vars );
              etype = if is_float then t_dynamic else basic.tfloat;
              epos = pos
            }
          in
          (*let call_expr = if is_float then mk_cast basic.tfloat call_expr else call_expr in*)

          let if_cond = if is_function then
            { eexpr=TBinop(Ast.OpNotEq, mk_this type_name basic.tint, mk_int (if is_float then 0 else 1) ); etype = basic.tbool; epos = pos }
          else
            mk_local look_ahead pos
          in

          let if_expr = if is_function then
            {
              eexpr = TIf(if_cond,
                { eexpr = TThrow(mk_string "Wrong number of arguments"); etype = basic.tstring; epos = pos },
                Some( { eexpr = TReturn( Some( call_expr ) ); etype = call_expr.etype; epos = pos } )
              );
              etype = t_dynamic;
              epos = pos;
            }
          else
            {
              eexpr = TIf(if_cond,
              { eexpr = TReturn( Some( call_expr ) ); etype = call_expr.etype; epos = pos },
              Some( { eexpr = TThrow(mk_string "Field not found or wrong number of arguments"); etype = basic.tstring; epos = pos } )
              );
              etype = t_dynamic;
              epos = pos;
            }
          in

          let args = if not is_function then (mk_additional_args()) @ [look_ahead, None] else [] in
          (args, if_expr)
        in

        let arities_processed = Hashtbl.create 10 in
        let max_arity = ref 0 in

        let rec loop_cases api arity acc =
          if arity < 0 then acc else
            loop_cases api (arity - 1) (mk_invoke_switch arity api :: acc)
        in
        (* let rec loop goes here *)
        let map_fn cur_arity fun_ret_type vars (api:(int->t->tconstant option->texpr)) =
          let is_float = like_float fun_ret_type in
          match cur_arity with
            | -1 ->
              let dynargs = api (-1) (t_dynamic) None in
              let switch_cond = mk_field_access gen dynargs "length" pos in
              let switch_cond = {
                eexpr = TIf(
                  { eexpr = TBinop(Ast.OpEq, dynargs, null dynargs.etype pos); etype = basic.tbool; epos = pos; },
                  { eexpr = TConst(TInt(Int32.zero)); etype = basic.tint; epos = pos },
                  Some switch_cond);
                etype = basic.tint;
                epos = pos;
              } in

              let switch =
              {
                eexpr = TSwitch( switch_cond,
                  loop_cases api !max_arity [],
                  Some({ eexpr = TThrow(mk_string "Too many arguments"); etype = basic.tvoid; epos = pos; }) );
                etype = basic.tvoid;
                epos = pos;
              } in

              ( (if not is_function then mk_additional_args () else []), switch )
            | _ ->
              if not (Hashtbl.mem arities_processed cur_arity) then begin
                Hashtbl.add arities_processed cur_arity true;
                if cur_arity > !max_arity then max_arity := cur_arity
              end;

              mk_expr cur_arity is_float vars
        in

        map_base_classfields cl is_function map_fn
      in

      let initialize_base_class cl =
        ()
      in

      {
        fgen = gen;

        func_class = null_class;

        closure_to_classfield = closure_to_classfield;

        dynamic_fun_call = dynamic_fun_call;

        (*
          called once so the implementation can make one of a time initializations in the base class
          for all functions
        *)
        initialize_base_class = initialize_base_class;

        (*
          Base classfields are the class fields for the abstract implementation of either the Function implementation,
          or the invokeField implementation for the classes
          They will either try to call the right function or will fail with

          (tclass - subject (so we know the type of this)) -> is_function_base -> list of the abstract implementation class fields
        *)
        get_base_classfields_for = get_base_classfields_for;

        map_base_classfields = map_base_classfields;

        (*
          for now we won't deal with the closures.
          They can be dealt with the module ReflectionCFs,
          or a custom implementation
        *)
        transform_closure = (fun tclosure texpr str -> tclosure);

      }

  end;;

end;;

(* ******************************************* *)
(* Type Parameters *)
(* ******************************************* *)

(*

  This module will handle type parameters. There are lots of changes we need to do to correctly support type parameters:

  traverse will:
    V Detect when parameterized function calls are made
    * Detect when a parameterized class instance is being cast to another parameter
    * Change new<> parameterized function calls
    *

  extras:
    * On languages that support "real" type parameters, a Cast function is provided that will convert from a <Dynamic> to the requested type.
      This cast will call createEmpty with the correct type, and then set each variable to the new form. Some types will be handled specially, namely the Native Array.
      Other implementations may be delegated to the runtime.
    * parameterized classes will implement a new interface (with only a Cast<> function added to it), so we can access the <Dynamic> type parameter for them. Also any reference to <Dynamic> will be replaced by a reference to this interface. (also on TTypeExpr - Std.is())
    * Type parameter renaming to avoid name clash
    * Detect type parameter casting and call Cast<> instead

  for java:
    * for specially assigned classes, parameters will be replaced by _d and _i versions of parameterized functions. This will only work for parameterized classes, not functions.

  dependencies:
    must run after casts are detected. This will be ensured at CastDetect module.

*)

module TypeParams =
struct

  let name = "type_params"

  let priority = max_dep -. 20.

  (* this function will receive the original function argument, the applied function argument and the original function parameters. *)
  (* from this info, it will infer the applied tparams for the function *)
  (* this function is used by CastDetection module *)
  let infer_params gen pos (original_args:((string * bool * t) list * t)) (applied_args:((string * bool * t) list * t)) (params:(string * t) list) impossible_tparam_is_dynamic : tparams =
    let args_list args = TFun( List.map (fun (n,o,t) -> (n,o,if impossible_tparam_is_dynamic then gen.greal_type t else t)) (fst args), if impossible_tparam_is_dynamic then t_dynamic else snd args ) in

    let monos = List.map (fun _ -> mk_mono()) params in
    let original = args_list original_args in

    let original = apply_params params monos original in
    let applied = args_list applied_args in

    (try
      unify applied original
    with | Unify_error el ->
        gen.gcon.warning ("This expression may be invalid") pos
    );

    List.map (fun t ->
      match follow t with
        | TMono _ -> t_empty
        | t -> t
    ) monos

  (* ******************************************* *)
  (* Real Type Parameters Module *)
  (* ******************************************* *)

  (*
    This submodule is by now specially made for the .NET platform. There might be other targets that will
    make use of this, but it IS very specific.

    On the .NET platform, generics are real specialized classes that are JIT compiled. For this reason, we cannot
    cast from one type parameter to another. Also there is no common type for the type parameters, so for example
    an instance of type Array<Int> will return false for instance is Array<object> .

    So we need to:
      1. create a common interface (without type parameters) (e.g. "Array") which will only contain a __Cast<> function, which will cast from one type into another
      2. Implement the __Cast function. This part is a little hard, as we must identify all type parameter-dependent fields contained in the class and convert them.
      In most cases the conversion will just be to call .__Cast<>() on the instances, or just a simple cast. But when the instance is a @:nativegen type, there will be no .__Cast
      function, and we will need to deal with this case either at compile-time (added handlers - specially for NativeArray), or at runtime (adding new runtime handlers)
      3. traverse the AST looking for casts involving type parameters, and replace them with .__Cast<>() calls. If type is @:nativegen, throw a warning. If really casting from one type parameter to another on a @:nativegen context, throw an error.


    special literals:
      it will use the special literal __typehandle__ that the target must implement in order to run this. This literal is a way to get the typehandle of e.g. the type parameters,
      so we can compare them. In C# it's the equivalent of typeof(T).TypeHandle (TypeHandle compare is faster than System.Type.Equals())

    dependencies:
      (module filter) Interface creation must run AFTER enums are converted into classes, otherwise there is no way to tell parameterized enums to implement an interface
      Must run AFTER CastDetect. This will be ensured per CastDetect

  *)

  module RealTypeParams =
  struct

    let name = "real_type_params"

    let priority = priority

    let cast_field_name = "cast"

    let rec has_type_params t =
      match follow t with
        | TInst( { cl_kind = KTypeParameter _ }, _) -> true
        | TAbstract(_, params)
        | TEnum(_, params)
        | TInst(_, params) -> List.fold_left (fun acc t -> acc || has_type_params t) false params
        | _ -> false

    let is_hxgeneric = function
      | TClassDecl(cl) ->
        not (Meta.has Meta.NativeGeneric cl.cl_meta)
      | TEnumDecl(e) ->
        not (Meta.has Meta.NativeGeneric e.e_meta)
      | TTypeDecl(t) ->
        not (Meta.has Meta.NativeGeneric t.t_meta)
      | TAbstractDecl a ->
        not (Meta.has Meta.NativeGeneric a.a_meta)

    let rec set_hxgeneric gen mds isfirst md =
      let path = t_path md in
      if List.exists (fun m -> path = t_path m) mds then begin
        if isfirst then
          None (* we still can't determine *)
        else
          Some true (* if we're in second pass and still can't determine, it's because it can be hxgeneric *)
      end else begin
        let has_unresolved = ref false in
        let is_false v =
          match v with
            | Some false -> true
            | None -> has_unresolved := true; false
            | Some true -> false
        in

        let mds = md :: mds in
        match md with
          | TClassDecl(cl)  ->
            (* first see if any meta is present (already processed) *)
            if Meta.has Meta.NativeGeneric cl.cl_meta then
              Some false
            else if Meta.has Meta.HaxeGeneric cl.cl_meta then
              Some true
            else if not (is_hxgen md) then
              (cl.cl_meta <- (Meta.NativeGeneric, [], cl.cl_pos) :: cl.cl_meta;
              Some false)
            else begin
              (*
                if it's not present, see if any superclass is nativegeneric.
                nativegeneric is inherited, while hxgeneric can be later changed to nativegeneric
              *)
              (* on the first pass, our job is to find any evidence that makes it not be hxgeneric. Otherwise it will be hxgeneric *)
              match cl.cl_super with
                | Some (c,_) when is_false (set_hxgeneric gen mds isfirst (TClassDecl c)) ->
                  cl.cl_meta <- (Meta.NativeGeneric, [], cl.cl_pos) :: cl.cl_meta;
                  Some false
                | _ ->
                  (* see if it's a generic class *)
                  match cl.cl_types with
                    | [] ->
                      (* if it's not, then it will be hxgeneric *)
                      cl.cl_meta <- (Meta.HaxeGeneric, [], cl.cl_pos) :: cl.cl_meta;
                      Some true
                    | _ ->
                      (* if it is, loop through all fields + statics and look for non-hxgeneric
                        generic classes that have KTypeParameter as params *)
                      let rec loop cfs =
                        match cfs with
                          | [] -> false
                          | cf :: cfs ->
                            let t = follow (gen.greal_type cf.cf_type) in
                            match t with
                              | TInst( { cl_kind = KTypeParameter _ }, _ ) -> loop cfs
                              | TInst(cl,p) when has_type_params t && is_false (set_hxgeneric gen mds isfirst (TClassDecl cl)) ->
                                if not (Hashtbl.mem gen.gtparam_cast cl.cl_path) then true else loop cfs
                              | TEnum(e,p) when has_type_params t && is_false (set_hxgeneric gen mds isfirst (TEnumDecl e)) ->
                                if not (Hashtbl.mem gen.gtparam_cast e.e_path) then true else loop cfs
                              | _ -> loop cfs (* TAbstracts / Dynamics can't be generic *)
                      in
                      if loop cl.cl_ordered_fields then begin
                        cl.cl_meta <- (Meta.NativeGeneric, [], cl.cl_pos) :: cl.cl_meta;
                        Some false
                      end else if isfirst && !has_unresolved then
                        None
                      else begin
                        cl.cl_meta <- (Meta.HaxeGeneric, [], cl.cl_pos) :: cl.cl_meta;
                        Some true
                      end
            end
          | TEnumDecl e ->
            if Meta.has Meta.NativeGeneric e.e_meta then
              Some false
            else if Meta.has Meta.HaxeGeneric e.e_meta then
              Some true
            else if not (is_hxgen (TEnumDecl e)) then begin
              e.e_meta <- (Meta.NativeGeneric, [], e.e_pos) :: e.e_meta;
              Some false
            end else begin
              (* if enum is not generic, then it's hxgeneric *)
              match e.e_types with
                | [] ->
                  e.e_meta <- (Meta.HaxeGeneric, [], e.e_pos) :: e.e_meta;
                  Some true
                | _ ->
                  let rec loop efs =
                    match efs with
                      | [] -> false
                      | ef :: efs ->
                        let t = follow (gen.greal_type ef.ef_type) in
                        match t with
                          | TFun(args, _) ->
                            if List.exists (fun (n,o,t) ->
                              let t = follow t in
                              match t with
                                | TInst( { cl_kind = KTypeParameter _ }, _ ) ->
                                  false
                                | TInst(cl,p) when has_type_params t && is_false (set_hxgeneric gen mds isfirst (TClassDecl cl)) ->
                                  not (Hashtbl.mem gen.gtparam_cast cl.cl_path)
                                | TEnum(e,p) when has_type_params t && is_false (set_hxgeneric gen mds isfirst (TEnumDecl e)) ->
                                  not (Hashtbl.mem gen.gtparam_cast e.e_path)
                                | _ -> false
                            ) args then
                              true
                            else
                              loop efs
                          | _ -> loop efs
                  in
                  let efs = PMap.fold (fun ef acc -> ef :: acc) e.e_constrs [] in
                  if loop efs then begin
                    e.e_meta <- (Meta.NativeGeneric, [], e.e_pos) :: e.e_meta;
                    Some false
                  end else if isfirst && !has_unresolved then
                    None
                  else begin
                    e.e_meta <- (Meta.HaxeGeneric, [], e.e_pos) :: e.e_meta;
                    Some true
                  end
            end
          | _ -> assert false
      end

    let set_hxgeneric gen md =
      match set_hxgeneric gen [] true md with
        | None ->
          get (set_hxgeneric gen [] false md)
        | Some v -> v

    let params_has_tparams params =
      List.fold_left (fun acc t -> acc || has_type_params t) false params

    (* ******************************************* *)
    (* RealTypeParamsModf *)
    (* ******************************************* *)

    (*

      This is the module filter of Real Type Parameters. It will traverse through all types and look for hxgeneric classes (only classes).
      When found, a parameterless interface will be created and associated via the "ifaces" Hashtbl to the original class.
      Also a "cast" function will be automatically generated which will handle unsafe downcasts to more specific type parameters (necessary for serialization)

      dependencies:
        Anything that may create hxgeneric classes must run before it.
        Should run before ReflectionCFs (this dependency will be added to ReflectionCFs), so the added interfaces also get to be real IHxObject's

    *)

    module RealTypeParamsModf =
    struct

      let name = "real_type_params_modf"

      let priority = solve_deps name []

      let rec get_fields gen cl params_cl params_cf acc =
        let fields = List.fold_left (fun acc cf ->
          match follow (gen.greal_type (gen.gfollow#run_f (cf.cf_type))) with
            | TInst(cli, ((_ :: _) as p)) when (not (is_hxgeneric (TClassDecl cli))) && params_has_tparams p ->
              (cf, apply_params cl.cl_types params_cl cf.cf_type, apply_params cl.cl_types params_cf cf.cf_type) :: acc
            | TEnum(e, ((_ :: _) as p)) when not (is_hxgeneric (TEnumDecl e)) && params_has_tparams p ->
              (cf, apply_params cl.cl_types params_cl cf.cf_type, apply_params cl.cl_types params_cf cf.cf_type) :: acc
            | _ -> acc
        ) [] cl.cl_ordered_fields in
        match cl.cl_super with
          | Some(cs, tls) ->
            get_fields gen cs (List.map (apply_params cl.cl_types params_cl) tls) (List.map (apply_params cl.cl_types params_cf) tls) (fields @ acc)
          | None -> (fields @ acc)

      (*
        Creates a cast classfield, with the desired name

        Will also look for previous cast() definitions and override them, to reflect the current type and fields

        FIXME: this function still doesn't support generics that extend generics, and are cast as one of its subclasses. This needs to be taken care, by
        looking at previous superclasses and whenever a generic class is found, its cast argument must be overriden. the toughest part is to know how to type
        the current type correctly.
      *)
      let create_cast_cfield gen cl name =
        let basic = gen.gcon.basic in
        let cparams = List.map (fun (s,t) -> (s, TInst (map_param (get_cl_t t), []))) cl.cl_types in
        let cfield = mk_class_field name (TFun([], t_dynamic)) false cl.cl_pos (Method MethNormal) cparams in
        let params = List.map snd cparams in

        let fields = get_fields gen cl (List.map snd cl.cl_types) params [] in

        (* now create the contents of the function *)
        (*
          it will look something like:
          if (typeof(T) == typeof(T2)) return this;

          var new_me = new CurrentClass<T2>(EmptyInstnace);

          for (field in Reflect.fields(this))
          {
            switch(field)
            {
              case "aNativeArray":
                var newArray = new NativeArray(this.aNativeArray.Length);

              default:
                Reflect.setField(new_me, field, Reflect.field(this, field));
            }
          }
        *)

        let new_t = TInst(cl, params) in
        let pos = cl.cl_pos in

        let new_me_var = alloc_var "new_me" new_t in
        let local_new_me = { eexpr = TLocal(new_me_var); etype = new_t; epos = pos } in
        let this = { eexpr = TConst(TThis); etype = (TInst(cl, List.map snd cl.cl_types)); epos = pos } in
        let field_var = alloc_var "field" gen.gcon.basic.tstring in
        let local_field = { eexpr = TLocal(field_var); etype = field_var.v_type; epos = pos } in

        let get_path t =
          match follow t with
            | TInst(cl,_) -> cl.cl_path
            | TEnum(e,_) -> e.e_path
            | TAbstract(a,_) -> a.a_path
            | TMono _
            | TDynamic _ -> ([], "Dynamic")
            | _ -> assert false
        in

        (* this will take all fields that were *)
        let fields_to_cases fields =
          List.map (fun (cf, t_cl, t_cf) ->
            let this_field = { eexpr = TField(this, FInstance(cl, cf)); etype = t_cl; epos = pos } in
            let expr =
            {
              eexpr = TBinop(OpAssign, { eexpr = TField(local_new_me, FInstance(cl, cf) ); etype = t_cf; epos = pos },
                try (Hashtbl.find gen.gtparam_cast (get_path t_cf)) this_field t_cf with | Not_found -> (* if not found tparam cast, it shouldn't be a valid hxgeneric *) assert false
              );
              etype = t_cf;
              epos = pos;
            } in

            ([{ eexpr = TConst(TString(cf.cf_name)); etype = gen.gcon.basic.tstring; epos = pos }], expr)
          ) fields
        in

        let thandle = alloc_var "__typeof__" t_dynamic in
        let mk_typehandle cl =
          { eexpr = TCall(mk_local thandle pos, [ mk_classtype_access cl pos ]); etype = t_dynamic; epos = pos }
        in

        let mk_eq cl1 cl2 =
          { eexpr = TBinop(Ast.OpEq, mk_typehandle cl1, mk_typehandle cl2); etype = basic.tbool; epos = pos }
        in

        let rec mk_typehandle_cond thisparams cfparams =
          match thisparams, cfparams with
            | TInst(cl_this,[]) :: [], TInst(cl_cf,[]) :: [] ->
              mk_eq cl_this cl_cf
            | TInst(cl_this,[]) :: hd, TInst(cl_cf,[]) :: hd2 ->
              { eexpr = TBinop(Ast.OpBoolAnd, mk_eq cl_this cl_cf, mk_typehandle_cond hd hd2); etype = basic.tbool; epos = pos }
            | v :: hd, v2 :: hd2 ->
              (match follow v, follow v2 with
                | (TInst(cl1,[]) as v), (TInst(cl2,[]) as v2) ->
                  mk_typehandle_cond (v :: hd) (v2 :: hd2)
                | _ ->
                  assert false
              )
            | _ -> assert false
        in

        let ref_fields = gen.gtools.r_fields true this in
        let fn =
        {
          tf_args = [];
          tf_type = t_dynamic;
          tf_expr =
            {
              eexpr = TBlock([
                (* if (typeof(T) == typeof(T2)) return this *)
                {
                  eexpr = TIf(
                    mk_typehandle_cond (List.map snd cl.cl_types) params,
                    mk_return this,
                    None);
                  etype = basic.tvoid;
                  epos = pos;
                };
                (* var new_me = /*special create empty with tparams construct*/ *)
                { eexpr = TVars([new_me_var, Some(
                  gen.gtools.rf_create_empty cl params pos
                )]); etype = gen.gcon.basic.tvoid; epos = pos };
                { eexpr = TFor( (* for (field in Reflect.fields(this)) *)
                  field_var,
                  mk_iterator_access gen gen.gcon.basic.tstring ref_fields,
                  (* { *)
                    (* switch(field) *)
                    {
                      eexpr = TSwitch(local_field, fields_to_cases fields, Some(
                        (* default: Reflect.setField(new_me, field, Reflect.field(this, field)) *)
                        gen.gtools.r_set_field (gen.gcon.basic.tvoid) local_new_me local_field (gen.gtools.r_field false t_dynamic this local_field)
                      ));
                      etype = t_dynamic;
                      epos = pos;
                    }
                  (* } *)
                ); etype = t_dynamic; epos = pos };
                (* return new_me *)
                mk_return (mk_local new_me_var pos)
              ]);
              etype = t_dynamic;
              epos = pos;
            };
        }
        in

        cfield.cf_expr <- Some( { eexpr = TFunction(fn); etype = cfield.cf_type; epos = pos } );

        cfield

      let create_static_cast_cf gen iface cf =
        let p = iface.cl_pos in
        let basic = gen.gcon.basic in
        let cparams = List.map (fun (s,t) -> ("To_" ^ s, TInst (map_param (get_cl_t t), []))) cf.cf_params in
        let me_type = TInst(iface,[]) in
        let cfield = mk_class_field "__hx_cast" (TFun(["me",false,me_type], t_dynamic)) false iface.cl_pos (Method MethNormal) (cparams) in
        let params = List.map snd cparams in

        let me = alloc_var "me" me_type in
        let field = { eexpr = TField(mk_local me p, FInstance(iface,cf)); etype = apply_params cf.cf_params params cf.cf_type; epos = p } in
        let call =
        {
          eexpr = TCall(field, []);
          etype = t_dynamic;
          epos = p;
        } in
        let call = gen.gparam_func_call call field params [] in

        let delay () =
          cfield.cf_expr <-
          Some {
            eexpr = TFunction(
            {
              tf_args = [me,None];
              tf_type = t_dynamic;
              tf_expr =
              {
                eexpr = TReturn( Some
                {
                  eexpr = TIf(
                    { eexpr = TBinop(Ast.OpNotEq, mk_local me p, null me.v_type p); etype = basic.tbool; epos = p },
                    call,
                    Some( null me.v_type p )
                  );
                  etype = t_dynamic;
                  epos = p;
                });
                etype = basic.tvoid;
                epos = p;
              }
            });
            etype = cfield.cf_type;
            epos = p;
          }
        in
        cfield, delay

      let default_implementation gen ifaces base_generic =
        let add_iface cl =
          gen.gadd_to_module (TClassDecl cl) (max_dep);
        in

        let rec run md =
          match md with
            | TClassDecl ({ cl_extern = false; cl_types = hd :: tl } as cl) when set_hxgeneric gen md ->
              let iface = mk_class cl.cl_module cl.cl_path cl.cl_pos in
              iface.cl_array_access <- Option.map (apply_params (cl.cl_types) (List.map (fun _ -> t_dynamic) cl.cl_types)) cl.cl_array_access;
              iface.cl_module <- cl.cl_module;
              iface.cl_meta <- (Meta.HxGen, [], cl.cl_pos) :: iface.cl_meta;
              Hashtbl.add ifaces cl.cl_path iface;

              iface.cl_implements <- (base_generic, []) :: iface.cl_implements;
              iface.cl_interface <- true;
              cl.cl_implements <- (iface, []) :: cl.cl_implements;

              let original_name = cast_field_name in
              let name = String.concat "." ((fst cl.cl_path) @ [snd cl.cl_path; original_name]) (* explicitly define it *) in
              let cast_cf = create_cast_cfield gen cl name in

              (if not cl.cl_interface then cl.cl_ordered_fields <- cast_cf :: cl.cl_ordered_fields);
              let iface_cf = mk_class_field original_name cast_cf.cf_type false cast_cf.cf_pos (Method MethNormal) cast_cf.cf_params in
              let cast_static_cf, delay = create_static_cast_cf gen iface iface_cf in

              cl.cl_ordered_statics <- cast_static_cf :: cl.cl_ordered_statics;
              cl.cl_statics <- PMap.add cast_static_cf.cf_name cast_static_cf cl.cl_statics;
              gen.gafter_filters_ended <- delay :: gen.gafter_filters_ended; (* do not let filters alter this expression content *)

              iface_cf.cf_type <- cast_cf.cf_type;
              iface.cl_fields <- PMap.add original_name iface_cf iface.cl_fields;
              iface.cl_ordered_fields <- [iface_cf];

              add_iface iface;
              md
            | TTypeDecl _ | TAbstractDecl _ -> md
            | TEnumDecl _ ->
              ignore (set_hxgeneric gen md);
              md
            | _ -> ignore (set_hxgeneric gen md); md
        in
        run

      let configure gen mapping_func =
        let map e = Some(mapping_func e) in
        gen.gmodule_filters#add ~name:name ~priority:(PCustom priority) map

    end;;

    (* create a common interface without type parameters and only a __Cast<> function *)
    let default_implementation gen (dyn_tparam_cast:texpr->t->texpr) ifaces =
      let change_expr e cl iface params =
        let field = mk_static_field_access_infer cl "__hx_cast" e.epos params in
        let elist = [mk_cast (TInst(iface,[])) e] in
        let call = { eexpr = TCall(field, elist); etype = t_dynamic; epos = e.epos } in

        gen.gparam_func_call call field params elist
      in

      let rec run e =
        match e.eexpr with
            | TCast(cast_expr, _) ->
              (* see if casting to a native generic class *)
              let t = follow (gen.greal_type e.etype) in
              (match t with
                | TInst(cl, p1 :: pl) when is_hxgeneric (TClassDecl cl) ->
                  let iface = Hashtbl.find ifaces cl.cl_path in
                  mk_cast e.etype (change_expr (Type.map_expr run cast_expr) cl iface (p1 :: pl))
                | _ -> Type.map_expr run e
              )
            | _ -> Type.map_expr run e
      in
      run

    let configure gen traverse =
      gen.ghas_tparam_cast_handler <- true;
      let map e = Some(traverse e) in
      gen.gsyntax_filters#add ~name:name ~priority:(PCustom priority) map

    let default_config gen (dyn_tparam_cast:texpr->t->texpr) ifaces base_generic =
      configure gen (default_implementation gen dyn_tparam_cast ifaces);
      RealTypeParamsModf.configure gen (RealTypeParamsModf.default_implementation gen ifaces base_generic)

  end;;

  (* ******************************************* *)
  (* Rename Type Parameters *)
  (* ******************************************* *)

  (*

    This module should run after everything is already applied,
    it will look for possible type parameter name clashing and change the classes names to a

    dependencies:
      should run after everything is already applied. There's no configure on this module, only 'run'.

  *)

  module RenameTypeParameters =
  struct

    let name = "rename_type_parameters"

    let run gen =
      let i = ref 0 in
      let found_types = ref PMap.empty in
      let check_type name on_changed =
        let rec loop name =
          incr i;
          let changed_name = (name ^ (string_of_int !i)) in
          if PMap.mem changed_name !found_types then loop name else changed_name
        in
        if PMap.mem name !found_types then begin
          let new_name = loop name in
          found_types := PMap.add new_name true !found_types;
          on_changed new_name
        end else found_types := PMap.add name true !found_types
      in

      let get_cls t =
        match follow t with
          | TInst(cl,_) -> cl
          | _ -> assert false
      in

      let iter_types (_,t) =
        let cls = get_cls t in
        check_type (snd cls.cl_path) (fun name -> cls.cl_path <- (fst cls.cl_path, name))
      in

      List.iter (function
        | TClassDecl cl ->
          i := 0;

          found_types := PMap.empty;
          List.iter iter_types cl.cl_types;
          let cur_found_types = !found_types in
          List.iter (fun cf ->
            found_types := cur_found_types;
            List.iter iter_types cf.cf_params
          ) (cl.cl_ordered_fields @ cl.cl_ordered_statics)

        | TEnumDecl ( ({ e_types = hd :: tl }) ) ->
          i := 0;
          found_types := PMap.empty;
          List.iter iter_types (hd :: tl)

        | TAbstractDecl { a_types = hd :: tl } ->
          i := 0;
          found_types := PMap.empty;
          List.iter iter_types (hd :: tl)

        | _ -> ()

      ) gen.gcon.types

  end;;


  let configure gen (param_func_call:texpr->texpr->tparams->texpr list->texpr) =
    (*let map e = Some(mapping_func e) in
    gen.gexpr_filters#add ~name:name ~priority:(PCustom priority) map*)
    gen.gparam_func_call <- param_func_call

end;;

(* ******************************************* *)
(* Casts detection v2 *)
(* ******************************************* *)

(*

  Will detect implicit casts and add TCast for them. Since everything is already followed by follow_all, typedefs are considered a new type altogether

  Types shouldn't be cast if:
    * When an instance is being coerced to a superclass or to an implemented interface
    * When anything is being coerced to Dynamic

  edit:
    As a matter of performance, we will also run the type parameters casts in here. Otherwise the exact same computation would have to be performed twice,
    with maybe even some loss of information

    * TAnon / TDynamic will call
    * Type parameter handling will be abstracted

  dependencies:
    MUST be one of the first called filters, (right after FollowAll), as it needs the AST still mainly untouched
    This module depends physically on some methods declared on TypeParams. And it must be executed before TypeParams.

*)

module CastDetect =
struct

  let name = "cast_detect_2"

  let priority = solve_deps name [DBefore TypeParams.priority]

  let get_args t = match follow t with
    | TFun(args,ret) -> args,ret
    | _ -> trace (debug_type t); assert false

  let s_path (pack,n) = (String.concat "." (pack @ [n]))

  (*
    Since this function is applied under native-context only, the type paraters will already be changed
  *)
  let map_cls gen also_implements fn super =
    let rec loop c tl =
      if c == super then
        fn c tl
      else (match c.cl_super with
        | None -> false
        | Some (cs,tls) ->
          let tls = gen.greal_type_param (TClassDecl cs) tls in
          loop cs (List.map (apply_params c.cl_types tl) tls)
      ) || (if also_implements then List.exists (fun (cs,tls) ->
        loop cs (List.map (apply_params c.cl_types tl) tls)
      ) c.cl_implements else false)
    in
    loop

  let follow_dyn t = match follow t with
    | TMono _ | TLazy _ -> t_dynamic
    | t -> t

  (*
    this has a slight change from the type.ml version, in which it doesn't
    change a TMono into the other parameter
  *)
  let rec type_eq gen param a b =
    if a == b then
      ()
    else match follow_dyn (gen.greal_type a) , follow_dyn (gen.greal_type b) with
    | TEnum (e1,tl1) , TEnum (e2,tl2) ->
      if e1 != e2 && not (param = EqCoreType && e1.e_path = e2.e_path) then Type.error [cannot_unify a b];
      List.iter2 (type_eq gen param) tl1 tl2
    | TAbstract (a1,tl1) , TAbstract (a2,tl2) ->
      if a1 != a2 && not (param = EqCoreType && a1.a_path = a2.a_path) then Type.error [cannot_unify a b];
      List.iter2 (type_eq gen param) tl1 tl2
    | TInst (c1,tl1) , TInst (c2,tl2) ->
      if c1 != c2 && not (param = EqCoreType && c1.cl_path = c2.cl_path) && (match c1.cl_kind, c2.cl_kind with KExpr _, KExpr _ -> false | _ -> true) then Type.error [cannot_unify a b];
      List.iter2 (type_eq gen param) tl1 tl2
    | TFun (l1,r1) , TFun (l2,r2) when List.length l1 = List.length l2 ->
      (try
        type_eq gen param r1 r2;
        List.iter2 (fun (n,o1,t1) (_,o2,t2) ->
          if o1 <> o2 then Type.error [Not_matching_optional n];
          type_eq gen param t1 t2
        ) l1 l2
      with
        Unify_error l -> Type.error (cannot_unify a b :: l))
    | TDynamic a , TDynamic b ->
      type_eq gen param a b
    | TAnon a1, TAnon a2 ->
      (try
        PMap.iter (fun n f1 ->
          try
            let f2 = PMap.find n a2.a_fields in
            if f1.cf_kind <> f2.cf_kind && (param = EqStrict || param = EqCoreType || not (unify_kind f1.cf_kind f2.cf_kind)) then Type.error [invalid_kind n f1.cf_kind f2.cf_kind];
            try
              type_eq gen param f1.cf_type f2.cf_type
            with
              Unify_error l -> Type.error (invalid_field n :: l)
          with
            Not_found ->
              if is_closed a2 then Type.error [has_no_field b n];
              if not (link (ref None) b f1.cf_type) then Type.error [cannot_unify a b];
              a2.a_fields <- PMap.add n f1 a2.a_fields
        ) a1.a_fields;
        PMap.iter (fun n f2 ->
          if not (PMap.mem n a1.a_fields) then begin
            if is_closed a1 then Type.error [has_no_field a n];
            if not (link (ref None) a f2.cf_type) then Type.error [cannot_unify a b];
            a1.a_fields <- PMap.add n f2 a1.a_fields
          end;
        ) a2.a_fields;
      with
        Unify_error l -> Type.error (cannot_unify a b :: l))
    | _ , _ ->
      if b == t_dynamic && (param = EqRightDynamic || param = EqBothDynamic) then
        ()
      else if a == t_dynamic && param = EqBothDynamic then
        ()
      else
        Type.error [cannot_unify a b]

  let type_iseq gen a b =
    try
      type_eq gen EqStrict a b;
      true
    with
      Unify_error _ -> false

  (* Helpers for cast handling *)
  (* will return true if 'super' is a superclass of 'cl' or if cl implements super or if they are the same class *)
  let can_be_converted gen cl tl super_t super_tl =
    map_cls gen (gen.guse_tp_constraints || (match cl.cl_kind,super_t.cl_kind with KTypeParameter _, _ | _,KTypeParameter _ -> false | _ -> true)) (fun _ tl ->
      try
        List.iter2 (type_eq gen (if gen.gallow_tp_dynamic_conversion then EqRightDynamic else EqStrict)) tl super_tl;
        true
      with | Unify_error _ -> false
    ) super_t cl tl

  (* will return true if both arguments are compatible. If it's not the case, a runtime error is very likely *)
  let is_cl_related gen cl tl super superl =
    let is_cl_related cl tl super superl = map_cls gen (gen.guse_tp_constraints || (match cl.cl_kind,super.cl_kind with KTypeParameter _, _ | _,KTypeParameter _ -> false | _ -> true)) (fun _ _ -> true) super cl tl in
    is_cl_related cl tl super superl || is_cl_related super superl cl tl


  let rec is_unsafe_cast gen to_t from_t =
    match (follow to_t, follow from_t) with
      | TInst(cl_to, to_params), TInst(cl_from, from_params) ->
        not (is_cl_related gen cl_from from_params cl_to to_params)
      | TEnum(e_to, _), TEnum(e_from, _) ->
        e_to.e_path <> e_from.e_path
      | TFun _, TFun _ ->
        (* functions are never unsafe cast by default. This behavior might be changed *)
        (* with a later AST pass which will run through TFun to TFun casts *)
        false
      | TMono _, _
      | _, TMono _
      | TDynamic _, _
      | _, TDynamic _ ->
        false
      | TAnon _, _
      | _, TAnon _ ->
        (* anonymous are never unsafe also. *)
        (* Though they will generate a cast, so if this cast is unneeded it's better to avoid them by tweaking gen.greal_type *)
        false
      | TAbstract _, _
      | _, TAbstract _ ->
        (try
          unify from_t to_t;
          false
        with | Unify_error _ ->
          try
            unify to_t from_t; (* still not unsafe *)
            false
          with | Unify_error _ ->
            true)
      | _ -> true

  let do_unsafe_cast gen from_t to_t e  =
    let t_path t =
      match t with
        | TInst(cl, _) -> cl.cl_path
        | TEnum(e, _) -> e.e_path
        | TType(t, _) -> t.t_path
        | TAbstract(a, _) -> a.a_path
        | TDynamic _ -> ([], "Dynamic")
        | _ -> raise Not_found
    in
    let do_default () =
      gen.gon_unsafe_cast to_t e.etype e.epos;
      mk_cast to_t (mk_cast t_dynamic e)
    in
    (* TODO: there really should be a better way to write that *)
    try
      if (Hashtbl.find gen.gsupported_conversions (t_path from_t)) from_t to_t then
        mk_cast to_t e
      else
        do_default()
    with
      | Not_found ->
        try
          if (Hashtbl.find gen.gsupported_conversions (t_path to_t)) from_t to_t then
            mk_cast to_t e
          else
            do_default()
        with
          | Not_found -> do_default()

  (* ****************************** *)
  (* cast handler *)
  (* decides if a cast should be emitted, given a from and a to type *)
  (*
    this function is like a mini unify, without e.g. subtyping, which makes sense
    at the backend level, since most probably Anons and TInst will have a different representation there
  *)
  let rec handle_cast gen e real_to_t real_from_t =
    let do_unsafe_cast () = do_unsafe_cast gen real_from_t real_to_t { e with etype = real_from_t } in
    let to_t, from_t = real_to_t, real_from_t in

    let mk_cast t e =
      match e.eexpr with
        (* TThrow is always typed as Dynamic, we just need to type it accordingly *)
        | TThrow _ -> { e with etype = t }
        | _ -> mk_cast t e
    in

    let e = { e with etype = real_from_t } in
    if try fast_eq real_to_t real_from_t with Invalid_argument("List.for_all2") -> false then e else
    match real_to_t, real_from_t with
      (* string is the only type that can be implicitly converted from any other *)
      | TInst( { cl_path = ([], "String") }, []), _ ->
        mk_cast to_t e
      | TInst(cl_to, params_to), TInst(cl_from, params_from) ->
        let ret = ref None in
        (*
          this is a little confusing:
          we are here mapping classes until we have the same to and from classes, applying the type parameters in each step, so we can
          compare the type parameters;

          If a class is found - meaning that the cl_from can be converted without a cast into cl_to,
          we still need to check their type parameters.
        *)
        ignore (map_cls gen (gen.guse_tp_constraints || (match cl_from.cl_kind,cl_to.cl_kind with KTypeParameter _, _ | _,KTypeParameter _ -> false | _ -> true)) (fun _ tl ->
          try
            (* type found, checking type parameters *)
            List.iter2 (type_eq gen EqStrict) tl params_to;
            ret := Some e;
            true
          with | Unify_error _ ->
            (* type parameters need casting *)
            if gen.ghas_tparam_cast_handler then begin
              (*
                if we are already handling type parameter casts on other part of code (e.g. RealTypeParameters),
                we'll just make a cast to indicate that this place needs type parameter-involved casting
              *)
              ret := Some (mk_cast to_t e);
              true
            end else
              (*
                if not, we're going to check if we only need a simple cast,
                or if we need to first cast into the dynamic version of it
              *)
              try
                List.iter2 (type_eq gen EqRightDynamic) tl params_to;
                ret := Some (mk_cast to_t e);
                true
              with | Unify_error _ ->
                ret := Some (mk_cast to_t (mk_cast (TInst(cl_to, List.map (fun _ -> t_dynamic) params_to)) e));
                true
        ) cl_to cl_from params_from);
        if is_some !ret then
          get !ret
        else if is_cl_related gen cl_from params_from cl_to params_to then
          mk_cast to_t e
        else
          (* potential unsafe cast *)
          (do_unsafe_cast ())
      | TMono _, TMono _
      | TMono _, TDynamic _
      | TDynamic _, TDynamic _
      | TDynamic _, TMono _ ->
        e
      | TMono _, _
      | TDynamic _, _
      | TAnon _, _ when gen.gneeds_box real_from_t ->
        mk_cast to_t e
      | TMono _, _
      | TDynamic _, _ -> e
      | _, TMono _
      | _, TDynamic _ -> mk_cast to_t e
      | TAbstract (a_to, _), TAbstract(a_from, _) when a_to == a_from ->
        e
      | TAbstract _, _
      | _, TAbstract _ ->
        (try
          unify from_t to_t;
          mk_cast to_t e
        with | Unify_error _ ->
          try
            unify to_t from_t;
            mk_cast to_t e
          with | Unify_error _ ->
            do_unsafe_cast())
      | TEnum(e_to, []), TEnum(e_from, []) ->
        if e_to == e_from then
          e
        else
          (* potential unsafe cast *)
          (do_unsafe_cast ())
      | TEnum(e_to, params_to), TEnum(e_from, params_from) when e_to.e_path = e_from.e_path ->
        (try
            List.iter2 (type_eq gen (if gen.gallow_tp_dynamic_conversion then EqRightDynamic else EqStrict)) params_from params_to;
            e
          with
            | Unify_error _ -> do_unsafe_cast ()
        )
      | TEnum(en, params_to), TInst(cl, params_from)
      | TInst(cl, params_to), TEnum(en, params_from) ->
        (* this is here for max compatibility with EnumsToClass module *)
        if en.e_path = cl.cl_path && en.e_extern then begin
          (try
            List.iter2 (type_eq gen (if gen.gallow_tp_dynamic_conversion then EqRightDynamic else EqStrict)) params_from params_to;
            e
          with
            | Invalid_argument("List.iter2") ->
              (*
                this is a hack for RealTypeParams. Since there is no way at this stage to know if the class is the actual
                EnumsToClass derived from the enum, we need to imply from possible ArgumentErrors (because of RealTypeParams interfaces),
                that they would only happen if they were a RealTypeParams created interface
              *)
              e
            | Unify_error _ -> do_unsafe_cast ()
          )
        end else
          do_unsafe_cast ()
      | TType(t_to, params_to), TType(t_from, params_from) when t_to == t_from ->
        if gen.gspecial_needs_cast real_to_t real_from_t then
          (try
            List.iter2 (type_eq gen (if gen.gallow_tp_dynamic_conversion then EqRightDynamic else EqStrict)) params_from params_to;
            e
          with
            | Unify_error _ -> do_unsafe_cast ()
          )
        else
          e
      | TType(t_to, _), TType(t_from,_) ->
        if gen.gspecial_needs_cast real_to_t real_from_t then
          mk_cast to_t e
        else
          e
      | TType _, _ when gen.gspecial_needs_cast real_to_t real_from_t ->
        mk_cast to_t e
      | _, TType _ when gen.gspecial_needs_cast real_to_t real_from_t ->
        mk_cast to_t e
      (*| TType(t_to, _), TType(t_from, _) ->
        if t_to.t_path = t_from.t_path then
          e
        else if is_unsafe_cast gen real_to_t real_from_t then (* is_unsafe_cast will already follow both *)
          (do_unsafe_cast ())
        else
          mk_cast to_t e*)
      | TType _, _
      | _, TType _ ->
        if is_unsafe_cast gen real_to_t real_from_t then (* is_unsafe_cast will already follow both *)
          (do_unsafe_cast ())
        else
          mk_cast to_t e
      | TAnon (a_to), TAnon (a_from) ->
        if a_to == a_from then
          e
        else if type_iseq gen to_t from_t then (* FIXME apply unify correctly *)
          e
        else
          mk_cast to_t e
      | TAnon anon, _ ->
        if PMap.is_empty anon.a_fields then
          e
        else
          mk_cast to_t e
      | _, TAnon _ ->
        mk_cast to_t e
      | TFun(args, ret), TFun(args2, ret2) ->
        let get_args = List.map (fun (_,_,t) -> t) in
        (try List.iter2 (type_eq gen (EqBothDynamic)) (ret :: get_args args) (ret2 :: get_args args2); e with | Unify_error _ | Invalid_argument("List.iter2") -> mk_cast to_t e)
      | _, _ ->
        do_unsafe_cast ()

  (* end of cast handler *)
  (* ******************* *)

  (*

    Type parameter handling
    It will detect if/what type parameters were used, and call the cast handler
    It will handle both TCall(TField) and TCall by receiving a texpr option field: e
    Also it will transform the type parameters with greal_type_param and make

    handle_impossible_tparam - should cases where the type parameter is impossible to be determined from the called parameters be Dynamic?
    e.g. static function test<T>():T {}
  *)

  (* match e.eexpr with | TCall( ({ eexpr = TField(ef, f) }) as e1, elist ) -> *)
  let handle_type_parameter gen e e1 ef f elist impossible_tparam_is_dynamic =
    (* the ONLY way to know if this call has parameters is to analyze the calling field. *)
    (* To make matters a little worse, on both C# and Java only in some special cases that type parameters will be used *)
    (* Namely, when using reflection type parameters are useless, of course. This also includes anonymous types *)
    (* this will have to be handled by gparam_func_call *)

    let return_var efield =
      match e with
        | None ->
          efield
        | Some ecall ->
          match follow efield.etype with
            | TFun(_,ret) ->
              (* closures will be handled by the closure handler. So we will just hint what's the expected type *)
              (* FIXME: should closures have also its arguments cast correctly? In the current implementation I think not. TO_REVIEW *)
              handle_cast gen { ecall with eexpr = TCall(efield, elist) } (gen.greal_type ecall.etype) ret
            | _ ->
              { ecall with eexpr = TCall(efield, elist) }
    in

    let real_type = gen.greal_type ef.etype in
    (match field_access gen real_type (field_name f) with
      | FClassField (cl, params, _, cf, is_static, actual_t) ->
        (match cf.cf_kind with
          | Method MethDynamic | Var _ ->
            (* if it's a var, we will just try to apply the class parameters that have been changed with greal_type_param *)
            let t = apply_params cl.cl_types (gen.greal_type_param (TClassDecl cl) params) (gen.greal_type actual_t) in
            return_var (handle_cast gen { e1 with eexpr = TField(ef, f) } (gen.greal_type e1.etype) (gen.greal_type t))
          | _ when e = None && (try PMap.find cf.cf_name gen.gbase_class_fields == cf with Not_found -> false) ->
            return_var (handle_cast gen { e1 with eexpr = TField({ ef with etype = t_dynamic }, f) } e1.etype t_dynamic) (* force dynamic and cast back to needed type *)
          | _ ->
            let ecall = match e with | None -> trace (field_name f); trace cf.cf_name; gen.gcon.error "This field should be called immediately" ef.epos; assert false | Some ecall -> ecall in
            match cf.cf_params with
              | [] when cf.cf_overloads <> [] ->
                let args, ret = get_args e1.etype in
                let args, ret = List.map (fun (n,o,t) -> (n,o,gen.greal_type t)) args, gen.greal_type ret in
                (try
                  handle_cast gen
                  { ecall with
                    eexpr = TCall(
                      { e1 with eexpr = TField(ef, f) },
                      List.map2 (fun param (_,_,t) ->
                        match param.eexpr with
                          | TConst TNull -> (* when we have overloads and null const, we must force a cast otherwise we may get ambiguous call errors *)
                            mk_cast (gen.greal_type t) param
                          | _ ->
                            handle_cast gen param (gen.greal_type t) (gen.greal_type param.etype)) elist args
                    )
                  } (gen.greal_type ecall.etype) (gen.greal_type ret)
                with | Invalid_argument("List.map2") ->
                  gen.gcon.warning "This expression may be invalid" ecall.epos;
                  handle_cast gen ({ ecall with eexpr = TCall({ e1 with eexpr = TField(ef, f) }, elist )  }) (gen.greal_type ecall.etype) (gen.greal_type ret)
                )
              | _ when cf.cf_overloads <> [] ->
                (* this case still needs Issue #915 to be solved, so we will just ignore the need to cast any parameter by now *)
                (* TODO issue was solved, solve this issue ASAP *)
                mk_cast ecall.etype { ecall with eexpr = TCall({ e1 with eexpr = TField(ef, f) }, elist ) }
              | [] ->
                let args, ret = get_args actual_t in
                let actual_t = TFun(List.map (fun (n,o,t) -> (n,o,gen.greal_type t)) args, gen.greal_type ret) in
                let t = apply_params cl.cl_types (gen.greal_type_param (TClassDecl cl) params) actual_t in
                let args, ret = get_args t in
                (try
                  handle_cast gen { ecall with eexpr = TCall({ e1 with eexpr = TField(ef, f) }, List.map2 (fun param (_,_,t) -> handle_cast gen param (gen.greal_type t) (gen.greal_type param.etype)) elist args) } (gen.greal_type ecall.etype) (gen.greal_type ret)
                with | Invalid_argument("List.map2") ->
                  gen.gcon.warning "This expression may be invalid" ecall.epos;
                  handle_cast gen ({ ecall with eexpr = TCall({ e1 with eexpr = TField(ef, f) }, elist )  }) (gen.greal_type ecall.etype) (gen.greal_type ret)
                )
              | _ ->
                let _params = TypeParams.infer_params gen ecall.epos (get_fun (apply_params cl.cl_types params cf.cf_type)) (get_fun e1.etype) cf.cf_params impossible_tparam_is_dynamic in
                let args, ret = get_args actual_t in
                let actual_t = TFun(List.map (fun (n,o,t) -> (n,o,gen.greal_type t)) args, gen.greal_type ret) in

                (*
                  because of differences on how <Dynamic> is handled on the platforms, this is a hack to be able to
                  correctly use class field type parameters with RealTypeParams
                *)
                let cf_params = List.map (fun t -> match follow t with | TDynamic _ -> t_empty | _ -> t) _params in
                let t = apply_params cl.cl_types (gen.greal_type_param (TClassDecl cl) params) actual_t in
                let t = apply_params cf.cf_params (gen.greal_type_param (TClassDecl cl) cf_params) t in

                let args, ret = get_args t in

                let elist = List.map2 (fun param (_,_,t) -> handle_cast gen (param) (gen.greal_type t) (gen.greal_type param.etype)) elist args in
                let e1 = { e1 with eexpr = TField(ef, f) } in
                let new_ecall = gen.gparam_func_call ecall e1 _params elist in

                handle_cast gen new_ecall (gen.greal_type ecall.etype) (gen.greal_type ret)
        )
      | FEnumField (en, efield, true) ->
        let ecall = match e with | None -> trace (field_name f); trace efield.ef_name; gen.gcon.error "This field should be called immediately" ef.epos; assert false | Some ecall -> ecall in
        (match en.e_types with
          (*
          | [] ->
            let args, ret = get_args (efield.ef_type) in
            let ef = { ef with eexpr = TTypeExpr( TEnumDecl en ); etype = TEnum(en, []) } in
            handle_cast gen { ecall with eexpr = TCall({ e1 with eexpr = TField(ef, FEnum(en, efield)) }, List.map2 (fun param (_,_,t) -> handle_cast gen param (gen.greal_type t) (gen.greal_type param.etype)) elist args) } (gen.greal_type ecall.etype) (gen.greal_type ret)
        *)
          | _ ->
            let pt = match e with | None -> real_type | Some _ -> snd (get_fun e1.etype) in
            let _params = match follow pt with | TEnum(_, p) -> p | _ -> gen.gcon.warning (debug_expr e1) e1.epos; assert false in
            let args, ret = get_args efield.ef_type in
            let actual_t = TFun(List.map (fun (n,o,t) -> (n,o,gen.greal_type t)) args, gen.greal_type ret) in
            (*
              because of differences on how <Dynamic> is handled on the platforms, this is a hack to be able to
              correctly use class field type parameters with RealTypeParams
            *)
            let cf_params = List.map (fun t -> match follow t with | TDynamic _ -> t_empty | _ -> t) _params in
            (* params are inverted *)
            let cf_params = List.rev cf_params in
            let t = apply_params en.e_types (gen.greal_type_param (TEnumDecl en) cf_params) actual_t in
            let t = apply_params efield.ef_params (List.map (fun _ -> t_dynamic) efield.ef_params) t in

            let args, ret = get_args t in

            let elist = List.map2 (fun param (_,_,t) -> handle_cast gen (param) (gen.greal_type t) (gen.greal_type param.etype)) elist args in
            let e1 = { e1 with eexpr = TField({ ef with eexpr = TTypeExpr( TEnumDecl en ); etype = TEnum(en, _params) }, FEnum(en, efield) ) } in
            let new_ecall = gen.gparam_func_call ecall e1 _params elist in

            handle_cast gen new_ecall (gen.greal_type ecall.etype) (gen.greal_type ret)
        )
      | FEnumField _ when is_some e -> assert false
      | FEnumField (en,efield,_) ->
          return_var { e1 with eexpr = TField({ ef with eexpr = TTypeExpr( TEnumDecl en ); },FEnum(en,efield)) }
      (* no target by date will uses this.so this code may not be correct at all *)
      | FAnonField cf ->
        let t = gen.greal_type cf.cf_type in
        return_var (handle_cast gen { e1 with eexpr = TField(ef, f) } (gen.greal_type e1.etype) t)
      | FNotFound
      | FDynamicField _ ->
        if is_some e then
          return_var { e1 with eexpr = TField(ef, f) }
        else
          return_var (handle_cast gen { e1 with eexpr = TField({ ef with etype = t_dynamic }, f) } e1.etype t_dynamic) (* force dynamic and cast back to needed type *)
    )

  (* end of type parameter handling *)
  (* ****************************** *)

  let default_implementation gen ?(native_string_cast = true) maybe_empty_t impossible_tparam_is_dynamic =

    let current_ret_type = ref None in

    let handle e t1 t2 = handle_cast gen e (gen.greal_type t1) (gen.greal_type t2) in

    let in_value = ref false in

    let rec get_ctor_p cl p =
      match cl.cl_constructor with
        | Some c -> follow (apply_params cl.cl_types p c.cf_type), cl, p
        | None -> match cl.cl_super with
          | Some (cls,tl) ->
            get_ctor_p cls (List.map (apply_params cls.cl_types p) tl)
          | None -> TFun([],gen.gcon.basic.tvoid), cl, p
    in

    let get_f t =
      match follow t with | TFun(p,_) -> List.map (fun (_,_,t) -> t) p | _ -> assert false
    in

    let rec run ?(just_type = false) e =
      let handle = if not just_type then handle else fun e t1 t2 -> { e with etype = gen.greal_type t2 } in
      let was_in_value = !in_value in
      in_value := true;
      match e.eexpr with
        | TBinop ( (Ast.OpAssign as op),({ eexpr = TField(tf, f) } as e1), e2 )
        | TBinop ( (Ast.OpAssignOp _ as op),({ eexpr = TField(tf, f) } as e1), e2 ) ->
          (match field_access gen (gen.greal_type tf.etype) (field_name f) with
            | FClassField(cl,params,_,_,is_static,actual_t) ->
              let actual_t = if is_static then actual_t else apply_params cl.cl_types params actual_t in

              let e1 = run e1 ~just_type:true in
              { e with eexpr = TBinop(op, e1, handle (run e2) actual_t e2.etype); etype = e1.etype }
            | _ ->
              let e1 = run e1 ~just_type:true in
              { e with eexpr = TBinop(op, e1, handle (run e2) e1.etype e2.etype); etype = e1.etype }
          )
        | TBinop ( (Ast.OpAssign as op),e1,e2)
        | TBinop ( (Ast.OpAssignOp _ as op),e1,e2) ->
          let e1 = run e1 ~just_type:true in
          { e with eexpr = TBinop(op, e1, handle (run e2) e1.etype e2.etype); etype = e1.etype }
        (* this is an exception so we can avoid infinite loop on Std.String and haxe.lang.Runtime.toString(). It also takes off unnecessary casts to string *)
        | TBinop ( Ast.OpAdd, ( { eexpr = TCast(e1, _) } as e1c), e2 ) when native_string_cast && is_string e1c.etype && is_string e2.etype ->
          { e with eexpr = TBinop( Ast.OpAdd, run e1, run e2 ) }
        | TField(ef, f) ->
          handle_type_parameter gen None e (run ef) f [] impossible_tparam_is_dynamic
        | TArrayDecl el ->
          let et = e.etype in
          let base_type = match follow et with
            | TInst({ cl_path = ([], "Array") } as cl, bt) -> gen.greal_type_param (TClassDecl cl) bt
            | _ -> assert false
          in
          let base_type = List.hd base_type in
          { e with eexpr = TArrayDecl( List.map (fun e -> handle (run e) base_type e.etype) el ); etype = et }
        | TCall( ({ eexpr = TField({ eexpr = TLocal(v) },_) } as tf), params ) when String.get v.v_name 0 = '_' &&String.get v.v_name 1 = '_' && Hashtbl.mem gen.gspecial_vars v.v_name ->
          { e with eexpr = TCall(tf, List.map run params) }
        | TCall( ({ eexpr = TLocal v } as local), params ) when String.get v.v_name 0 = '_' && String.get v.v_name 1 = '_' && Hashtbl.mem gen.gspecial_vars v.v_name ->
          { e with eexpr = TCall(local, List.map run params) }
        | TCall( ({ eexpr = TField(ef, f) }) as e1, elist ) ->
          handle_type_parameter gen (Some e) (e1) (run ef) f (List.map run elist) impossible_tparam_is_dynamic

        | TCall( { eexpr = TConst TSuper } as ef, [ maybe_empty ]) when is_some maybe_empty_t && type_iseq gen (get maybe_empty_t) maybe_empty.etype ->
          { e with eexpr = TCall(ef, [ run maybe_empty ]); }
        | TCall( { eexpr = TConst TSuper } as ef, eparams ) ->
          (* handle special distinction between EmptyConstructor vs one argument contructor *)
          let handle = if (List.length eparams = 1) then
            (fun e t1 t2 -> mk_cast (gen.greal_type t1) e)
          else
            handle
          in
          let cl,tparams = match follow ef.etype with | TInst(c,p) -> c,p | _ -> assert false in
          let t, c, p = get_ctor_p cl tparams in
          let called_t = TFun(List.map (fun e -> "arg",false,e.etype) eparams, gen.gcon.basic.tvoid) in
          (match c.cl_constructor with
          | None ->
            { e with eexpr = TCall(ef, List.map run eparams); }
          | Some cf when cf.cf_overloads <> [] ->
            (try
              (* TODO use the same sorting algorithm as in typer *)
              let cf = List.find (fun cf -> try unify cf.cf_type called_t; true with | Unify_error _ -> false) (cf :: cf.cf_overloads) in
              let t = apply_params c.cl_types p cf.cf_type in
              { e with eexpr = TCall(ef, List.map2 (fun e t -> handle (run e) t e.etype) eparams (get_f t)); }
            with | Not_found ->
              { e with eexpr = TCall(ef, List.map run eparams); })
          | _ ->
            { e with eexpr = TCall(ef, List.map2 (fun e t -> handle (run e) t e.etype) eparams (get_f t)); }
          )
        | TCall (ef, eparams) ->
          (match ef.etype with
            | TFun(p, ret) ->
              handle ({ e with eexpr = TCall(run ef, List.map2 (fun param (_,_,t) -> handle (run param) t param.etype) eparams p) }) e.etype ret
            | _ -> Type.map_expr run e
          )
        | TNew (cl, tparams, [ maybe_empty ]) when is_some maybe_empty_t && type_iseq gen (get maybe_empty_t) maybe_empty.etype ->
          { e with eexpr = TNew(cl, tparams, [ maybe_empty ]); etype = TInst(cl, tparams) }
        | TNew (cl, tparams, eparams) ->
          (* handle special distinction between EmptyConstructor vs one argument contructor *)
          let handle = if (List.length eparams = 1) then
            (fun e t1 t2 -> mk_cast (gen.greal_type t1) e)
          else
            handle
          in
          (* choose best overload *)
          let t, c, p = get_ctor_p cl tparams in
          let called_t = TFun(List.map (fun e -> "arg",false,e.etype) eparams, gen.gcon.basic.tvoid) in
          (match c.cl_constructor with
          | None ->
            { e with eexpr = TNew(cl, tparams, List.map run eparams); etype = TInst(cl, tparams) }
          | Some cf when cf.cf_overloads <> [] ->
            (try
              (* TODO use the same sorting algorithm as in typer *)
              let cf = List.find (fun cf -> try unify cf.cf_type called_t; true with | Unify_error _ -> false) (cf :: cf.cf_overloads) in
              let t = apply_params c.cl_types p cf.cf_type in
              { e with eexpr = TNew(cl, tparams, List.map2 (fun e t -> handle (run e) t e.etype) eparams (get_f t)) }
            with | Not_found ->
              { e with eexpr = TNew(cl, tparams, List.map run eparams); etype = TInst(cl, tparams) })
          | _ ->
            { e with eexpr = TNew(cl, tparams, List.map2 (fun e t -> handle (run e) t e.etype) eparams (get_f t)) }
          )
        | TArray(arr, idx) ->
          let arr_etype = match follow arr.etype with
          | (TInst _ as t) -> t
          | TAbstract ({ a_impl = Some _ } as a, pl) ->
            follow (Codegen.Abstract.get_underlying_type a pl)
          | t -> t in
          (* get underlying class (if it's a class *)
          (match arr_etype with
            | TInst(cl, params) ->
              (* see if it implements ArrayAccess *)
              (match cl.cl_array_access with
                | None -> Type.map_expr run e (*FIXME make it loop through all super types *)
                | Some t ->
                  (* if it does, apply current parameters (and change them) *)
                  (* let real_t = apply_params_internal (List.map (gen.greal_type_param (TClassDecl cl))) cl params t in *)
                  let param = apply_params cl.cl_types (gen.greal_type_param (TClassDecl cl) params) t in
                  let real_t = apply_params cl.cl_types params param in
                  (* see if it needs a cast *)

                  handle (Type.map_expr run e) (gen.greal_type e.etype) (gen.greal_type real_t)
              )
            | _ -> Type.map_expr run e)
        | TVars (veopt_l) ->
          { e with eexpr = TVars (List.map (fun (v,eopt) ->
            match eopt with
              | None -> (v,eopt)
              | Some e ->
                (v, Some( handle (run e) v.v_type e.etype ))
          ) veopt_l) }
        (* FIXME deal with in_value when using other statements that may not have a TBlock wrapped on them *)
        | TIf (econd, ethen, Some(eelse)) when was_in_value ->
          { e with eexpr = TIf (handle (run econd) gen.gcon.basic.tbool econd.etype, handle (run ethen) e.etype ethen.etype, Some( handle (run eelse) e.etype eelse.etype ) ) }
        | TIf (econd, ethen, eelse) ->
          { e with eexpr = TIf (handle (run econd) gen.gcon.basic.tbool econd.etype, run (mk_block ethen), Option.map (fun e -> run (mk_block e)) eelse) }
        | TWhile (econd, e1, flag) ->
          { e with eexpr = TWhile (handle (run econd) gen.gcon.basic.tbool econd.etype, run (mk_block e1), flag) }
        | TSwitch (cond, el_e_l, edef) ->
          { e with eexpr = TSwitch(run cond, List.map (fun (el,e) -> (List.map run el, run (mk_block e))) el_e_l, Option.map (fun e -> run (mk_block e)) edef) }
        | TMatch (cond, en, il_vl_e_l, edef) ->
          { e with eexpr = TMatch(run cond, en, List.map (fun (il, vl, e) -> (il, vl, run (mk_block e))) il_vl_e_l, Option.map (fun e -> run (mk_block e)) edef) }
        | TFor (v,cond,e1) ->
          { e with eexpr = TFor(v, run cond, run (mk_block e1)) }
        | TTry (e, ve_l) ->
          { e with eexpr = TTry(run (mk_block e), List.map (fun (v,e) -> (v, run (mk_block e))) ve_l) }
        | TReturn (eopt) ->
          (* a return must be inside a function *)
          let ret_type = match !current_ret_type with | Some(s) -> s | None -> gen.gcon.error "Invalid return outside function declaration." e.epos; assert false in
          (match eopt with
            | None -> e
            | Some eret ->
              { e with eexpr = TReturn( Some(handle (run eret) ret_type eret.etype ) ) }
          )
        | TBlock el ->
          { e with eexpr = TBlock ( List.map (fun e -> in_value := false; run e) el ) }
        | TFunction(tfunc) ->
          (match follow e.etype with
            | TFun(_,ret) ->
              let last_ret = !current_ret_type in
              current_ret_type := Some(ret);
              let ret = Type.map_expr run e in
              current_ret_type := last_ret;
              ret
            | _ -> trace (debug_type (follow e.etype)); trace (debug_expr e); gen.gcon.error "assert false" e.epos; assert false
          )
        | TCast (expr, md) when is_void (follow e.etype) ->
          run expr
        | TCast (expr, md) ->
          let rec get_null e =
            match e.eexpr with
            | TConst TNull -> Some e
            | TParenthesis e -> get_null e
            | _ -> None
          in
          (match get_null expr with
          | Some enull -> { enull with etype = e.etype }
          | _ ->
            let last_unsafe = gen.gon_unsafe_cast in
            gen.gon_unsafe_cast <- (fun t t2 pos -> ());
            let ret = handle (run expr) e.etype expr.etype in
            gen.gon_unsafe_cast <- last_unsafe;
            match ret.eexpr with
              | TCast _ -> ret
              | _ -> { e with eexpr = TCast(ret,md); etype = gen.greal_type e.etype }
          )
        (*| TCast _ ->
          (* if there is already a cast, we should skip this cast check *)
          Type.map_expr run e*)
        | _ -> Type.map_expr run e
    in
    run

  let configure gen (mapping_func:texpr->texpr) =
    gen.ghandle_cast <- (fun tto tfrom expr -> handle_cast gen expr (gen.greal_type tto) (gen.greal_type tfrom));
    let map e = Some(mapping_func e) in
    gen.gsyntax_filters#add ~name:name ~priority:(PCustom priority) map

end;;

(**************************************************************************************************************************)
(*                                                   SYNTAX FILTERS                                                       *)
(**************************************************************************************************************************)

(* ******************************************* *)
(* Expression Unwrap *)
(* ******************************************* *)

(*

  This is the most important module for source-code based targets. It will follow a convention of what's an expression and what's a statement,
  and will unwrap statements where expressions are expected, and vice-versa.

  It should be one of the first syntax filters to be applied. As a consequence, it's applied after all filters that add code to the AST, and by being
  the first of the syntax filters, it will also have the AST retain most of the meaning of normal HaXe code. So it's easier to detect cases which are
  side-effects free, for example

  Any target can make use of this, but there is one requirement: The target must accept null to be set to any kind of variable. For example,
  var i:Int = null; must be accepted. The best way to deal with this is to (like it's done in C#) make null equal to "default(Type)"

  dependencies:
    While it's best for Expression Unwrap to delay its execution as much as possible, since theoretically any
    filter can return an expression that needs to be unwrapped, it is also desirable for ExpresionUnwrap to have
    the AST as close as possible as HaXe's, so it can make some correct predictions (for example, so it can
    more accurately know what can be side-effects-free and what can't).
    This way, it will run slightly after the Normal priority, so if you don't say that a syntax filter must run
    before Expression Unwrap, it will run after it.

  TODO : While statement must become do / while, with the actual block inside an if for the condition, and else for 'break'
*)

module ExpressionUnwrap =
struct

  let name = "expression_unwrap"

  (* priority: first syntax filter *)
  let priority = (-10.0)


  (*
    We always need to rely on Blocks to be able to unwrap expressions correctly.
    So the the standard traverse will always be based on blocks.
    Normal block statements, like for(), while(), if(), ... will be mk_block'ed so there is always a block inside of them.

      At the block level, we'll define an "add_statement" function, which will allow the current expression to
      add statements to the block. This statement may or may not contain statements as expressions, so the texpr will be evaluated recursively before being added.

      - traverse will always evaluate TBlocks
      - for each texpr in a TBlock list,
        check shallow type
          if type is Statement or Both when it has problematic expression (var problematic_expr = count_problematic_expressions),
            if we can eagerly call unwrap_statement on the whole expression (try_call_unwrap_statement), use the return expression
            else
              check expr_type of each underlying type (with expr_stat_map)
                if it has ExprWithStatement or Statement,
                  call problematic_expression_unwrap in it
                  problematic_expr--
                else if problematic_expr == 0, just add the unchanged expression
                else if NoSideEffects and doesn't have short-circuit, just add the unchanged expression
                else call problematic_expression_unwrap in it
          if type is Expression, check if there are statements or Both inside.
            if there are, problematic_expression_unwrap in it
          aftewards, use on_expr_as_statement to get it

    helpers:
      try_call_unwrap_statement: (returns texpr option)
        if underlying statement is TBinop(OpAssign/OpAssignOp), or TVars, with the right side being a Statement or a short circuit op, we can call apply_assign.

      apply_assign:
        if is TVar, first declare the tvar with default expression = null;
        will receive the left and right side of the assignment; right-side must be Statement
        see if right side is a short-circuit operation, call short_circuit_op_unwrap
        else see eexpr of the right side
          if it's void, just add the statement with add_statement, and set the right side as null;
          if not, it will have a block inside. set the left side = to the last expression on each block inside. add_statement for it.

      short_circuit_op_unwrap: x() && (1 + {var x = 0; x + 1;} == 2) && z()
        -> var x = x();
           var y = false;
           var z = false;
           if (x) //for &&, neg for ||
           {
            var temp = null;
            {
              var x = 0;
              temp = x + 1;
            }

            y = (1 + temp) == 2;
            if (y)
            {
              z = z();
            }
           }
        expects to receive a texpr with TBinop(OpBoolAnd/OpBoolOr)
        will traverse the AST while there is a TBinop(OpBoolAnd/OpBoolOr) as a right-side expr, and declare new temp vars in the  for each found.
        will collect the return value, a mapped expr with all exprs as TLocal of the temp vars created


      problematic_expression_unwrap:
        check expr_kind:
          if it is NoSideEffects and not short-circuit, leave it there
          if it is ExprWithStatement and not short-circuit, call Type.map_expr problematic_expression_unwrap
          if it is Statement or Expression or short-circuit expr, call add_assign for this expression

      add_assign:
        see if the type is void. If it is, just add_statement the expression argument, and return a null value
        else create a new variable, set TVars with Some() with the expression argument, add TVar with add_statement, and return the TLocal of this expression.

      map_problematic_expr:
        call expr_stat_map on statement with problematic_expression_unwrap

    types:
      type shallow_expr_type = | Statement | Expression | Both (* shallow expression classification. Both means that they can be either Statements as Expressions *)

      type expr_kind = | NormalExpr | ExprNoSideEffects (* -> short-circuit is considered side-effects *) | ExprWithStatement | Statement
        evaluates an expression (as in not a statement) type. If it is ExprWithStatement or Statement, it means it contains errors

    functions:
      shallow_expr_type (expr:texpr) : shallow_expr_type

      expr_kind (expr:texpr) : expr_kind
        deeply evaluates an expression type

      expr_stat_map (fn:texpr->texpr) (expr:texpr) : texpr
        it will traverse the AST looking for places where an expression is expected, and map the value according to fn

      aggregate_expr_type (is_side_effects_free:bool) (children:expr_type list) : expr_type
        helper function to deal with expr_type aggregation (e.g. an Expression + a Statement as a children, is a ExprWithStatement)

      check_statement_in_expression (expr:texpr) : texpr option :
        will check

  *)

  type shallow_expr_type = | Statement | Expression of texpr | Both of texpr (* shallow expression classification. Both means that they can be either Statements as Expressions *)

  type expr_kind = | KNormalExpr | KNoSideEffects (* -> short-circuit is considered side-effects *) | KExprWithStatement | KStatement

  let rec no_paren e =
    match e.eexpr with
      | TParenthesis e -> no_paren e
      | _ -> e

  (* must be called in a statement. Will execute fn whenever an expression (not statement) is expected *)
  let expr_stat_map fn (expr:texpr) =
    match (no_paren expr).eexpr with
      | TBinop ( (Ast.OpAssign as op), left_e, right_e )
      | TBinop ( (Ast.OpAssignOp _ as op), left_e, right_e ) ->
        { expr with eexpr = TBinop(op, fn left_e, fn right_e) }
      | TParenthesis _ -> assert false
      | TCall(left_e, params) ->
        { expr with eexpr = TCall(fn left_e, List.map fn params) }
      | TNew(cl, tparams, params) ->
        { expr with eexpr = TNew(cl, tparams, List.map fn params) }
      | TVars(vars) ->
        { expr with eexpr = TVars( List.map (fun (v,eopt) -> (v, Option.map fn eopt)) vars ) }
      | TFor (v,cond,block) ->
        { expr with eexpr = TFor(v, fn cond, block) }
      | TIf(cond,eif,eelse) ->
        { expr with eexpr = TIf(fn cond, eif, eelse) }
      | TWhile(cond, block, flag) ->
        { expr with eexpr = TWhile(fn cond, block, flag) }
      | TSwitch(cond, el_block_l, default) ->
        { expr with eexpr = TSwitch( fn cond, List.map (fun (el,block) -> (List.map fn el, block)) el_block_l, default ) }
      | TMatch(cond, enum, cases, default) ->
        { expr with eexpr = TMatch(fn cond, enum, cases, default) }
      | TReturn(eopt) ->
        { expr with eexpr = TReturn(Option.map fn eopt) }
      | TThrow (texpr) ->
        { expr with eexpr = TThrow(fn texpr) }
      | TBreak
      | TContinue
      | TTry _
      | TUnop (Ast.Increment, _, _)
      | TUnop (Ast.Decrement, _, _) (* unop is a special case because the haxe compiler won't let us generate complex expressions with Increment/Decrement *)
      | TBlock _ -> expr (* there is no expected expression here. Only statements *)
      | _ -> assert false (* we only expect valid statements here. other expressions aren't valid statements *)

  (* statements: *)
  (* Error CS0201: Only assignment, call, increment,           *)
  (* decrement, and new object expressions can be used as a    *)
  (* statement (CS0201). *)
  let rec shallow_expr_type expr : shallow_expr_type =
    match expr.eexpr with
      | TCall _ when not (is_void expr.etype) -> Both expr
      | TNew _
      | TUnop (Ast.Increment, _, _)
      | TUnop (Ast.Decrement, _, _)
      | TBinop (Ast.OpAssign, _, _)
      | TBinop (Ast.OpAssignOp _, _, _) -> Both expr
      | TIf (cond, eif, Some(eelse)) when (shallow_expr_type eif <> Statement) && (shallow_expr_type eelse <> Statement) -> Both expr
      | TConst _
      | TLocal _
      | TArray _
      | TBinop _
      | TField _
      | TTypeExpr _
      | TObjectDecl _
      | TArrayDecl _
      | TFunction _
      | TCast _
      | TUnop _ -> Expression (expr)
      | TParenthesis p -> shallow_expr_type p
      | TBlock ([e]) -> shallow_expr_type e
      | TCall _
      | TVars _
      | TBlock _
      | TFor _
      | TWhile _
      | TSwitch _
      | TMatch _
      | TTry _
      | TReturn _
      | TBreak
      | TContinue
      | TIf _
      | TThrow _ -> Statement

  let aggregate_expr_type map_fn side_effects_free children =
    let rec loop acc children =
      match children with
        | [] -> acc
        | hd :: children ->
          match acc, map_fn hd with
            | _, KExprWithStatement
            | _, KStatement
            | KExprWithStatement, _
            | KStatement, _ -> KExprWithStatement
            | KNormalExpr, KNoSideEffects
            | KNoSideEffects, KNormalExpr
            | KNormalExpr, KNormalExpr -> loop KNormalExpr children
            | KNoSideEffects, KNoSideEffects -> loop KNoSideEffects children
    in
    loop (if side_effects_free then KNoSideEffects else KNormalExpr) children

  let rec expr_kind expr =
    match shallow_expr_type expr with
      | Statement -> KStatement
      | Both expr | Expression expr ->
        let aggregate = aggregate_expr_type expr_kind in
        match expr.eexpr with
          | TConst _
          | TLocal _
          | TFunction _
          | TTypeExpr _ ->
            KNoSideEffects
          | TCall (ecall, params) ->
            aggregate false (ecall :: params)
          | TNew (_,_,params) ->
            aggregate false params
          | TUnop (Increment,_,e)
          | TUnop (Decrement,_,e) ->
            aggregate false [e]
          | TUnop (_,_,e) ->
            aggregate true [e]
          | TBinop (Ast.OpBoolAnd, e1, e2)
          | TBinop (Ast.OpBoolOr, e1, e2) ->  (* TODO: should OpBool never be side-effects free? *)
            aggregate true [e1;e2]
          | TBinop (Ast.OpAssign, e1, e2)
          | TBinop (Ast.OpAssignOp _, e1, e2) ->
            aggregate false [e1;e2]
          | TBinop (_, e1, e2) ->
            aggregate true [e1;e2]
          | TIf (cond, eif, Some(eelse)) ->
            aggregate true [cond;eif;eelse]
          | TArray (e1,e2) ->
            aggregate true [e1;e2]
          | TParenthesis e
          | TField (e,_) ->
            aggregate true [e]
          | TArrayDecl (el) ->
            aggregate true el
          | TObjectDecl (sel) ->
            aggregate true (List.map snd sel)
          | TCast (e,_) ->
            aggregate true [e]
          | _ -> trace (debug_expr expr); assert false (* should have been read as Statement by shallow_expr_type *)

  let is_side_effects_free e =
    match expr_kind e with | KNoSideEffects -> true | _ -> false

  let get_kinds (statement:texpr) =
    let kinds = ref [] in
    ignore (expr_stat_map (fun e ->
      kinds := (expr_kind e) :: !kinds;
      e
    ) statement);
    List.rev !kinds

  let has_problematic_expressions (kinds:expr_kind list) =
    let rec loop kinds =
      match kinds with
        | [] -> false
        | KStatement :: _
        | KExprWithStatement :: _ -> true
        | _ :: tl -> loop tl
    in
    loop kinds

  let count_problematic_expressions (statement:texpr) =
    let count = ref 0 in
    ignore (expr_stat_map (fun e ->
      (match expr_kind e with
        | KStatement | KExprWithStatement -> incr count
        | _ -> ()
      );
      e
    ) statement);
    !count

  let apply_assign_block assign_fun elist =
    let rec assign acc elist =
      match elist with
        | [] -> assert false
        | last :: [] ->
          (assign_fun last) :: acc
        | hd :: tl ->
          assign (hd :: acc) tl
    in
    List.rev (assign [] elist)

  let mk_get_block assign_fun e =
    match e.eexpr with
      | TBlock (el) ->
        { e with eexpr = TBlock(apply_assign_block assign_fun el) }
      | _ ->
        { e with eexpr = TBlock([ assign_fun e ]) }

  let add_assign gen add_statement expr =
    match follow expr.etype with
      | TEnum({ e_path = ([],"Void") },[])
      | TAbstract ({ a_path = ([],"Void") },[]) ->
        add_statement expr;
        null expr.etype expr.epos
      | _ ->
        let var = mk_temp gen "stmt" expr.etype in
        let tvars = { expr with eexpr = TVars([var,Some(expr)]) } in
        let local = { expr with eexpr = TLocal(var) } in
        add_statement tvars;
        local

  (* requirement: right must be a statement *)
  let rec apply_assign assign_fun right =
    match right.eexpr with
      | TBlock el ->
        { right with eexpr = TBlock(apply_assign_block assign_fun el) }
      | TSwitch (cond, elblock_l, default) ->
        { right with eexpr = TSwitch(cond, List.map (fun (el,block) -> (el, mk_get_block assign_fun block)) elblock_l, Option.map (mk_get_block assign_fun) default) }
      | TMatch (cond, ep, il_vlo_e_l, default) ->
        { right with eexpr = TMatch(cond, ep, List.map (fun (il,vlo,e) -> (il,vlo,mk_get_block assign_fun e)) il_vlo_e_l, Option.map (mk_get_block assign_fun) default) }
      | TTry (block, catches) ->
        { right with eexpr = TTry(mk_get_block assign_fun block, List.map (fun (v,block) -> (v,mk_get_block assign_fun block) ) catches) }
      | TIf (cond,eif,eelse) ->
        { right with eexpr = TIf(cond, mk_get_block assign_fun eif, Option.map (mk_get_block assign_fun) eelse) }
      | TThrow _
      | TWhile _
      | TFor _
      | TReturn _
      | TBreak
      | TContinue -> right
      | TParenthesis p ->
        apply_assign assign_fun p
      | _ ->
        match follow right.etype with
          | TEnum( { e_path = ([], "Void") }, [] )
          | TAbstract ({ a_path = ([], "Void") },[]) ->
            right
          | _ -> trace (debug_expr right); assert false (* a statement is required *)

  let short_circuit_op_unwrap gen add_statement expr :texpr =
    let do_not expr =
      { expr with eexpr = TUnop(Ast.Not, Ast.Prefix, expr) }
    in

    (* loop will always return its own TBlock, and the mapped expression *)
    let rec loop acc expr =
      match expr.eexpr with
        | TBinop ( (Ast.OpBoolAnd as op), left, right) ->
          let var = mk_temp gen "boolv" right.etype in
          let tvars = { right with eexpr = TVars([var, Some( { right with eexpr = TConst(TBool false); etype = gen.gcon.basic.tbool } )]); etype = gen.gcon.basic.tvoid } in
          let local = { right with eexpr = TLocal(var) } in

          let mapped_left, ret_acc = loop ( (local, { right with eexpr = TBinop(Ast.OpAssign, local, right) } ) :: acc) left in

          add_statement tvars;
          ({ expr with eexpr = TBinop(op, mapped_left, local) }, ret_acc)
        (* we only accept OpBoolOr when it's the first to be evaluated *)
        | TBinop ( (Ast.OpBoolOr as op), left, right) when acc = [] ->
          let left = match left.eexpr with
            | TLocal _ | TConst _ -> left
            | _ -> add_assign gen add_statement left
          in

          let var = mk_temp gen "boolv" right.etype in
          let tvars = { right with eexpr = TVars([var, Some( { right with eexpr = TConst(TBool false); etype = gen.gcon.basic.tbool } )]); etype = gen.gcon.basic.tvoid } in
          let local = { right with eexpr = TLocal(var) } in
          add_statement tvars;

          ({ expr with eexpr = TBinop(op, left, local) }, [ do_not left, { right with eexpr = TBinop(Ast.OpAssign, local, right) } ])
        | _ when acc = [] -> assert false
        | _ ->
          let var = mk_temp gen "boolv" expr.etype in
          let tvars = { expr with eexpr = TVars([var, Some( { expr with etype = gen.gcon.basic.tbool } )]); etype = gen.gcon.basic.tvoid } in
          let local = { expr with eexpr = TLocal(var) } in

          let last_local = ref local in
          let acc = List.map (fun (local, assign) ->
            let l = !last_local in
            last_local := local;
            (l, assign)
          ) acc in

          add_statement tvars;
          (local, acc)
    in

    let mapped_expr, local_assign_list = loop [] expr in

    let rec loop local_assign_list : texpr =
      match local_assign_list with
        | [local, assign] ->
          { eexpr = TIf(local, assign, None); etype = gen.gcon.basic.tvoid; epos = assign.epos }
        | (local, assign) :: tl ->
          { eexpr = TIf(local,
            {
              eexpr = TBlock ( assign :: [loop tl] );
              etype = gen.gcon.basic.tvoid;
              epos = assign.epos;
            },
          None); etype = gen.gcon.basic.tvoid; epos = assign.epos }
        | [] -> assert false
    in

    add_statement (loop local_assign_list);
    mapped_expr

  (* there are two short_circuit fuctions as I'm still testing the best way to do it *)
  (*let short_circuit_op_unwrap gen add_statement expr :texpr =
    let block = ref [] in
    let rec short_circuit_op_unwrap is_first last_block expr =
      match expr.eexpr with
        | TBinop ( (Ast.OpBoolAnd as op), left, right)
        | TBinop ( (Ast.OpBoolOr as op), left, right) ->
          let var = mk_temp gen "boolv" left.etype in
          let tvars = { left with eexpr = TVars([var, if is_first then Some(left) else Some( { left with eexpr = TConst(TBool false) } )]); etype = gen.gcon.basic.tvoid } in
          let local = { left with eexpr = TLocal(var) } in
          if not is_first then begin
            last_block := !last_block @ [ { left with eexpr = TBinop(Ast.OpAssign, local, left) } ]
          end;

          add_statement tvars;
          let local_op = match op with | Ast.OpBoolAnd -> local | Ast.OpBoolOr -> { local with eexpr = TUnop(Ast.Not, Ast.Prefix, local) } | _ -> assert false in

          let new_block = ref [] in
          let new_right = short_circuit_op_unwrap false new_block right in
          last_block := !last_block @ [ { expr with eexpr = TIf(local_op, { right with eexpr = TBlock(!new_block) }, None) } ];

          { expr with eexpr = TBinop(op, local, new_right) }
        | _ when is_first -> assert false
        | _ ->
          let var = mk_temp gen "boolv" expr.etype in
          let tvars = { expr with eexpr = TVars([var, Some ( { expr with eexpr = TConst(TBool false) } ) ]); etype = gen.gcon.basic.tvoid } in
          let local = { expr with eexpr = TLocal(var) } in
          last_block := !last_block @ [ { expr with eexpr = TBinop(Ast.OpAssign, local, expr) } ];
          add_statement tvars;

          local
    in
    let mapped_expr = short_circuit_op_unwrap true block expr in
    add_statement { eexpr = TBlock(!block); etype = gen.gcon.basic.tvoid; epos = expr.epos };
    mapped_expr*)

  let twhile_with_condition_statement gen add_statement twhile cond e1 flag =
    (* when a TWhile is found with a problematic condition *)
    let basic = gen.gcon.basic in

    let block = if flag = Ast.NormalWhile then
      { e1 with eexpr = TIf(cond, e1, Some({ e1 with eexpr = TBreak; etype = basic.tvoid })) }
    else
      Codegen.concat e1 { e1 with
        eexpr = TIf({
          eexpr = TUnop(Ast.Not, Ast.Prefix, mk_paren cond);
          etype = basic.tbool;
          epos = cond.epos
        }, { e1 with eexpr = TBreak; etype = basic.tvoid }, None);
        etype = basic.tvoid
      }
    in

    add_statement { twhile with
      eexpr = TWhile(
        { eexpr = TConst(TBool true); etype = basic.tbool; epos = cond.epos },
        block,
        Ast.DoWhile
      );
    }

  let try_call_unwrap_statement gen problematic_expression_unwrap (add_statement:texpr->unit) (expr:texpr) : texpr option =
    let check_left left =
      match expr_kind left with
        | KExprWithStatement ->
          problematic_expression_unwrap add_statement left KExprWithStatement
        | KStatement -> assert false (* doesn't make sense a KStatement as a left side expression *)
        | _ -> left
    in

    let handle_assign op left right =
      let left = check_left left in
      Some (apply_assign (fun e -> { e with eexpr = TBinop(op, left, if is_void left.etype then e else gen.ghandle_cast left.etype e.etype e) }) right )
    in

    let handle_return e =
      Some( apply_assign (fun e ->
        match e.eexpr with
          | TThrow _ -> e
          | _ when is_void e.etype ->
              { e with eexpr = TBlock([e; { e with eexpr = TReturn None }]) }
          | _ ->
              { e with eexpr = TReturn( Some e ) }
      ) e )
    in

    let is_problematic_if right =
      match expr_kind right with
        | KStatement | KExprWithStatement -> true
        | _ -> false
    in

    match expr.eexpr with
      | TBinop((Ast.OpAssign as op),left,right)
      | TBinop((Ast.OpAssignOp _ as op),left,right) when shallow_expr_type right = Statement ->
        handle_assign op left right
      | TReturn( Some right ) when shallow_expr_type right = Statement ->
        handle_return right
      | TBinop((Ast.OpAssign as op),left, ({ eexpr = TBinop(Ast.OpBoolAnd,_,_) } as right) )
      | TBinop((Ast.OpAssign as op),left,({ eexpr = TBinop(Ast.OpBoolOr,_,_) } as right))
      | TBinop((Ast.OpAssignOp _ as op),left,({ eexpr = TBinop(Ast.OpBoolAnd,_,_) } as right) )
      | TBinop((Ast.OpAssignOp _ as op),left,({ eexpr = TBinop(Ast.OpBoolOr,_,_) } as right) ) ->
        let right = short_circuit_op_unwrap gen add_statement right in
        Some { expr with eexpr = TBinop(op, check_left left, right) }
      | TVars([v,Some({ eexpr = TBinop(Ast.OpBoolAnd,_,_) } as right)])
      | TVars([v,Some({ eexpr = TBinop(Ast.OpBoolOr,_,_) } as right)]) ->
        let right = short_circuit_op_unwrap gen add_statement right in
        Some { expr with eexpr = TVars([v, Some(right)]) }
      | TVars([v,Some(right)]) when shallow_expr_type right = Statement ->
        add_statement ({ expr with eexpr = TVars([v, Some(null right.etype right.epos)]) });
        handle_assign Ast.OpAssign { expr with eexpr = TLocal(v) } right
      (* TIf handling *)
      | TBinop((Ast.OpAssign as op),left, ({ eexpr = TIf _ } as right))
      | TBinop((Ast.OpAssignOp _ as op),left,({ eexpr = TIf _ } as right)) when is_problematic_if right ->
        handle_assign op left right
      | TVars([v,Some({ eexpr = TIf _ } as right)]) when is_problematic_if right ->
        add_statement ({ expr with eexpr = TVars([v, Some(null right.etype right.epos)]) });
        handle_assign Ast.OpAssign { expr with eexpr = TLocal(v) } right
      | TWhile(cond, e1, flag) when is_problematic_if cond ->
        twhile_with_condition_statement gen add_statement expr cond e1 flag;
        Some (null expr.etype expr.epos)
      | _ -> None


  let traverse gen (on_expr_as_statement:texpr->texpr option) =

    let add_assign = add_assign gen in

    let problematic_expression_unwrap add_statement expr e_type =
      let rec problematic_expression_unwrap is_first expr e_type =
        match e_type, expr.eexpr with
          | _, TBinop(Ast.OpBoolAnd, _, _)
          | _, TBinop(Ast.OpBoolOr, _, _) -> add_assign add_statement expr (* add_assign so try_call_unwrap_expr *)
          | KNoSideEffects, _ -> expr
          | KStatement, _
          | KNormalExpr, _ -> add_assign add_statement expr
          | KExprWithStatement, TCall _
          | KExprWithStatement, TNew _
          | KExprWithStatement, TBinop (Ast.OpAssign,_,_)
          | KExprWithStatement, TBinop (Ast.OpAssignOp _,_,_)
          | KExprWithStatement, TUnop (Ast.Increment,_,_) (* all of these may have side-effects, so they must also be add_assign'ed . is_first avoids infinite loop *)
          | KExprWithStatement, TUnop (Ast.Decrement,_,_) when not is_first -> add_assign add_statement expr

          (* bugfix: Type.map_expr doesn't guarantee the correct order of execution *)
          | KExprWithStatement, TBinop(op,e1,e2) ->
            let e1 = problematic_expression_unwrap false e1 (expr_kind e1) in
            let e2 = problematic_expression_unwrap false e2 (expr_kind e2) in
            { expr with eexpr = TBinop(op, e1, e2) }
          | KExprWithStatement, TArray(e1,e2) ->
            let e1 = problematic_expression_unwrap false e1 (expr_kind e1) in
            let e2 = problematic_expression_unwrap false e2 (expr_kind e2) in
            { expr with eexpr = TArray(e1, e2) }
          (* bugfix: calls should not be transformed into closure calls *)
          | KExprWithStatement, TCall(( { eexpr = TField (ef_left, f) } as ef ), eargs) ->
            { expr with eexpr = TCall(
              { ef with eexpr = TField(problematic_expression_unwrap false ef_left (expr_kind ef_left), f) },
              List.map (fun e -> problematic_expression_unwrap false e (expr_kind e)) eargs)
            }
          | KExprWithStatement, _ -> Type.map_expr (fun e -> problematic_expression_unwrap false e (expr_kind e)) expr
      in
      problematic_expression_unwrap true expr e_type
    in

    let rec traverse e =
      match e.eexpr with
        | TBlock el ->
          let new_block = ref [] in
          let rec process_statement e =
            let e = no_paren e in
            match e.eexpr, shallow_expr_type e with
              | TVars( (hd1 :: hd2 :: _) as vars ), _ ->
                List.iter (fun v -> process_statement { e with eexpr = TVars([v]) }) vars
              | TCall( { eexpr = TLocal v } as elocal, elist ), _ when String.get v.v_name 0 = '_' && Hashtbl.mem gen.gspecial_vars v.v_name ->
                new_block := { e with eexpr = TCall( elocal, List.map (fun e ->
                  match e.eexpr with
                    | TBlock _ -> traverse e
                    | _ -> e
                ) elist ) } :: !new_block
              | _, Statement | _, Both _ ->
                let e = match e.eexpr with | TReturn (Some ({ eexpr = TThrow _ } as ethrow)) -> ethrow | _ -> e in
                let kinds = get_kinds e in
                if has_problematic_expressions kinds then begin
                  match try_call_unwrap_statement gen problematic_expression_unwrap add_statement e with
                    | Some { eexpr = TConst(TNull) } (* no op *) ->
                      ()
                    | Some e ->
                      if has_problematic_expressions (get_kinds e) then begin
                        process_statement e
                      end else
                        new_block := (traverse e) :: !new_block
                    | None ->
                    (
                      let acc = ref kinds in
                      let new_e = expr_stat_map (fun e ->
                        match !acc with
                          | hd :: tl ->
                            acc := tl;
                            if has_problematic_expressions (hd :: tl) then begin
                              problematic_expression_unwrap add_statement e hd
                            end else
                              e
                          | [] -> assert false
                      ) e in

                      new_block := (traverse new_e) :: !new_block
                    )
                end else begin new_block := (traverse e) :: !new_block end
              | _, Expression e ->
                match on_expr_as_statement e with
                  | None -> ()
                  | Some e -> process_statement e
          and add_statement expr =
            process_statement expr
          in

          List.iter (process_statement) el;
          let block = List.rev !new_block in
          { e with eexpr = TBlock(block) }
        | TTry (block, catches) ->
          { e with eexpr = TTry(traverse (mk_block block), List.map (fun (v,block) -> (v, traverse (mk_block block))) catches) }
        | TMatch (cond,ep,il_vol_e_l,default) ->
          { e with eexpr = TMatch(cond,ep,List.map (fun (il,vol,e) -> (il,vol,traverse (mk_block e))) il_vol_e_l, Option.map (fun e -> traverse (mk_block e)) default) }
        | TSwitch (cond,el_e_l, default) ->
          { e with eexpr = TSwitch(cond, List.map (fun (el,e) -> (el, traverse (mk_block e))) el_e_l, Option.map (fun e -> traverse (mk_block e)) default) }
        | TWhile (cond,block,flag) ->
          {e with eexpr = TWhile(cond,traverse (mk_block block), flag) }
        | TIf (cond, eif, eelse) ->
          { e with eexpr = TIf(cond, traverse (mk_block eif), Option.map (fun e -> traverse (mk_block e)) eelse) }
        | TFor (v,it,block) ->
          { e with eexpr = TFor(v,it, traverse (mk_block block)) }
        | TFunction (tfunc) ->
          { e with eexpr = TFunction({ tfunc with tf_expr = traverse (mk_block tfunc.tf_expr) }) }
        | _ -> e (* if expression doesn't have a block, we will exit *)
    in
    traverse

  let configure gen (mapping_func:texpr->texpr) =
    let map e = Some(mapping_func e) in
    gen.gsyntax_filters#add ~name:name ~priority:(PCustom priority) map

end;;

(* ******************************************* *)
(* Reflection-enabling Class fields *)
(* ******************************************* *)

(*
  This is the most hardcore codegen part of the code. There's much to improve so this code can be more readable, but at least it's running correctly right now! This will be improved. (TODO)

  This module will create class fields that enable reflection for targets that have a slow or inexistent reflection abilities. Because of the similarity
  of strategies between what should have been different modules, they are all unified in this reflection-enabling class fields.

  They include:
    * Get(isStatic, throwErrors, isCheck) / Set fields . Remember to allow implements Dynamic also.
    * Invoke fields(isStatic) -> You need to configure how many invoke_field fields there will be. + invokeDynamic
    * Has field -> parameter in get field that returns __undefined__ if it doesn't exist.

    * GetType -> return the current Class<> / Enum<>
    * Fields(isStatic) -> returns all the fields / static fields. Remember to allow implements Dynamic also

    * Create(arguments array), CreateEmpty - calls new() or create empty
    * getInstanceFields / getClassFields -> show even function fields, everything!

    * deleteField -> only for implements Dynamic

    for enums:
    * createEnum -> invokeField for classes
    * createEnumIndex -> use invokeField as well, and use numbers e.g. "0", "1", "2" .... For this, use "@:alias" metadata
    * getEnumConstructs -> fields()

    need to be solved outside:
    * getEnumName
    * enumIndex
    *

    need to be solved by haxe code:
    * enumParameters -> for (field in Reflect.fields(enum)) arr.push(Reflect.field(enum, field))

  Standard:
    if a class contains a @:$enum metadata, it's treated as a converted enum to class


  Optimizations:
    * if optimize is true, all fields will be hashed by the same hashing function as neko (31 bits int : always positive). Every function that expects a string for the field will expect also an int, for the hash
      a string (which is nullable for compile-time hashes) + an int.
      At compile-time, a collision will throw an error (like neko).
      At runtime, a collision will make a negative int. Negative ints will always resolve to a special Hash<> field which takes a string.
    * if optimize is true, Reflect.field/setField will be replaced by either the runtime version (with already hashed string), either by the own .Field()/.SetField() HxObject's version,
      if the type is detected to already be hxgen
    * TODO: if for() optimization for arrays is disabled, we can replace for(field in Reflect.fields(obj)) to:
      for (field in ( (Std.is(obj, HxObject) ? ((HxObject)obj).Fields() : Reflect.fields(obj)) )) // no array copying . for further optimization this could be guaranteed to return
      the already hashed fields.

  Mappings:
    * if create Dynamic class is true, TObjectDecl will be mapped to new DynamicClass(fields, [hashedFields], values)
    *

  dependencies:
    There is no big dependency from this target. Though it should be a syntax filter, mainly one of the first so most expression generation has already been done,
    while the AST has its meaning close to haxe's.
    Should run before InitFunction so it detects variables containing expressions as "always-execute" expressions, even when using CreateEmpty

    * Must run before switch() syntax changes

*)

open ClosuresToClass;;
module ReflectionCFs =
struct

  let name = "reflection_cfs"

  type rcf_ctx =
  {
    rcf_gen : generator_ctx;
    rcf_ft : ClosuresToClass.closures_ctx;
    rcf_optimize : bool;
    mutable rcf_float_special_case : bool;

    mutable rcf_object_iface : tclass;

    mutable rcf_create_getsetinvoke_fields : bool;
    (* should we create the get type (get Class)? *)
    mutable rcf_create_get_type : bool;
    (* should we handle implements dynamic? *)
    mutable rcf_handle_impl_dynamic : bool;
    (*
      create_dyn_overloading_ctor :
        when creating the implements dynamic code, we can also create a special constructor for
        the actual DynamicObject class, which will receive all its <implements Dynamic> fields from the code outside.
        Note that this will only work on targets that support overloading contrstuctors, as any class that extends
        our DynamicObject will have an empty super() call
    *)
    mutable rcf_create_dyn_ctor : bool;

    mutable rcf_max_func_arity : int;

    (*
      the hash lookup function. can be an inlined expr or simply a function call.
      its only needed features is that it should return the index of the key if found, and the
      complement of the index of where it should be inserted if not found (Ints).

      hash->hash_array->returning expression
    *)
    mutable rcf_hash_function : texpr->texpr->texpr;

    mutable rcf_lookup_function : texpr->texpr;

    (*
      class_cl is the real class for Class<> instances.
      In the current implementation, due to some targets' limitations, (in particular, Java),
      we have to use an empty object so we can access its virtual mehtods.
      FIXME find a better way to create Class<> objects in a performant way
    *)
    mutable rcf_class_cl : tclass option;
    (*
      Also about the Class<> type, should we crate all classes eagerly?
      If false, it means that we should have a way at runtime to create the class when needed by
      Type.resolveClass/Enum
    *)
    mutable rcf_class_eager_creation : bool;

    rcf_hash_fields : (int, string) Hashtbl.t;

    (*
      main expr -> field expr -> field string -> possible hash int (if optimize) -> possible set expr -> should_throw_exceptions -> changed expression

      Changes a get / set field to the runtime resolution function
    *)
    mutable rcf_on_getset_field : texpr->texpr->string->int32 option->texpr option->bool->texpr;

    mutable rcf_on_call_field : texpr->texpr->string->int32 option->texpr list->texpr;

    mutable rcf_handle_statics : bool;
  }

  let new_ctx gen ft object_iface optimize dynamic_getset_field dynamic_call_field hash_function lookup_function handle_statics =
    {
      rcf_gen = gen;
      rcf_ft = ft;

      rcf_optimize = optimize;

      rcf_float_special_case = true;

      rcf_object_iface = object_iface;

      rcf_create_getsetinvoke_fields = true;
      rcf_create_get_type = true;

      rcf_handle_impl_dynamic = true;
      rcf_create_dyn_ctor = true;

      rcf_max_func_arity = 10;

      rcf_hash_function = hash_function;
      rcf_lookup_function = lookup_function;

      rcf_class_cl = None;
      rcf_class_eager_creation = false;

      rcf_hash_fields = Hashtbl.create 100;

      rcf_on_getset_field = dynamic_getset_field;
      rcf_on_call_field = dynamic_call_field;

      rcf_handle_statics = handle_statics;
    }

  (*
    methods as a bool option is a little laziness of my part.
      None means that methods are included with normal fields;
      Some(true) means collect only methods
      Some(false) means collect only fields (and MethDynamic fields)
  *)
  let collect_fields cl (methods : bool option) (statics : bool option) =
    let collected = Hashtbl.create 0 in
    let collect cf acc =
      if Meta.has Meta.CompilerGenerated cf.cf_meta || Meta.has Meta.SkipReflection cf.cf_meta then
        acc
      else match methods, cf.cf_kind with
        | None, _ when not (Hashtbl.mem collected cf.cf_name) -> Hashtbl.add collected cf.cf_name true; ([cf.cf_name], cf) :: acc
        | Some true, Method MethDynamic -> acc
        | Some true, Method _ when not (Hashtbl.mem collected cf.cf_name) -> Hashtbl.add collected cf.cf_name true; ([cf.cf_name], cf) :: acc
        | Some false, Method MethDynamic
        | Some false, Var _ when not (Hashtbl.mem collected cf.cf_name) -> Hashtbl.add collected cf.cf_name true; ([cf.cf_name], cf) :: acc
        | _ -> acc
    in
    let collect_cfs cfs acc =
      let rec loop cfs acc =
        match cfs with
          | [] -> acc
          | hd :: tl -> loop tl (collect hd acc)
      in
      loop cfs acc
    in
    let rec loop cl acc =
      let acc = match statics with
        | None -> collect_cfs cl.cl_ordered_fields (collect_cfs cl.cl_ordered_statics acc)
        | Some true -> collect_cfs cl.cl_ordered_statics acc
        | Some false -> collect_cfs cl.cl_ordered_fields acc
      in
      match cl.cl_super with
        | None -> acc
        | Some(cl,_) ->
          if not (is_hxgen (TClassDecl cl)) then loop cl acc else acc
    in

    loop cl []

  let hash f =
    let h = ref 0 in
    for i = 0 to String.length f - 1 do
      h := !h * 223 + int_of_char (String.unsafe_get f i);
    done;
    !h

  let hash_field ctx f pos =
    let h = hash f in
    (try
      let f2 = Hashtbl.find ctx.rcf_hash_fields h in
      if f <> f2 then ctx.rcf_gen.gcon.error ("Field conflict between " ^ f ^ " and " ^ f2) pos
    with Not_found ->
      Hashtbl.add ctx.rcf_hash_fields h f);
    h

  (* ( tf_args, switch_var ) *)
  let field_type_args ctx pos =
    match ctx.rcf_optimize with
      | true ->
        let field_name, field_hash = alloc_var "field" ctx.rcf_gen.gcon.basic.tstring, alloc_var "hash" ctx.rcf_gen.gcon.basic.tint in

        [field_name, None; field_hash, None], field_hash
      | false ->
        let field_name = alloc_var "field" ctx.rcf_gen.gcon.basic.tstring in
        [field_name, None], field_name

  let hash_field_i32 ctx pos field_name =
    let i = hash_field ctx field_name pos in
    let i = Int32.of_int (i) in
    if i < Int32.zero then
      Int32.logor (Int32.logand i (Int32.of_int 0x3FFFFFFF)) (Int32.shift_left Int32.one 30)
    else i

  let switch_case ctx pos field_name =
    match ctx.rcf_optimize with
      | true ->
        let i = hash_field_i32 ctx pos field_name in
        { eexpr = TConst(TInt(i)); etype = ctx.rcf_gen.gcon.basic.tint; epos = pos }
      | false ->
        { eexpr = TConst(TString(field_name)); etype = ctx.rcf_gen.gcon.basic.tstring; epos = pos }

  (*
    Will implement getField / setField which will follow the following rule:
      function getField(field, isStatic, throwErrors, isCheck, handleProperty, isFirst):Dynamic
      {
        if (isStatic)
        {
          switch(field)
          {
            case "aStaticField": return ThisClass.aStaticField;
            case "aDynamicField": return ThisClass.aDynamicField;
            default:
              if (isFirst) return getField_d(field, isStatic, throwErrors, handleProperty, false);
              if(throwErrors) throw "Field not found"; else if (isCheck) return __undefined__ else   return null;
          }
        } else {
          switch(field)
          {
            case "aNormalField": return this.aNormalField;
            case "aBoolField": return this.aBoolField;
            case "aDoubleField": return this.aDoubleField;
            default: return getField_d(field, isStatic, throwErrors, isCheck);
          }
        }
      }

      function getField_d(field, isStatic, throwErrors, handleProperty, isFirst):Float
      {
        if (isStatic)
        {
          switch(field)
          {
            case "aDynamicField": return cast ThisClass.aDynamicField;
            default: if (throwErrors) throw "Field not found"; else return null;
          }
        }
        etc...
      }

      function setField(field, value, isStatic):Dynamic  {}
      function setField_d(field, value:Float, isStatic):Float {}
  *)

  let call_super ctx fn_args ret_t cf cl this_t pos =
    {
      eexpr = TCall({
        eexpr = TField({ eexpr = TConst(TSuper); etype = this_t; epos = pos }, FInstance(cl,cf));
        etype = TFun(fun_args fn_args, ret_t);
        epos = pos;
      }, List.map (fun (v,_) -> mk_local v pos) fn_args);
      etype = ret_t;
      epos = pos;
    }

  let mk_string ctx str pos =
    { eexpr = TConst(TString(str)); etype = ctx.rcf_gen.gcon.basic.tstring; epos = pos }

  let mk_int ctx i pos =
    { eexpr = TConst(TInt(Int32.of_int i)); etype = ctx.rcf_gen.gcon.basic.tint; epos = pos }

  let mk_bool ctx b pos =
    { eexpr = TConst(TBool(b)); etype = ctx.rcf_gen.gcon.basic.tbool; epos = pos }

  let mk_throw ctx str pos = { eexpr = TThrow (mk_string ctx str pos); etype = ctx.rcf_gen.gcon.basic.tvoid; epos = pos }

  let enumerate_dynamic_fields ctx cl when_found =
    let gen = ctx.rcf_gen in
    let basic = gen.gcon.basic in
    let pos = cl.cl_pos in

    let mk_for arr =
      let t = if ctx.rcf_optimize then basic.tint else basic.tstring in
      let convert_str e = if ctx.rcf_optimize then ctx.rcf_lookup_function e else e in
      let var = mk_temp gen "field" t in
      {
        eexpr = TFor(var, mk_iterator_access gen t arr, mk_block (when_found (convert_str (mk_local var pos))));
        etype = basic.tvoid;
        epos = pos;
      }
    in

    let this_t = TInst(cl, List.map snd cl.cl_types) in
    let this = { eexpr = TConst(TThis); etype = this_t; epos = pos } in
    let mk_this field t = { (mk_field_access gen this field pos) with etype = t } in
    if ctx.rcf_optimize then
    [
      mk_for (mk_this (gen.gmk_internal_name "hx" "hashes") (basic.tarray basic.tint));
      mk_for (mk_this (gen.gmk_internal_name "hx" "hashes_f") (basic.tarray basic.tint));
    ] else [
      mk_for (mk_this (gen.gmk_internal_name "hx" "hashes") (basic.tarray basic.tstring));
      mk_for (mk_this (gen.gmk_internal_name "hx" "hashes_f") (basic.tarray basic.tstring));
    ]

  (* *********************
     Dynamic lookup
     *********************

     This is the behavior of standard <implements Dynamic> classes. It will replace the error throwing
     if a field doesn't exists when looking it up.

     In order for it to work, an implementation for hash_function must be created.
     hash_function is the function to be called/inlined that will allow us to lookup the hash into a sorted array of hashes.
     A binary search or linear search algorithm may be implemented. The only need is that if not found, the NegBits of
     the place where it should be inserted must be returned.
  *)
  let abstract_dyn_lookup_implementation ctx this hash_local may_value is_float pos =
    let gen = ctx.rcf_gen in
    let basic = gen.gcon.basic in
    let mk_this field t = { (mk_field_access gen this field pos) with etype = t } in
    let a_t = if ctx.rcf_optimize then basic.tint else basic.tstring in
    let hx_hashes = mk_this (gen.gmk_internal_name "hx" "hashes") (basic.tarray a_t) in
    let hx_hashes_f = mk_this (gen.gmk_internal_name "hx" "hashes_f") (basic.tarray a_t) in
    let hx_dynamics = mk_this (gen.gmk_internal_name "hx" "dynamics") (basic.tarray t_empty) in
    let hx_dynamics_f = mk_this (gen.gmk_internal_name "hx" "dynamics_f") (basic.tarray basic.tfloat) in
    let res = alloc_var "res" basic.tint in
    let fst_hash, snd_hash, fst_dynamics, snd_dynamics =
      if is_float then hx_hashes_f, hx_hashes, hx_dynamics_f, hx_dynamics else hx_hashes, hx_hashes_f, hx_dynamics, hx_dynamics_f
    in
    let res_local = mk_local res pos in
    let gte = {
      eexpr = TBinop(Ast.OpGte, res_local, { eexpr = TConst(TInt(Int32.zero)); etype = basic.tint; epos = pos });
      etype = basic.tbool;
      epos = pos;
    } in
    let get_array_t t = match follow t with | TInst({ cl_path = ([],"Array") },[arrtype]) -> arrtype | _ -> assert false in
    let mk_tarray arr idx =
      let t = get_array_t arr.etype in
      {
        eexpr = TArray(arr, idx);
        etype = t;
        epos = pos;
      }
    in
    let ret_t = if is_float then basic.tfloat else t_dynamic in

    match may_value with
      | None ->
        (*
          var res = lookup(this.__hx_hashes/f, hash);
          if (res < 0)
          {
            res = lookup(this.__hx_hashes_f/_, hash);
            if(res < 0)
              return null;
            else
              return __hx_dynamics_f[res];
          } else {
            return __hx_dynamics[res];
          }
        *)
        let block =
        [
          { eexpr = TVars([res, Some(ctx.rcf_hash_function hash_local fst_hash)]); etype = basic.tvoid; epos = pos };
          { eexpr = TIf(gte, mk_return (mk_tarray fst_dynamics res_local), Some({
            eexpr = TBlock(
            [
              { eexpr = TBinop(Ast.OpAssign, res_local, ctx.rcf_hash_function hash_local snd_hash); etype = basic.tint; epos = pos };
              { eexpr = TIf(gte, mk_return (mk_tarray snd_dynamics res_local), None); etype = ret_t; epos = pos }
            ]);
            etype = ret_t;
            epos = pos;
          })); etype = ret_t; epos = pos }
        ] in
        block
      | Some value_local ->
        (*
          //if is not float:
          //if (isNumber(value_local)) return this.__hx_setField_f(field, getNumber(value_local), false(not static));
          var res = lookup(this.__hx_hashes/f, hash);
          if (res >= 0)
          {
            return __hx_dynamics/f[res] = value_local;
          } else {
            res = lookup(this.__hx_hashes_f/_, hash);
            if (res >= 0)
            {
              __hx_dynamics_f/_.splice(res,1);
              __hx_hashes_f/_.splice(res,1);
            }
          }

          __hx_hashses/_f.insert(~res, hash);
          __hx_dynamics/_f.insert(~res, value_local);
          return value_local;
        *)
        let mk_splice arr at_pos = {
          eexpr = TCall(
            mk_field_access gen arr "splice" pos,
            [at_pos; { eexpr = TConst(TInt Int32.one); etype = basic.tint; epos = pos }]
          );
          etype = arr.etype;
          epos = pos
        } in

        let mk_insert arr at_pos value = {
          eexpr = TCall(
            mk_field_access gen arr "insert" pos,
            [at_pos; value]);
          etype = basic.tvoid;
          epos = pos
        } in

        let neg_res = { eexpr = TUnop(Ast.NegBits, Ast.Prefix, res_local); etype = basic.tint; epos = pos } in

        let res2 = alloc_var "res2" basic.tint in
        let res2_local = mk_local res2 pos in

        let block =
        [
          { eexpr = TVars([res, Some(ctx.rcf_hash_function hash_local fst_hash)]); etype = basic.tvoid; epos = pos };
          {
            eexpr = TIf(gte,
              mk_return { eexpr = TBinop(Ast.OpAssign, mk_tarray fst_dynamics res_local, value_local); etype = value_local.etype; epos = pos },
              Some({ eexpr = TBlock([
                { eexpr = TVars([ res2, Some(ctx.rcf_hash_function hash_local snd_hash)]); etype = basic.tvoid; epos = pos };
                {
                  eexpr = TIf(gte, { eexpr = TBlock([
                    mk_splice snd_hash res2_local;
                    mk_splice snd_dynamics res2_local
                  ]); etype = t_dynamic; epos = pos }, None);
                  etype = t_dynamic;
                  epos = pos;
                }
              ]); etype = t_dynamic; epos = pos }));
            etype = t_dynamic;
            epos = pos;
          };
          mk_insert fst_hash neg_res hash_local;
          mk_insert fst_dynamics neg_res value_local;
          mk_return value_local
        ] in
        block

  let get_delete_field ctx cl is_dynamic =
    let pos = cl.cl_pos in
    let this_t = TInst(cl, List.map snd cl.cl_types) in
    let this = { eexpr = TConst(TThis); etype = this_t; epos = pos } in
    let gen = ctx.rcf_gen in
    let basic = gen.gcon.basic in
    let tf_args, switch_var = field_type_args ctx pos in
    let local_switch_var = mk_local switch_var pos in
    let fun_type = TFun(fun_args tf_args,basic.tbool) in
    let cf = mk_class_field (gen.gmk_internal_name "hx" "deleteField") fun_type false pos (Method MethNormal) [] in
    let body = if is_dynamic then begin
      let mk_this field t = { (mk_field_access gen this field pos) with etype = t } in
      let a_t = if ctx.rcf_optimize then basic.tint else basic.tstring in
      let hx_hashes = mk_this (gen.gmk_internal_name "hx" "hashes") (basic.tarray a_t) in
      let hx_hashes_f = mk_this (gen.gmk_internal_name "hx" "hashes_f") (basic.tarray a_t) in
      let hx_dynamics = mk_this (gen.gmk_internal_name "hx" "dynamics") (basic.tarray t_empty) in
      let hx_dynamics_f = mk_this (gen.gmk_internal_name "hx" "dynamics_f") (basic.tarray basic.tfloat) in
      let res = alloc_var "res" basic.tint in
      let res_local = mk_local res pos in
      let gte = {
        eexpr = TBinop(Ast.OpGte, res_local, { eexpr = TConst(TInt(Int32.zero)); etype = basic.tint; epos = pos });
        etype = basic.tbool;
        epos = pos;
      } in
      let mk_splice arr at_pos = {
        eexpr = TCall(
          mk_field_access gen arr "splice" pos,
          [at_pos; { eexpr = TConst(TInt Int32.one); etype = basic.tint; epos = pos }]
        );
        etype = arr.etype;
        epos = pos
      } in
      (*
        var res = lookup(this.__hx_hashes, hash);
        if (res >= 0)
        {
          __hx_dynamics.splice(res,1);
          __hx_hashes.splice(res,1);

          return true;
        } else {
          res = lookup(this.__hx_hashes_f, hash);
          if (res >= 0)
          {
            __hx_dynamics_f.splice(res,1);
            __hx_hashes_f.splice(res,1);

            return true;
          }
        }

        return false;
      *)
      [
        { eexpr = TVars([res,Some(ctx.rcf_hash_function local_switch_var hx_hashes)]); etype = basic.tvoid; epos = pos };
        {
          eexpr = TIf(gte, { eexpr = TBlock([
            mk_splice hx_hashes res_local;
            mk_splice hx_dynamics res_local;
            mk_return { eexpr = TConst(TBool true); etype = basic.tbool; epos = pos }
          ]); etype = t_dynamic; epos = pos }, Some({ eexpr = TBlock([
            { eexpr = TBinop(Ast.OpAssign, res_local, ctx.rcf_hash_function local_switch_var hx_hashes_f); etype = basic.tint; epos = pos };
            { eexpr = TIf(gte, { eexpr = TBlock([
              mk_splice hx_hashes_f res_local;
              mk_splice hx_dynamics_f res_local;
              mk_return { eexpr = TConst(TBool true); etype = basic.tbool; epos = pos }
            ]); etype = t_dynamic; epos = pos }, None); etype = t_dynamic; epos = pos }
          ]); etype = t_dynamic; epos = pos }));
          etype = t_dynamic;
          epos = pos;
        };
        mk_return { eexpr = TConst(TBool false); etype = basic.tbool; epos = pos }
      ]
    end else
    [
      mk_return { eexpr = TConst(TBool false); etype = basic.tbool; epos = pos }
    ] in

    (* create function *)
    let fn =
    {
      tf_args = tf_args;
      tf_type = basic.tbool;
      tf_expr = { eexpr = TBlock(body); etype = t_dynamic; epos = pos }
    } in
    cf.cf_expr <- Some({ eexpr = TFunction(fn); etype = fun_type; epos = pos });
    cf

  let rec is_first_dynamic cl =
    match cl.cl_super with
      | Some(cl,_) ->
        if is_some cl.cl_dynamic then false else is_first_dynamic cl
      | None -> true

  let is_override cl = match cl.cl_super with
    | Some (cl, _) when is_hxgen (TClassDecl cl) -> true
    | _ -> false

  let get_args t = match follow t with
    | TFun(args,ret) -> args,ret
    | _ -> assert false

  (* WARNING: this will only work if overloading contructors is possible on target language *)
  let implement_dynamic_object_ctor ctx cl =
    let rec is_side_effects_free e =
      match e.eexpr with
        | TConst _
        | TLocal _
        | TFunction _
        | TTypeExpr _ ->
          true
        | TNew(clnew,[],params) when clnew == cl ->
          List.for_all is_side_effects_free params
        | TUnop(Increment,_,_)
        | TUnop(Decrement,_,_)
        | TBinop(OpAssign,_,_)
        | TBinop(OpAssignOp _,_,_) ->
          false
        | TUnop(_,_,e) ->
          is_side_effects_free e
        | TArray(e1,e2)
        | TBinop(_,e1,e2) ->
          is_side_effects_free e1 && is_side_effects_free e2
        | TIf(cond,e1,Some e2) ->
          is_side_effects_free cond && is_side_effects_free e1 && is_side_effects_free e2
        | TField(e,_)
        | TParenthesis e -> is_side_effects_free e
        | TArrayDecl el -> List.for_all is_side_effects_free el
        | TCast(e,_) -> is_side_effects_free e
        | _ -> false
    in

    let pos = cl.cl_pos in
    let gen = ctx.rcf_gen in
    let basic = gen.gcon.basic in
    let hasht = if ctx.rcf_optimize then basic.tint else basic.tstring in

    let fields =
    [
      gen.gmk_internal_name "hx" "hashes", basic.tarray hasht;
      gen.gmk_internal_name "hx" "dynamics", basic.tarray t_empty;
      gen.gmk_internal_name "hx" "hashes_f", basic.tarray hasht;
      gen.gmk_internal_name "hx" "dynamics_f", basic.tarray basic.tfloat;
    ] in
    let tf_args = List.map (fun (name, t) ->
      alloc_var name t, None
    ) fields in

    let this = { eexpr = TConst TThis; etype = TInst(cl, List.map snd cl.cl_types); epos = pos } in
    let mk_this field t = { (mk_field_access gen this field pos) with etype = t } in
    let fun_t = TFun(fun_args tf_args,basic.tvoid) in
    let ctor = mk_class_field "new" fun_t true pos (Method MethNormal) [] in
    ctor.cf_expr <- Some(
    {
      eexpr = TFunction({
        tf_args = tf_args;
        tf_type = basic.tvoid;
        tf_expr =
        {
          eexpr = TBlock(List.map (fun (v,_) ->
              { eexpr = TBinop(Ast.OpAssign, mk_this v.v_name v.v_type, mk_local v pos); etype = v.v_type; epos = pos }
            ) tf_args);
          etype = basic.tvoid;
          epos = pos
        }
      });
      etype = fun_t;
      epos = pos
    });

    add_constructor cl ctor;
    (* and finally we will return a function that transforms a TObjectDecl into a new DynamicObject() call *)
    let rec loop objdecl acc acc_f =
      match objdecl with
        | [] -> acc,acc_f
        | (name,expr) :: tl ->
          let real_t = gen.greal_type expr.etype in
          match follow expr.etype with
            | TInst ( { cl_path = ["haxe"], "Int64" }, [] ) ->
              loop tl ((name, gen.ghandle_cast t_dynamic real_t expr) :: acc) acc_f
            | _ ->
              if like_float real_t then
                loop tl acc ((name, gen.ghandle_cast basic.tfloat real_t expr) :: acc_f)
              else
                loop tl ((name, gen.ghandle_cast t_dynamic real_t expr) :: acc) acc_f
    in

    let may_hash_field s =
      if ctx.rcf_optimize then begin
        (* let hash_field ctx f pos = *)
        { eexpr = TConst(TInt (hash_field_i32 ctx pos s)); etype = basic.tint; epos = pos }
      end else begin
        { eexpr = TConst(TString s); etype = basic.tstring; epos = pos }
      end
    in

    let do_objdecl e objdecl =
      let exprs_before = ref [] in
      let rec change_exprs decl acc = match decl with
        | (name,expr) :: tl ->
          if is_side_effects_free expr then
            change_exprs tl ((name,expr) :: acc)
          else begin
            let var = mk_temp gen "odecl" expr.etype in
            exprs_before := { eexpr = TVars([var,Some expr]); etype = basic.tvoid; epos = expr.epos } :: !exprs_before;
            change_exprs tl ((name,mk_local var expr.epos) :: acc)
          end
        | [] -> acc
      in
      let objdecl = change_exprs objdecl [] in

      let odecl, odecl_f = loop objdecl [] [] in
      let changed_expr = List.map (fun (s,e) -> (may_hash_field s,e)) in
      let odecl, odecl_f = changed_expr odecl, changed_expr odecl_f in
      let sort_fn (e1,_) (e2,_) =
        match e1.eexpr, e2.eexpr with
          | TConst(TInt i1), TConst(TInt i2) -> compare i1 i2
          | TConst(TString s1), TConst(TString s2) -> compare s1 s2
          | _ -> assert false
      in

      let odecl, odecl_f = List.sort sort_fn odecl, List.sort sort_fn odecl_f in

      let mk_arrdecl el t = { eexpr = TArrayDecl(el); etype = t; epos = pos } in
      let ret = {
        e with eexpr = TNew(cl,[],
          [
            mk_arrdecl (List.map fst odecl) (basic.tarray hasht);
            mk_arrdecl (List.map snd odecl) (basic.tarray t_empty);
            mk_arrdecl (List.map fst odecl_f) (basic.tarray hasht);
            mk_arrdecl (List.map snd odecl_f) (basic.tarray basic.tfloat)
          ]);
      } in
      match !exprs_before with
        | [] -> ret
        | block ->
          {
            eexpr = TBlock(List.rev block @ [ret]);
            etype = ret.etype;
            epos = ret.epos;
          }
    in
    do_objdecl

  let implement_dynamics ctx cl =
    let pos = cl.cl_pos in
    let is_override = is_override cl in
    if is_some cl.cl_dynamic then begin
      if is_first_dynamic cl then begin
        (*
          * add hx_hashes, hx_hashes_f, hx_dynamics, hx_dynamics_f to class
          * implement hx_deleteField
        *)
        let gen = ctx.rcf_gen in
        let basic = gen.gcon.basic in
        let hasht = if ctx.rcf_optimize then basic.tint else basic.tstring in

        let new_fields =
        [
          mk_class_field (gen.gmk_internal_name "hx" "hashes") (basic.tarray hasht) false pos (Var { v_read = AccNormal; v_write = AccNormal }) [];
          mk_class_field (gen.gmk_internal_name "hx" "dynamics") (basic.tarray t_empty) false pos (Var { v_read = AccNormal; v_write = AccNormal }) [];
          mk_class_field (gen.gmk_internal_name "hx" "hashes_f") (basic.tarray hasht) false pos (Var { v_read = AccNormal; v_write = AccNormal }) [];
          mk_class_field (gen.gmk_internal_name "hx" "dynamics_f") (basic.tarray basic.tfloat) false pos (Var { v_read = AccNormal; v_write = AccNormal }) [];
        ] in

        (if cl.cl_path <> (["haxe"; "lang"], "DynamicObject") then
          List.iter (fun cf -> cf.cf_expr <- Some { eexpr = TArrayDecl([]); etype = cf.cf_type; epos = cf.cf_pos }) new_fields
        );

        let delete = get_delete_field ctx cl true in
        List.iter (fun cf ->
          cl.cl_fields <- PMap.add cf.cf_name cf cl.cl_fields
        ) (delete :: new_fields);

		(*
        let rec last_ctor cl =
          match cl.cl_constructor with
            | None -> (match cl.cl_super with | None -> None | Some (cl,_) -> last_ctor cl)
            | Some c -> Some c
        in
		*)
        (*
          in order for the next to work, we need to execute our script before InitFunction, so the expressions inside the variables are initialized by the constructor
        *)
        (*
          Now we need to add their initialization.
          This will consist of different parts:
            Check if there are constructors. If not, create one and add initialization to it (calling super, ok)
            If there are, add as first statement (or second if there is a super() call in the first)
            If class has @:$DynamicObject meta, also create another new() class with its parameters as constructor arguments
        *)

        List.iter (fun cf ->
          cf.cf_expr <- Some({ eexpr = TArrayDecl([]); etype = cf.cf_type; epos = cf.cf_pos })
        ) new_fields;

        cl.cl_ordered_fields <- cl.cl_ordered_fields @ (delete :: new_fields);
        if is_override then cl.cl_overrides <- delete :: cl.cl_overrides
      end
    end else if not is_override then begin
      let delete = get_delete_field ctx cl false in
      cl.cl_ordered_fields <- cl.cl_ordered_fields @ [delete];
      cl.cl_fields <- PMap.add delete.cf_name delete cl.cl_fields
    end

  let implement_create_empty ctx cl =
    let gen = ctx.rcf_gen in
    let basic = gen.gcon.basic in
    let pos = cl.cl_pos in
    let is_override = is_override cl in
    let tparams = List.map (fun _ -> t_empty) cl.cl_types in

    let create =
      let arr = alloc_var "arr" (basic.tarray t_dynamic) in
      let tf_args = [ arr, None ] in
      let t = TFun(fun_args tf_args, t_dynamic) in
      let cf = mk_class_field (gen.gmk_internal_name "hx" "create") t false pos (Method MethNormal) [] in
      let i = ref 0 in

      let arr_local = mk_local arr pos in
      let ctor = if is_some cl.cl_constructor then cl.cl_constructor else get_last_ctor cl in
      let params = match ctor with
        | None -> []
        | Some ctor ->
          List.map (fun (n,_,t) ->
            let old = !i in
            incr i;
            {
              eexpr = TArray(arr_local, { eexpr = TConst(TInt (Int32.of_int old)); etype = basic.tint; epos = pos } );
              etype = t_dynamic;
              epos = pos
            }
          ) ( fst ( get_fun ctor.cf_type ) )
      in
      let expr = mk_return {
        eexpr = TNew(cl, tparams, params);
        etype = TInst(cl, tparams);
        epos = pos
      } in
      let fn = {
        eexpr = TFunction({
          tf_args = tf_args;
          tf_type = t_dynamic;
          tf_expr = mk_block expr
        });
        etype = t;
        epos = pos
      } in
      cf.cf_expr <- Some fn;
      cf
    in

    let create_empty =
      let t = TFun([],t_dynamic) in
      let cf = mk_class_field (gen.gmk_internal_name "hx" "createEmpty") t false pos (Method MethNormal) [] in
      let fn = {
        eexpr = TFunction({
          tf_args = [];
          tf_type = t_dynamic;
          tf_expr = mk_block (mk_return ( gen.gtools.rf_create_empty cl tparams pos ))
        });
        etype = t;
        epos = pos
      } in
      cf.cf_expr <- Some fn;
      cf
    in

    (* if rcf_handle_statics is false, there is no reason to make createEmpty/create not be static *)
    if ctx.rcf_handle_statics then begin
      cl.cl_ordered_fields <- cl.cl_ordered_fields @ [create_empty; create];
      cl.cl_fields <- PMap.add create_empty.cf_name create_empty cl.cl_fields;
      cl.cl_fields <- PMap.add create.cf_name create cl.cl_fields;
      if is_override then begin
        cl.cl_overrides <- create_empty :: create :: cl.cl_overrides
      end
    end else begin
      cl.cl_ordered_statics <- cl.cl_ordered_statics @ [create_empty; create];
      cl.cl_statics <- PMap.add create_empty.cf_name create_empty cl.cl_statics;
      cl.cl_statics <- PMap.add create.cf_name create cl.cl_statics
    end


  (*
    Implements:
      __hx_lookupField(field:String, throwErrors:Bool, isCheck:Bool, handleProperties:Bool, isFirst:Bool):Dynamic

      __hx_lookupField_f(field:String, throwErrors:Bool, handleProperties:Bool, isFirst:Bool):Float

      __hx_lookupSetField(field:String, value:Dynamic, handleProperties:Bool, isFirst:Bool):Dynamic;

      __hx_lookupSetField(field:String, value:Float, handleProperties:Bool, isFirst:Bool):Float;
  *)
  let implement_final_lookup ctx cl =
    let gen = ctx.rcf_gen in
    let basic = gen.gcon.basic in
    let pos = cl.cl_pos in
    let is_override = is_override cl in

    let this = { eexpr = TConst(TThis); etype = TInst(cl, List.map snd cl.cl_types); epos = pos } in

    (*
      this function will create the class fields and call callback for each version

      callback : is_float fields_args switch_var throw_errors_option is_check_option value_option : texpr list
    *)
    let create_cfs is_dynamic callback =
      let create_cf is_float is_set =
        let name = gen.gmk_internal_name "hx" ( (if is_set then "lookupSetField" else "lookupField") ^ (if is_float then "_f" else "") ) in
        let field_args, switch_var = field_type_args ctx pos in
        let ret_t = if is_float then basic.tfloat else t_dynamic in
        let tf_args, throw_errors_opt =
          if is_set then
            field_args, None
          else
            let v = alloc_var "throwErrors" basic.tbool in
            field_args @ [v,None], Some v
        in
        let tf_args, is_check_opt =
          if is_set || is_float then
            tf_args, None
          else
            let v = alloc_var "isCheck" basic.tbool in
            tf_args @ [v,None], Some v
        in
        let tf_args, value_opt =
          if not is_set then
            tf_args, None
          else
            let v = alloc_var "value" ret_t in
            field_args @ [v,None], Some v
        in

        let fun_t = TFun(fun_args tf_args, ret_t) in
        let cf = mk_class_field name fun_t false pos (Method MethNormal) [] in
        let block = callback is_float field_args switch_var throw_errors_opt is_check_opt value_opt in
        let block = if not is_set then let tl = begin
          let throw_errors_local = mk_local (get throw_errors_opt) pos in
          let mk_check_throw msg =
          {
            eexpr = TIf(throw_errors_local, mk_throw ctx msg pos, Some (mk_return (null ret_t pos)));
            etype = ret_t;
            epos = pos
          } in

          let mk_may_check_throw msg = if is_dynamic then mk_return (null ret_t pos) else mk_check_throw msg in
          if is_float then begin
            [
              mk_may_check_throw "Field not found or incompatible field type.";
            ]
          end else begin
            let undefined = alloc_var "__undefined__" t_dynamic in
            let undefined_local = mk_local undefined pos in
            let is_check_local = mk_local (get is_check_opt) pos in
            [
              {
                eexpr = TIf(is_check_local, mk_return undefined_local, Some( mk_may_check_throw "Field not found." ));
                etype = ret_t;
                epos = pos;
              }
            ]
          end
        end in block @ tl else block in
        cf.cf_expr <- Some(
          {
            eexpr = TFunction({
              tf_args = tf_args;
              tf_type = ret_t;
              tf_expr = { eexpr = TBlock(block); etype = ret_t; epos = pos }
            });
            etype = fun_t;
            epos = pos
          }
        );
        cf
      in
      let cfs =
      [
        create_cf false false;
        create_cf true false;
        create_cf false true;
        create_cf true true
      ] in
      cl.cl_ordered_fields <- cl.cl_ordered_fields @ cfs;
      List.iter (fun cf ->
        cl.cl_fields <- PMap.add cf.cf_name cf cl.cl_fields;
        if is_override then cl.cl_overrides <- cf :: cl.cl_overrides
      ) cfs
    in

    if is_some cl.cl_dynamic then begin
      (* let abstract_dyn_lookup_implementation ctx this hash_local may_value is_float pos = *)
      (* callback : is_float fields_args switch_var throw_errors_option is_check_option value_option : texpr list *)
      if is_first_dynamic cl then
        create_cfs true (fun is_float fields_args switch_var _ _ value_opt ->
          abstract_dyn_lookup_implementation ctx this (mk_local switch_var pos) (Option.map (fun v -> mk_local v pos) value_opt) is_float pos
        )
    end else if not is_override then begin
      create_cfs false (fun is_float fields_args switch_var _ _ value_opt ->
        match value_opt with
          | None -> (* is not set *)
            []
          | Some _ -> (* is set *)
            if is_float then
              [ mk_throw ctx "Cannot access field for writing or incompatible type." pos ]
            else
              [ mk_throw ctx "Cannot access field for writing." pos ]
      )
    end

  (* *)
  let implement_get_set ctx cl =
    let gen = ctx.rcf_gen in
    let mk_cfield is_set is_float =
      let pos = cl.cl_pos in
      let basic = ctx.rcf_gen.gcon.basic in
      let tf_args, switch_var = field_type_args ctx pos in
      let field_args = tf_args in
      let local_switch_var = { eexpr = TLocal(switch_var); etype = switch_var.v_type; epos = pos } in
      let is_static = alloc_var "isStatic" basic.tbool in
      let is_static_local = { eexpr = TLocal(is_static); etype = basic.tbool; epos = pos } in

      let handle_prop = alloc_var "handleProperties" basic.tbool in
      let handle_prop_local = mk_local handle_prop pos in

      let this = { eexpr = TConst TThis; etype = TInst(cl, List.map snd cl.cl_types); epos = pos } in
      let mk_this_call_raw name fun_t params =
        { eexpr = TCall( { (mk_field_access gen this name pos) with etype = fun_t; }, params ); etype = snd (get_args fun_t); epos = pos }
      in

      let tf_args = if ctx.rcf_handle_statics then tf_args @ [is_static, None] else tf_args in

      let fun_type = ref (TFun([], basic.tvoid)) in
      let fun_name = ctx.rcf_gen.gmk_internal_name "hx" ( (if is_set then "setField" else "getField") ^ (if is_float then "_f" else "") ) in
      let cfield = mk_class_field fun_name !fun_type false pos (Method MethNormal) [] in

      let maybe_cast e = e in

      let t = TInst(cl, List.map snd cl.cl_types) in

      (* if it's not latest hxgen class -> check super *)
      let mk_do_default args do_default =
        match cl.cl_super with
          | None -> fun () -> maybe_cast (do_default ())
          | Some (super, sparams) when not (is_hxgen (TClassDecl super)) ->
            fun () -> maybe_cast (do_default ())
          | _ ->
            fun () ->
              mk_return {
                eexpr = TCall(
                  { eexpr = TField({ eexpr = TConst TSuper; etype = t; epos = pos }, FInstance(cl, cfield)); etype = !fun_type; epos = pos },
                  (List.map (fun (v,_) -> mk_local v pos) args) );
                etype = if is_float then basic.tfloat else t_dynamic;
                epos = pos;
              };
      in

      (* if it is set function, there are some different set fields to do *)
      let do_default, do_default_static , do_field, tf_args = if is_set then begin
        let value_var = alloc_var "value" (if is_float then basic.tfloat else t_dynamic) in
        let value_local = { eexpr = TLocal(value_var); etype = value_var.v_type; epos = pos } in
        let tf_args = tf_args @ [value_var,None; handle_prop, None; ] in
        let lookup_name = gen.gmk_internal_name "hx" ("lookupSetField" ^ if is_float then "_f" else "") in

        let do_default =
            fun () ->
              mk_return (mk_this_call_raw lookup_name (TFun(fun_args (field_args @ [value_var,None]),value_var.v_type)) ( List.map (fun (v,_) -> mk_local v pos) field_args @ [ value_local ] ))
        in

        let do_field cf cf_type is_static =
          let get_field ethis = { eexpr = TField (ethis, if is_static then FStatic (cl, cf) else FInstance(cl, cf)); etype = cf_type; epos = pos } in
          let this = if is_static then mk_classtype_access cl pos else { eexpr = TConst(TThis); etype = t; epos = pos } in

          let ret =
          {
            eexpr = TBlock([
              {
                eexpr = TBinop(Ast.OpAssign,
                  get_field this,
                  mk_cast cf_type value_local);
                etype = cf_type;
                epos = pos;
              };
              mk_return value_local
            ]);
            etype = cf_type;
            epos = pos;
          } in
          match cf.cf_kind with
            | Var { v_write = AccCall fn } ->
              let bl =
              [
                mk_this_call_raw fn (TFun(["value",false,cf.cf_type], cf.cf_type)) [ value_local ];
                mk_return value_local
              ] in
              if Type.is_extern_field cf then
                { eexpr = TBlock bl; etype = value_local.etype; epos = pos }
              else
                {
                  eexpr = TIf(
                    handle_prop_local,
                    { eexpr = TBlock bl; etype = value_local.etype; epos = pos },
                    Some ret);
                  etype = value_local.etype;
                  epos = pos;
                }
            | _ ->
              ret
        in

        (mk_do_default tf_args do_default, do_default, do_field, tf_args)
      end else begin
        (* (field, isStatic, throwErrors, isCheck):Dynamic *)
        let throw_errors = alloc_var "throwErrors" basic.tbool in
        let throw_errors_local = mk_local throw_errors pos in
        let do_default, tf_args = if not is_float then begin
          let is_check = alloc_var "isCheck" basic.tbool in
          let is_check_local = mk_local is_check pos in

          let tf_args = tf_args @ [ throw_errors,None; ] in

          (* default: if (isCheck) return __undefined__ else if(throwErrors) throw "Field not found"; else return null; *)
          let lookup_name = gen.gmk_internal_name "hx" "lookupField" in
          let do_default =
              fun () ->
                mk_return (mk_this_call_raw lookup_name (TFun(fun_args (field_args @ [throw_errors,None;is_check,None; ]),t_dynamic)) ( List.map (fun (v,_) -> mk_local v pos) field_args @ [ throw_errors_local; is_check_local; ] ))
          in

          (do_default, tf_args @ [ is_check,None; handle_prop,None; ])
        end else begin
          let tf_args = tf_args @ [ throw_errors,None; ] in

          let lookup_name = gen.gmk_internal_name "hx" "lookupField_f" in
          let do_default =
              fun () ->
                mk_return (mk_this_call_raw lookup_name (TFun(fun_args (field_args @ [throw_errors,None; ]),basic.tfloat)) ( List.map (fun (v,_) -> mk_local v pos) field_args @ [ throw_errors_local; ] ))
          in

          (do_default, tf_args @ [ handle_prop,None; ])
        end in

         let get_field cf cf_type ethis cl name =
          match cf.cf_kind with
            | Var { v_read = AccCall fn } when Type.is_extern_field cf ->
              mk_return (mk_this_call_raw fn (TFun(["value",false,cf.cf_type], cf.cf_type)) [  ])
            | Var { v_read = AccCall fn } ->
              {
                eexpr = TIf(
                  handle_prop_local,
                  mk_return (mk_this_call_raw fn (TFun(["value",false,cf.cf_type], cf.cf_type)) [  ]),
                  Some { eexpr = TField (ethis, FInstance(cl, cf)); etype = cf_type; epos = pos }
                );
                etype = cf_type;
                epos = pos;
              }
            | Var _
            | Method MethDynamic -> { eexpr = TField (ethis, FInstance(cl,cf)); etype = cf_type; epos = pos }
            | _ ->
                { eexpr = TField (this, FClosure(Some cl, cf)); etype = cf_type; epos = pos }
        in

        let do_field cf cf_type static =
          let this = if static then mk_classtype_access cl pos else { eexpr = TConst(TThis); etype = t; epos = pos } in
          match is_float, follow cf_type with
            | true, TInst( { cl_kind = KTypeParameter _ }, [] ) ->
              mk_return (mk_cast basic.tfloat (mk_cast t_dynamic (get_field cf cf_type this cl cf.cf_name)))
            | _ ->
              mk_return (maybe_cast (get_field cf cf_type this cl cf.cf_name ))
        in
        (mk_do_default tf_args do_default, do_default, do_field, tf_args)
      end in

      let get_fields static =
        let ret = collect_fields cl ( if is_float || is_set then Some (false) else None ) (Some static) in
        let ret = if is_set then List.filter (fun (_,cf) -> not (Meta.has Meta.ReadOnly cf.cf_meta)) ret else ret in
        if is_float then
          List.filter (fun (_,cf) -> (* TODO: maybe really apply_params in cf.cf_type. The benefits would be limited, though *)
            match follow (ctx.rcf_gen.greal_type (ctx.rcf_gen.gfollow#run_f cf.cf_type)) with
              | TDynamic _ | TMono _
              | TInst ({ cl_kind = KTypeParameter _ }, _) -> true
              | t when like_float t -> true
              | _ -> false
          ) ret
        else
          (* dynamic will always contain all references *)
          ret
      in

      (* now we have do_default, do_field and tf_args *)
      (* so create the switch expr *)
      fun_type := TFun(List.map (fun (v,_) -> (v.v_name, false, v.v_type)) tf_args, if is_float then basic.tfloat else t_dynamic );
      let has_fields = ref false in

      let mk_switch static =
        let fields = get_fields static in
        let fields = List.filter (fun (_, cf) -> match is_set, cf.cf_kind with
          | true, Var { v_write = AccCall _ } -> true
          | false, Var { v_read = AccCall _ } -> true
          | _ -> not (Type.is_extern_field cf)) fields
        in
        (if fields <> [] then has_fields := true);
        let cases = List.map (fun (names, cf) ->
          (if names = [] then assert false);
          (List.map (switch_case ctx pos) names, do_field cf cf.cf_type static)
        ) fields in
        let default = Some(if static then do_default_static() else do_default()) in

        { eexpr = TSwitch(local_switch_var, cases, default); etype = basic.tvoid; epos = pos }
      in

      let content = if ctx.rcf_handle_statics then
        mk_block { eexpr = TIf(is_static_local, mk_switch true, Some(mk_switch false)); etype = basic.tvoid; epos = pos }
      else
        mk_block (mk_switch false)
      in

      let is_override = match cl.cl_super with
        | Some (cl, _) when is_hxgen (TClassDecl cl) -> true
        | _ -> false
      in

      if !has_fields || (not is_override) then begin
        let func =
        {
          tf_args = tf_args;
          tf_type = if is_float then basic.tfloat else t_dynamic;
          tf_expr = content;
        } in

        let func = { eexpr = TFunction(func); etype = !fun_type; epos = pos } in

        cfield.cf_type <- !fun_type;
        cfield.cf_expr <- Some func;

        cl.cl_ordered_fields <- cl.cl_ordered_fields @ [cfield];
        cl.cl_fields <- PMap.add fun_name cfield cl.cl_fields;

        (if is_override then cl.cl_overrides <- cfield  :: cl.cl_overrides)
      end else ()
    in
    (if ctx.rcf_float_special_case then mk_cfield true true);
    mk_cfield true false;
    mk_cfield false false;
    (if ctx.rcf_float_special_case then mk_cfield false true)

  let mk_field_access_r ctx pos local field is_float is_static throw_errors set_option =
    let is_set = is_some set_option in
    let gen = ctx.rcf_gen in
    let basic = gen.gcon.basic in

    let fun_name = ctx.rcf_gen.gmk_internal_name "hx" ( (if is_set then "setField" else "getField") ^ (if is_float then "_f" else "") ) in
    let tf_args, _ = field_type_args ctx pos in
    let tf_args, args = fun_args tf_args, field in

    let rett = if is_float then basic.tfloat else t_dynamic in
    let tf_args, args = if ctx.rcf_handle_statics then tf_args @ [ "isStatic", false, basic.tbool ], args @ [is_static] else tf_args, args in
    let tf_args, args = if is_set then tf_args @ [ "setVal", false, rett ], args @ [get set_option] else tf_args, args in
    let tf_args, args = tf_args @ [ "throwErrors",false,basic.tbool ], args @ [throw_errors] in
    let tf_args, args = if is_set || is_float then tf_args, args else tf_args @ [ "isCheck", false, basic.tbool ], args @ [{ eexpr = TConst(TBool false); etype = basic.tbool; epos = pos }] in
    let tf_args, args = tf_args @ [ "handleProperties",false,basic.tbool; ], args @ [ mk_bool ctx false pos; ] in

    {
      eexpr = TCall(
        { (mk_field_access gen local fun_name pos) with etype = TFun(tf_args, rett) },
        args);
      etype = rett;
      epos = pos;
    }


  let implement_fields ctx cl =
    (*
      implement two kinds of fields get:
        classFields
        generic 'fields': receives a parameter isInstance
          will receive an Array<String> and start pushing the fields into it.
          //add all common fields
          if(isInstance)
          {
            //add methods
          } else {
            super.fields(isInstance, array);
          }
    *)
    let gen = ctx.rcf_gen in
    let basic = gen.gcon.basic in
    let pos = cl.cl_pos in
	(*
    let rec has_no_dynamic cl =
      if is_some cl.cl_dynamic then
        false
      else match cl.cl_super with
        | None -> true
        | Some(cl,_) -> has_no_dynamic cl
    in
	*)
    (* Type.getClassFields() *)
    if ctx.rcf_handle_statics then begin
      let name = gen.gmk_internal_name "hx" "classFields" in
      let v_base_arr = alloc_var "baseArr" (basic.tarray basic.tstring) in
      let base_arr = mk_local v_base_arr pos in

      let tf_args = [v_base_arr,None] in
      let t = TFun(fun_args tf_args, basic.tvoid) in
      let cf = mk_class_field name t false pos (Method MethNormal) [] in
      cl.cl_ordered_fields <- cl.cl_ordered_fields @ [cf];
      cl.cl_fields <- PMap.add cf.cf_name cf cl.cl_fields;
      (if is_override cl then cl.cl_overrides <- cf :: cl.cl_overrides);
      (*
        var newarr = ["field1", "field2"] ...;
      *)
      let fields = collect_fields cl None (Some true) in
      let mk_push value =
        { eexpr = TCall({ (mk_field_access gen base_arr "push" pos) with etype = TFun(["x", false, basic.tstring], basic.tint) }, [value] ); etype = basic.tint; epos = pos }
      in

      let new_arr_contents =
      {
        eexpr = TBlock(
          List.map (fun (_,cf) -> mk_push { eexpr = TConst(TString(cf.cf_name)); etype = basic.tstring; epos = pos }) fields
        );
        etype = basic.tvoid;
        epos = pos
      } in

      let expr = new_arr_contents in
      let fn =
      {
        tf_args = tf_args;
        tf_type = basic.tvoid;
        tf_expr = mk_block expr
      } in

      cf.cf_expr <- Some { eexpr = TFunction(fn); etype = t; epos = pos }
    end;

    let fields =
      (*
        function __hx_fields(baseArr:Array<String>, isInstanceFields:Bool)
        {
          //add all variable fields
          //then:
          if (isInstanceFields)
          {
            //add all method fields as well
          } else {
            super.__hx_fields(baseArr, isInstanceFields);
          }
        }
      *)
      let name = gen.gmk_internal_name "hx" "getFields" in
      let v_base_arr, v_is_inst = alloc_var "baseArr" (basic.tarray basic.tstring), alloc_var "isInstanceFields" basic.tbool in
      let base_arr, is_inst = mk_local v_base_arr pos, mk_local v_is_inst pos in

      let tf_args = (v_base_arr,None) :: (if ctx.rcf_handle_statics then [v_is_inst, None] else []) in
      let t = TFun(fun_args tf_args, basic.tvoid) in
      let cf = mk_class_field name t false pos (Method MethNormal) [] in

      let mk_push value =
        { eexpr = TCall({ (mk_field_access gen base_arr "push" pos) with etype = TFun(["x", false, basic.tstring], basic.tint); }, [value] ); etype = basic.tint; epos = pos }
      in

      let has_value = ref false in
      let map_fields =
        List.map (fun (_,cf) ->
          match cf.cf_kind with
            | Var _
            | Method MethDynamic when not (List.memq cf cl.cl_overrides) ->
              has_value := true;
              mk_push { eexpr = TConst(TString(cf.cf_name)); etype = basic.tstring; epos = pos }
            | _ -> null basic.tvoid pos
        )
      in

      (*
        if it is first_dynamic, then we need to enumerate the dynamic fields
      *)
      let if_not_inst = if is_some cl.cl_dynamic && is_first_dynamic cl then begin
        has_value := true;
        Some (enumerate_dynamic_fields ctx cl mk_push)
      end else
        None
      in

      let if_not_inst = if is_override cl then
        Some(
          {
            eexpr = TBlock(
              (if is_some if_not_inst then get if_not_inst else []) @
              [{
                eexpr = TCall(
                  { eexpr = TField({ eexpr = TConst TSuper; etype = TInst(cl, List.map snd cl.cl_types); epos = pos }, FInstance(cl, cf)); etype = t; epos = pos },
                  base_arr :: (if ctx.rcf_handle_statics then [is_inst] else [])
                );
                etype = basic.tvoid;
                epos = pos
              }]
            );
            etype = basic.tvoid;
            epos = pos
          }
        ) else if is_some if_not_inst then
          Some({ eexpr = TBlock(get if_not_inst); etype = basic.tvoid; epos = pos })
        else
          None
      in

      let expr_contents = map_fields (collect_fields cl (Some false) (Some false)) in
      let expr_contents = if ctx.rcf_handle_statics then
        expr_contents @
        [ {
          eexpr = TIf(is_inst,
            { eexpr = TBlock( map_fields (collect_fields cl (Some true) (Some false)) ); etype = basic.tvoid; epos = pos },
            if_not_inst
          );
          etype = basic.tvoid;
          epos = pos
        } ]
      else
        expr_contents @ (if is_some if_not_inst then [ get if_not_inst ] else [])
      in

      let expr =
      {
        eexpr = TBlock( expr_contents );
        etype = basic.tvoid;
        epos = pos;
      } in

      let fn =
      {
        tf_args = tf_args;
        tf_type = basic.tvoid;
        tf_expr = expr
      } in

      (if !has_value || (not (is_override cl)) then begin
        cl.cl_ordered_fields <- cl.cl_ordered_fields @ [cf];
        cl.cl_fields <- PMap.add cf.cf_name cf cl.cl_fields;
        (if is_override cl then cl.cl_overrides <- cf :: cl.cl_overrides)
      end);
      cf.cf_expr <- Some { eexpr = TFunction(fn); etype = t; epos = pos }
    in
    ignore fields

  let implement_class_methods ctx cl =
    ctx.rcf_class_cl <- Some cl;

    let pos = cl.cl_pos in
    let gen = ctx.rcf_gen in
    let basic = gen.gcon.basic in
    (*
      fields -> redirected to classFields
      getField -> redirected to getField with isStatic true
      setField -> isStatic true
      invokeField -> isStatic true
      getClass -> null
      create -> proxy
      createEmpty -> proxy
    *)
    let is_override = is_override cl in
    let name = "classProxy" in
    let t = (TInst(ctx.rcf_object_iface,[])) in
    (* let cf = mk_class_field name t false pos (Var { v_read = AccNormal; v_write = AccNormal }) [] in *)
    let register_cf cf override =
      cl.cl_ordered_fields <- cf :: cl.cl_ordered_fields;
      cl.cl_fields <- PMap.add cf.cf_name cf cl.cl_fields;
      if override then cl.cl_overrides <- cf :: cl.cl_overrides
    in
    (* register_cf cf false; *)

    let this_t = TInst(cl, List.map snd cl.cl_types) in
    let this = { eexpr = TConst(TThis); etype = this_t; epos = pos } in
    let mk_this field t = { (mk_field_access gen this field pos) with etype = t } in
    let proxy = mk_this name t in

    (*let ctor =
      let cls = alloc_var "cls" t in
      let tf_args = [cls, None] in
      let t = TFun(fun_args tf_args, basic.tvoid) in
      let cf = mk_class_field "new" t true pos (Method MethNormal) [] in
      cf.cf_expr <- Some({
        eexpr = TFunction({
          tf_args = tf_args;
          tf_type = basic.tvoid;
          tf_expr = mk_block {
            eexpr = TBinop(Ast.OpAssign, proxy, mk_local cls pos);
            etype = cls.v_type;
            epos = pos;
          }
        });
        etype = t;
        epos = pos;
      });
      cf
    in
    register_cf ctor false;*)

    (* setting it as DynamicObject makes getClass return null *)
    let get_class =
      cl.cl_meta <- (Meta.DynamicObject, [], pos) :: cl.cl_meta
    in
    ignore get_class;

    (* fields -> if isInstanceField, redir the method. If not, return classFields *)
    let fields =
      let name = gen.gmk_internal_name "hx" "getFields" in
      let v_base_arr, v_is_inst = alloc_var "baseArr" (basic.tarray basic.tstring), alloc_var "isInstanceFields" basic.tbool in
      let base_arr, is_inst = mk_local v_base_arr pos, mk_local v_is_inst pos in

      let tf_args = [ v_base_arr,None; v_is_inst, None ] in
      let t = TFun(fun_args tf_args, basic.tvoid) in
      let cf = mk_class_field name t false pos (Method MethNormal) [] in
      cf.cf_expr <- Some({
        eexpr = TFunction({
          tf_args = tf_args;
          tf_type = basic.tvoid;
          tf_expr = mk_block {
            eexpr = TIf(is_inst,
              { eexpr = TCall( { (mk_field_access gen proxy name pos) with etype = t }, [base_arr;is_inst]); etype = basic.tvoid; epos = pos },
              Some { eexpr = TCall(mk_this (gen.gmk_internal_name "hx" "classFields") (TFun(["baseArr",false,basic.tarray basic.tstring], basic.tvoid)), [base_arr]); etype = basic.tvoid; epos = pos });
            etype = basic.tvoid;
            epos = pos
          }
        });
        etype = t;
        epos = pos;
      });
      cf
    in
    register_cf fields (is_override);

    let do_proxy field tf_args ret is_static_argnum =
      let field = gen.gmk_internal_name "hx" field in
      let t = TFun(fun_args tf_args, ret) in
      let cf = mk_class_field field t false pos (Method MethNormal) [] in
      let is_void = is_void ret in
      let may_return e = if is_void then mk_block e else mk_block (mk_return e) in
      let i = ref 0 in
      cf.cf_expr <- Some({
        eexpr = TFunction({
          tf_args = tf_args;
          tf_type = ret;
          tf_expr = may_return {
            eexpr = TCall(
              { (mk_field_access gen proxy field pos) with etype = t },
              List.map (fun (v,_) ->
                let lasti = !i in
                incr i;
                if lasti = is_static_argnum then
                  { eexpr = TConst(TBool true); etype = basic.tbool; epos = pos }
                else
                  mk_local v pos
              ) tf_args);
            etype = ret;
            epos = pos
          }
        });
        etype = t;
        epos = pos;
      });
      cf
    in

    (* getClassFields -> redir *)
    register_cf (do_proxy "classFields" [ alloc_var "baseArr" (basic.tarray basic.tstring), None ] basic.tvoid (-1)) true;

    (*register_cf (do_proxy "classFields" [ alloc_var "baseArr" (basic.tarray basic.tstring), None ] basic.tvoid (-1)) true;*)

    let fst_args, _ = field_type_args ctx pos in
    let fst_args_len = List.length fst_args in

    (* getField -> redir the method with static = true *)
    (* setField -> redir the methods with static = true *)
    (if ctx.rcf_float_special_case then
      register_cf (do_proxy "getField_f" (fst_args @ [ alloc_var "isStatic" basic.tbool, None; alloc_var "throwErrors" basic.tbool, None ]) basic.tfloat fst_args_len) true;
      register_cf (do_proxy "setField_f" (fst_args @ [ alloc_var "isStatic" basic.tbool, None; alloc_var "value" basic.tfloat, None ]) basic.tfloat fst_args_len) true
    );
    register_cf (do_proxy "getField" (fst_args @ [ alloc_var "isStatic" basic.tbool, None; alloc_var "throwErrors" basic.tbool, None; alloc_var "isCheck" basic.tbool, None; alloc_var "handleProperties" basic.tbool,None; ]) t_dynamic fst_args_len) true;
    register_cf (do_proxy "setField" (fst_args @ [ alloc_var "isStatic" basic.tbool, None; alloc_var "value" t_dynamic, None; alloc_var "handleProperties" basic.tbool,None; ]) t_dynamic fst_args_len) true;

    (* invokeField -> redir the method with static = true *)
    register_cf (do_proxy "invokeField" (fst_args @ [ alloc_var "isStatic" basic.tbool, None; alloc_var "dynArgs" (basic.tarray t_dynamic), None ]) t_dynamic fst_args_len) true;

    (* create / createEmpty -> redir the method *)
    register_cf (do_proxy "create" [ alloc_var "arr" (basic.tarray t_dynamic), None ] t_dynamic (-1)) true;
    register_cf (do_proxy "createEmpty" [ ] t_dynamic (-1)) true

  let implement_get_class ctx cl =
    (*
      if it is DynamicObject, return null;
      if it is not, just do the following:
        if (typehandle(this.class) == typehandle(MyClass))
          return (MyClass.__hx_class != null ? MyClass.__hx_class : MyClass.__hx_class = create_empty(MyClass));
        return MyClass.__hx_class = haxe.lang.Runtime.getClass(MyClass);

      implement both on static and non-static contexts. This way we can call without references.
    *)
    let gen = ctx.rcf_gen in
    let basic = gen.gcon.basic in
    let pos = cl.cl_pos in

    let tclass = get_cl ( (Hashtbl.find gen.gtypes ([],"Class")) ) in
    let cls = TInst(tclass, [ TInst(cl, List.map (fun _ -> t_dynamic) cl.cl_types) ]) in
    let cls_dyn = TInst(tclass, [t_dynamic]) in

    let expr, static_cfs =
      if Meta.has Meta.DynamicObject cl.cl_meta then
        mk_return (null t_dynamic pos), []
      else
        let cache_name = (gen.gmk_internal_name "hx" "class") in
        let cache = mk_class_field cache_name cls false pos (Var { v_read = AccNormal; v_write = AccNormal }) [] in
        cl.cl_ordered_statics <- cl.cl_ordered_statics @ [ cache ];
        cl.cl_statics <- PMap.add cache_name cache cl.cl_statics;

        let cache_access = mk_static_field_access cl cache_name cls pos in


        let create_expr = {
          eexpr = TNew(get ctx.rcf_class_cl, [], [gen.gtools.rf_create_empty cl (List.map (fun _ -> t_dynamic) cl.cl_types) pos]);
          etype = cls;
          epos = pos
        } in

        (if ctx.rcf_class_eager_creation then cache.cf_expr <- Some(create_expr));

        let expr = if ctx.rcf_class_eager_creation then
          mk_return cache_access
        else
          mk_return {
            eexpr = TIf(
              { eexpr = TBinop(Ast.OpNotEq, cache_access, null cls pos); etype = basic.tbool; epos = pos },
              cache_access,
              Some({ eexpr = TBinop(Ast.OpAssign, cache_access, create_expr); etype = cls; epos = pos })
            );
            etype = cls;
            epos = pos
          }
        in
        expr, []
    in

    let func =
    {
      eexpr = TFunction({
        tf_args = [];
        tf_type = cls_dyn;
        tf_expr = expr
      });
      etype = TFun([],cls_dyn);
      epos = pos
    } in

    let get_cl_static = mk_class_field (gen.gmk_internal_name "hx" "getClassStatic") (TFun([],cls_dyn)) false pos (Method MethNormal) [] in
    let get_cl = mk_class_field (gen.gmk_internal_name "hx" "getClass") (TFun([],cls_dyn)) false pos (Method MethNormal) [] in

    get_cl_static.cf_expr <- Some func;
    get_cl.cf_expr <- Some func;

    let all_f = [get_cl] in
    cl.cl_ordered_fields <- cl.cl_ordered_fields @ all_f;
    List.iter (fun cf -> cl.cl_fields <- PMap.add cf.cf_name cf cl.cl_fields) all_f;

    let all_f = get_cl_static :: static_cfs in
    cl.cl_ordered_statics <- cl.cl_ordered_statics @ all_f;
    List.iter (fun cf -> cl.cl_statics <- PMap.add cf.cf_name cf cl.cl_statics) all_f;

    if is_override cl then cl.cl_overrides <- get_cl :: cl.cl_overrides

  let implement_invokeField ctx ~slow_invoke cl =
    (*
      There are two ways to implement an haxe reflection-enabled class:
      When we extend a non-hxgen class, and when we extend the base HxObject class.

      Because of the added boiler plate we'd add every time we extend a non-hxgen class to implement a big IHxObject
      interface, we'll handle the cases differently when implementing each interface.

      At the IHxObject interface, there's only invokeDynamic(field, args[]), while at the HxObject class there are
      the other, more optimized methods, that follow the Function class interface.

      Since this will only be called by the Closure class, this conversion can be properly dealt with later.

      TODO: create the faster version. By now only invokeDynamic will be implemented
    *)
    let gen = ctx.rcf_gen in
    let basic = gen.gcon.basic in
    let pos = cl.cl_pos in

    let has_method = ref false in

    let is_override = ref false in
    let rec extends_hxobject cl =
      match cl.cl_super with
        | None -> true
        | Some (cl,_) when is_hxgen (TClassDecl cl) -> is_override := true; extends_hxobject cl
        | _ -> false
    in

    let field_args, switch_var = field_type_args ctx cl.cl_pos in
    let field_args_exprs = List.map (fun (v,_) -> mk_local v pos) field_args in

    let is_static = alloc_var "isStatic" basic.tbool in
    let dynamic_arg = alloc_var "dynargs" (basic.tarray t_dynamic) in
    let all_args = field_args @ (if ctx.rcf_handle_statics then [ is_static,None; dynamic_arg,None ] else [ dynamic_arg, None ] ) in
    let fun_t = TFun(fun_args all_args, t_dynamic) in

    let this_t = TInst(cl, List.map snd cl.cl_types) in
    let this = { eexpr = TConst(TThis); etype = this_t; epos = pos } in
    let apply_object cf = apply_params cf.cf_params (List.map (fun _ -> t_dynamic) cf.cf_params) cf.cf_type in

    let mk_this_call_raw name fun_t params =
      { eexpr = TCall( { (mk_field_access gen this name pos) with etype = fun_t }, params ); etype = snd (get_args fun_t); epos = pos }
    in

    let mk_this_call cf params =
      let t = apply_object cf in
      (* the return type transformation into Dynamic *)
      (* is meant to avoid return-type casting after functions with *)
      (* type parameters are properly inferred at TypeParams.infer_params *)
      (* e.g. function getArray<T : SomeType>(t:T):Array<T>; after infer_params, *)
      (* T will be inferred as SomeType, but the returned type will still be typed *)
      (* as Array<Dynamic> *)
      let args, ret = get_args t in
      let ret = match follow ret with
        | TEnum({ e_path = ([], "Void") }, [])
        | TAbstract ({ a_path = ([], "Void") },[]) -> ret
        | _ -> t_dynamic
      in
      mk_this_call_raw cf.cf_name (TFun(args, ret)) params
    in

    let mk_static_call cf params =
      let t = apply_object cf in
      let _, ret = get_fun (follow t) in
      { eexpr = TCall( mk_static_field_access cl cf.cf_name t pos, params ); etype = ret; epos = pos }
    in

    let extends_hxobject = extends_hxobject cl in
    ignore extends_hxobject;
    (* creates a dynamicInvoke of the class fields listed here *)
    (*
      function dynamicInvoke(field, isStatic, dynargs)
      {
        switch(field)
        {
          case "a": this.a(dynargs[0], dynargs[1], dynargs[2]...);
          default: super.dynamicInvoke //or this.getField(field).invokeField(dynargs)
        }
      }
    *)

    let dyn_fun = mk_class_field (ctx.rcf_gen.gmk_internal_name "hx" "invokeField") fun_t false cl.cl_pos (Method MethNormal) [] in

    let mk_switch_dyn cfs static old =
      (* mk_class_field name t public pos kind params = *)

      let get_case (names,cf) =
        has_method := true;
        let i = ref 0 in
        let dyn_arg_local = mk_local dynamic_arg pos in
        let cases = List.map (switch_case ctx pos) names in
        (cases,
          { eexpr = TReturn(Some ( (if static then mk_static_call else mk_this_call) cf (List.map (fun (name,_,t) ->
              let ret = { eexpr = TArray(dyn_arg_local, mk_int ctx !i pos); etype = t_dynamic; epos = pos } in
              incr i;
              ret
            ) (fst (get_args (cf.cf_type))) ) ));
            etype = basic.tvoid;
            epos = pos
          }
        )
      in

      let cfs = List.filter (fun (_,cf) -> match cf.cf_kind with
        | Method _ -> if List.memq cf cl.cl_overrides then false else true
        | _ -> true) cfs
      in

      let cases = List.map get_case cfs in
      let cases = match old with
        | [] -> cases
        | _ ->
          let ncases = List.map (fun cf -> switch_case ctx pos cf.cf_name) old in
          ( ncases, mk_return ((get slow_invoke) this (mk_local (fst (List.hd field_args)) pos) (mk_local dynamic_arg pos)) ) :: cases
      in

      let default = if !is_override && not(static) then
        (* let call_super ctx fn_args ret_t fn_name this_t pos = *)
        { eexpr = TReturn(Some (call_super ctx all_args t_dynamic dyn_fun cl this_t pos) ); etype = basic.tvoid; epos = pos }
      (*else if ctx.rcf_create_getsetinvoke_fields then (* we always need to run create_getset before *)
        let get_field_name = gen.gmk_internal_name "hx" "getField" in
        { eexpr = TReturn( Some (mk_this_call (PMap.find get_field_name cl.cl_fields) [mk_local dynamic_arg pos] ) ); etype = basic.tvoid; epos = pos }*)
      else (
        (*let field = (gen.gtools.r_field false (TInst(ctx.rcf_ft.func_class,[])) this (mk_local (fst (List.hd all_args)) pos)) in*)
        (* let mk_field_access ctx pos local field is_float is_static throw_errors set_option = *)
        let field = mk_field_access_r ctx pos this field_args_exprs false {eexpr = TConst(TBool static); etype = basic.tbool; epos = pos} { eexpr = TConst(TBool true); etype = basic.tbool; epos = pos } None in
        let field = mk_cast (TInst(ctx.rcf_ft.func_class,[])) field in
        mk_return {
          eexpr = TCall(
            mk_field_access gen field (gen.gmk_internal_name "hx" "invokeDynamic") pos,
            [mk_local dynamic_arg pos]);
          etype = t_dynamic;
          epos = pos
        } )
      in

      {
        eexpr = TSwitch(mk_local switch_var pos, cases, Some default);
        etype = basic.tvoid;
        epos = pos;
      }
    in

    let contents =
      let statics = collect_fields cl (Some true) (Some true) in
      let nonstatics = collect_fields cl (Some true) (Some false) in

      let old_nonstatics = ref [] in

      let nonstatics = match slow_invoke with
        | None -> nonstatics
        | Some _ ->
          List.filter (fun (n,cf) ->
            let is_old = not (PMap.mem cf.cf_name cl.cl_fields) || List.memq cf cl.cl_overrides in
            (if is_old then old_nonstatics := cf :: !old_nonstatics);
            not is_old
          ) nonstatics
      in

      if ctx.rcf_handle_statics then
      {
        eexpr = TIf(mk_local is_static pos, mk_switch_dyn statics true [], Some(mk_switch_dyn nonstatics false !old_nonstatics));
        etype = basic.tvoid;
        epos = pos;
      } else
        mk_switch_dyn nonstatics false !old_nonstatics
    in

    dyn_fun.cf_expr <- Some
      {
        eexpr = TFunction(
        {
          tf_args = all_args;
          tf_type = t_dynamic;
          tf_expr = mk_block contents;
        });
        etype = TFun(fun_args all_args, t_dynamic);
        epos = pos;
      };
    if !is_override && not (!has_method) then () else begin
      cl.cl_ordered_fields <- cl.cl_ordered_fields @ [dyn_fun];
      cl.cl_fields <- PMap.add dyn_fun.cf_name dyn_fun cl.cl_fields;
      (if !is_override then cl.cl_overrides <- dyn_fun :: cl.cl_overrides)
    end

  let implement_varargs_cl ctx cl =
    let pos = cl.cl_pos in
    let gen = ctx.rcf_gen in
    let basic = gen.gcon.basic in

    let this_t = TInst(cl, List.map snd cl.cl_types) in
    let this = { eexpr = TConst(TThis); etype = this_t ; epos = pos } in
    let mk_this field t = { (mk_field_access gen this field pos) with etype = t } in

    let invokedyn = gen.gmk_internal_name "hx" "invokeDynamic" in
    let idyn_t = TFun([gen.gmk_internal_name "fn" "dynargs", false, basic.tarray t_dynamic], t_dynamic) in
    let this_idyn = mk_this invokedyn idyn_t in

    let map_fn arity ret vars api =

      let rec loop i acc =
        if i < 0 then
          acc
        else
          let obj = api i t_dynamic None in
          loop (i - 1) (obj :: acc)
      in

      let call_arg = if arity = (-1) then
        api (-1) t_dynamic None
      else if arity = 0 then
        null (basic.tarray t_empty) pos
      else
        { eexpr = TArrayDecl(loop (arity - 1) []); etype = basic.tarray t_empty; epos = pos }
      in

      let expr = {
        eexpr = TCall(
          this_idyn,
          [ call_arg ]
        );
        etype = t_dynamic;
        epos = pos
      } in

      let expr = if like_float ret && not (like_int ret) then mk_cast ret expr else expr in

      [], mk_return expr
    in

    let all_cfs = List.filter (fun cf -> cf.cf_name <> "new" && cf.cf_name <> (invokedyn) && match cf.cf_kind with Method _ -> true | _ -> false) (ctx.rcf_ft.map_base_classfields cl true map_fn) in

    cl.cl_ordered_fields <- cl.cl_ordered_fields @ all_cfs;
    List.iter (fun cf ->
      cl.cl_fields <- PMap.add cf.cf_name cf cl.cl_fields
    ) all_cfs;

    List.iter (fun cf ->
      cl.cl_overrides <- cf :: cl.cl_overrides
    ) cl.cl_ordered_fields

  let implement_closure_cl ctx cl =
    let pos = cl.cl_pos in
    let gen = ctx.rcf_gen in
    let basic = gen.gcon.basic in

    let field_args, _ = field_type_args ctx pos in
    let obj_arg = alloc_var "target" (TInst(ctx.rcf_object_iface, [])) in

    let this_t = TInst(cl, List.map snd cl.cl_types) in
    let this = { eexpr = TConst(TThis); etype = this_t ; epos = pos } in
    let mk_this field t = { (mk_field_access gen this field pos) with etype = t } in

    let tf_args = field_args @ [obj_arg, None] in
    let cfs, ctor_body = List.fold_left (fun (acc_cf,acc_expr) (v,_) ->
      let cf = mk_class_field v.v_name v.v_type false pos (Var { v_read = AccNormal; v_write = AccNormal } ) [] in
      let expr = { eexpr = TBinop(Ast.OpAssign, mk_this v.v_name v.v_type, mk_local v pos); etype = v.v_type; epos = pos } in
      (cf :: acc_cf, expr :: acc_expr)
    ) ([], [])  tf_args in

    let map_fn arity ret vars api =
      let this_obj = mk_this "target" (TInst(ctx.rcf_object_iface, [])) in

      let rec loop i acc =
        if i < 0 then
          acc
        else
          let obj = api i t_dynamic None in
          loop (i - 1) (obj :: acc)
      in

      let call_arg = if arity = (-1) then
        api (-1) t_dynamic None
      else if arity = 0 then
        null (basic.tarray t_empty) pos
      else
        { eexpr = TArrayDecl(loop (arity - 1) []); etype = basic.tarray t_empty; epos = pos }
      in

      let expr = {
        eexpr = TCall(
          mk_field_access gen this_obj (gen.gmk_internal_name "hx" "invokeField") pos,
          (List.map (fun (v,_) -> mk_this v.v_name v.v_type) field_args) @
            (if ctx.rcf_handle_statics then
              [ { eexpr = TConst(TBool false); etype = basic.tbool; epos = pos }; call_arg ]
            else
              [ call_arg ]
            )
        );
        etype = t_dynamic;
        epos = pos
      } in

      let expr = if like_float ret && not (like_int ret) then mk_cast ret expr else expr in

      [], mk_return expr
    in

    let all_cfs = List.filter (fun cf -> cf.cf_name <> "new" && match cf.cf_kind with Method _ -> true | _ -> false) (ctx.rcf_ft.map_base_classfields cl true map_fn) in

    List.iter (fun cf ->
      cl.cl_overrides <- cf :: cl.cl_overrides
    ) all_cfs;
    let all_cfs = cfs @ all_cfs in

    cl.cl_ordered_fields <- cl.cl_ordered_fields @ all_cfs;
    List.iter (fun cf ->
      cl.cl_fields <- PMap.add cf.cf_name cf cl.cl_fields
    ) all_cfs;

    let ctor_t = TFun(fun_args tf_args, basic.tvoid) in
    let ctor_cf = mk_class_field "new" ctor_t true pos (Method MethNormal) [] in
    ctor_cf.cf_expr <- Some {
      eexpr = TFunction({
        tf_args = tf_args;
        tf_type = basic.tvoid;
        tf_expr = { eexpr = TBlock({
          eexpr = TCall({ eexpr = TConst(TSuper); etype = TInst(cl,[]); epos = pos }, [mk_int ctx (-1) pos; mk_int ctx (-1) pos]);
          etype = basic.tvoid;
          epos = pos
        } :: ctor_body); etype = basic.tvoid; epos = pos }
      });
      etype = ctor_t;
      epos = pos
    };

    cl.cl_constructor <- Some ctor_cf;

    let closure_fun eclosure e field is_static =
      let f = { eexpr = TConst(TString field); etype = basic.tstring; epos = eclosure.epos } in
      let args = if ctx.rcf_optimize then [ f; { eexpr = TConst(TInt (hash_field_i32 ctx eclosure.epos field)); etype = basic.tint; epos = eclosure.epos } ] else [ f ] in
      let args = args @ [ mk_cast (TInst(ctx.rcf_object_iface, [])) e ] in

      { eclosure with eexpr = TNew(cl,[],args) }
    in
    closure_fun

  let get_closure_func ctx closure_cl =
    let gen = ctx.rcf_gen in
    let basic = gen.gcon.basic in
    let closure_func eclosure e field is_static =
      { eclosure with
        eexpr = TNew(closure_cl, [], [
          e;
          { eexpr = TConst(TString field); etype = basic.tstring; epos = eclosure.epos }
        ] @ (
          if ctx.rcf_optimize then [ { eexpr = TConst(TInt (hash_field_i32 ctx eclosure.epos field)); etype = basic.tint; epos = eclosure.epos } ] else []
        ))
      }
    in
    closure_func

  (*
      main expr -> field expr -> field string -> possible set expr -> should_throw_exceptions -> changed expression

      Changes a get / set
    *
    mutable rcf_on_getset_field : texpr->texpr->string->texpr option->bool->texpr;*)

  let configure_dynamic_field_access ctx is_synf =
    let gen = ctx.rcf_gen in
    let is_dynamic expr fexpr field = match field_access gen (gen.greal_type fexpr.etype) field with
      | FEnumField _
      | FClassField _ -> false
      | _ -> true
    in

    let configure = if is_synf then DynamicFieldAccess.configure_as_synf else DynamicFieldAccess.configure in
    let maybe_hash = if ctx.rcf_optimize then fun str pos -> Some (hash_field_i32 ctx pos str) else fun str pos -> None in
    configure gen (DynamicFieldAccess.abstract_implementation gen is_dynamic
      (fun expr fexpr field set is_unsafe ->
        let hash = maybe_hash field fexpr.epos in
        ctx.rcf_on_getset_field expr fexpr field hash set is_unsafe
      )
      (fun ecall fexpr field call_list ->
        let hash = maybe_hash field fexpr.epos in
        ctx.rcf_on_call_field ecall fexpr field hash call_list
      )
    );
    ()

  let replace_reflection ctx cl =
    let gen = ctx.rcf_gen in
    let pos = cl.cl_pos in

    let this_t = TInst(cl, List.map snd cl.cl_types) in
    let this = { eexpr = TConst(TThis); etype = this_t; epos = pos } in

    let last_fields = match cl.cl_super with
      | None -> PMap.empty
      | Some (super,_) -> super.cl_fields
    in

    let new_fields = ref [] in
    let process_cf static cf =
      match cf.cf_kind with
        | Var _ -> ()
        | _ when Meta.has Meta.ReplaceReflection cf.cf_meta ->
          let name = if String.get cf.cf_name 0 = '_' then String.sub cf.cf_name 1 (String.length cf.cf_name - 1) else cf.cf_name in
          let new_name = gen.gmk_internal_name "hx" name in
          let new_cf = mk_class_field new_name cf.cf_type cf.cf_public cf.cf_pos cf.cf_kind cf.cf_params in
          let fn_args, ret = get_fun (follow cf.cf_type) in

          let tf_args = List.map (fun (name,_,t) -> alloc_var name t, None) fn_args in
          let is_void = is_void ret in
          let expr = {
            eexpr = TCall(
              {
                eexpr = (if static then TField(mk_classtype_access cl pos, FStatic(cl, cf)) else TField(this, FInstance(cl, cf)));
                etype = cf.cf_type;
                epos = cf.cf_pos;
              },
              List.map (fun (v,_) -> mk_local v cf.cf_pos) tf_args);
            etype = ret;
            epos = cf.cf_pos
          } in

          let new_f =
          {
            tf_args = tf_args;
            tf_type = ret;
            tf_expr = {
              eexpr = TBlock([if is_void then expr else mk_return expr]);
              etype = ret;
              epos = pos;
            }
          } in

          new_cf.cf_expr <- Some({ eexpr = TFunction(new_f); etype = cf.cf_type; epos = cf.cf_pos});

          new_fields := new_cf :: !new_fields;

          (if static then cl.cl_statics <- PMap.add new_name new_cf cl.cl_statics else cl.cl_fields <- PMap.add new_name new_cf cl.cl_fields);

          if not static && PMap.mem new_name last_fields then cl.cl_overrides <- new_cf :: cl.cl_overrides
        | _ -> ()
    in

    List.iter (process_cf false) cl.cl_ordered_fields;
    cl.cl_ordered_fields <- cl.cl_ordered_fields @ !new_fields;
    new_fields := [];
    List.iter (process_cf true) cl.cl_ordered_statics;
    cl.cl_ordered_statics <- cl.cl_ordered_statics @ !new_fields

  (* ******************************************* *)
  (* UniversalBaseClass *)
  (* ******************************************* *)

  (*

    Sets the universal base class for hxgen types (HxObject / IHxObject)

    dependencies:
      As a rule, it should be one of the last module filters to run so any @:hxgen class created in the process
      -Should- only run after TypeParams.RealTypeParams.Modf, since

  *)

  module UniversalBaseClass =
  struct

    let name = "rcf_universal_base_class"

    let priority = min_dep +. 10.

    let default_implementation gen baseclass baseinterface basedynamic =
      baseinterface.cl_meta <- (Meta.BaseInterface, [], baseinterface.cl_pos) :: baseinterface.cl_meta;
      let rec run md =
        (if is_hxgen md then
          match md with
            | TClassDecl ( { cl_interface = true } as cl ) when cl.cl_path <> baseclass.cl_path && cl.cl_path <> baseinterface.cl_path && cl.cl_path <> basedynamic.cl_path ->
              cl.cl_implements <- (baseinterface, []) :: cl.cl_implements
            | TClassDecl ( { cl_super = None } as cl ) when cl.cl_path <> baseclass.cl_path && cl.cl_path <> baseinterface.cl_path && cl.cl_path <> basedynamic.cl_path ->
              if is_some cl.cl_dynamic then
                cl.cl_super <- Some (basedynamic,[])
              else
                cl.cl_super <- Some (baseclass,[])
            | TClassDecl ( { cl_super = Some(super,_) } as cl ) when cl.cl_path <> baseclass.cl_path && cl.cl_path <> baseinterface.cl_path && not ( is_hxgen (TClassDecl super) ) ->
              cl.cl_implements <- (baseinterface, []) :: cl.cl_implements
            | _ -> ()
        );
        md
      in
      run

    let configure gen mapping_func =
      let map e = Some(mapping_func e) in
      gen.gmodule_filters#add ~name:name ~priority:(PCustom priority) map

    let default_config gen baseclass baseinterface basedynamic =
      let impl = (default_implementation gen baseclass baseinterface basedynamic) in
      configure gen impl

  end;;

  (*
    Priority: must run AFTER UniversalBaseClass
  *)
  let priority = solve_deps name [DAfter UniversalBaseClass.priority]

  let configure ?slow_invoke ctx =
    let gen = ctx.rcf_gen in
    let run = (fun md -> match md with
      | TClassDecl cl when is_hxgen md && ( not cl.cl_interface || Meta.has Meta.BaseInterface cl.cl_meta ) ->
        (if Meta.has Meta.ReplaceReflection cl.cl_meta then replace_reflection ctx cl);
        (implement_dynamics ctx cl);
        (if not (PMap.mem (gen.gmk_internal_name "hx" "lookupField") cl.cl_fields) then implement_final_lookup ctx cl);
        (if not (PMap.mem (gen.gmk_internal_name "hx" "getField") cl.cl_fields) then implement_get_set ctx cl);
        (if not (PMap.mem (gen.gmk_internal_name "hx" "invokeField") cl.cl_fields) then implement_invokeField ctx ~slow_invoke:slow_invoke cl);
        (if not (PMap.mem (gen.gmk_internal_name "hx" "classFields") cl.cl_fields) then implement_fields ctx cl);
        (if ctx.rcf_handle_statics && not (PMap.mem (gen.gmk_internal_name "hx" "getClassStatic") cl.cl_statics) then implement_get_class ctx cl);
        (if not (PMap.mem (gen.gmk_internal_name "hx" "create") cl.cl_fields) then implement_create_empty ctx cl);
        None
      | _ -> None)
    in

    gen.gmodule_filters#add ~name:name ~priority:(PCustom priority) run

end;;

(* ******************************************* *)
(* Object Declaration Mapper *)
(* ******************************************* *)

(*

  A simple Object Declaration Mapper. By default it will be a syntax filter, which only runs
  after

  dependencies:


*)

module ObjectDeclMap =
struct

  let name = "object_decl_map"

  let priority = solve_deps name []

  let traverse gen map_fn =
    let rec run e =
      match e.eexpr with
          | TObjectDecl odecl ->
            let e = Type.map_expr run e in
            (match e.eexpr with | TObjectDecl odecl -> map_fn e odecl | _ -> assert false)
          | _ -> Type.map_expr run e
    in
    run

  let configure gen (mapping_func:texpr->texpr) =
    let map e = Some(mapping_func e) in
    gen.gsyntax_filters#add ~name:name ~priority:(PCustom priority) map

end;;


(* ******************************************* *)
(* EnumToClass *)
(* ******************************************* *)

(*

  For languages that don't support parameterized enums and/or metadata in enums, we need to transform
  enums into normal classes. This is done at the first module pass by creating new classes with the same
  path inside the modules, and removing the actual enum module by setting it as en extern.

  Later, on the last expression pass, it will transform the TMatch codes into TSwitch. it will introduce a new
  dependency, though:
    * The target must create its own strategy to deal with reflection. As it is right now, we will have a base class
    which the class will extend, create @:$IsEnum metadata for the class, and create @:alias() metadatas for the fields,
    with their tag order (as a string) as their alias. If you are using ReflectionCFs, then you don't have to worry
    about that, as it's already generating all information needed by the haxe runtime.
    so they can be

  dependencies:
    The MatchToSwitch part must run after ExprStatementUnwrap as modified expressions might confuse it (not so true anymore)

*)

module EnumToClass =
struct

  let name = "enum_to_class"

  let priority = solve_deps name []

  type t = {
    ec_tbl : (path, tclass) Hashtbl.t;
  }

  let new_t () =
  {
    ec_tbl = Hashtbl.create 10
  }

  (* ******************************************* *)
  (* EnumToClassModf *)
  (* ******************************************* *)

  (*

    The actual Module Filter that will transform the enum into a class

    dependencies:
      Should run before ReflectionCFs, in order to enable proper reflection access.
      Should run before TypeParams.RealTypeParams.RealTypeParamsModf, since generic enums must be first converted to generic classes
  *)

  module EnumToClassModf =
  struct

    let name = "enum_to_class_mod"

    let priority = solve_deps name [DBefore ReflectionCFs.priority; DBefore TypeParams.RealTypeParams.RealTypeParamsModf.priority]

    let pmap_exists fn pmap = try PMap.iter (fun a b -> if fn a b then raise Exit) pmap; false with | Exit -> true

    let has_any_meta en =
      let has_meta meta = List.exists (fun (m,_,_) -> match m with Meta.Custom _ -> true | _ -> false) meta in
      has_meta en.e_meta || pmap_exists (fun _ ef -> has_meta ef.ef_meta) en.e_constrs

    let has_parameters e =
      try
        (PMap.iter (fun _ ef -> match follow ef.ef_type with | TFun _ -> raise Exit | _ -> ()) e.e_constrs);
        false
      with | Exit -> true

    let convert gen t base_class en should_be_hxgen handle_type_params =
      let basic = gen.gcon.basic in
      let pos = en.e_pos in

      (* create the class *)
      let cl = mk_class en.e_module en.e_path pos in
      Hashtbl.add t.ec_tbl en.e_path cl;

      (match Codegen.build_metadata gen.gcon (TEnumDecl en) with
        | Some expr ->
          let cf = mk_class_field "__meta__" expr.etype false expr.epos (Var { v_read = AccNormal; v_write = AccNormal }) [] in
          cf.cf_expr <- Some expr;
          cl.cl_statics <- PMap.add "__meta__" cf cl.cl_statics;
          cl.cl_ordered_statics <- cf :: cl.cl_ordered_statics
        | _ -> ()
      );

      cl.cl_super <- Some(base_class,[]);
      cl.cl_extern <- en.e_extern;
      en.e_extern <- true;
      en.e_meta <- (Meta.Class, [], pos) :: en.e_meta;
      cl.cl_module <- en.e_module;
      cl.cl_meta <- ( Meta.Enum, [], pos ) :: cl.cl_meta;
      let c_types =
        if handle_type_params then
          List.map (fun (s,t) -> (s, TInst (map_param (get_cl_t t), []))) en.e_types
        else
          []
      in

      cl.cl_types <- c_types;

      let i = ref 0 in
      let cfs = List.map (fun name ->
        let ef = PMap.find name en.e_constrs in
        let pos = ef.ef_pos in
        let old_i = !i in
        incr i;

        let cf = match follow ef.ef_type with
          | TFun(params,ret) ->
            let dup_types =
              if handle_type_params then
                List.map (fun (s,t) -> (s, TInst (map_param (get_cl_t t), []))) en.e_types
              else
                []
            in

            let ef_type =
              let fn, types = if handle_type_params then snd, dup_types else (fun _ -> t_dynamic), en.e_types in
              let t = apply_params en.e_types (List.map fn types) ef.ef_type in
              apply_params ef.ef_params (List.map fn ef.ef_params) t
            in

            let params, ret = get_fun ef_type in
            let cf_params = if handle_type_params then dup_types @ ef.ef_params else [] in

            let cf = mk_class_field name ef_type true pos (Method MethNormal) cf_params in
            cf.cf_meta <- [];

            let tf_args = List.map (fun (name,opt,t) ->  (alloc_var name t, if opt then Some TNull else None) ) params in
            let arr_decl = { eexpr = TArrayDecl(List.map (fun (v,_) -> mk_local v pos) tf_args); etype = basic.tarray t_empty; epos = pos } in
            let expr = {
              eexpr = TFunction({
                tf_args = tf_args;
                tf_type = ret;
                tf_expr = mk_block ( mk_return { eexpr = TNew(cl,List.map snd dup_types, [mk_int gen old_i pos; arr_decl] ); etype = TInst(cl, List.map snd dup_types); epos = pos } );
              });
              etype = ef_type;
              epos = pos
            } in
            cf.cf_expr <- Some expr;
            cf
          | _ ->
            let actual_t = match follow ef.ef_type with
              | TEnum(e, p) -> TEnum(e, List.map (fun _ -> t_dynamic) p)
              | _ -> assert false
            in
            let cf = mk_class_field name actual_t true pos (Var { v_read = AccNormal; v_write = AccNormal }) [] in
            cf.cf_meta <- [];
            cf.cf_expr <- Some {
              eexpr = TNew(cl, List.map (fun _ -> t_empty) cl.cl_types, [mk_int gen old_i pos; { eexpr = TArrayDecl []; etype = basic.tarray t_empty; epos = pos }]);
              etype = TInst(cl, List.map (fun _ -> t_empty) cl.cl_types);
              epos = pos;
            };
            cf
        in
        cl.cl_statics <- PMap.add cf.cf_name cf cl.cl_statics;
        cf.cf_meta <- (Meta.Alias, [ EConst( String (string_of_int old_i) ), pos ], pos) :: [];
        cf
      ) en.e_names in
      let constructs_cf = mk_class_field "constructs" (basic.tarray basic.tstring) true pos (Var { v_read = AccNormal; v_write = AccNormal }) [] in
      constructs_cf.cf_meta <- [];
      constructs_cf.cf_expr <- Some {
        eexpr = TArrayDecl (List.map (fun s -> { eexpr = TConst(TString s); etype = basic.tstring; epos = pos }) en.e_names);
        etype = basic.tarray basic.tstring;
        epos = pos;
      };

      cl.cl_ordered_statics <- constructs_cf :: cfs @ cl.cl_ordered_statics ;
      cl.cl_statics <- PMap.add "constructs" constructs_cf cl.cl_statics;

      (if should_be_hxgen then
        cl.cl_meta <- (Meta.HxGen,[],cl.cl_pos) :: cl.cl_meta
      else begin
        (* create the constructor *)
        let tf_args = [ alloc_var "index" basic.tint, None; alloc_var "params" (basic.tarray t_empty), None ] in
        let ftype = TFun(fun_args tf_args, basic.tvoid) in
        let ctor = mk_class_field "new" ftype true pos (Method MethNormal) [] in
        let me = TInst(cl, List.map snd cl.cl_types) in
        ctor.cf_expr <-
        Some {
          eexpr = TFunction(
          {
            tf_args = tf_args;
            tf_type = basic.tvoid;
            tf_expr = mk_block {
              eexpr = TCall({ eexpr = TConst TSuper; etype = me; epos = pos }, List.map (fun (v,_) -> mk_local v pos) tf_args);
              etype = basic.tvoid;
              epos = pos;
            }
          });
          etype = ftype;
          epos = pos
        };

        cl.cl_constructor <- Some ctor
      end);
      gen.gadd_to_module (TClassDecl cl) (max_dep);

      TEnumDecl en

    (*
      traverse
        gen - gen context
        convert_all : bool - should we convert all enums? If set, convert_if_has_meta will be ignored.
        convert_if_has_meta : bool - should we convert only if it has meta?
        enum_base_class : tclass - the enum base class.
        should_be_hxgen : bool - should the created enum be hxgen?
    *)
    let traverse gen t convert_all convert_if_has_meta enum_base_class should_be_hxgen handle_tparams =
      let convert e = convert gen t enum_base_class e should_be_hxgen handle_tparams in
      let run md = match md with
        | TEnumDecl e when is_hxgen md ->
          if convert_all then
            convert e
          else if convert_if_has_meta && has_any_meta e then
            convert e
          else if has_parameters e then
            convert e
          else begin
            (* take off the :hxgen meta from it, if there's any *)
            e.e_meta <- List.filter (fun (n,_,_) -> not (n = Meta.HxGen)) e.e_meta;
            md
          end
        | _ -> md
      in
      run

    let configure gen (mapping_func:module_type->module_type) =
      let map md = Some(mapping_func md) in
      gen.gmodule_filters#add ~name:name ~priority:(PCustom priority) map

  end;;

  (* ******************************************* *)
  (* EnumToClassExprf *)
  (* ******************************************* *)

  (*

    Enum to class Expression Filter

    will convert TMatch into TSwitch

    dependencies:
      Should run before TArrayTransform, since it generates array access expressions

  *)

  module EnumToClassExprf =
  struct

    let name = "enum_to_class_exprf"

    let priority = solve_deps name [DBefore TArrayTransform.priority]

    let ensure_local gen cond =
      let exprs_before, new_cond = match cond.eexpr with
        | TLocal v ->
          [], cond
        | _ ->
          let v = mk_temp gen "cond" cond.etype in
          [ { eexpr = TVars([v, Some cond]); etype = gen.gcon.basic.tvoid; epos = cond.epos } ], mk_local v cond.epos
      in
      exprs_before, new_cond

    let get_index gen cond cls tparams =
      { (mk_field_access gen { cond with etype = TInst(cls, tparams) } "index" cond.epos) with etype = gen.gcon.basic.tint }

    (* stolen from Hugh's hxcpp sources *)
    let tmatch_params_to_vars params =
      (match params with
      | None | Some [] -> []
      | Some l ->
        let n = ref (-1) in
        List.fold_left
          (fun acc v -> incr n; match v with None -> acc | Some v -> (v,!n) :: acc) [] l)

    let tmatch_params_to_exprs gen params cond_local =
      let vars = tmatch_params_to_vars params in
      let cond_array = { (mk_field_access gen cond_local "params" cond_local.epos) with etype = gen.gcon.basic.tarray t_empty } in
      let tvars = List.map (fun (v, n) ->
        (v, Some({ eexpr = TArray(cond_array, mk_int gen n cond_array.epos); etype = t_dynamic; epos = cond_array.epos }))
      ) vars in
      match vars with
        | [] ->
            []
        | _ ->
            [ { eexpr = TVars(tvars); etype = gen.gcon.basic.tvoid; epos = cond_local.epos } ]

    let traverse gen t opt_get_native_enum_tag =
      let rec run e =
        match e.eexpr with
          | TMatch(cond,(en,eparams),cases,default) ->
            let cond = run cond in (* being safe *)
            (* check if en was converted to class *)
            (* if it was, switch on tag field and change cond type *)
            let exprs_before, cond_local, cond = try
              let cl = Hashtbl.find t.ec_tbl en.e_path in
              let cond = { cond with etype = TInst(cl, eparams) } in
              let exprs_before, new_cond = ensure_local gen cond in
              exprs_before, new_cond, get_index gen new_cond cl eparams
            with | Not_found ->
              (*
                if it's not a class, we'll either use get_native_enum_tag or in a last resource,
                call Type.getEnumIndex
              *)
              match opt_get_native_enum_tag with
                | Some get_native_etag ->
                  [], cond, get_native_etag cond
                | None ->
                  [], cond, { eexpr = TCall(mk_static_field_access_infer gen.gclasses.cl_type "enumIndex" e.epos [], [cond]); etype = gen.gcon.basic.tint; epos = cond.epos }
            in

            (* for each case, change cases to expr int, and see if there is any var create *)
            let change_case (il, params, expr) =
              let expr = run expr in
              (* if there are, set var with tarray *)
              let exprs = tmatch_params_to_exprs gen params cond_local in
              let expr = match expr.eexpr with
                | TBlock(bl) -> { expr with eexpr = TBlock(exprs @ bl) }
                | _ -> { expr with eexpr = TBlock ( exprs @ [expr] ) }
              in
              (List.map (fun i -> mk_int gen i e.epos) il, expr)
            in

            let tswitch = { e with eexpr = TSwitch(cond, List.map change_case cases, Option.map run default) } in
            (match exprs_before with
              | [] -> tswitch
              | _ -> { e with eexpr = TBlock(exprs_before @ [tswitch]) })
          | _ -> Type.map_expr run e
      in

      run

    let configure gen (mapping_func:texpr->texpr) =
      let map e = Some(mapping_func e) in
      gen.gexpr_filters#add ~name:name ~priority:(PCustom priority) map

  end;;

  let configure gen opt_get_native_enum_tag convert_all convert_if_has_meta enum_base_class should_be_hxgen handle_tparams =
    let t = new_t () in
    EnumToClassModf.configure gen (EnumToClassModf.traverse gen t convert_all convert_if_has_meta enum_base_class should_be_hxgen handle_tparams);
    EnumToClassExprf.configure gen (EnumToClassExprf.traverse gen t opt_get_native_enum_tag)

end;;

(* ******************************************* *)
(* IteratorsInterface *)
(* ******************************************* *)

(*

  This module will handle with Iterators, Iterables and TFor() expressions.
  At first, a module filter will receive a Iterator<T> and Iterable<T> interface, which will be implemented
  if hasNext(), next() or iterator() fields are detected with the correct type.
  At this part a custom function will be called which can adequate the class fields so they are compatible with
  native Iterators as well

  The expression filter part of this module will look for TFor() expressions, and transform like that:
  for (anInt in value.iterator())
  {

  }

  {
    var s:haxe.lang.Iterator<Int> = ExternalFunction.getIterator(value.iterator());
    while (s.hasNext())
    {
      var anInt:Int = s.next();

    }
  }

  dependencies:
    None.

*)

module IteratorsInterface =
struct

  let name = "iterators_interface"
  (* TODO later
  (* ******************************************* *)
  (* IteratorsInterfaceModf *)
  (* ******************************************* *)

  (*

    The module filter for Iterators Interface, which will implement the iterator/iterable interface on each
    class that conforms with the typedefs Iterator<> and Iterable<>

    It's a very simple module and it will rely on cast detection to work correctly. This is so that
    when the

    dependencies:
      Must run at the Module Filters, so cast detection can detect a cast to the interface and we can

  *)

  module IteratorsInterfaceModf =
  struct

    let name = "iterators_interface_modf"

    let conforms_cfs has_next next =
      try (match follow has_next.cf_type with
        | TFun([],ret) when
          (match follow ret with | TEnum({ e_path = ([], "Bool") }, []) -> () | _ -> raise Not_found) ->
            ()
        | _ -> raise Not_found);
      (match follow next.cf_type with
        | TFun([], ret) -> ret
        | _ -> raise Not_found
      )

    let conforms_type_iterator t =
      try match follow t with
        | TInst(cl,params) ->
            let has_next = PMap.find "hasNext" cl.cl_fields in
            let next = PMap.find "next" cl.cl_fields in
            Some (conforms_cfs has_next next)
        | TAnon(anon) ->
            let has_next = PMap.find "hasNext" anon.a_fields in
            let next = PMap.find "next" anon.a_fields in
            Some (conforms_cfs has_next next)
        | _ -> None
      with | Not_found -> None

    let conforms_as_iterable cl =
      try
        let iterator = PMap.find "iterator" cl.cl_fields in
        match follow iterator.cf_type with
          | TFun([], ret) -> conforms_type_iterator ret
          | _ -> None
      with | Not_found -> None

    let conforms_as_iterator cl =
      try
        let has_next = PMap.find "hasNext" cl.cl_fields in
        let next = PMap.find "next" cl.cl_fields in
        Some (conforms_cfs has_next next)
      with | Not_found -> None

    let priority = solve_deps name []

    let traverse gen iterator_iface iterable_iface on_found_iterator on_found_iterable =
      let rec run md =
        match md with
          | TClassDecl cl when not cl.cl_extern && is_hxgen cl ->
            let conforms_iterator = conforms_as_iterator cl in
            let conforms_iterable = conforms_as_iterable cl in
            if is_some conforms_iterator then begin
              let it_t = get conforms_iterator in
              cl.cl_interfaces <- (iterator_iface, [it_t]);
              on_found_iterator cl
            end;
            if is_some conforms_iterable then begin
              let it_t = get conforms_iterable in
              cl.cl_interfaces <- (iterable_iface, [it_t]);
              on_found_iterable cl
            end;

            md
          | _ -> md
      in
      run

    let configure gen (mapping_func:texpr->texpr) =
      let map e = Some(mapping_func e) in
      gen.gexpr_filters#add ~name:name ~priority:(PCustom priority) map

  end;;
  *)

  (* ******************************************* *)
  (* IteratorsInterfaceExprf *)
  (* ******************************************* *)

  (*

    The expression filter for Iterators. Will look for TFor, transform it into
    {
      var iterator = // in expression here
      while (iterator.hasNext())
      {
        var varName = iterator.next();
      }
    }

    dependencies:
      Must run before Dynamic fields access is run

  *)

  module IteratorsInterfaceExprf =
  struct

    let name = "iterators_interface_exprf"

    let priority = solve_deps name [DBefore DynamicFieldAccess.priority]

    let priority_as_synf = solve_deps name [DBefore DynamicFieldAccess.priority_as_synf]

    let mk_access gen v name pos =
      let field_t =
        try match follow v.v_type with
          | TInst(cl, params) ->
            let field = PMap.find name cl.cl_fields in
            apply_params cl.cl_types params field.cf_type
          | TAnon(anon) ->
            let field = PMap.find name anon.a_fields in
            field.cf_type
          | _ -> t_dynamic
        with | Not_found -> t_dynamic
      in
      { (mk_field_access gen (mk_local v pos) name pos) with etype = field_t }

    let traverse gen change_in_expr =
      let basic = gen.gcon.basic in
      let rec run e =
        match e.eexpr with
          | TFor(var, in_expr, block) ->
            let in_expr = change_in_expr (run in_expr) in
            let temp = mk_temp gen "iterator" in_expr.etype in
            let block =
            [
              { eexpr = TVars([temp, Some(in_expr)]); etype = basic.tvoid; epos = in_expr.epos };
              {
                eexpr = TWhile(
                  { eexpr = TCall(mk_access gen temp "hasNext" in_expr.epos, []); etype = basic.tbool; epos = in_expr.epos },
                  Codegen.concat ({
                    eexpr = TVars([var, Some({ eexpr = TCall(mk_access gen temp "next" in_expr.epos, []); etype = var.v_type; epos = in_expr.epos })]);
                    etype = basic.tvoid;
                    epos = in_expr.epos
                  }) ( run block ),
                  Ast.NormalWhile);
                etype = basic.tvoid;
                epos = e.epos
              }
            ] in
            { eexpr = TBlock(block); etype = e.etype; epos = e.epos }
          | _ -> Type.map_expr run e
      in
      run

    let configure gen (mapping_func:texpr->texpr) =
      let map e = Some(mapping_func e) in
      gen.gexpr_filters#add ~name:name ~priority:(PCustom priority) map

    let configure_as_synf gen (mapping_func:texpr->texpr) =
      let map e = Some(mapping_func e) in
      gen.gexpr_filters#add ~name:name ~priority:(PCustom priority_as_synf) map

  end;;

  let configure gen change_in_expr =
    IteratorsInterfaceExprf.configure gen (IteratorsInterfaceExprf.traverse gen change_in_expr)

  let configure_as_synf gen change_in_expr =
    IteratorsInterfaceExprf.configure_as_synf gen (IteratorsInterfaceExprf.traverse gen change_in_expr)

end;;

(* ******************************************* *)
(* SwitchToIf *)
(* ******************************************* *)

(*

  Just a syntax filter which changes switch expressions to if() else if() else if() ...
  It can be also an expression filter
  dependencies:


*)

module SwitchToIf =
struct

  let name = "switch_to_if"

  let priority = solve_deps name []

  let traverse gen (should_convert:texpr->bool) (handle_nullables:bool) =
    let basic = gen.gcon.basic in
    let rec run e =
      match e.eexpr with
        | TSwitch(cond,cases,default) when should_convert e ->
          let cond_etype, should_cache = match handle_nullables, gen.gfollow#run_f cond.etype with
            | true, TType({ t_path = ([], "Null") }, [t]) ->
              let rec take_off_nullable t = match gen.gfollow#run_f t with
                | TType({ t_path = ([], "Null") }, [t]) -> take_off_nullable t
                | _ -> t
              in

              take_off_nullable t, true
            | _, _ -> cond.etype, false
          in

          if should_cache && not (should_convert { e with eexpr = TSwitch({ cond with etype = cond_etype }, cases, default) }) then begin
            { e with eexpr = TSwitch(mk_cast cond_etype (run cond), List.map (fun (cs,e) -> (List.map run cs, run e)) cases, Option.map run default) }
          end else begin
            let local, fst_block = match cond.eexpr, should_cache with
              | TLocal _, false -> cond, []
              | _ ->
                let var = mk_temp gen "switch" cond_etype in
                let cond = run cond in
                let cond = if should_cache then mk_cast cond_etype cond else cond in

                mk_local var cond.epos, [ { eexpr = TVars([var,Some(cond)]); etype = basic.tvoid; epos = cond.epos } ]
            in

            let mk_eq cond =
              { eexpr = TBinop(Ast.OpEq, local, cond); etype = basic.tbool; epos = cond.epos }
            in

            let rec mk_many_cond conds =
              match conds with
                | cond :: [] ->
                  mk_eq cond
                | cond :: tl ->
                  { eexpr = TBinop(Ast.OpBoolOr, mk_eq (run cond), mk_many_cond tl); etype = basic.tbool; epos = cond.epos }
                | [] -> assert false
            in

            let mk_many_cond conds =
              let ret = mk_many_cond conds in
              (*
                this might be considered a hack. But since we're on a syntax filter and
                the condition is guaranteed to not have run twice, we can really run the
                expr filters again for it (so to change e.g. OpEq accordingly
              *)
              gen.gexpr_filters#run_f ret
            in

            let rec loop cases = match cases with
              | (conds,e) :: [] ->
                { eexpr = TIf(mk_many_cond conds, run e, Option.map run default); etype = e.etype; epos = e.epos }
              | (conds,e) :: tl ->
                { eexpr = TIf(mk_many_cond conds, run e, Some(loop tl)); etype = e.etype; epos = e.epos }
              | [] -> match default with
                | None -> gen.gcon.error "Empty switch" e.epos; assert false
                | Some d -> run d
            in

            { e with eexpr = TBlock(fst_block @ [loop cases]) }
          end
        | _ -> Type.map_expr run e
    in
    run

  let configure gen (mapping_func:texpr->texpr) =
    let map e = Some(mapping_func e) in
    gen.gsyntax_filters#add ~name:name ~priority:(PCustom priority) map

end;;

(* ******************************************* *)
(* Anonymous Class object handling *)
(* ******************************************* *)

(*

  (syntax)
  When we pass a class as an object, in some languages we will need a special construct to be able to
  access its statics as if they were normal object fields. On C# and Java the way found to do that is
  by handling statics reflection also by a normal instance. This also happens in hxcpp and neko, so I
  guess it's a valid practice.
  So if we want to handle the reflection of the static MyClass, here's roughly how it will be done:

  var x = MyClass;
  gets converted into
  Haxe.Lang.Class x = Haxe.Lang.Runtime.GetType(typeof(MyClass).RuntimeHandle);

  which will in turn look in its cache but roughly would do:
  Haxe.Lang.Class x = new Haxe.Lang.Class(new MyClass(EmptyObject.EMPTY));

  This module will of course let the caller choose how this will be implemented. It will just identify all
  uses of class that will require it to be cast as an object.

  dependencies:

*)

module ClassInstance =
struct

  let priority = solve_deps "class_instance" []

  let traverse gen (change_expr:texpr->module_type->texpr) =
    let rec run e =
      match e.eexpr with
          | TCall( ({ eexpr = TLocal(v) } as local), calls ) when String.get v.v_name 0 = '_' && Hashtbl.mem gen.gspecial_vars v.v_name ->
            { e with eexpr = TCall(local, List.map (fun e -> Type.map_expr run e) calls) }
          | TField({ eexpr = TTypeExpr(mt) }, f) ->
              e
          | TField(ef, f) ->
            (match anon_class ef.etype with
              | None -> Type.map_expr run e
              | Some t ->
                { e with eexpr = TField( { ef with eexpr = TTypeExpr(t) }, f) }
            )
          | TTypeExpr(mt) -> change_expr e mt
          | _ -> Type.map_expr run e
    in
    run

  let configure gen (mapping_func:texpr->texpr) =
    let map e = Some(mapping_func e) in
    gen.gsyntax_filters#add ~name:"class_instance" ~priority:(PCustom priority) map

end;;

(* ******************************************* *)
(* HardNullableSynf *)
(* ******************************************* *)

(*

  This module will handle Null<T> types for languages that offer a way of dealing with
  stack-allocated structures or tuples and generics. Essentialy on those targets a Null<T>
  will be a tuple ( 'a * bool ), where bool is whether the value is null or not.

  At first (configure-time), we will modify the follow function so it can follow correctly nested Null<Null<T>>,
  and do not follow Null<T> to its underlying type

  Then we will run a syntax filter, which will look for casts to Null<T> and replace them by
  a call to the new Null<T> creation;
  Also casts from Null<T> to T or direct uses of Null<T> (call, field access, array access, closure)
  will result in the actual value being accessed
  For compatibility with the C# target, HardNullable will accept both Null<T> and haxe.lang.Null<T> types

  dependencies:


*)

module HardNullableSynf =
struct

  let name = "hard_nullable"

  let priority = solve_deps name []

  let rec is_null_t gen t = match gen.greal_type t with
    | TType( { t_path = ([], "Null") }, [of_t])
    | TInst( { cl_path = (["haxe";"lang"], "Null") }, [of_t]) ->
      let rec take_off_null t =
        match is_null_t gen t with | None -> t | Some s -> take_off_null s
      in

      Some (take_off_null of_t)
    | TMono r -> (match !r with | Some t -> is_null_t gen t | None -> None)
    | TLazy f -> is_null_t gen (!f())
    | TType (t, tl) ->
      is_null_t gen (apply_params t.t_types tl t.t_type)
    | _ -> None

  let follow_addon gen t =
    let rec strip_off_nullable t =
      let t = gen.gfollow#run_f t in
      match t with
        (* haxe.lang.Null<haxe.lang.Null<>> wouldn't be a valid construct, so only follow Null<> *)
        | TType ( { t_path = ([], "Null") }, [of_t] ) -> strip_off_nullable of_t
        | _ -> t
    in

    match t with
      | TType( ({ t_path = ([], "Null") } as tdef), [of_t]) ->
        Some( TType(tdef, [ strip_off_nullable of_t ]) )
      | _ -> None

  let traverse gen unwrap_null wrap_val null_to_dynamic has_value opeq_handler handle_opeq handle_cast =
    let handle_unwrap to_t e =
      let e_null_t = get (is_null_t gen e.etype) in
      match gen.greal_type to_t with
        | TDynamic _ | TMono _ | TAnon _ ->
          (match e_null_t with
            | TDynamic _ | TMono _ | TAnon _ ->
              gen.ghandle_cast to_t e_null_t (unwrap_null e)
            | _ -> null_to_dynamic e
          )
        | _ ->
          gen.ghandle_cast to_t e_null_t (unwrap_null e)
    in

    let handle_wrap e t =
      match e.eexpr with
        | TConst(TNull) ->
          wrap_val e t false
        | _ ->
          wrap_val e t true
    in

    let is_null_t = is_null_t gen in
    let rec run e =
      match e.eexpr with
        | TCast(v, _) ->
          let null_et = is_null_t e.etype in
          let null_vt = is_null_t v.etype in
          (match null_vt, null_et with
            | Some(vt), None ->
              (match v.eexpr with
                (* is there an unnecessary cast to Nullable? *)
                | TCast(v2, _) ->
                  run { v with etype = e.etype }
                | _ ->
                  handle_unwrap e.etype (run v)
              )
            | None, Some(et) ->
              handle_wrap (run v) et
            | Some(vt), Some(et) when handle_cast ->
              handle_wrap (gen.ghandle_cast et vt (handle_unwrap vt (run v))) et
            | _ ->
              Type.map_expr run e
          )
        | TField(ef, field) when is_some (is_null_t ef.etype) ->
          let to_t = get (is_null_t ef.etype) in
          { e with eexpr = TField(handle_unwrap to_t (run ef), field) }
        | TCall(ecall, params) when is_some (is_null_t ecall.etype) ->
          let to_t = get (is_null_t ecall.etype) in
          { e with eexpr = TCall(handle_unwrap to_t (run ecall), List.map run params) }
        | TArray(earray, p) when is_some (is_null_t earray.etype) ->
          let to_t = get (is_null_t earray.etype) in
          { e with eexpr = TArray(handle_unwrap to_t (run earray), p) }
        | TBinop(op, e1, e2) ->
          let e1_t = is_null_t e1.etype in
          let e2_t = is_null_t e2.etype in

          (match op with
            | Ast.OpAssign
            | Ast.OpAssignOp _ ->
              (match e1_t, e2_t with
                | Some t1, Some t2 ->
                  (match op with
                    | Ast.OpAssign ->
                      { e with eexpr = TBinop( op, run e1, handle_wrap ( handle_unwrap t2 (run e2) ) t1 ) }
                    | Ast.OpAssignOp op ->
                      (match e1.eexpr with
                        | TLocal _ ->
                          { e with eexpr = TBinop( Ast.OpAssign, e1, handle_wrap { e with eexpr = TBinop (op, handle_unwrap t1 e1, handle_unwrap t2 (run e2) ) } t1 ) }
                        | _ ->
                          let v, e1, evars = match e1.eexpr with
                            | TField(ef, f) ->
                              let v = mk_temp gen "nullbinop" ef.etype in
                              v, { e1 with eexpr = TField(mk_local v ef.epos, f) }, ef
                            | _ ->
                              let v = mk_temp gen "nullbinop" e1.etype in
                              v, mk_local v e1.epos, e1
                          in
                          { e with eexpr = TBlock([
                            { eexpr = TVars([v, Some evars ]); etype = gen.gcon.basic.tvoid; epos = e.epos };
                            { e with eexpr = TBinop( Ast.OpAssign, e1, handle_wrap { e with eexpr = TBinop (op, handle_unwrap t1 e1, handle_unwrap t2 (run e2) ) } t1 ) }
                          ]) }
                      )
                    | _ -> assert false
                  )

                | _ ->
                  Type.map_expr run e (* casts are already dealt with normal CastDetection module *)
              )
            | Ast.OpEq | Ast.OpNotEq when not handle_opeq ->
              Type.map_expr run e
            | Ast.OpEq | Ast.OpNotEq ->
              (match e1.eexpr, e2.eexpr with
                | TConst(TNull), _ when is_some e2_t ->
                  let e = has_value e2 in
                  if op = Ast.OpEq then
                    { e with eexpr = TUnop(Ast.Not, Ast.Prefix, e) }
                  else
                    e
                | _, TConst(TNull) when is_some e1_t ->
                  let e = has_value e1 in
                  if op = Ast.OpEq then
                    { e with eexpr = TUnop(Ast.Not, Ast.Prefix, e) }
                  else
                    e
                | _ when is_some e1_t || is_some e2_t ->
                    let e1, e2 =
                      if not (is_some e1_t) then
                        run e2, handle_wrap (run e1) (get e2_t)
                      else if not (is_some e2_t) then
                        run e1, handle_wrap (run e2) (get e1_t)
                      else
                        run e1, run e2
                    in
                    let e = opeq_handler e1 e2 in
                    if op = Ast.OpEq then
                      { e with eexpr = TUnop(Ast.Not, Ast.Prefix, e) }
                    else
                      e
                | _ ->
                  Type.map_expr run e
              )
            | _ ->
              let e1 = if is_some e1_t then
                handle_unwrap (get e1_t) (run e1)
              else run e1 in
              let e2 = if is_some e2_t then
                handle_unwrap (get e2_t) (run e2)
              else
                run e2 in

              (* if it is Null<T>, we need to convert the result again to null *)
              let e_t = (is_null_t e.etype) in
              if is_some e_t then
                wrap_val { eexpr = TBinop(op, e1, e2); etype = get e_t; epos = e.epos } (get e_t) true
              else
                { e with eexpr = TBinop(op, e1, e2) }
          )
        (*| TUnop( (Ast.Increment as op)*)
        | _ -> Type.map_expr run e
    in
    run

  let configure gen (mapping_func:texpr->texpr) =
    gen.gfollow#add ~name:(name ^ "_follow") (follow_addon gen);

    let map e = Some(mapping_func e) in
    gen.gsyntax_filters#add ~name:name ~priority:(PCustom priority) map

end;;

(* ******************************************* *)
(* ArrayDeclSynf *)
(* ******************************************* *)

(*

  A syntax filter that will change array declarations to the actual native array declarations plus
  the haxe array initialization

  dependencies:
    Must run after ObjectDeclMap since it can add TArrayDecl expressions

*)

module ArrayDeclSynf =
struct

  let name = "array_decl_synf"

  let priority = solve_deps name [DAfter ObjectDeclMap.priority]

  let default_implementation gen native_array_cl =
    let rec run e =
      match e.eexpr with
        | TArrayDecl el ->
          let cl, params = match follow e.etype with
            | TInst(({ cl_path = ([], "Array") } as cl), ( _ :: _  as params)) -> cl, params
            | TInst(({ cl_path = ([], "Array") } as cl), []) -> cl, [t_dynamic]
            | _ -> assert false
          in

          let changed_params = gen.greal_type_param (TClassDecl cl) params in
          { e with eexpr = TNew(cl, changed_params, [ { e with eexpr = TArrayDecl(List.map run el); etype = TInst(native_array_cl, changed_params) } ]  ); }
        | _ -> Type.map_expr run e
    in
    run

  let configure gen (mapping_func:texpr->texpr) =
    let map e = Some(mapping_func e) in
    gen.gsyntax_filters#add ~name:name ~priority:(PCustom priority) map

end;;

(* ******************************************* *)
(* SwitchBreakSynf *)
(* ******************************************* *)

(*

  In most languages, 'break' is used as a statement also to break from switch statements.
  This generates an incompatibility with haxe code, as we can use break to break from loops from inside a switch

  This script will detect 'breaks' inside switch statements, and will offer the opportunity to change both
  when this pattern is found.

  Some options are possible:
    On languages that support goto, 'break' may mean goto " after the loop ". There also can be special labels for
      loops, so you can write "break label" (javascript, java, d)
    On languages that do not support goto, a custom solution must be enforced

  dependencies:
    Since UnreachableCodeElimination must run before it, and Unreachable should be one of the
    very last filters to run, we will make a fixed value which runs after UnreachableCodeElimination
    (meaning: it's the very last filter)

*)

module SwitchBreakSynf =
struct

  let name = "switch_break_synf"

  let priority = min_dep -. 150.0

  type add_to_block_api = texpr->bool->unit

  let traverse gen (change_loop:texpr->int->add_to_block_api->texpr) (change_break:texpr->int->add_to_block_api->texpr) =
    let in_switch = ref false in
    let cur_block = ref [] in
    let to_add = ref [] in
    let did_found = ref (-1) in

    let api expr before =
      if before then cur_block := expr :: !cur_block else to_add := expr :: !to_add
    in
    let num = ref 0 in

    let rec run e =
      match e.eexpr with
        | TFunction _ ->
          let old_num = !num in
          num := 0;
            let ret = Type.map_expr run e in
          num := old_num;
          ret
        | TFor _
        | TWhile _ ->
          let last_switch = !in_switch in
          let last_found = !did_found in
          in_switch := false;
          incr num;
          did_found := -1;
            let new_e = Type.map_expr run e in (* assuming that no loop will be found in the condition *)
            let new_e = if !did_found <> -1 then change_loop new_e !did_found api else new_e in
          did_found := last_found;
          in_switch := last_switch;

          new_e
        | TSwitch _
        | TMatch _ ->
          let last_switch = !in_switch in
          in_switch := true;

            let new_e = Type.map_expr run e in

          in_switch := last_switch;
          new_e
        | TBlock bl ->
          let last_block = !cur_block in
          let last_toadd = !to_add in
          to_add := [];
          cur_block := [];

            List.iter (fun e ->
              let new_e = run e in
              cur_block := new_e :: !cur_block;
              match !to_add with
                | [] -> ()
                | _ -> cur_block := !to_add @ !cur_block; to_add := []
            ) bl;

          let ret = List.rev !cur_block in
          cur_block := last_block;
          to_add := last_toadd;

          { e with eexpr = TBlock(ret) }
        | TBreak ->
          if !in_switch then (did_found := !num; change_break e !num api) else e
        | _ -> Type.map_expr run e
    in
    run

  let configure gen (mapping_func:texpr->texpr) =
    let map e = Some(mapping_func e) in
    gen.gsyntax_filters#add ~name:name ~priority:(PCustom priority) map

end;;

(* ******************************************* *)
(* Unreachable Code Elimination *)
(* ******************************************* *)

(*

  In some source code platforms, the code won't compile if there is Unreachable code, so this filter will take off any unreachable code.
    If the parameter "handle_switch_break" is set to true, it will already add a "break" statement on switch cases when suitable;
      in order to not confuse with while break, it will be a special expression __sbreak__
    If the parameter "handle_not_final_returns" is set to true, it will also add final returns when functions are detected to be lacking of them.
      (Will respect __fallback__ expressions)
    If the parameter "java_mode" is set to true, some additional checks following the java unreachable specs
      (http://docs.oracle.com/javase/specs/jls/se7/html/jls-14.html#jls-14.21) will be added

  dependencies:
    This must run before SwitchBreakSynf (see SwitchBreakSynf dependecy value)
    This must be the LAST syntax filter to run. It expects ExpressionUnwrap to have run correctly, since this will only work for source-code based targets

*)

module UnreachableCodeEliminationSynf =
struct

  let name = "unreachable_synf"

  let priority = min_dep -. 100.0

  type uexpr_kind =
    | Normal
    | BreaksLoop
    | BreaksFunction

  let aggregate_kind e1 e2 =
    match e1, e2 with
      | Normal, _
      | _, Normal -> Normal
      | BreaksLoop, _
      | _, BreaksLoop -> BreaksLoop
      | BreaksFunction, BreaksFunction -> BreaksFunction

  let aggregate_constant op c1 c2=
    match op, c1, c2 with
      | OpEq, Some v1, Some v2 -> Some (TBool (v1 = v2))
      | OpNotEq, Some v1, Some v2 -> Some (TBool (v1 <> v2))
      | OpBoolOr, Some (TBool v1) , Some (TBool v2) -> Some (TBool (v1 || v2))
      | OpBoolAnd, Some (TBool v1) , Some (TBool v2) -> Some (TBool (v1 && v2))
      | OpAssign, _, Some v2 -> Some v2
      | _ -> None

  let rec get_constant_expr e =
    match e.eexpr with
      | TConst (v) -> Some v
      | TBinop(op, v1, v2) -> aggregate_constant op (get_constant_expr v1) (get_constant_expr v2)
      | TParenthesis(e) -> get_constant_expr e
      | _ -> None

  let traverse gen should_warn handle_switch_break handle_not_final_returns java_mode =
    let basic = gen.gcon.basic in

    let do_warn =
      if should_warn then gen.gcon.warning "Unreachable code" else (fun pos -> ())
    in

    let return_loop expr kind =
      match kind with
        | Normal | BreaksLoop -> expr, Normal
        | _ -> expr, kind
    in

    let sbreak = alloc_var "__sbreak__" t_dynamic in
    let mk_sbreak = mk_local sbreak in

    let rec has_fallback expr = match expr.eexpr with
      | TBlock(bl) -> (match List.rev bl with
        | { eexpr = TLocal { v_name = "__fallback__" } } :: _ -> true
        | ({ eexpr = TBlock(_) } as bl) :: _ -> has_fallback bl
        | _ -> false)
      | TLocal { v_name = "__fallback__" } -> true
      | _ -> false
    in

    let handle_case = if handle_switch_break then
      (fun (expr,kind) ->
        match kind with
          | Normal when has_fallback expr -> expr
          | Normal -> Codegen.concat expr (mk_sbreak expr.epos)
          | BreaksLoop | BreaksFunction -> expr
      )
    else
      fst
    in

    let has_break = ref false in

    let rec process_expr expr =
      match expr.eexpr with
        | TReturn _ | TThrow _ -> expr, BreaksFunction
        | TContinue -> expr, BreaksLoop
        | TBreak -> has_break := true; expr, BreaksLoop
        | TCall( { eexpr = TLocal { v_name = "__goto__" } }, _ ) -> expr, BreaksLoop

        | TBlock bl ->
          let new_block = ref [] in
          let is_unreachable = ref false in
          let ret_kind = ref Normal in

          List.iter (fun e ->
            if !is_unreachable then
              do_warn e.epos
            else begin
              let changed_e, kind = process_expr e in
              new_block := changed_e :: !new_block;
              match kind with
                | BreaksLoop | BreaksFunction ->
                  ret_kind := kind;
                  is_unreachable := true
                | _ -> ()
            end
          ) bl;

          { expr with eexpr = TBlock(List.rev !new_block) }, !ret_kind
        | TFunction tf ->
          let changed, kind = process_expr tf.tf_expr in
          let changed = if handle_not_final_returns && not (is_void tf.tf_type) && kind <> BreaksFunction then
            Codegen.concat changed { eexpr = TReturn( Some (null tf.tf_type expr.epos) ); etype = basic.tvoid; epos = expr.epos }
          else
            changed
          in

          { expr with eexpr = TFunction({ tf with tf_expr = changed }) }, Normal
        | TFor(var, cond, block) ->
          let last_has_break = !has_break in
          has_break := false;

          let changed_block, _ = process_expr block in
          has_break := last_has_break;
          let expr = { expr with eexpr = TFor(var, cond, changed_block) } in
          return_loop expr Normal
        | TIf(cond, eif, None) ->
          if java_mode then
            match get_constant_expr cond with
              | Some (TBool true) ->
                process_expr eif
              | _ ->
                { expr with eexpr = TIf(cond, fst (process_expr eif), None) }, Normal
          else
            { expr with eexpr = TIf(cond, fst (process_expr eif), None) }, Normal
        | TIf(cond, eif, Some eelse) ->
          let eif, eif_k = process_expr eif in
          let eelse, eelse_k = process_expr eelse in
          let k = aggregate_kind eif_k eelse_k in
          { expr with eexpr = TIf(cond, eif, Some eelse) }, k
        | TWhile(cond, block, flag) ->
          let last_has_break = !has_break in
          has_break := false;

          let block, k = process_expr block in
          if java_mode then
            match get_constant_expr cond, !has_break with
              | Some (TBool true), false ->
                has_break := last_has_break;
                { expr with eexpr = TWhile(cond, block, flag) }, BreaksFunction
              | Some (TBool false), _ ->
                has_break := last_has_break;
                do_warn expr.epos;
                null expr.etype expr.epos, Normal
              | _ ->
                has_break := last_has_break;
                return_loop { expr with eexpr = TWhile(cond,block,flag) } Normal
          else begin
            has_break := last_has_break;
            return_loop { expr with eexpr = TWhile(cond,block,flag) } Normal
          end
        | TSwitch(cond, el_e_l, None) ->
          { expr with eexpr = TSwitch(cond, List.map (fun (el, e) -> (el, handle_case (process_expr e))) el_e_l, None) }, Normal
        | TSwitch(cond, el_e_l, Some def) ->
          let def, k = process_expr def in
          let def = handle_case (def, k) in
          let k = ref k in
          let ret = { expr with eexpr = TSwitch(cond, List.map (fun (el, e) ->
            let e, ek = process_expr e in
            k := aggregate_kind !k ek;
            (el, handle_case (e, ek))
          ) el_e_l, Some def) } in
          ret, !k
        | TMatch(cond, ep, il_vopt_e_l, None) ->
          { expr with eexpr = TMatch(cond, ep, List.map (fun (il, vopt, e) -> (il, vopt, handle_case (process_expr e))) il_vopt_e_l, None) }, Normal
        | TMatch(cond, ep, il_vopt_e_l, Some def) ->
          let def, k = process_expr def in
          let def = handle_case (def, k) in
          let k = ref k in
          let ret = { expr with eexpr = TMatch(cond, ep, List.map (fun (il, vopt, e) ->
            let e, ek = process_expr e in
            k := aggregate_kind !k ek;
            (il, vopt, handle_case (e, ek))
          ) il_vopt_e_l, Some def) } in
          ret, !k
        | TTry (e, catches) ->
          let e, k = process_expr e in
          let k = ref k in
          let ret = { expr with eexpr = TTry(e, List.map (fun (v, e) ->
            let e, ek = process_expr e in
            k := aggregate_kind !k ek;
            (v, e)
          ) catches) } in
          ret, !k
        | _ -> expr, Normal
    in

    let run e = fst (process_expr e) in
    run

  let configure gen (mapping_func:texpr->texpr) =
    let map e = Some(mapping_func e) in
    gen.gsyntax_filters#add ~name:name ~priority:(PCustom priority) map

end;;

(* ******************************************* *)
(* DefaultArguments *)
(* ******************************************* *)

(*

  This Module Filter will go through all defined functions in all modules and change them
  so they set all default arguments to be of a Nullable type, and adds the unroll from nullable to
  the not-nullable type in the beginning of the function.

  dependencies:
    It must run before OverloadingCtors, since OverloadingCtors will change optional structures behavior

*)

module DefaultArguments =
struct

  let name = "default_arguments"

  let priority = solve_deps name [ DBefore OverloadingConstructor.priority ]

  let add_opt gen block pos (var,opt) =
    match opt with
      | None | Some TNull -> (var,opt)
      | Some (TString str) ->
        block := Codegen.set_default gen.gcon var (TString str) pos :: !block;
        (var, opt)
      | Some const ->
        let basic = gen.gcon.basic in
        let nullable_var = mk_temp gen var.v_name (basic.tnull var.v_type) in
        let const_t = match const with
          | TString _ -> basic.tstring | TInt _ -> basic.tint | TFloat _ -> basic.tfloat
          | TNull -> var.v_type | TBool _ -> basic.tbool | _ -> assert false
        in
        (* var v = (temp_var == null) ? const : cast temp_var; *)
        block :=
        {
          eexpr = TVars([var, Some(
          {
            eexpr = TIf(
              { eexpr = TBinop(Ast.OpEq, mk_local nullable_var pos, null nullable_var.v_type pos); etype = basic.tbool; epos = pos },
              mk_cast var.v_type { eexpr = TConst(const); etype = const_t; epos = pos },
              Some(mk_cast var.v_type (mk_local nullable_var pos))
            );
            etype = var.v_type;
            epos = pos;
          })]);
          etype = basic.tvoid;
          epos = pos;
        } :: !block;
        (nullable_var, opt)

  let change_func gen cf =
    let basic = gen.gcon.basic in
    match cf.cf_kind, follow cf.cf_type with
      | Var _, _ | Method MethDynamic, _ -> ()
      | _, TFun(args, ret) ->
        let found = ref false in
        let args = ref (List.map (fun (n,opt,t) ->
          (n,opt, if opt then (found := true; basic.tnull t) else t)
        ) args) in
        (match !found, cf.cf_expr with
          | true, Some ({ eexpr = TFunction tf } as texpr) ->
            let block = ref [] in
            let tf_args = List.map (add_opt gen block tf.tf_expr.epos) tf.tf_args in

            args := fun_args tf_args;
            cf.cf_expr <- Some( {texpr with eexpr = TFunction( { tf with
              tf_args = tf_args;
              tf_expr = Codegen.concat { tf.tf_expr with eexpr = TBlock(!block); etype = basic.tvoid } tf.tf_expr
            } ); etype = TFun(!args, ret) } );
            cf.cf_type <- TFun(!args, ret)

          | _ -> ()
        );
        (if !found then cf.cf_type <- TFun(!args, ret))
      | _, _ -> assert false

  let traverse gen =
    let run md = match md with
      | TClassDecl cl ->
        List.iter (change_func gen) cl.cl_ordered_fields;
        List.iter (change_func gen) cl.cl_ordered_statics;
        (match cl.cl_constructor with | None -> () | Some cf -> change_func gen cf);
        md
      | _ -> md
    in
    run

  let configure gen (mapping_func:module_type->module_type) =
    let map md = Some(mapping_func md) in
    gen.gmodule_filters#add ~name:name ~priority:(PCustom priority) map

end;;

(* ******************************************* *)
(* Interface Variables Removal Modf *)
(* ******************************************* *)

(*

  This module filter will take care of sanitizing interfaces for targets that do not support
  variables declaration in interfaces. By now this will mean that if anything is typed as the interface,
  and a variable access is made, a FNotFound will be returned for the field_access, so
  the field will be only accessible by reflection.
  Speed-wise, ideally it would be best to create getProp/setProp functions in this case and change
  the AST to call them when accessing by interface. (TODO)
  But right now it will be accessed by reflection.

  dependencies:


*)

module InterfaceVarsDeleteModf =
struct

  let name = "interface_vars"

  let priority = solve_deps name []

  let run gen =
    let run md = match md with
      | TClassDecl ( { cl_interface = true } as cl ) ->
        let to_add = ref [] in
        let fields = List.filter (fun cf ->
          match cf.cf_kind with
            | Var vkind ->
              (match vkind.v_read with
                | AccCall str ->
                  let newcf = mk_class_field str (TFun([],cf.cf_type)) true cf.cf_pos (Method MethNormal) [] in
                  to_add := newcf :: !to_add;
                | _ -> ()
              );
              (match vkind.v_write with
                | AccCall str ->
                  let newcf = mk_class_field str (TFun(["val",false,cf.cf_type],cf.cf_type)) true cf.cf_pos (Method MethNormal) [] in
                  to_add := newcf :: !to_add;
                | _ -> ()
              );
              cl.cl_fields <- PMap.remove cf.cf_name cl.cl_fields;
              false
            | _ -> true
        ) cl.cl_ordered_fields in

        cl.cl_ordered_fields <- fields;

        List.iter (fun cf ->
          if not (PMap.mem cf.cf_name cl.cl_fields) then begin
            cl.cl_ordered_fields <- cf :: cl.cl_ordered_fields;
            cl.cl_fields <- PMap.add cf.cf_name cf cl.cl_fields
          end
        ) !to_add;

        md
      | _ -> md
    in
    run

  let configure gen =
    let run = run gen in
    let map md = Some(run md) in
    gen.gmodule_filters#add ~name:name ~priority:(PCustom priority) map


end;;

(* ******************************************* *)
(* Int Division Synf *)
(* ******************************************* *)

(*

  On targets that support int division, this module will force a float division to be performed,
  so compatibility with current haxe targets is ensured.
  If catch_int_div is set to true, though, it will look for casts to int or use of Std.int() to optimize
  this kind of operation.

  dependencies:
    since it depends on nothing, but many modules might generate division expressions,
    it will be one of the last modules to run

*)

module IntDivisionSynf =
struct

  let name = "int_division_synf"

  let priority = solve_deps name [ DAfter ExpressionUnwrap.priority; DAfter ObjectDeclMap.priority; DAfter ArrayDeclSynf.priority ]

  let is_int = like_int

  let default_implementation gen catch_int_div =
    let basic = gen.gcon.basic in
    let rec run e =
      match e.eexpr with
        | TBinop((Ast.OpDiv as op), e1, e2) when is_int e1.etype && is_int e2.etype ->
          { e with eexpr = TBinop(op, mk_cast basic.tfloat (run e1), run e2) }
        | TCall(
            { eexpr = TField(_, FStatic({ cl_path = ([], "Std") }, { cf_name = "int" })) },
            [ ({ eexpr = TBinop((Ast.OpDiv as op), e1, e2) } as ebinop ) ]
          ) when catch_int_div && is_int e1.etype && is_int e2.etype ->
          { ebinop with eexpr = TBinop(op, run e1, run e2); etype = basic.tint }
        | TCast( ({ eexpr = TBinop((Ast.OpDiv as op), e1, e2) } as ebinop ), _ )
        | TCast( ({ eexpr = TBinop(( (Ast.OpAssignOp Ast.OpDiv) as op), e1, e2) } as ebinop ), _ ) when catch_int_div && is_int e1.etype && is_int e2.etype && is_int e.etype ->
          { ebinop with eexpr = TBinop(op, run e1, run e2); etype = basic.tint }
        | _ -> Type.map_expr run e
    in
    run

  let configure gen (mapping_func:texpr->texpr) =
    let map e = Some(mapping_func e) in
    gen.gsyntax_filters#add ~name:name ~priority:(PCustom priority) map

end;;

(* ******************************************* *)
(* UnnecessaryCastsRemoval *)
(* ******************************************* *)

(*

  This module will take care of simplifying unnecessary casts, specially those made by the compiler
  when inlining. Right now, it will only take care of casts used as a statement, which are always useless;
  TODO: Take care of more cases, e.g. when the to and from types are the same

  dependencies:
    This must run after CastDetection, but before ExpressionUnwrap

*)

module UnnecessaryCastsRemoval =
struct

  let name = "casts_removal"

  let priority = solve_deps name [DAfter CastDetect.priority; DBefore ExpressionUnwrap.priority]

  let rec take_off_cast run e =
    match e.eexpr with
      | TCast (c, _) ->
        take_off_cast run c
      | _ -> run e

  let default_implementation gen =
    let rec traverse e =
      match e.eexpr with
        | TBlock bl ->
          let bl = List.map (fun e ->
            take_off_cast traverse e
          ) bl in
          { e with eexpr = TBlock bl }
        | TTry (block, catches) ->
          { e with eexpr = TTry(traverse (mk_block block), List.map (fun (v,block) -> (v, traverse (mk_block block))) catches) }
        | TMatch (cond,ep,il_vol_e_l,default) ->
          { e with eexpr = TMatch(cond,ep,List.map (fun (il,vol,e) -> (il,vol,traverse (mk_block e))) il_vol_e_l, Option.map (fun e -> traverse (mk_block e)) default) }
        | TSwitch (cond,el_e_l, default) ->
          { e with eexpr = TSwitch(cond, List.map (fun (el,e) -> (el, traverse (mk_block e))) el_e_l, Option.map (fun e -> traverse (mk_block e)) default) }
        | TWhile (cond,block,flag) ->
          {e with eexpr = TWhile(cond,traverse (mk_block block), flag) }
        | TIf (cond, eif, eelse) ->
          { e with eexpr = TIf(cond, traverse (mk_block eif), Option.map (fun e -> traverse (mk_block e)) eelse) }
        | TFor (v,it,block) ->
          { e with eexpr = TFor(v,it, traverse (mk_block block)) }
        | TFunction (tfunc) ->
          { e with eexpr = TFunction({ tfunc with tf_expr = traverse (mk_block tfunc.tf_expr) }) }
        | _ -> e (* if expression doesn't have a block, we will exit *)
    in
    traverse

  let configure gen =
    let map e = Some(default_implementation gen e) in
    gen.gsyntax_filters#add ~name:name ~priority:(PCustom priority) map

end;;

(* ******************************************* *)
(* OverrideFix *)
(* ******************************************* *)

(*

  When DCE is on, sometimes a field is marked as override when it
  really doesn't override anything. This module filter will take care of this.

  dependencies:
    No dependencies

*)

module OverrideFix =
struct

  let name = "override_fix"

  let priority = solve_deps name []

  let default_implementation gen =
    let rec run e =
      match e.eexpr with
        | _ -> Type.map_expr run e
    in
    run

  let configure gen =
    let map md =
      match md with
        | TClassDecl cl ->
          cl.cl_overrides <- List.filter (fun s ->
            let rec loop cl =
              match cl.cl_super with
                | Some (cl,_) when PMap.mem s.cf_name cl.cl_fields -> true
                | Some (cl,_) -> loop cl
                | None -> false
            in
            loop cl
          ) cl.cl_overrides;
          Some md
        | _ -> Some md
    in
    gen.gmodule_filters#add ~name:name ~priority:(PCustom priority) map

end;;

(* ******************************************* *)
(* AbstractImplementationFix *)
(* ******************************************* *)

(*

  This module filter will map the compiler created classes from abstract
  implementations to valid haxe code, as needed by gencommon

  dependencies:
    No dependencies

*)

module AbstractImplementationFix =
struct

  let name = "abstract_implementation_fix"

  let priority = solve_deps name []

  let default_implementation gen =
    let rec run md =
      match md with
        | TClassDecl ({ cl_kind = KAbstractImpl a } as c) ->
            List.iter (function
              | cf when Meta.has Meta.Impl cf.cf_meta ->
                  (* add type parameters to all implementation functions *)
                  cf.cf_params <- cf.cf_params @ a.a_types
              | _ -> ()
            ) c.cl_ordered_statics;
            Some md
        | _ -> Some md
    in
    run

  let configure gen =
    let map = default_implementation gen in
    gen.gmodule_filters#add ~name:name ~priority:(PCustom priority) map

end;;

(*
(* ******************************************* *)
(* Example *)
(* ******************************************* *)

(*

  description

  dependencies:


*)

module Example =
struct

  let name = "example"

  let priority = solve_deps name []

  let default_implementation gen =
    let rec run e =
      match e.eexpr with
        | _ -> Type.map_expr run e
    in
    run

  let configure gen (mapping_func:texpr->texpr) =
    let map e = Some(mapping_func e) in
    gen.gsyntax_filters#add ~name:name ~priority:(PCustom priority) map

end;;
*)
