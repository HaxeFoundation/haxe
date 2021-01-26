(*
	The Haxe Compiler
	Copyright (C) 2005-2019  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*)

(*
	Gen Common API

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
open Unix
open Ast
open Type
open Common
open Globals
open Option
open Printf
open ExtString
open Codegen
open Overloads

(* ******************************************* *)
(* common helpers *)
(* ******************************************* *)

let rec like_float t =
	match follow t with
	| TAbstract ({ a_path = ([], "Float") }, [])
	| TAbstract ({ a_path = ([], "Int") }, []) ->
		true
	| TAbstract ({ a_path = (["cs"], "Pointer") }, _) ->
		false
	| TAbstract (a, _) ->
		List.exists like_float a.a_from || List.exists like_float a.a_to
	| _ ->
		false

let rec like_int t =
	match follow t with
	| TAbstract ({ a_path = ([], "Int") }, []) ->
		true
	| TAbstract ({ a_path = (["cs"], "Pointer") }, _) ->
		false
	| TAbstract (a, _) ->
		List.exists like_int a.a_from || List.exists like_int a.a_to
	| _ ->
		false

let rec like_i64 t =
	match follow t with
	| TAbstract ({ a_path = (["cs"], ("Int64" | "UInt64")) }, [])
	| TAbstract ({ a_path = (["java"], "Int64") }, [])
	| TAbstract ({ a_path = (["haxe"], "Int64") }, []) ->
		true
	| TAbstract (a, _) ->
		List.exists like_i64 a.a_from || List.exists like_i64 a.a_to
	| _ ->
		false

let follow_once t =
	match t with
	| TMono r ->
		(match r.tm_type with
		| Some t -> t
		| _ -> t_dynamic) (* avoid infinite loop / should be the same in this context *)
	| TLazy f ->
		lazy_type f
	| TType (t,tl) ->
		apply_params t.t_params tl t.t_type
	| TAbstract({a_path = [],"Null"},[t]) ->
		t
	| _ ->
		t

let t_empty = mk_anon (ref Closed)

let alloc_var n t = Type.alloc_var VGenerated n t null_pos

let mk_local = Texpr.Builder.make_local

(* the undefined is a special var that works like null, but can have special meaning *)
let undefined =
	(fun pos -> mk (TIdent "__undefined__") t_dynamic pos)

let path_of_md_def md_def =
	match md_def.m_types with
		| [TClassDecl c] -> c.cl_path
		| _ -> md_def.m_path

let debug_type t = (s_type (print_context())) t
let debug_expr = s_expr debug_type

let debug_mode = ref false
let trace s = if !debug_mode then print_endline s else ()
let timer name = if !debug_mode then Timer.timer name else fun () -> ()

let is_string t =
	match follow t with
	| TInst({ cl_path = ([], "String") }, []) -> true
	| _ -> false

let anon_class t =
	match follow t with
	| TAnon anon ->
		(match !(anon.a_status) with
		| Statics cl -> Some (TClassDecl cl)
		| EnumStatics e -> Some (TEnumDecl e)
		| AbstractStatics a -> Some (TAbstractDecl a)
		| _ -> None)
	| _ -> None

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
			| _ -> die "" __LOC__)
	| TLazy f -> t_to_md (lazy_type f)
	| TMono r -> (match r.tm_type with | Some t -> t_to_md t | None -> die "" __LOC__)
	| _ -> die "" __LOC__


let get_cl mt = match mt with TClassDecl cl -> cl | _ -> failwith (Printf.sprintf "Unexpected module type (class expected) for %s: %s" (s_type_path (t_path mt)) (s_module_type_kind mt))
let get_abstract mt = match mt with TAbstractDecl a -> a | _ -> failwith (Printf.sprintf "Unexpected module type (abstract expected) for %s: %s" (s_type_path (t_path mt)) (s_module_type_kind mt))

let get_fun t =
	match follow t with
	| TFun (args, ret) -> args, ret
	| t -> (trace (debug_type t)); die "" __LOC__

let mk_cast t e = Type.mk_cast e t e.epos

(** TODO: when adding new AST, make a new cast type for those fast casts. For now, we're using this hack
 *        of using null_class to tell a fast cast from a normal one. Also note that this only works since both
 *        C# and Java do not use the second part of TCast for anything *)
let mk_castfast t e = { e with eexpr = TCast(e, Some (TClassDecl null_class)); etype = t }

let mk_static_field_access_infer cl field pos params =
	try
		let e_type = Texpr.Builder.make_static_this cl pos in
		let cf = PMap.find field cl.cl_statics in
		let t = if params = [] then cf.cf_type else apply_params cf.cf_params params cf.cf_type in
		mk (TField(e_type, FStatic(cl, cf))) t pos
	with Not_found ->
		failwith ("Cannot find field " ^ field ^ " in class " ^ (s_type_path cl.cl_path))

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
class ['tp, 'ret] rule_dispatcher name =
	object(self)
	val tbl = Hashtbl.create 16
	val mutable keys = []
	val names = Hashtbl.create 16

	method add (name : string) (* name helps debugging *) (priority : priority) (rule : 'tp->'ret option) =
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
		(if Hashtbl.mem names name then raise (DuplicateName(name)));
		Hashtbl.add names name q;

		Stack.push (name, rule) q

	method describe =
		Hashtbl.iter (fun s _ -> (trace s)) names;

	method run_f tp = get (self#run tp)

	method run_from (priority:float) (tp:'tp) : 'ret option =
		let ok = ref false in
		let ret = ref None in
		indent := "\t" :: !indent;

		(try begin
			List.iter (fun key ->
				if key < priority then begin
					let q = Hashtbl.find tbl key in
					Stack.iter (fun (n, rule) ->
						let t = if !debug_mode then Timer.timer [("rule dispatcher rule: " ^ n)] else fun () -> () in
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
class ['tp] rule_map_dispatcher name = object(self)
	val tbl = Hashtbl.create 16
	val mutable keys = []
	val names = Hashtbl.create 16

	method add (name : string) (* name helps debugging *) (priority : priority) (rule : 'tp->'tp) =
		let p = match priority with
			| PFirst -> infinity
			| PLast -> neg_infinity
			| PZero -> 0.0
			| PCustom i -> i
		in
		let q = if not (Hashtbl.mem tbl p) then begin
			let q = Stack.create() in
			Hashtbl.add tbl p q;
			keys <- p :: keys;
			keys <- List.sort (fun x y -> - (compare x y)) keys;
			q
		end else Hashtbl.find tbl p in
		if Hashtbl.mem names name then raise (DuplicateName name);
		Hashtbl.add names name q;

		Stack.push (name, rule) q

	method describe =
		Hashtbl.iter (fun s _ -> (trace s)) names;

	method run (tp:'tp) : 'tp =
		self#run_from infinity tp

	method run_from (priority:float) (tp:'tp) : 'tp =
		let cur = ref tp in
		List.iter (fun key ->
			if key < priority then begin
				let q = Hashtbl.find tbl key in
				Stack.iter (fun (n, rule) ->
					trace ("running rule " ^ n);
					let t = if !debug_mode then Timer.timer [("rule map dispatcher rule: " ^ n)] else fun () -> () in
					cur := rule !cur;
					t();
				) q
			end
		) keys;
		!cur
end;;


type generator_ctx =
{
	(* these are the basic context fields. If another target is using this context, *)
	(* this is all you need to care about *)
	gcon : Common.context;

	gentry_point : (string * tclass * texpr) option;

	gclasses : gen_classes;

	gtools : gen_tools;

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
	mutable gtypes_list : module_type list;
	mutable gmodules : Type.module_def list;

	(* cast detection helpers / settings *)
	(* this is a cache for all field access types *)
	greal_field_types : (path * string, (tclass_field (* does the cf exist *) * t (*cf's type in relation to current class type params *) * t * tclass (* declared class *) ) option) Hashtbl.t;
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
	(* during expr filters, add_to_module will be available so module_types can be added to current module_def. we must pass the priority argument so the filters can be resumed	*)
	mutable gadd_to_module : module_type -> float -> unit;
	(* during expr filters, shows the current class path *)
	mutable gcurrent_path : path;
	(* current class *)
	mutable gcurrent_class : tclass option;
	(* current class field, if any *)
	mutable gcurrent_classfield : tclass_field option;

	(* events *)
	(* after module filters ended *)
	mutable gafter_mod_filters_ended : (unit -> unit) list;
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

	(* internal apis *)
	(* param_func_call : used by RealTypeParams and CastDetection *)
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

	mutable nativearray_len : texpr -> pos -> texpr;
	mutable nativearray_type : Type.t -> Type.t;
	mutable nativearray : Type.t -> Type.t;
}

(* add here all reflection transformation additions *)
and gen_tools =
{
	(* Reflect.fields(). The bool is if we are iterating in a read-only manner. If it is read-only we might not need to allocate a new array *)
	r_fields : bool->texpr->texpr;
	(* (first argument = return type. should be void in most cases) Reflect.setField(obj, field, val) *)
	r_set_field : t->texpr->texpr->texpr->texpr;
	(* Reflect.field. bool indicates if is safe (no error throwing) or unsafe; t is the expected return type true = safe *)
	r_field : bool->t->texpr->texpr->texpr;

	(*
		return an expression that creates an unitialized instance of a class, used for the generic cast helper method.
	*)
	mutable r_create_empty : tclass->tparams->pos->texpr;
}

(**
	Function that receives a desired name and makes it "internal", doing the best to ensure that it will not be called from outside.
	To avoid name clashes between internal names, user must specify two strings: a "namespace" and the name itself
*)
let mk_internal_name ns name = Printf.sprintf "__%s_%s" ns name

let mk_temp, reset_temps =
	let tmp_count = ref 0 in
	(fun name t ->
		incr tmp_count;
		let name = mk_internal_name "temp" (name ^ (string_of_int !tmp_count)) in
		alloc_var name t
	),
	(fun () -> tmp_count := 0)

let new_ctx con =
	let types = Hashtbl.create (List.length con.types) in
	List.iter (fun mt ->
		match mt with
			| TClassDecl cl -> Hashtbl.add types cl.cl_path mt
			| TEnumDecl e -> Hashtbl.add types e.e_path mt
			| TTypeDecl t -> Hashtbl.add types t.t_path mt
			| TAbstractDecl a ->
				(* There are some cases where both an abstract and a class
				   have the same name (e.g. java.lang.Double/Integer/etc)
				   in this case we generally want the class to have priority *)
				if not (Hashtbl.mem types a.a_path) then
					Hashtbl.add types a.a_path mt
	) con.types;

	let get_type path =
		List.find (fun md -> (t_path md) = path) con.types
	in

	let cl_dyn = match get_type  ([], "Dynamic") with
		| TClassDecl c -> c
		| TAbstractDecl a ->
				mk_class a.a_module ([], "Dynamic") a.a_pos null_pos
		| _ -> die "" __LOC__
	in

	let rec gen = {
		gcon = con;
		gentry_point = get_entry_point con;
		gclasses = {
			cl_reflect = get_cl (get_type ([], "Reflect"));
			cl_type = get_cl (get_type ([], "Type"));
			cl_dyn = cl_dyn;

			nativearray = (fun _ -> die "" __LOC__);
			nativearray_type = (fun _ -> die "" __LOC__);
			nativearray_len = (fun _ -> die "" __LOC__);
		};
		gtools = {
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

			r_create_empty = (fun _ _ pos -> gen.gcon.error "r_create_empty implementation is not provided" pos; die "" __LOC__);
		};
		gexpr_filters = new rule_map_dispatcher "gexpr_filters";
		gmodule_filters = new rule_map_dispatcher "gmodule_filters";
		gsyntax_filters = new rule_map_dispatcher "gsyntax_filters";
		gfollow = new rule_dispatcher "gfollow";
		gtypes = types;
		gtypes_list = con.types;
		gmodules = con.modules;

		greal_field_types = Hashtbl.create 0;
		ghandle_cast = (fun to_t from_t e -> mk_cast to_t e);
		gon_unsafe_cast = (fun t t2 pos -> (gen.gcon.warning ("Type " ^ (debug_type t2) ^ " is being cast to the unrelated type " ^ (s_type (print_context()) t)) pos));
		gneeds_box = (fun t -> false);
		gspecial_needs_cast = (fun to_t from_t -> false);
		gsupported_conversions = Hashtbl.create 0;

		gadd_type = (fun md should_filter ->
			if should_filter then begin
				gen.gtypes_list <- md :: gen.gtypes_list;
				gen.gmodules <- { m_id = alloc_mid(); m_path = (t_path md); m_types = [md]; m_statics = None; m_extra = module_extra "" "" 0. MFake [] } :: gen.gmodules;
				Hashtbl.add gen.gtypes (t_path md) md;
			end else gen.gafter_filters_ended <- (fun () ->
				gen.gtypes_list <- md :: gen.gtypes_list;
				gen.gmodules <- { m_id = alloc_mid(); m_path = (t_path md); m_types = [md]; m_statics = None; m_extra = module_extra "" "" 0. MFake [] } :: gen.gmodules;
				Hashtbl.add gen.gtypes (t_path md) md;
			) :: gen.gafter_filters_ended;
		);
		gadd_to_module = (fun md pr -> failwith "module added outside expr filters");
		gcurrent_path = ([],"");
		gcurrent_class = None;
		gcurrent_classfield = None;

		gafter_mod_filters_ended = [];
		gafter_expr_filters_ended = [];
		gafter_filters_ended = [];

		gbase_class_fields = PMap.empty;

		greal_type = (fun t -> t);
		greal_type_param = (fun _ t -> t);

		gallow_tp_dynamic_conversion = false;

		(* as a default, ignore the params *)
		gparam_func_call = (fun ecall efield params elist -> { ecall with eexpr = TCall(efield, elist) });
		ghas_tparam_cast_handler = false;
		gtparam_cast = Hashtbl.create 0;

		gspecial_vars = Hashtbl.create 0;
	} in
	gen

let init_ctx gen =
	(* ultimately add a follow once handler as the last follow handler *)
	let follow_f = gen.gfollow#run in
	let follow t =
		match t with
		| TMono r ->
			(match r.tm_type with
			| Some t -> follow_f t
			| _ -> Some t)
		| TLazy f ->
			follow_f (lazy_type f)
		| TType (t,tl) ->
			follow_f (apply_params t.t_params tl t.t_type)
		| TAbstract({a_path = [],"Null"},[t]) ->
			follow_f t
		| _ -> Some t
	in
	gen.gfollow#add "final" PLast follow

let run_follow gen = gen.gfollow#run_f

let reorder_modules gen =
	let modules = Hashtbl.create 20 in
	List.iter (fun md ->
		Hashtbl.add modules ( (t_infos md).mt_module ).m_path md
	) gen.gtypes_list;

	gen.gmodules <- [];
	let processed = Hashtbl.create 20 in
	Hashtbl.iter (fun md_path md ->
		if not (Hashtbl.mem processed md_path) then begin
			Hashtbl.add processed md_path true;
			gen.gmodules <- { m_id = alloc_mid(); m_path = md_path; m_types = List.rev ( Hashtbl.find_all modules md_path ); m_statics = None; m_extra = (t_infos md).mt_module.m_extra } :: gen.gmodules
		end
	) modules

let run_filters_from gen t filters =
	match t with
	| TClassDecl c when not (FiltersCommon.is_removable_class c) ->
		trace (snd c.cl_path);
		gen.gcurrent_path <- c.cl_path;
		gen.gcurrent_class <- Some(c);

		gen.gcurrent_classfield <- None;
		let rec process_field f =
			reset_temps();
			gen.gcurrent_classfield <- Some(f);

			trace f.cf_name;
			(match f.cf_expr with
			| None -> ()
			| Some e ->
				f.cf_expr <- Some (List.fold_left (fun e f -> f e) e filters));
			List.iter process_field f.cf_overloads;
		in
		List.iter process_field c.cl_ordered_fields;
		List.iter process_field c.cl_ordered_statics;

		(match c.cl_constructor with
		| None -> ()
		| Some f -> process_field f);
		gen.gcurrent_classfield <- None;
		(match c.cl_init with
		| None -> ()
		| Some e ->
			c.cl_init <- Some (List.fold_left (fun e f -> f e) e filters));
	| TClassDecl _ | TEnumDecl _ | TTypeDecl _ | TAbstractDecl _ ->
		()

let run_filters gen =
	let last_error = gen.gcon.error in
	let has_errors = ref false in
	gen.gcon.error <- (fun msg pos -> has_errors := true; last_error msg pos);
	(* first of all, we have to make sure that the filters won't trigger a major Gc collection *)
	let t = Timer.timer ["gencommon_filters"] in
	(if Common.defined gen.gcon Define.GencommonDebug then debug_mode := true else debug_mode := false);
	let run_filters (filter : texpr rule_map_dispatcher) =
		let rec loop acc mds =
			match mds with
				| [] -> acc
				| md :: tl ->
					let filters = [ filter#run ] in
					let added_types = ref [] in
					gen.gadd_to_module <- (fun md_type priority ->
						gen.gtypes_list <- md_type :: gen.gtypes_list;
						added_types := (md_type, priority) :: !added_types
					);

					run_filters_from gen md filters;

					let added_types = List.map (fun (t,p) ->
						run_filters_from gen t [ fun e -> filter#run_from p e ];
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
		List.rev (loop [] gen.gtypes_list)
	in

	let run_mod_filter (filter : module_type rule_map_dispatcher) =
		let last_add_to_module = gen.gadd_to_module in
		let added_types = ref [] in
		gen.gadd_to_module <- (fun md_type priority ->
			Hashtbl.add gen.gtypes (t_path md_type) md_type;
			added_types := (md_type, priority) :: !added_types
		);

		let rec loop processed not_processed =
			match not_processed with
				| hd :: tl ->
					(match hd with
						| TClassDecl c ->
							gen.gcurrent_class <- Some c
						| _ ->
							gen.gcurrent_class <- None);
					let new_hd = filter#run hd in

					let added_types_new = !added_types in
					added_types := [];
					let added_types = List.map (fun (t,p) -> filter#run_from p t) added_types_new in

					loop ( added_types @ (new_hd :: processed) ) tl
				| [] ->
					processed
		in

		let filtered = loop [] gen.gtypes_list in
		gen.gadd_to_module <- last_add_to_module;
		gen.gtypes_list <- List.rev (filtered)
	in

	run_mod_filter gen.gmodule_filters;
	List.iter (fun fn -> fn()) gen.gafter_mod_filters_ended;

	let last_add_to_module = gen.gadd_to_module in
	gen.gtypes_list <- run_filters gen.gexpr_filters;
	gen.gadd_to_module <- last_add_to_module;

	List.iter (fun fn -> fn()) gen.gafter_expr_filters_ended;
	gen.gtypes_list <- run_filters gen.gsyntax_filters;
	List.iter (fun fn -> fn()) gen.gafter_filters_ended;

	reorder_modules gen;
	t();
	if !has_errors then raise (Abort("Compilation aborted with errors",null_pos))

(* ******************************************* *)
(* basic generation module that source code compilation implementations can use *)
(* ******************************************* *)

let write_file gen w source_dir path extension out_files =
	let t = timer ["write";"file"] in
	let s_path = source_dir	^ "/" ^ (snd path) ^ "." ^ (extension) in
	(* create the folders if they don't exist *)
	Path.mkdir_from_path s_path;

	let contents = SourceWriter.contents w in
	let should_write = if not (Common.defined gen.gcon Define.ReplaceFiles) && Sys.file_exists s_path then begin
		let in_file = open_in s_path in
		let old_contents = Std.input_all in_file in
		close_in in_file;
		contents <> old_contents
	end else true in

	if should_write then begin
		let f = open_out_bin s_path in
		output_string f contents;
		close_out f
	end;

	out_files := (gen.gcon.file_keys#get s_path) :: !out_files;

	t()


let clean_files gen path excludes verbose =
	let rec iter_files pack dir path = try
		let file = Unix.readdir dir in

		if file <> "." && file <> ".." then begin
			let filepath = path ^ "/" ^ file in
			if (Unix.stat filepath).st_kind = S_DIR then
				let pack = pack @ [file] in
				iter_files (pack) (Unix.opendir filepath) filepath;
				try Unix.rmdir filepath with Unix.Unix_error (ENOTEMPTY,_,_) -> ();
			else if not (String.ends_with filepath ".meta") && not (List.mem (gen.gcon.file_keys#get filepath) excludes) then begin
				if verbose then print_endline ("Removing " ^ filepath);
			 	Sys.remove filepath
			end
		end;

		iter_files pack dir path
	with | End_of_file | Unix.Unix_error _ ->
		Unix.closedir dir
	in
	iter_files [] (Unix.opendir path) path


let dump_descriptor gen name path_s module_s =
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
	) gen.gcon.defines.Define.values;
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
	) gen.gcon.defines.Define.values;
	SourceWriter.write w "end defines_data";
	SourceWriter.newline w;
	(* dump all generated types *)
	SourceWriter.write w "begin modules";
	SourceWriter.newline w;
	let main_paths = Hashtbl.create 0 in
	List.iter (fun md_def ->
		SourceWriter.write w "M ";
		SourceWriter.write w (path_s (path_of_md_def md_def));
		SourceWriter.newline w;
		List.iter (fun m ->
			match m with
				| TClassDecl cl when not (has_class_flag cl CExtern) ->
					SourceWriter.write w "C ";
					let s = module_s m in
					Hashtbl.add main_paths cl.cl_path s;
					SourceWriter.write w (s);
					SourceWriter.newline w
				| TEnumDecl e when not e.e_extern ->
					SourceWriter.write w "E ";
					SourceWriter.write w (module_s m);
					SourceWriter.newline w
				| _ -> () (* still no typedef or abstract is generated *)
		) md_def.m_types
	) gen.gmodules;
	SourceWriter.write w "end modules";
	SourceWriter.newline w;
	(* dump all resources *)
	(match gen.gentry_point with
	| Some (_,cl,_) ->
		SourceWriter.write w "begin main";
		SourceWriter.newline w;
		let path = cl.cl_path in
		(try
			SourceWriter.write w (Hashtbl.find main_paths path)
		with Not_found ->
			SourceWriter.write w (path_s path));
		SourceWriter.newline w;
		SourceWriter.write w "end main";
		SourceWriter.newline w
	| _ -> ());
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
	let path file ext =
		if Sys.file_exists file then
			file
		else try Common.find_file gen.gcon file with
			| Not_found -> try Common.find_file gen.gcon (file ^ ext) with
			| Not_found ->
				file
	in
	if Common.platform gen.gcon Java then
		List.iter (fun java_lib ->
			if not (java_lib#has_flag NativeLibraries.FlagIsStd) && not (java_lib#has_flag NativeLibraries.FlagIsExtern) then begin
				SourceWriter.write w (path java_lib#get_file_path ".jar");
				SourceWriter.newline w;
			end
		) gen.gcon.native_libs.java_libs
	else if Common.platform gen.gcon Cs then
		List.iter (fun net_lib ->
			if not (net_lib#has_flag NativeLibraries.FlagIsStd) && not (net_lib#has_flag NativeLibraries.FlagIsExtern) then begin
				SourceWriter.write w (path net_lib#get_name ".dll");
				SourceWriter.newline w;
			end
		) gen.gcon.native_libs.net_libs;
	SourceWriter.write w "end libs";
	SourceWriter.newline w;
	let args = gen.gcon.c_args in
	if args <> [] then begin
		SourceWriter.write w "begin opts";
		SourceWriter.newline w;
		List.iter (fun opt -> SourceWriter.write w opt; SourceWriter.newline w) (List.rev args);
		SourceWriter.write w "end opts";
		SourceWriter.newline w;
	end;

	let contents = SourceWriter.contents w in
	let f = open_out (gen.gcon.file ^ "/" ^ name) in
	output_string f contents;
	close_out f

(*
	various helper functions
*)

let mk_paren e =
	match e.eexpr with | TParenthesis _ -> e | _ -> { e with eexpr=TParenthesis(e) }

(* private *)

let get_real_fun gen t =
	match follow t with
	| TFun(args,t) -> TFun(List.map (fun (n,o,t) -> n,o,gen.greal_type t) args, gen.greal_type t)
	| _ -> t

let mk_nativearray_decl gen t el pos =
	mk (TCall (mk (TIdent "__array__") t_dynamic pos, el)) (gen.gclasses.nativearray t) pos


(**
	Wraps rest arguments into a native array.
	E.g. transforms params from `callee(param, rest1, rest2, ..., restN)` into
	`callee(param, untyped __array__(rest1, rest2, ..., restN))`
*)
let wrap_rest_args gen callee_type params p =
	match follow callee_type with
	| TFun(args, _) ->
		let rec loop args params =
			match args, params with
			(* last argument expects rest parameters *)
			| [(_,_,t)], params when ExtType.is_rest (follow t) ->
				(match params with
				(* In case of `...rest` just use `rest` *)
				| [{ eexpr = TUnop(Spread,Prefix,e) }] -> [e]
				(* In other cases: `untyped __array__(param1, param2, ...)` *)
				| _ ->
					match Abstract.follow_with_abstracts t with
					| TInst ({ cl_path = _,"NativeArray" }, [t1]) ->
						let pos = punion_el (List.map (fun e -> ((),e.epos)) params) in
						let t1 = if Common.defined gen.gcon Define.EraseGenerics then t_dynamic else t1 in
						[mk_nativearray_decl gen t1 params pos]
					| _ ->
						die ~p "Unexpected rest arguments type" __LOC__
				)
			| a :: args, e :: params ->
				e :: loop args params
			| [], params ->
				params
			| _ :: _, [] ->
				[]
		in
		loop args params
	| _ -> params

let ensure_local com block name e =
	match e.eexpr with
	| TLocal _ -> e
	| _ ->
		let v = mk_temp name e.etype in
		block := (mk (TVar (v, Some e)) com.basic.tvoid e.epos) :: !block;
		mk_local v e.epos

let follow_module follow_func md = match md with
	| TClassDecl _
	| TEnumDecl _
	| TAbstractDecl _ -> md
	| TTypeDecl tdecl -> match (follow_func (TType(tdecl, List.map snd tdecl.t_params))) with
		| TInst(cl,_) -> TClassDecl cl
		| TEnum(e,_) -> TEnumDecl e
		| TType(t,_) -> TTypeDecl t
		| TAbstract(a,_) -> TAbstractDecl a
		| _ -> die "" __LOC__

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
		| TClassDecl cl -> TInst(cl, List.map (fun _ -> t_dynamic) cl.cl_params)
		| TEnumDecl e -> TEnum(e, List.map (fun _ -> t_dynamic) e.e_params)
		| TAbstractDecl a -> TAbstract(a, List.map (fun _ -> t_dynamic) a.a_params)
		| TTypeDecl t -> TType(t, List.map (fun _ -> t_dynamic) t.t_params)

(* replace open TMonos with TDynamic *)
let rec replace_mono t =
	match t with
	| TMono t ->
		(match t.tm_type with
		| None -> Monomorph.bind t t_dynamic
		| Some _ -> ())
	| TEnum (_,p) | TInst (_,p) | TType (_,p) | TAbstract (_,p) ->
		List.iter replace_mono p
	| TFun (args,ret) ->
		List.iter (fun (_,_,t) -> replace_mono t) args;
		replace_mono ret
	| TAnon _
	| TDynamic _ -> ()
	| TLazy f ->
		replace_mono (lazy_type f)

(* helper *)
let mk_class_field ?(static = false) name t public pos kind params =
	let f = mk_field name ~public ~static t pos null_pos in
	f.cf_meta <- [ Meta.CompilerGenerated, [], null_pos ]; (* annotate that this class field was generated by the compiler *)
	f.cf_kind <- kind;
	f.cf_params <- params;
	f

(* this helper just duplicates the type parameter class, which is assumed that cl is. *)
(* This is so we can use class parameters on function parameters, without running the risk of name clash *)
(* between both *)
let map_param cl =
	let ret = mk_class cl.cl_module (fst cl.cl_path, snd cl.cl_path ^ "_c") cl.cl_pos null_pos in
	ret.cl_implements <- cl.cl_implements;
	ret.cl_kind <- cl.cl_kind;
	ret

let get_cl_t t =
	match follow t with | TInst (cl,_) -> cl | _ -> die "" __LOC__

let mk_class m path pos =
	let cl = Type.mk_class m path pos null_pos in
	cl.cl_meta <- [ Meta.CompilerGenerated, [], null_pos ];
	cl

type tfield_access =
	| FClassField of tclass * tparams * tclass (* declared class *) * tclass_field * bool (* is static? *) * t (* the actual cf type, in relation to the class type params *) * t (* declared type *)
	| FEnumField of tenum * tenum_field * bool (* is parameterized enum ? *)
	| FAnonField of tclass_field
	| FDynamicField of t
	| FNotFound

let is_var f = match f.cf_kind with | Var _ -> true | _ -> false

let find_first_declared_field gen orig_cl ?get_vmtype ?exact_field field =
	let get_vmtype = match get_vmtype with None -> (fun t -> t) | Some f -> f in
	let chosen = ref None in
	let is_overload = ref false in
	let rec loop_cl depth c tl tlch =
		(try
			let ret = PMap.find field c.cl_fields in
			if has_class_field_flag ret CfOverload then is_overload := true;
			match !chosen, exact_field with
			| Some(d,f,_,_,_), _ when depth <= d || (is_var ret && not (is_var f)) -> ()
			| _, None ->
				chosen := Some(depth,ret,c,tl,tlch)
			| _, Some f2 ->
				List.iter (fun f ->
					let declared_t = apply_params c.cl_params tl f.cf_type in
					if same_overload_args ~get_vmtype declared_t f2.cf_type f f2 then
						chosen := Some(depth,f,c,tl,tlch)
				) (ret :: ret.cf_overloads)
		with | Not_found -> ());
		(match c.cl_super with
		| Some (sup,stl) ->
			let tl = List.map (apply_params c.cl_params tl) stl in
			let stl = gen.greal_type_param (TClassDecl sup) stl in
			let tlch = List.map (apply_params c.cl_params tlch) stl in
			loop_cl (depth+1) sup tl tlch
		| None -> ());
		if (has_class_flag c CInterface) then
			List.iter (fun (sup,stl) ->
				let tl = List.map (apply_params c.cl_params tl) stl in
				let stl = gen.greal_type_param (TClassDecl sup) stl in
				let tlch = List.map (apply_params c.cl_params tlch) stl in
				loop_cl (depth+1) sup tl tlch
			) c.cl_implements
	in
	loop_cl 0 orig_cl (List.map snd orig_cl.cl_params) (List.map snd orig_cl.cl_params);
	match !chosen with
	| None ->
		None
	| Some(_,f,c,tl,tlch) ->
		if !is_overload && not (has_class_field_flag f CfOverload) then
			add_class_field_flag f CfOverload;
		let declared_t = apply_params c.cl_params tl f.cf_type in
		let params_t = apply_params c.cl_params tlch f.cf_type in
		let actual_t = match follow params_t with
		| TFun(args,ret) -> TFun(List.map (fun (n,o,t) -> (n,o,gen.greal_type t)) args, gen.greal_type ret)
		| _ -> gen.greal_type params_t in
		Some(f,actual_t,declared_t,params_t,c,tl,tlch)

let rec field_access gen (t:t) (field:string) : (tfield_access) =
	(*
		t can be either an haxe-type as a real-type;
		'follow' should be applied here since we can generalize that a TType will be accessible as its
		underlying type.
	*)

	(* let pointers to values be accessed as the underlying values *)
	let t = match gen.greal_type t with
		| TAbstract({ a_path = ["cs"],"Pointer" },[t]) ->
			gen.greal_type t
		| _ -> t
	in

	match follow t with
		| TInst(cl, params) ->
			let orig_cl = cl in
			let orig_params = params in
			let rec not_found cl params =
				match cl.cl_dynamic with
					| Some t ->
						let t = apply_params cl.cl_params params t in
						FDynamicField t
					| None ->
						match cl.cl_super with
							| None -> FNotFound
							| Some (super,p) ->  not_found super p
			in

			let not_found () =
				try
					let cf = PMap.find field gen.gbase_class_fields in
					FClassField (orig_cl, orig_params, gen.gclasses.cl_dyn, cf, false, cf.cf_type, cf.cf_type)
				with
					| Not_found -> not_found cl params
			in

			(* this is a hack for C#'s different generic types with same path *)
			let hashtbl_field = (String.concat "" (List.map (fun _ -> "]") cl.cl_params)) ^ field in
			let types = try
				Hashtbl.find gen.greal_field_types (orig_cl.cl_path, hashtbl_field)
			with | Not_found ->
				let ret = find_first_declared_field gen cl field in
				let ret = match ret with
					| None -> None
					| Some(cf,t,dt,_,cl,_,_) -> Some(cf,t,dt,cl)
				in
				if ret <> None then Hashtbl.add gen.greal_field_types (orig_cl.cl_path, hashtbl_field) ret;
				ret
			in
			(match types with
					| None -> not_found()
					| Some (cf, actual_t, declared_t, declared_cl) ->
						FClassField(orig_cl, orig_params, declared_cl, cf, false, actual_t, declared_t))
		| TEnum (en,params) when Meta.has Meta.Class en.e_meta ->
			(* A field access to an enum could mean accessing field of its generated class (e.g. `index` for switches).
			   Ideally, we should change all TEnum instances to relevant TInst instances so we never reach this case,
			   but for now, we're going to find the generated class and make a field access to it instead. *)
			(try
				let cl_enum = List.find (function TClassDecl cl when cl.cl_path = en.e_path && Meta.has Meta.Enum cl.cl_meta -> true | _ -> false) gen.gtypes_list in
				let cl_enum = match cl_enum with TClassDecl cl -> TInst (cl,params) | _ -> die "" __LOC__ in
				field_access gen cl_enum field
			with Not_found ->
				FNotFound)
		| TAnon anon ->
			(try match !(anon.a_status) with
				| Statics cl ->
					let cf = PMap.find field cl.cl_statics in
					FClassField(cl, List.map (fun _ -> t_dynamic) cl.cl_params, cl, cf, true, cf.cf_type, cf.cf_type)
				| EnumStatics e ->
					let f = PMap.find field e.e_constrs in
					let is_param = match follow f.ef_type with | TFun _ -> true | _ -> false in
					FEnumField(e, f, is_param)
				| _ when PMap.mem field gen.gbase_class_fields ->
					let cf = PMap.find field gen.gbase_class_fields in
					FClassField(gen.gclasses.cl_dyn, [t_dynamic], gen.gclasses.cl_dyn, cf, false, cf.cf_type, cf.cf_type)
				| _ ->
					FAnonField(PMap.find field anon.a_fields)
			with | Not_found -> FNotFound)
		| _ when PMap.mem field gen.gbase_class_fields ->
			let cf = PMap.find field gen.gbase_class_fields in
			FClassField(gen.gclasses.cl_dyn, [t_dynamic], gen.gclasses.cl_dyn, cf, false, cf.cf_type, cf.cf_type)
		| TDynamic t -> FDynamicField t
		| TMono _ -> FDynamicField t_dynamic
		| _ -> FNotFound

let field_access_esp gen t field = match field with
	| FStatic(cl,cf) | FInstance(cl,_,cf) when has_class_field_flag cf CfExtern ->
		let static = match field with
			| FStatic _ -> true
			| _ -> false
		in
		let p = match follow (run_follow gen t) with
			| TInst(_,p) -> p
			| _ -> List.map snd cl.cl_params
		in
		FClassField(cl,p,cl,cf,static,cf.cf_type,cf.cf_type)
	| _ -> field_access gen t (field_name field)

let mk_field_access gen expr field pos =
	match field_access gen expr.etype field with
		| FClassField(c,p,dc,cf,false,at,_) ->
				{ eexpr = TField(expr, FInstance(dc,p,cf)); etype = apply_params c.cl_params p at; epos = pos }
		| FClassField(c,p,dc,cf,true,at,_) ->
				{ eexpr = TField(expr, FStatic(dc,cf)); etype = at; epos = pos }
		| FAnonField cf ->
				{ eexpr = TField(expr, FAnon cf); etype = cf.cf_type; epos = pos }
		| FDynamicField t ->
				{ eexpr = TField(expr, FDynamic field); etype = t; epos = pos }
		| FNotFound ->
				{ eexpr = TField(expr, FDynamic field); etype = t_dynamic; epos = pos }
		| FEnumField _ -> die "" __LOC__

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


let fun_args l =
	List.map (fun (v,s) -> (v.v_name, (s <> None), v.v_type)) l

