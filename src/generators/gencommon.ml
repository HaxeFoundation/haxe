(*
	The Haxe Compiler
	Copyright (C) 2005-2016  Haxe Foundation

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
open Option
open Printf
open ExtString
open Codegen
open Overloads

let alloc_var n t = alloc_var n t null_pos

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
		| TAbstract({ a_path = (["cs"], "Pointer") },_) -> false
		| TAbstract(a, _) -> List.exists (fun t -> like_float t) a.a_from || List.exists (fun t -> like_float t) a.a_to
		| _ -> false

let rec like_int t =
	match follow t with
		| TAbstract({ a_path = ([], "Int") },[]) -> true
		| TAbstract({ a_path = (["cs"], "Pointer") },_) -> false
		| TAbstract(a, _) -> List.exists (fun t -> like_int t) a.a_from || List.exists (fun t -> like_int t) a.a_to
		| _ -> false

let rec like_i64 t =
	match follow t with
		| TAbstract({ a_path = (["cs"], "Int64") },[])
		| TAbstract({ a_path = (["cs"], "UInt64") },[])
		| TAbstract({ a_path = (["java"], "Int64") },[])
		| TAbstract({ a_path = (["haxe"], "Int64") },[]) -> true
		| TAbstract(a, _) -> List.exists (fun t -> like_i64 t) a.a_from || List.exists (fun t -> like_i64 t) a.a_to
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
		apply_params t.t_params tl t.t_type
	| _ -> t

let t_empty = TAnon({ a_fields = PMap.empty; a_status = ref (Closed) })

let tmp_count = ref 0

let reset_temps () = tmp_count := 0

(* the undefined is a special var that works like null, but can have special meaning *)
let v_undefined = alloc_var "__undefined__" t_dynamic

let undefined pos = ExprBuilder.make_local v_undefined pos

let path_of_md_def md_def =
	match md_def.m_types with
		| [TClassDecl c] -> c.cl_path
		| _ -> md_def.m_path


(* ******************************************* *)
(* common helpers *)
(* ******************************************* *)

let assertions = false (* when assertions == true, many assertions will be made to guarantee the quality of the data input *)
let debug_mode = ref false
let trace s = if !debug_mode then print_endline s else ()
let timer name = if !debug_mode then Common.timer name else fun () -> ()

let is_string t =
	match follow t with
	| TInst({ cl_path = ([], "String") }, []) -> true
	| _ -> false

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
		| Statics cl -> Some(TClassDecl cl)
		| EnumStatics e -> Some(TEnumDecl e)
		| AbstractStatics a -> Some(TAbstractDecl a)
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
			| _ -> assert false)
	| TLazy f -> t_to_md (!f())
	| TMono r -> (match !r with | Some t -> t_to_md t | None -> assert false)
	| _ -> assert false

let get_cl mt = match mt with | TClassDecl cl -> cl | _ -> failwith (Printf.sprintf "Unexpected module type (class expected) for %s: %s" (s_type_path (t_path mt)) (s_module_type_kind mt))

let get_abstract mt = match mt with | TAbstractDecl a -> a | _ -> failwith (Printf.sprintf "Unexpected module type (abstract expected) for %s: %s" (s_type_path (t_path mt)) (s_module_type_kind mt))

let get_tdef mt = match mt with | TTypeDecl t -> t | _ -> assert false

let mk_mt_access mt pos = { eexpr = TTypeExpr(mt); etype = anon_of_mt mt; epos = pos }

let mk_local = ExprBuilder.make_local

(* this function is used by CastDetection module *)
let get_fun t =
	match follow t with
	| TFun(r1,r2) -> (r1,r2)
	| t -> (trace (debug_type t)); assert false

let mk_cast t e = Type.mk_cast e t e.epos

(** TODO: when adding new AST, make a new cast type for those fast casts. For now, we're using this hack
 *        of using null_class to tell a fast cast from a normal one. Also note that this only works since both
 *        C# and Java do not use the second part of TCast for anything *)
let mk_castfast t e = { e with eexpr = TCast(e, Some (TClassDecl null_class)); etype = t }

let mk_static_field_access_infer cl field pos params =
	try
		let cf = (PMap.find field cl.cl_statics) in
		{ eexpr = TField(ExprBuilder.make_static_this cl pos, FStatic(cl, cf)); etype = (if params = [] then cf.cf_type else apply_params cf.cf_params params cf.cf_type); epos = pos }
	with | Not_found -> failwith ("Cannot find field " ^ field ^ " in type " ^ (s_type_path cl.cl_path))

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
	gcon : Common.context;

	gclasses : gen_classes;

	gtools : gen_tools;

	(*
		configurable function that receives a desired name and makes it "internal", doing the best
		to ensure that it will not be called from outside.
		To avoid name clashes between internal names, user must specify two strings: a "namespace" and the name itself
	 *)
	gmk_internal_name : string->string->string;

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
	(* is executed once every new classfield *)
	mutable gon_classfield_start : (unit -> unit) list;
	(* is executed once every new module type *)
	mutable gon_new_module_type : (unit -> unit) list;
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
				mk_class a.a_module ([], "Dynamic") a.a_pos null_pos
		| _ -> assert false
	in

	let rec gen = {
		gcon = con;
		gclasses = {
			cl_reflect = get_cl (get_type con.types ([], "Reflect"));
			cl_type = get_cl (get_type con.types ([], "Type"));
			cl_dyn = cl_dyn;

			nativearray = (fun _ -> assert false);
			nativearray_type = (fun _ -> assert false);
			nativearray_len = (fun _ -> assert false);
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

			r_create_empty = (fun _ _ pos -> gen.gcon.error "r_create_empty implementation is not provided" pos; assert false);
		};
		gmk_internal_name = (fun ns s -> sprintf "__%s_%s" ns s);
		gexpr_filters = new rule_map_dispatcher "gexpr_filters";
		gmodule_filters = new rule_map_dispatcher "gmodule_filters";
		gsyntax_filters = new rule_map_dispatcher "gsyntax_filters";
		gfollow = new rule_dispatcher "gfollow" false;
		gtypes = types;
		gtypes_list = con.types;
		gmodules = con.modules;

		greal_field_types = Hashtbl.create 0;
		ghandle_cast = (fun to_t from_t e -> mk_cast to_t e);
		gon_unsafe_cast = (fun t t2 pos -> (gen.gcon.warning ("Type " ^ (debug_type t2) ^ " is being cast to the unrelated type " ^ (s_type (print_context()) t)) pos));
		gneeds_box = (fun t -> false);
		gspecial_needs_cast = (fun to_t from_t -> true);
		gsupported_conversions = Hashtbl.create 0;

		gadd_type = (fun md should_filter ->
			if should_filter then begin
				gen.gtypes_list <- md :: gen.gtypes_list;
				gen.gmodules <- { m_id = alloc_mid(); m_path = (t_path md); m_types = [md]; m_extra = module_extra "" "" 0. MFake } :: gen.gmodules;
				Hashtbl.add gen.gtypes (t_path md) md;
			end else gen.gafter_filters_ended <- (fun () ->
				gen.gtypes_list <- md :: gen.gtypes_list;
				gen.gmodules <- { m_id = alloc_mid(); m_path = (t_path md); m_types = [md]; m_extra = module_extra "" "" 0. MFake } :: gen.gmodules;
				Hashtbl.add gen.gtypes (t_path md) md;
			) :: gen.gafter_filters_ended;
		);
		gadd_to_module = (fun md pr -> failwith "module added outside expr filters");
		gcurrent_path = ([],"");
		gcurrent_class = None;
		gcurrent_classfield = None;

		gon_classfield_start = [];
		gon_new_module_type = [];
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
			(match !r with
			| Some t -> follow_f t
			| _ -> Some t)
		| TLazy f ->
			follow_f (!f())
		| TType (t,tl) ->
			follow_f (apply_params t.t_params tl t.t_type)
		| _ -> Some t
	in
	gen.gfollow#add ~name:"final" ~priority:PLast follow

(* run_follow (gen:generator_ctx) (t:t) *)
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
			gen.gmodules <- { m_id = alloc_mid(); m_path = md_path; m_types = List.rev ( Hashtbl.find_all modules md_path ); m_extra = (t_infos md).mt_module.m_extra } :: gen.gmodules
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
		let rec process_field f =
			reset_temps();
			gen.gcurrent_classfield <- Some(f);
			List.iter (fun fn -> fn()) gen.gon_classfield_start;

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
	| TEnumDecl _ | TTypeDecl _ | TAbstractDecl _ ->
		()

let run_filters gen =
	let last_error = gen.gcon.error in
	let has_errors = ref false in
	gen.gcon.error <- (fun msg pos -> has_errors := true; last_error msg pos);
	(* first of all, we have to make sure that the filters won't trigger a major Gc collection *)
	let t = Common.timer "gencommon_filters" in
	(if Common.defined gen.gcon Define.GencommonDebug then debug_mode := true else debug_mode := false);
	let run_filters filter =
		let rec loop acc mds =
			match mds with
				| [] -> acc
				| md :: tl ->
					let filters = [ filter#run_f ] in
					let added_types = ref [] in
					gen.gadd_to_module <- (fun md_type priority ->
						gen.gtypes_list <- md_type :: gen.gtypes_list;
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
		List.rev (loop [] gen.gtypes_list)
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
					(match hd with
						| TClassDecl c ->
							gen.gcurrent_class <- Some c
						| _ ->
							gen.gcurrent_class <- None);
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
	(* Codegen.post_process gen.gtypes_list [gen.gexpr_filters#run_f]; *)
	gen.gtypes_list <- run_filters gen.gsyntax_filters;
	List.iter (fun fn -> fn()) gen.gafter_filters_ended;

	reorder_modules gen;
	t();
	if !has_errors then raise (Abort("Compilation aborted with errors",null_pos))

(* ******************************************* *)
(* basic generation module that source code compilation implementations can use *)
(* ******************************************* *)

let write_file gen w source_dir path extension out_files =
	let t = timer "write file" in
	let s_path = source_dir	^ "/" ^ (snd path) ^ "." ^ (extension) in
	(* create the folders if they don't exist *)
	mkdir_from_path s_path;

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

	out_files := (Path.unique_full_path s_path) :: !out_files;

	t()


let clean_files path excludes verbose =
	let rec iter_files pack dir path = try
		let file = Unix.readdir dir in

		if file <> "." && file <> ".." then begin
			let filepath = path ^ "/" ^ file in
			if (Unix.stat filepath).st_kind = S_DIR then
				let pack = pack @ [file] in
				iter_files (pack) (Unix.opendir filepath) filepath;
				try Unix.rmdir filepath with Unix.Unix_error (ENOTEMPTY,_,_) -> ();
			else if not (String.ends_with filepath ".meta") && not (List.mem (Path.unique_full_path filepath) excludes) then begin
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
	let main_paths = Hashtbl.create 0 in
	List.iter (fun md_def ->
		SourceWriter.write w "M ";
		SourceWriter.write w (path_s (path_of_md_def md_def));
		SourceWriter.newline w;
		List.iter (fun m ->
			match m with
				| TClassDecl cl when not cl.cl_extern ->
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
	(match gen.gcon.main_class with
		| Some path ->
			SourceWriter.write w "begin main";
			SourceWriter.newline w;
			(try
				SourceWriter.write w (Hashtbl.find main_paths path)
			with
				| Not_found -> SourceWriter.write w (path_s path));
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
	let path file ext =
		if Sys.file_exists file then
			file
		else try Common.find_file gen.gcon file with
			| Not_found -> try Common.find_file gen.gcon (file ^ ext) with
			| Not_found ->
				file
	in
	if Common.platform gen.gcon Java then
		List.iter (fun (s,std,_,_,_) ->
			if not std then begin
				SourceWriter.write w (path s ".jar");
				SourceWriter.newline w;
			end
		) gen.gcon.java_libs
	else if Common.platform gen.gcon Cs then
		List.iter (fun (s,std,_,_) ->
			if not std then begin
				SourceWriter.write w (path s ".dll");
				SourceWriter.newline w;
			end
		) gen.gcon.net_libs;
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

let mk_return e = { eexpr = TReturn (Some e); etype = e.etype; epos = e.epos }

let mk_temp gen name t =
	incr tmp_count;
	let name = gen.gmk_internal_name "temp" (name ^ (string_of_int !tmp_count)) in
	alloc_var name t

let v_nativearray = alloc_var "__array__" t_dynamic
let mk_nativearray_decl gen t el pos =
	{
		eexpr = TCall(mk_local v_nativearray pos, el);
		etype = gen.gclasses.nativearray t;
		epos = pos;
	}

let ensure_local gen block name e =
	match e.eexpr with
		| TLocal _ -> e
		| _ ->
			let var = mk_temp gen name e.etype in
			block := { e with eexpr = TVar(var, Some e); etype = gen.gcon.basic.tvoid; } :: !block;
			{ e with eexpr = TLocal var }

let follow_module follow_func md = match md with
	| TClassDecl _
	| TEnumDecl _
	| TAbstractDecl _ -> md
	| TTypeDecl tdecl -> match (follow_func (TType(tdecl, List.map snd tdecl.t_params))) with
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
		| TClassDecl cl -> TInst(cl, List.map (fun _ -> t_dynamic) cl.cl_params)
		| TEnumDecl e -> TEnum(e, List.map (fun _ -> t_dynamic) e.e_params)
		| TAbstractDecl a -> TAbstract(a, List.map (fun _ -> t_dynamic) a.a_params)
		| TTypeDecl t -> TType(t, List.map (fun _ -> t_dynamic) t.t_params)

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
	match t with
	| TMono t ->
		(match !t with
		| None -> t := Some t_dynamic
		| Some _ -> ())
	| TEnum (_,p) | TInst (_,p) | TType (_,p) | TAbstract (_,p) ->
		List.iter replace_mono p
	| TFun (args,ret) ->
		List.iter (fun (_,_,t) -> replace_mono t) args;
		replace_mono ret
	| TAnon _
	| TDynamic _ -> ()
	| TLazy f ->
		replace_mono (!f())

(* helper *)
let mk_class_field name t public pos kind params =
	{
		cf_name = name;
		cf_type = t;
		cf_public = public;
		cf_pos = pos;
		cf_name_pos = null_pos;
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
	let ret = mk_class cl.cl_module (fst cl.cl_path, snd cl.cl_path ^ "_c") cl.cl_pos null_pos in
	ret.cl_implements <- cl.cl_implements;
	ret.cl_kind <- cl.cl_kind;
	ret

let get_cl_t t =
	match follow t with | TInst (cl,_) -> cl | _ -> assert false

let mk_class m path pos =
	let cl = Type.mk_class m path pos null_pos in
	cl.cl_meta <- [ Meta.CompilerGenerated, [], Ast.null_pos ];
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
			if Meta.has Meta.Overload ret.cf_meta then is_overload := true;
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
		if c.cl_interface then
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
		if !is_overload && not (Meta.has Meta.Overload f.cf_meta) then
			f.cf_meta <- (Meta.Overload,[],f.cf_pos) :: f.cf_meta;
		let declared_t = apply_params c.cl_params tl f.cf_type in
		let params_t = apply_params c.cl_params tlch f.cf_type in
		let actual_t = match follow params_t with
		| TFun(args,ret) -> TFun(List.map (fun (n,o,t) -> (n,o,gen.greal_type t)) args, gen.greal_type ret)
		| _ -> gen.greal_type params_t in
		Some(f,actual_t,declared_t,params_t,c,tl,tlch)

let field_access gen (t:t) (field:string) : (tfield_access) =
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
		| TEnum _ | TAbstract _ ->
			(* enums have no field *) FNotFound
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
	| FStatic(cl,cf) | FInstance(cl,_,cf) when Meta.has Meta.Extern cf.cf_meta ->
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
		| FEnumField _ -> assert false

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
(* set hxgen module *)
(* ******************************************* *)
(*
	Goes through all module types and adds the @:hxGen or @:nativeGen meta to them.
	Basically, everything that is extern is assumed to not be hxgen, unless meta :hxGen is set,
	and everything that is not extern is assumed to be hxgen, unless meta :nativeGgen is set.
*)
module SetHXGen =
struct
	(*
		The only option is to run this filter eagerly, because it must be one of the first filters to run,
		since many others depend of it.
	*)
	let run_filter gen =
		let rec is_hxgen md =
			match md with
			| TClassDecl { cl_kind = KAbstractImpl a } ->
				is_hxgen (TAbstractDecl a)
			| TClassDecl cl ->
				let rec is_hxgen_class (c,_) =
					if c.cl_extern then begin
						if Meta.has Meta.HxGen c.cl_meta then
							true
						else
							Option.map_default (is_hxgen_class) false c.cl_super || List.exists is_hxgen_class c.cl_implements
					end else begin
						if Meta.has Meta.NativeChildren c.cl_meta || Meta.has Meta.NativeGen c.cl_meta then
							Option.map_default is_hxgen_class false c.cl_super || List.exists is_hxgen_class c.cl_implements
						else
							let rec has_nativec (c,p) =
								if is_hxgen_class (c,p) then
									false
								else
									(Meta.has Meta.NativeChildren c.cl_meta && not (Option.map_default is_hxgen_class false c.cl_super || List.exists is_hxgen_class c.cl_implements))
									|| Option.map_default has_nativec false c.cl_super
							in
							if Option.map_default has_nativec false c.cl_super && not (List.exists is_hxgen_class c.cl_implements) then
								false
							else
								true
					end
				in
				is_hxgen_class (cl,[])
			| TEnumDecl e ->
				if e.e_extern then
					Meta.has Meta.HxGen e.e_meta
				else if Meta.has Meta.NativeGen e.e_meta then
					if Meta.has Meta.FlatEnum e.e_meta then
						false
					else begin
						gen.gcon.error "Only flat enums may be @:nativeGen" e.e_pos;
						true
					end
				else
					true
			| TAbstractDecl a when Meta.has Meta.CoreType a.a_meta ->
				not (Meta.has Meta.NativeGen a.a_meta)
			| TAbstractDecl a ->
				(match follow a.a_this with
				| TInst _ | TEnum _ | TAbstract _ ->
					is_hxgen (t_to_md (follow a.a_this))
				| _ ->
					not (Meta.has Meta.NativeGen a.a_meta))
			| TTypeDecl t -> (* TODO see when would we use this *)
				false
		in

		let filter md =
			let meta = if is_hxgen md then Meta.HxGen else Meta.NativeGen in
			match md with
			| TClassDecl cl -> cl.cl_meta <- (meta, [], cl.cl_pos) :: cl.cl_meta
			| TEnumDecl e -> e.e_meta <- (meta, [], e.e_pos) :: e.e_meta
			| TTypeDecl t -> t.t_meta <- (meta, [], t.t_pos) :: t.t_meta
			| TAbstractDecl a -> a.a_meta <- (meta, [], a.a_pos) :: a.a_meta
		in

		List.iter filter gen.gtypes_list
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

	Then it always adds two constructors to the class: one that receives a special marker class,
	indicating that the object should be constructed without executing constructor body,
	and one that executes its normal constructor.
	Both will only include a super() call to the superclasses' emtpy constructor.

	This enables two things:
		empty construction without the need of incompatibility with the platform's native construction method
		the ability to call super() constructor in any place in the constructor

	This will insert itself in the default reflection-related module filter
*)
module OverloadingConstructor =
struct
	let priority = 0.0
	let name = "overloading_constructor"

	let rec cur_ctor c tl =
		match c.cl_constructor with
		| Some ctor ->
			ctor, c, tl
		| None ->
			match c.cl_super with
			| None ->
				raise Not_found
			| Some (sup,stl) ->
				cur_ctor sup (List.map (apply_params c.cl_params tl) stl)

	let rec prev_ctor c tl =
		match c.cl_super with
		| None ->
			raise Not_found
		| Some (sup,stl) ->
			let stl = List.map (apply_params c.cl_params tl) stl in
			match sup.cl_constructor with
			| None -> prev_ctor sup stl
			| Some ctor -> ctor, sup, stl

	let make_static_ctor_name gen cl =
		let name = gen.gmk_internal_name "hx" "ctor" in
		name ^ "_" ^ (String.concat "_" (fst cl.cl_path)) ^ "_" ^ (snd cl.cl_path)

	(* replaces super() call with last static constructor call *)
	let replace_super_call gen c tl with_params me p =
		let rec loop_super c tl =
			match c.cl_super with
			| None ->
				raise Not_found
			| Some(sup,stl) ->
				let stl = List.map (apply_params c.cl_params tl) stl in
				try
					let static_ctor_name = make_static_ctor_name gen sup in
					sup, stl, PMap.find static_ctor_name sup.cl_statics
				with Not_found ->
					loop_super sup stl
		in
		let sup, stl, cf = loop_super c tl in
		let with_params = (mk (TLocal me) me.v_type p) :: with_params in
		let cf =
			try
				(* choose best super function *)
				List.iter (fun e -> replace_mono e.etype) with_params;
				List.find (fun cf ->
					replace_mono cf.cf_type;
					let args, _ = get_fun (apply_params cf.cf_params stl cf.cf_type) in
					try
						List.for_all2 (fun (_,_,t) e -> try
							let e_etype = run_follow gen e.etype in
							let t = run_follow gen t in
							unify e_etype t; true
						with Unify_error _ ->
							false
						) args with_params
					with Invalid_argument("List.for_all2") ->
						false
				) (cf :: cf.cf_overloads)
			with Not_found ->
				gen.gcon.error "No suitable overload for the super call arguments was found" p; cf
		in
		{
			eexpr = TCall(
				{
					eexpr = TField(ExprBuilder.make_static_this sup p, FStatic(sup,cf));
					etype = apply_params cf.cf_params stl cf.cf_type;
					epos = p
				},
				with_params
			);
			etype = gen.gcon.basic.tvoid;
			epos = p;
		}

	(* will create a static counterpart of 'ctor', and replace its contents to a call to the static version*)
	let create_static_ctor gen ~empty_ctor_expr cl ctor =
		match Meta.has Meta.SkipCtor ctor.cf_meta with
		| true -> ()
		| false when is_none ctor.cf_expr -> ()
		| false ->
			let static_ctor_name = make_static_ctor_name gen cl in
			(* create the static constructor *)
			let basic = gen.gcon.basic in
			let ctor_types = List.map (fun (s,t) -> (s, TInst(map_param (get_cl_t t), []))) cl.cl_params in
			let me = alloc_var "__hx_this" (TInst(cl, List.map snd ctor_types)) in
			me.v_capture <- true;

			let fn_args, _ = get_fun ctor.cf_type in
			let ctor_params = List.map snd ctor_types in
			let fn_type = TFun((me.v_name,false, me.v_type) :: List.map (fun (n,o,t) -> (n,o,apply_params cl.cl_params ctor_params t)) fn_args, basic.tvoid) in
			let cur_tf_args = match ctor.cf_expr with
			| Some { eexpr = TFunction(tf) } -> tf.tf_args
			| _ -> assert false
			in

			let changed_tf_args = List.map (fun (v,_) -> (v,None)) cur_tf_args in

			let local_map = Hashtbl.create (List.length cur_tf_args) in
			let static_tf_args = (me, None) :: List.map (fun (v,b) ->
				let new_v = alloc_var v.v_name (apply_params cl.cl_params ctor_params v.v_type) in
				new_v.v_capture <- v.v_capture;
				Hashtbl.add local_map v.v_id new_v;
				(new_v, b)
			) cur_tf_args in

			let static_ctor = mk_class_field static_ctor_name fn_type false ctor.cf_pos (Method MethNormal) ctor_types in

			(* change ctor contents to reference the 'me' var instead of 'this' *)
			let actual_super_call = ref None in
			let rec map_expr ~is_first e = match e.eexpr with
				| TCall (({ eexpr = TConst TSuper } as tsuper), params) -> (try
					let params = List.map (fun e -> map_expr ~is_first:false e) params in
					actual_super_call := Some { e with eexpr = TCall(tsuper, [empty_ctor_expr]) };
					replace_super_call gen cl ctor_params params me e.epos
				with | Not_found ->
					(* last static function was not found *)
					actual_super_call := Some e;
					if not is_first then
						gen.gcon.error "Super call must be the first call when extending native types" e.epos;
					{ e with eexpr = TBlock([]) })
				| TFunction tf when is_first ->
					do_map ~is_first:true e
				| TConst TThis ->
					mk_local me e.epos
				| TBlock (fst :: bl) ->
					let fst = map_expr ~is_first:is_first fst in
					{ e with eexpr = TBlock(fst :: List.map (fun e -> map_expr ~is_first:false e) bl); etype = apply_params cl.cl_params ctor_params e.etype }
				| _ ->
					do_map e
			and do_map ?(is_first=false) e =
				let do_t = apply_params cl.cl_params ctor_params in
				let do_v v = try
						Hashtbl.find local_map v.v_id
					with | Not_found ->
						v.v_type <- do_t v.v_type; v
				in
				Type.map_expr_type (map_expr ~is_first:is_first) do_t do_v e
			in

			let expr = do_map ~is_first:true (get ctor.cf_expr) in
			let expr = match expr.eexpr with
			| TFunction(tf) ->
				{ expr with etype = fn_type; eexpr = TFunction({ tf with tf_args = static_tf_args }) }
			| _ -> assert false in
			static_ctor.cf_expr <- Some expr;
			(* add to the statics *)
			(try
				let stat = PMap.find static_ctor_name cl.cl_statics in
				stat.cf_overloads <- static_ctor :: stat.cf_overloads
			with | Not_found ->
				cl.cl_ordered_statics <- static_ctor :: cl.cl_ordered_statics;
				cl.cl_statics <- PMap.add static_ctor_name static_ctor cl.cl_statics);
			(* change current super call *)
			match ctor.cf_expr with
			| Some({ eexpr = TFunction(tf) } as e) ->
				let block_contents, p = match !actual_super_call with
				| None -> [], ctor.cf_pos
				| Some super -> [super], super.epos
				in
				let block_contents = block_contents @ [{
					eexpr = TCall(
						{
							eexpr = TField(
								ExprBuilder.make_static_this cl p,
								FStatic(cl, static_ctor));
							etype = apply_params static_ctor.cf_params (List.map snd cl.cl_params) static_ctor.cf_type;
							epos = p
						},
						[{ eexpr = TConst TThis; etype = TInst(cl, List.map snd cl.cl_params); epos = p }]
						@ List.map (fun (v,_) -> mk_local v p) cur_tf_args
					);
					etype = basic.tvoid;
					epos = p
				}] in
				ctor.cf_expr <- Some { e with eexpr = TFunction({ tf with tf_expr = { tf.tf_expr with eexpr = TBlock block_contents }; tf_args = changed_tf_args }) }
			| _ -> assert false

	(* makes constructors that only call super() for the 'ctor' argument *)
	let clone_ctors gen ctor sup stl cl =
		let basic = gen.gcon.basic in
		let rec clone cf =
			let ncf = mk_class_field "new" (apply_params sup.cl_params stl cf.cf_type) cf.cf_public cf.cf_pos cf.cf_kind cf.cf_params in
			let args, ret = get_fun ncf.cf_type in
			(* single expression: call to super() *)
			let tf_args = List.map (fun (name,_,t) ->
				(* the constructor will have no optional arguments, as presumably this will be handled by the underlying expr *)
				alloc_var name t, None
			) args in
			let super_call =
			{
				eexpr = TCall(
					{ eexpr = TConst TSuper; etype = TInst(cl, List.map snd cl.cl_params); epos = ctor.cf_pos },
					List.map (fun (v,_) -> mk_local v ctor.cf_pos) tf_args);
				etype = basic.tvoid;
				epos = ctor.cf_pos;
			} in
			ncf.cf_expr <- Some
			{
				eexpr = TFunction {
					tf_args = tf_args;
					tf_type = basic.tvoid;
					tf_expr = mk_block super_call;
				};
				etype = ncf.cf_type;
				epos = ctor.cf_pos;
			};
			ncf
		in
		(* take off createEmpty *)
		let all = List.filter (fun cf -> replace_mono cf.cf_type; not (Meta.has Meta.SkipCtor cf.cf_meta)) (ctor :: ctor.cf_overloads) in
		let clones = List.map clone all in
		match clones with
		| [] ->
			(* raise Not_found *)
			assert false (* should never happen *)
		| cf :: [] -> cf
		| cf :: overl ->
			cf.cf_meta <- (Meta.Overload,[],cf.cf_pos) :: cf.cf_meta;
			cf.cf_overloads <- overl; cf

	let rec descends_from_native_or_skipctor cl =
		not (is_hxgen (TClassDecl cl)) || Meta.has Meta.SkipCtor cl.cl_meta || match cl.cl_super with
		| None -> false
		| Some(c,_) -> descends_from_native_or_skipctor c

	let ensure_super_is_first gen cf =
		let rec loop e =
			match e.eexpr with
			| TBlock (b :: block) ->
				loop b
			| TBlock []
			| TCall({ eexpr = TConst TSuper },_) -> ()
			| _ ->
				gen.gcon.error "Types that derive from a native class must have its super() call as the first statement in the constructor" cf.cf_pos
		in
		match cf.cf_expr with
		| None -> ()
		| Some e -> Type.iter loop e

	let configure ~(empty_ctor_type : t) ~(empty_ctor_expr : texpr) gen =
		gen.gtools.r_create_empty <- (fun cl params pos -> mk (TNew(cl,params,[empty_ctor_expr])) (TInst(cl,params)) pos);

		let basic = gen.gcon.basic in
		let should_change cl = not cl.cl_interface && (not cl.cl_extern || is_hxgen (TClassDecl cl)) && (match cl.cl_kind with KAbstractImpl _ -> false | _ -> true) in
		let msize = List.length gen.gtypes_list in
		let processed, empty_ctors = Hashtbl.create msize, Hashtbl.create msize in


		let rec get_last_empty cl =
			try
				Hashtbl.find empty_ctors cl.cl_path
			with | Not_found ->
				match cl.cl_super with
				| None -> raise Not_found
				| Some (sup,_) -> get_last_empty sup
		in

		let rec change cl =
			if not (Hashtbl.mem processed cl.cl_path) then begin
				Hashtbl.add processed cl.cl_path true;

				(* make sure we've processed the super types *)
				Option.may (fun (super,_) -> if should_change super then change super) cl.cl_super;

				(* implement static hx_ctor and reimplement constructors *)
				(try
					let ctor =
						match cl.cl_constructor with
						| Some ctor ->
							ctor
						| None ->
							try
								let sctor, sup, stl = prev_ctor cl (List.map snd cl.cl_params) in
								(* we'll make constructors that will only call super() *)
								let ctor = clone_ctors gen sctor sup stl cl in
								cl.cl_constructor <- Some ctor;
								ctor
							with Not_found -> (* create default constructor *)
								let ctor = mk_class_field "new" (TFun ([], basic.tvoid)) false cl.cl_pos (Method MethNormal) [] in
								ctor.cf_expr <- Some {
									eexpr = TFunction {
										tf_args = [];
										tf_type = basic.tvoid;
										tf_expr = mk (TBlock []) basic.tvoid cl.cl_pos;
									};
									etype = ctor.cf_type;
									epos = ctor.cf_pos;
								};
								cl.cl_constructor <- Some ctor;
								ctor
					in
					(* now that we made sure we have a constructor, exit if native gen *)
					if not (is_hxgen (TClassDecl cl)) || Meta.has Meta.SkipCtor cl.cl_meta then begin
						if descends_from_native_or_skipctor cl && is_some cl.cl_super then
							List.iter (fun cf -> ensure_super_is_first gen cf) (ctor :: ctor.cf_overloads);
						raise Exit
					end;

					(* if cl descends from a native class, we cannot use the static constructor strategy *)
					if descends_from_native_or_skipctor cl && is_some cl.cl_super then
						List.iter (fun cf -> ensure_super_is_first gen cf) (ctor :: ctor.cf_overloads)
					else
						(* now that we have a current ctor, create the static counterparts *)
						List.iter (fun cf -> create_static_ctor gen ~empty_ctor_expr:empty_ctor_expr cl cf) (ctor :: ctor.cf_overloads)
				with Exit -> ());

				(* implement empty ctor *)
				(try
					(* now that we made sure we have a constructor, exit if native gen *)
					if not (is_hxgen (TClassDecl cl)) then raise Exit;

					(* get first *)
					let empty_type = TFun (["empty",false,empty_ctor_type],basic.tvoid) in
					let super =
						match cl.cl_super with
						| None -> (* implement empty *)
								[]
						| Some (sup,_) ->
							try
								ignore (get_last_empty sup);
								let esuper = mk (TConst TSuper) (TInst (cl, List.map snd cl.cl_params)) cl.cl_pos in
								[mk (TCall (esuper, [empty_ctor_expr])) basic.tvoid cl.cl_pos]
							with Not_found ->
								try
									(* super type is native: find super constructor with least arguments *)
									let sctor, sup, stl = prev_ctor cl (List.map snd cl.cl_params) in
									let rec loop remaining (best,n) =
										match remaining with
										| [] -> best
										| cf :: r ->
											let args,_ = get_fun cf.cf_type in
											if (List.length args) < n then
												loop r (cf,List.length args)
											else
												loop r (best,n)
									in
									let args,_ = get_fun sctor.cf_type in
									let best = loop sctor.cf_overloads (sctor, List.length args) in
									let args,_ = get_fun (apply_params sup.cl_params stl best.cf_type) in
									let esuper = mk (TConst TSuper) (TInst (sup, stl)) cl.cl_pos in
									[mk (TCall (esuper, List.map (fun (n,o,t) -> null t cl.cl_pos) args)) basic.tvoid cl.cl_pos]
								with Not_found ->
									(* extends native type, but no ctor found *)
									[]
					in
					let ctor = mk_class_field "new" empty_type false cl.cl_pos (Method MethNormal) [] in
					ctor.cf_expr <- Some {
						eexpr = TFunction {
							tf_type = basic.tvoid;
							tf_args = [alloc_var "empty" empty_ctor_type, None];
							tf_expr = mk (TBlock super) basic.tvoid cl.cl_pos
						};
						etype = empty_type;
						epos = cl.cl_pos;
					};
					ctor.cf_meta <- [Meta.SkipCtor, [], ctor.cf_pos];
					Hashtbl.add empty_ctors cl.cl_path ctor;
					match cl.cl_constructor with
					| None ->
						cl.cl_constructor <- Some ctor
					| Some c ->
						c.cf_overloads <- ctor :: c.cf_overloads
				with Exit -> ());
			end
		in

		let module_filter md =
			(match md with
			| TClassDecl cl when should_change cl ->
				change cl;
			| _ ->
				());
			None
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
		(ok) must run before OverloadingConstructor module so the constructor can be in the correct place
		(syntax) must run before FunctionToClass module
*)
module InitFunction =
struct
	let name = "init_funcs"
	let priority = solve_deps name [DBefore OverloadingConstructor.priority]

	let ensure_simple_expr gen e =
		let rec iter e = match e.eexpr with
			| TConst _ | TLocal _ | TArray _ | TBinop _
			| TField _ | TTypeExpr _ | TParenthesis _ | TCast _ | TMeta _
			| TCall _ | TNew _ | TUnop _ ->
				Type.iter iter e
			| _ ->
				print_endline (debug_expr e);
				gen.gcon.error "Expression is too complex for a readonly variable initialization" e.epos
		in
		iter e

	let configure gen =
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
								add_expr := Some { e with eexpr = TVar(v, Some this) }
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
				match cf.cf_kind with
					| Var v when Meta.has Meta.ReadOnly cf.cf_meta ->
							if v.v_write <> AccNever && not (Meta.has Meta.CoreApi cl.cl_meta) then gen.gcon.warning "@:readOnly variable declared without `never` setter modifier" cf.cf_pos;
							(match cf.cf_expr with
								| None -> gen.gcon.warning "Uninitialized readonly variable" cf.cf_pos; acc
								| Some e -> ensure_simple_expr gen e; acc)
					| Var _
					| Method MethDynamic when not (Type.is_extern_field cf) ->
						(match cf.cf_expr with
							| Some e ->
								(match cf.cf_params with
									| [] ->
										let var = { eexpr = TField(ExprBuilder.make_static_this cl cf.cf_pos, FStatic(cl,cf)); etype = cf.cf_type; epos = cf.cf_pos } in
										let ret = ({ eexpr = TBinop(Ast.OpAssign, var, e); etype = cf.cf_type; epos = cf.cf_pos; }) in
										cf.cf_expr <- None;

										ret :: acc
									| _ ->
										let params = List.map (fun _ -> t_dynamic) cf.cf_params in
										let fn = apply_params cf.cf_params params in
										let var = { eexpr = TField(ExprBuilder.make_static_this cl cf.cf_pos, FStatic(cl,cf)); etype = fn cf.cf_type; epos = cf.cf_pos } in
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

			(* FIXME: find a way to tell OverloadingConstructor to execute this code even with empty constructors *)
			let vars, funs = List.fold_left (fun (acc_vars,acc_funs) cf ->
				match cf.cf_kind with
					| Var v when Meta.has Meta.ReadOnly cf.cf_meta ->
							if v.v_write <> AccNever && not (Meta.has Meta.CoreApi cl.cl_meta) then gen.gcon.warning "@:readOnly variable declared without `never` setter modifier" cf.cf_pos;
							(match cf.cf_expr with
								| None -> (acc_vars,acc_funs)
								| Some e -> ensure_simple_expr gen e; (acc_vars,acc_funs))
					| Var _
					| Method MethDynamic ->
						let is_var = match cf.cf_kind with | Var _ -> true | _ -> false in
						(match cf.cf_expr, cf.cf_params with
							| Some e, [] ->
								let var = { eexpr = TField({ eexpr = TConst(TThis); epos = cf.cf_pos; etype = TInst(cl, List.map snd cl.cl_params); }, FInstance(cl, List.map snd cl.cl_params, cf)); etype = cf.cf_type; epos = cf.cf_pos } in
								let ret = ({ eexpr = TBinop(Ast.OpAssign, var, e); etype = cf.cf_type; epos = cf.cf_pos; }) in
								cf.cf_expr <- None;
								let is_override = List.memq cf cl.cl_overrides in

								if is_override then begin
									cl.cl_ordered_fields <- List.filter (fun f -> f.cf_name <> cf.cf_name) cl.cl_ordered_fields;
									cl.cl_fields <- PMap.remove cf.cf_name cl.cl_fields;
									acc_vars, handle_override_dynfun acc_funs ret var cf.cf_name
								end else if is_var then
									ret :: acc_vars, acc_funs
								else
									acc_vars, ret :: acc_funs
							| Some e, _ ->
								let params = List.map (fun _ -> t_dynamic) cf.cf_params in
								let fn = apply_params cf.cf_params params in
								let var = { eexpr = TField({ eexpr = TConst(TThis); epos = cf.cf_pos; etype = TInst(cl, List.map snd cl.cl_params); }, FInstance(cl, List.map snd cl.cl_params, cf)); etype = cf.cf_type; epos = cf.cf_pos } in
								let rec change_expr e =
									Type.map_expr_type (change_expr) fn (fun v -> v.v_type <- fn v.v_type; v) e
								in

								let ret = ({ eexpr = TBinop(Ast.OpAssign, var, change_expr e); etype = fn cf.cf_type; epos = cf.cf_pos; }) in
								cf.cf_expr <- None;
								let is_override = List.memq cf cl.cl_overrides in

								if is_override then begin
									cl.cl_ordered_fields <- List.filter (fun f -> f.cf_name <> cf.cf_name) cl.cl_ordered_fields;
									cl.cl_fields <- PMap.remove cf.cf_name cl.cl_fields;
									acc_vars, handle_override_dynfun acc_funs ret var cf.cf_name
								end else if is_var then
									ret :: acc_vars, acc_funs
								else
									acc_vars, ret :: acc_funs
							| None, _ -> acc_vars,acc_funs)
					| _ -> acc_vars,acc_funs
			) ([],[]) cl.cl_ordered_fields
			in
			(* let vars = List.rev vars in *)
			(* let funs = List.rev funs in *)
			(* see if there is any *)
			(match vars, funs with
				| [], [] -> ()
				| _ ->
					(* if there is, we need to find the constructor *)
					let ctors = match cl.cl_constructor with
					| Some ctor -> ctor
					| None -> try
						let sctor, sup, stl = OverloadingConstructor.prev_ctor cl (List.map snd cl.cl_params) in
						let ctor = OverloadingConstructor.clone_ctors gen sctor sup stl cl in
						cl.cl_constructor <- Some ctor;
						ctor
					with | Not_found ->
						let basic = gen.gcon.basic in
						let ctor = mk_class_field "new" (TFun([], basic.tvoid)) false cl.cl_pos (Method MethNormal) [] in
						ctor.cf_expr <- Some
						{
							eexpr = TFunction {
								tf_args = [];
								tf_type = basic.tvoid;
								tf_expr = { eexpr = TBlock[]; etype = basic.tvoid; epos = cl.cl_pos };
							};
							etype = ctor.cf_type;
							epos = ctor.cf_pos;
						};
						cl.cl_constructor <- Some ctor;
						ctor
					in

					let process ctor =
						let func = match ctor.cf_expr with
							| Some({eexpr = TFunction(tf)} as e) ->
								let rec add_fn e = match e.eexpr with
									| TBlock(hd :: tl) -> (match hd.eexpr with
										| TCall({ eexpr = TConst TSuper }, _) ->
											if not (OverloadingConstructor.descends_from_native_or_skipctor cl) then
												{ e with eexpr = TBlock(vars @ (hd :: (funs @ tl))) }
											else
												{ e with eexpr = TBlock(hd :: (vars @ funs @ tl)) }
										| TBlock(_) ->
											{ e with eexpr = TBlock( (add_fn hd) :: tl ) }
										| _ ->
											{ e with eexpr = TBlock( vars @ funs @ (hd :: tl) ) })
									| _ -> Type.concat { e with eexpr = TBlock(vars @ funs) } e
								in
								let tf_expr = add_fn (mk_block tf.tf_expr) in
								{ e with eexpr = TFunction({ tf with tf_expr = tf_expr }) }
							| _ -> assert false
						in
						ctor.cf_expr <- Some(func)
					in
					List.iter process (ctors :: ctors.cf_overloads)
			)
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
		must run before OverloadingConstructor due to later priority conflicts. Since ExpressionUnwrap is only
		defined afterwards, we will set this value with absolute values
*)
module DynamicOperators =
struct
	let name = "dyn_ops"
	let priority = 0.0

	let configure gen ?(handle_strings = true) (should_change:texpr->bool) (equals_handler:texpr->texpr->texpr) (dyn_plus_handler:texpr->texpr->texpr->texpr) (compare_handler:texpr->texpr->texpr) =
		let get_etype_one e =
			if like_int e.etype then
				ExprBuilder.make_int gen.gcon 1 e.epos
			else
				ExprBuilder.make_float gen.gcon "1.0" e.epos
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
									{ e1 with eexpr = TField(mk_local v ef.epos, f) }, [ { eexpr = TVar(v,Some (run ef)); etype = basic.tvoid; epos = ef.epos } ]
								| TArray(e1a, e2a) ->
									let v = mk_temp gen "dynop" e1a.etype in
									let v2 = mk_temp gen "dynopi" e2a.etype in
									{ e1 with eexpr = TArray(mk_local v e1a.epos, mk_local v2 e2a.epos) }, [
										{ eexpr = TVar(v,Some (run e1a)); etype = basic.tvoid; epos = e1.epos };
										{ eexpr = TVar(v2, Some (run e2a)); etype = basic.tvoid; epos = e1.epos }
									]
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
							if handle_strings && (is_string e.etype || is_string e1.etype || is_string e2.etype) then
								{ e with eexpr = TBinop(op, mk_cast gen.gcon.basic.tstring (run e1), mk_cast gen.gcon.basic.tstring (run e2)) }
							else
								dyn_plus_handler e (run e1) (run e2)
						| OpGt | OpGte | OpLt | OpLte  -> (* type 2 *)
							{ eexpr = TBinop(op, compare_handler (run e1) (run e2), { eexpr = TConst(TInt(Int32.zero)); etype = gen.gcon.basic.tint; epos = e.epos} ); etype = gen.gcon.basic.tbool; epos = e.epos }
						| OpMult | OpDiv | OpSub | OpMod -> (* always cast everything to double *)
							let etype = (get_etype_one e).etype in
							{ e with eexpr = TBinop(op, mk_cast etype (run e1), mk_cast etype (run e2)) }
						| OpBoolAnd | OpBoolOr ->
							{ e with eexpr = TBinop(op, mk_cast gen.gcon.basic.tbool (run e1), mk_cast gen.gcon.basic.tbool (run e2)) }
						| OpAnd | OpOr | OpXor | OpShl | OpShr | OpUShr ->
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
					let one = get_etype_one e in
					let etype = one.etype in
					let op = (match op with Increment -> OpAdd | Decrement -> OpSub | _ -> assert false) in

					let var, getvar =
						match e1.eexpr with
							| TField(fexpr, field) ->
								let tmp = mk_temp gen "getvar" fexpr.etype in
								let var = { eexpr = TVar(tmp, Some(run fexpr)); etype = gen.gcon.basic.tvoid; epos = e.epos } in
								(Some var, { eexpr = TField( { fexpr with eexpr = TLocal(tmp) }, field); etype = etype; epos = e1.epos })
							| _ ->
								(None, e1)
					in

					(match flag with
						| Prefix ->
							let block = (match var with | Some e -> [e] | None -> []) @
							[
								mk_cast etype { e with eexpr = TBinop(OpAssign, getvar,{ eexpr = TBinop(op, mk_cast etype getvar, one); etype = etype; epos = e.epos }); etype = getvar.etype; }
							]
							in
							{ eexpr = TBlock(block); etype = etype; epos = e.epos }
						| Postfix ->
							let ret = mk_temp gen "ret" etype in
							let vars = (match var with Some e -> [e] | None -> []) @ [{ eexpr = TVar(ret, Some (mk_cast etype getvar)); etype = gen.gcon.basic.tvoid; epos = e.epos }] in
							let retlocal = { eexpr = TLocal(ret); etype = etype; epos = e.epos } in
							let block = vars @
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
		let map e = Some(run e) in
		gen.gexpr_filters#add ~name:"dyn_ops" ~priority:(PCustom priority) map
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

	depends on:
		(ok) must run AFTER Binop/Unop handler - so Unops / Binops are already unrolled
*)
module DynamicFieldAccess =
struct
	let name = "dynamic_field_access"
	let priority = solve_deps name [DAfter DynamicOperators.priority]

	(*
		is_dynamic (expr) (field_access_expr) (field) : a function that indicates if the field access should be changed
		change_expr (expr) (field_access_expr) (field) (setting expr) (is_unsafe) : changes the expression
		call_expr (expr) (field_access_expr) (field) (call_params) : changes a call expression
	*)
	let configure gen (is_dynamic:texpr->texpr->Type.tfield_access->bool) (change_expr:texpr->texpr->string->texpr option->bool->texpr) (call_expr:texpr->texpr->string->texpr list->texpr) =
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
						| Not_found -> match f with
							| FStatic(cl,cf) when Meta.has Meta.Extern cf.cf_meta ->
								{ e with eexpr = TField({ fexpr with eexpr = TTypeExpr decl }, FStatic(cl, cf)) }
							| _ ->
								change_expr e { fexpr with eexpr = TTypeExpr decl } (field_name f) None true
				)
			| TField(fexpr, f) when is_dynamic e fexpr (f) ->
				change_expr e (run fexpr) (field_name f) None true
			| TCall(
				{ eexpr = TField(_, FStatic({ cl_path = ([], "Reflect") }, { cf_name = "field" })) } ,
					[obj; { eexpr = TConst(TString(field)) }]
				) ->
				let t = match gen.greal_type obj.etype with
					| TDynamic _ | TAnon _ | TMono _ -> t_dynamic
					| t -> t
				in
				change_expr (mk_field_access gen { obj with etype = t } field obj.epos) (run obj) field None false
			| TCall(
				{ eexpr = TField(_, FStatic({ cl_path = ([], "Reflect") }, { cf_name = "setField" } )) },
					[obj; { eexpr = TConst(TString(field)) }; evalue]
				) ->
				change_expr (mk_field_access gen obj field obj.epos) (run obj) field (Some (run evalue)) false
			| TBinop(OpAssign, ({eexpr = TField(fexpr, f)}), evalue) when is_dynamic e fexpr (f) ->
				change_expr e (run fexpr) (field_name f) (Some (run evalue)) true
			| TBinop(OpAssign, { eexpr = TField(fexpr, f) }, evalue) ->
					(match field_access_esp gen fexpr.etype (f) with
						| FClassField(_,_,_,cf,false,t,_) when (try PMap.find cf.cf_name gen.gbase_class_fields == cf with Not_found -> false) ->
								change_expr e (run fexpr) (field_name f) (Some (run evalue)) true
						| _ -> Type.map_expr run e
					)
			(* #if debug *)
			| TBinop(OpAssignOp op, ({eexpr = TField(fexpr, f)}), evalue) when is_dynamic e fexpr (f) -> assert false (* this case shouldn't happen *)
			| TUnop(Increment, _, ({eexpr = TField( ( { eexpr=TLocal(local) } as fexpr ), f)}))
			| TUnop(Decrement, _, ({eexpr = TField( ( { eexpr=TLocal(local) } as fexpr ), f)})) when is_dynamic e fexpr (f) -> assert false (* this case shouldn't happen *)
			(* #end *)
			| TCall( ({ eexpr = TField(fexpr, f) }), params ) when is_dynamic e fexpr (f) ->
				call_expr e (run fexpr) (field_name f) (List.map run params)
			| _ -> Type.map_expr run e
		in
		let map e = Some(run e) in
		gen.gexpr_filters#add ~name:"dynamic_field_access" ~priority:(PCustom(priority)) map
end;;


(* ******************************************* *)
(* Closure Detection *)
(* ******************************************* *)
(*
	Just a small utility filter that detects when a closure must be created.
	On the default implementation, this means when a function field is being accessed
	not via reflection and not to be called instantly

	dependencies:
		must run after DynamicFieldAccess, so any TAnon { Statics / EnumStatics } will be changed to the corresponding TTypeExpr
*)
module FilterClosures =
struct
	let name = "filter_closures"
	let priority = solve_deps name [DAfter DynamicFieldAccess.priority]

	let configure gen (should_change:texpr->string->bool) (filter:texpr->texpr->string->bool->texpr) =
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
					| TCall({ eexpr = TLocal{ v_name = "__delegate__" } } as local, [del]) ->
						{ e with eexpr = TCall(local, [Type.map_expr run del]) }
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
						(match field_access_esp gen tf.etype (f) with
							| FClassField(_,_,_,cf,_,_,_) ->
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
						(match cf.cf_kind with
						| Method MethDynamic | Var _ ->
							Type.map_expr run e
						| _ ->
							filter e (run e1) cf.cf_name false)
					| _ -> Type.map_expr run e
		in
		let map e = Some(run e) in
		gen.gexpr_filters#add ~name:name ~priority:(PCustom priority) map
end;;


(* ******************************************* *)
(* Dynamic TArray Handling *)
(* ******************************************* *)
(*
	In some languages you cannot overload the [] operator,
	so we need to decide what is kept as TArray and what gets mapped.

	depends on:
		(syntax) must run before expression/statment normalization because it may generate complex expressions
		(ok) must run before binop transformations because it may generate some untreated binop ops
		(ok) must run before dynamic field access is transformed into reflection
*)
module TArrayTransform =
struct
	let name = "dyn_tarray"
	let priority = solve_deps name [DBefore DynamicOperators.priority; DBefore DynamicFieldAccess.priority]

	(* should change signature: tarray expr -> binop operation -> should change? *)
	let configure gen (should_change:texpr->Ast.binop option->bool) (get_fun:string) (set_fun:string) =
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
					if should_change e None then mk_get e (run e1) (run e2) else Type.map_expr run e
				| TBinop (Ast.OpAssign, ({ eexpr = TArray(e1a,e2a) } as earray), evalue) when should_change earray (Some Ast.OpAssign) ->
					mk_set e (run e1a) (run e2a) (run evalue)
				| TBinop (Ast.OpAssignOp op,({ eexpr = TArray(e1a,e2a) } as earray) , evalue) when should_change earray (Some (Ast.OpAssignOp op)) ->
					(* cache all arguments in vars so they don't get executed twice *)
					(* let ensure_local gen block name e = *)
					let block = ref [] in

					let arr_local = ensure_local gen block "array" (run e1a) in
					let idx_local = ensure_local gen block "index" (run e2a) in
					block := (mk_set e arr_local idx_local ( { e with eexpr=TBinop(op, mk_get earray arr_local idx_local, run evalue) } )) :: !block;

					{ e with eexpr = TBlock (List.rev !block) }
				| TUnop(op, flag, ({ eexpr = TArray(e1a, e2a) } as earray)) ->
					if should_change earray None && match op with | Not | Neg -> false | _ -> true then begin

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
						block := { eexpr = TVar(val_v, Some(mk_get earray arr_local idx_local)); (* var val = x._get(idx) *)
											 etype = gen.gcon.basic.tvoid;
											 epos = e2a.epos
										 } :: !block;
						block := { eexpr = TVar(ret_v, Some { e with eexpr = TUnop(op, flag, val_local) }); (* var ret = val++ *)
												etype = gen.gcon.basic.tvoid;
												epos = e2a.epos
										 } :: !block;
						block := (mk_set e arr_local idx_local val_local) (*x._set(idx,val)*) :: !block;
						block := ret_local :: !block;
						{ e with eexpr = TBlock (List.rev !block) }
					end else
						Type.map_expr run e
				| _ -> Type.map_expr run e
		in
		let map e = Some(run e) in
		gen.gexpr_filters#add ~name:"dyn_tarray" ~priority:(PCustom priority) map
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
	let configure gen (should_wrap:t->bool) (wrap_throw:texpr->texpr->texpr) (unwrap_expr:tvar->pos->texpr) (rethrow_expr:texpr->texpr) (catchall_type:t) (wrapper_type:t) (catch_map:tvar->texpr->texpr) =
		let rec run e =
			match e.eexpr with
					| TThrow texpr when should_wrap texpr.etype -> wrap_throw e (run texpr)
					| TTry (ttry, catches) ->
						let nowrap_catches, must_wrap_catches, catchall = List.fold_left (fun (nowrap_catches, must_wrap_catches, catchall) (v, catch) ->
							(* first we'll see if the type is Dynamic (catchall) *)
							match follow v.v_type with
								| TDynamic _ ->
									assert (is_none catchall);
									(nowrap_catches, must_wrap_catches, Some(v,run catch))
								(* see if we should unwrap it *)
								| _ when should_wrap (follow v.v_type) ->
									(nowrap_catches, (v,run catch) :: must_wrap_catches, catchall)
								| _ ->
									( (v,catch_map v (run catch)) :: nowrap_catches, must_wrap_catches, catchall )
						) ([], [], None) catches
						in
						(* temp (?) fix for https://github.com/HaxeFoundation/haxe/issues/4134 *)
						let must_wrap_catches = List.rev must_wrap_catches in
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
								let catchall_decl = { eexpr = TVar(catchall_var, Some(temp_local)); etype=gen.gcon.basic.tvoid; epos = pos } in
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
											{ eexpr = TBlock({ eexpr=TVar(vcatch, Some(mk_cast vcatch.v_type catchall_local)); etype=gen.gcon.basic.tvoid; epos=catch.epos } :: [catch] ); etype = catch.etype; epos = catch.epos },
											Some (loop tl));
										etype = catch.etype; epos = catch.epos }
									| [] ->
										match catchall with
											| Some (v,s) ->
												Type.concat { eexpr = TVar(v, Some(catchall_local)); etype = gen.gcon.basic.tvoid; epos = pos } s
											| None ->
												mk_block (rethrow_expr temp_local)
								in
								[ ( temp_var, catch_map temp_var { e with eexpr = TBlock([ catchall_decl; if_is_wrapper_expr; loop must_wrap_catches ]) } ) ]
							| _ ->
								[]
						in
						{ e with eexpr = TTry(run ttry, (List.rev nowrap_catches) @ dyn_catch) }
					| _ -> Type.map_expr run e
		in
		let map e = Some(run e) in
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
		(module filter) must run after OverloadingConstructor so we can also change the dynamic function expressions

		uses TArray expressions for array. TODO see interaction
		uses TThrow expressions.
*)

module ClosuresToClass =
struct
	let name = "closures_to_class"
	let priority = solve_deps name [ DAfter DynamicFieldAccess.priority ]

	type closures_ctx = {
		func_class : tclass;

		(*
			this is what will actually turn the function into class field.
			The standard implementation by default will already take care of creating the class, and setting the captured variables.

			It will also return the super arguments to be called
		*)
		closure_to_classfield : tfunc->t->pos->tclass_field * (texpr list);

		(*
			when a dynamic function call is made, we need to convert it as if it were calling the dynamic function interface.

			TCall expr -> new TCall expr
		*)
		dynamic_fun_call : texpr->texpr;

		(*
			Provide a toolchain so we can easily create classes that extend Function and add more functionality on top of it.

			arguments:
				tclass -> subject (so we know the type of this)
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
		map_base_classfields : tclass->( int -> t -> (tvar list) -> (int->t->tconstant option->texpr) -> texpr )->tclass_field list;
	}

	type map_info = {
		in_unsafe : bool;
		in_unused : bool;
	}

	let null_map_info = { in_unsafe = false; in_unused = false; }

	(*
		the default implementation will take 3 transformation functions:
			* one that will transform closures that are not called immediately (instance.myFunc).
				normally on this case it's best to have a runtime handler that will take the instance, the function and call its invokeField when invoked
			* one that will actually handle the anonymous functions themselves.
			* one that will transform calling a dynamic function. So for example, dynFunc(arg1, arg2) might turn into dynFunc.apply2(arg1, arg2);
			( suspended ) * an option to match papplied functions
			* handling parameterized anonymous function declaration (optional - tparam_anon_decl and tparam_anon_acc)
	*)

	let rec cleanup_delegate e = match e.eexpr with
		| TParenthesis e | TMeta(_,e)
		| TCast(e,_) -> cleanup_delegate e
		| _ -> e

	let funct gen t = match follow (run_follow gen t) with
		| TFun(args,ret) -> args,ret
		| _ -> raise Not_found

	let mk_conversion_fun gen e =
		let args, ret = funct gen e.etype in
		let tf_args = List.map (fun (n,o,t) -> alloc_var n t,None) args in
		let block, local = match e.eexpr with
			| TLocal v ->
				v.v_capture <- true;
				[],e
			| _ ->
				let tmp = mk_temp gen "delegate_conv" e.etype in
				tmp.v_capture <- true;
				[{ eexpr = TVar(tmp,Some e); etype = gen.gcon.basic.tvoid; epos = e.epos }], mk_local tmp e.epos
		in
		let body = {
			eexpr = TCall(local, List.map (fun (v,_) -> mk_local v e.epos) tf_args);
			etype = ret;
			epos = e.epos;
		} in
		let body = if not (ExtType.is_void ret) then
			{ body with eexpr = TReturn( Some body ) }
		else
			body
		in
		let body = {
			eexpr = TBlock([body]);
			etype = body.etype;
			epos = body.epos;
		} in
		block, {
			tf_args = tf_args;
			tf_expr = body;
			tf_type = ret;
		}

	let traverse gen ?tparam_anon_decl ?tparam_anon_acc (handle_anon_func:texpr->tfunc->map_info->t option->texpr) (dynamic_func_call:texpr->texpr) e =
		let info = ref null_map_info in
		let rec run e =
			match e.eexpr with
				| TCast({ eexpr = TCall({ eexpr = TLocal{ v_name = "__delegate__" } } as local, [del] ) } as e2, _) ->
					let e2 = { e2 with etype = e.etype } in
					let replace_delegate ex =
						{ e with eexpr = TCast({ e2 with eexpr = TCall(local, [ex]) }, None) }
					in
					(* found a delegate; let's see if it's a closure or not *)
					let clean = cleanup_delegate del in
					(match clean.eexpr with
						| TField( ef, (FClosure _ as f)) | TField( ef, (FStatic _ as f)) ->
							(* a closure; let's leave this unchanged for FilterClosures to handle it *)
							replace_delegate { clean with eexpr = TField( run ef, f ) }
						| TFunction tf ->
							(* handle like we'd handle a normal function, but create an unchanged closure field for it *)
							let ret = handle_anon_func clean { tf with tf_expr = run tf.tf_expr } !info (Some e.etype) in
							replace_delegate ret
						| _ -> try
							let block, tf = mk_conversion_fun gen del in
							let block = List.map run block in
							let tf = { tf with tf_expr = run tf.tf_expr } in
							let ret = handle_anon_func { clean with eexpr = TFunction(tf) } { tf with tf_expr = run tf.tf_expr } !info (Some e.etype) in
							let ret = replace_delegate ret in
							if block = [] then
								ret
							else
								{ ret with eexpr = TBlock(block @ [ret]) }
						with Not_found ->
							gen.gcon.error "This delegate construct is unsupported" e.epos;
							replace_delegate (run clean))

				| TCall(({ eexpr = TLocal{ v_name = "__unsafe__" } } as local), [arg]) ->
					let old = !info in
					info := { !info with in_unsafe = true };
					let arg2 = run arg in
					info := old;
					{ e with eexpr = TCall(local,[arg2]) }
				(* parameterized functions handling *)
				| TVar(vv, ve) -> (match tparam_anon_decl with
					| None -> Type.map_expr run e
					| Some tparam_anon_decl ->
						(match (vv, ve) with
							| ({ v_extra = Some( _ :: _, _) } as v), Some ({ eexpr = TFunction tf } as f)
							| ({ v_extra = Some( _ :: _, _) } as v), Some { eexpr = TArrayDecl([{ eexpr = TFunction tf } as f]) | TCall({ eexpr = TLocal { v_name = "__array__" } }, [{ eexpr = TFunction tf } as f]) } -> (* captured transformation *)
								ignore(tparam_anon_decl v f { tf with tf_expr = run tf.tf_expr });
								{ e with eexpr = TBlock([]) }
							| _ ->
								Type.map_expr run { e with eexpr = TVar(vv, ve) })
						)
				| TLocal ({ v_extra = Some( _ :: _, _) } as v)
				| TArray ({ eexpr = TLocal ({ v_extra = Some( _ :: _, _) } as v) }, _) -> (* captured transformation *)
					(match tparam_anon_acc with
					| None -> Type.map_expr run e
					| Some tparam_anon_acc -> tparam_anon_acc v e)
				| TCall( { eexpr = TField(_, FEnum _) }, _ ) ->
					Type.map_expr run e
				(* if a TClosure is being call immediately, there's no need to convert it to a TClosure *)
				| TCall(( { eexpr = TField(ecl,f) } as e1), params) ->
					(* check to see if called field is known and if it is a MethNormal (only MethNormal fields can be called directly) *)
					(* let name = field_name f in *)
					(match field_access_esp gen (gen.greal_type ecl.etype) f with
						| FClassField(_,_,_,cf,_,_,_) ->
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
											dynamic_func_call { e with eexpr = TCall( mk_castfast t (run e1), List.map run params ) }
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
									dynamic_func_call { e with eexpr = TCall( mk_castfast t (run e1), List.map run params ) }
					)
				| TFunction tf ->
					handle_anon_func e { tf with tf_expr = run tf.tf_expr } !info None
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
							fun e -> mk_castfast t e
					in
					dynamic_func_call { e with eexpr = TCall(run (may_cast tc), List.map run params) }
				| _ -> Type.map_expr run e
		in

		(match e.eexpr with
			| TFunction(tf) -> Type.map_expr run e
			| _ -> run e)

	let rec get_type_params acc t =
		match t with
			| TInst(( { cl_kind = KTypeParameter _ } as cl), []) ->
				if List.memq cl acc then acc else cl :: acc
			| TFun (params,tret) ->
				List.fold_left get_type_params acc ( tret :: List.map (fun (_,_,t) -> t) params )
			| TDynamic t ->
				(match t with | TDynamic _ -> acc | _ -> get_type_params acc t)
			| TAbstract (a, pl) when not (Meta.has Meta.CoreType a.a_meta) ->
					get_type_params acc ( Abstract.get_underlying_type a pl)
			| TAnon a ->
				PMap.fold (fun cf acc ->
					let params = List.map (fun (_,t) -> match follow t with
						| TInst(c,_) -> c
						| _ -> assert false) cf.cf_params
					in
					List.filter (fun t -> not (List.memq t params)) (get_type_params acc cf.cf_type)
				) a.a_fields acc
			| TType(_, [])
			| TAbstract (_, [])
			| TInst(_, [])
			| TEnum(_, []) ->
				acc
			| TType(_, params)
			| TAbstract(_, params)
			| TEnum(_, params)
			| TInst(_, params) ->
				List.fold_left get_type_params acc params
			| TMono r -> (match !r with
				| Some t -> get_type_params acc t
				| None -> acc)
			| _ -> get_type_params acc (follow_once t)

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
					Type.iter traverse expr
				| TFunction(tf) ->
					List.iter (fun (v,_) -> Hashtbl.add ignored v.v_id v) tf.tf_args;
					(match follow expr.etype with
						| TFun(args,ret) ->
							List.iter (fun (_,_,t) ->
								check_params t
							) args;
							check_params ret
						| _ -> ());
					Type.iter traverse expr
				| TVar (v, opt) ->
					(match v.v_extra with
						| Some(_ :: _, _) -> ()
						| _ ->
							check_params v.v_type);
					Hashtbl.add ignored v.v_id v;
					ignore(Option.map traverse opt)
				| TLocal { v_extra = Some( (_ :: _ ),_) } ->
					()
				| TLocal(( { v_capture = true } ) as v) ->
					(if not (Hashtbl.mem ignored v.v_id || Hashtbl.mem ret v.v_id) then begin check_params v.v_type; Hashtbl.replace ret v.v_id expr end);
				| _ -> Type.iter traverse expr
		in traverse expr;
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
	let configure gen ft =

		let handle_anon_func fexpr tfunc mapinfo delegate_type : texpr * (tclass * texpr list) =
			let in_unsafe = mapinfo.in_unsafe || match gen.gcurrent_class, gen.gcurrent_classfield with
				| Some c, _ when Meta.has Meta.Unsafe c.cl_meta -> true
				| _, Some cf when Meta.has Meta.Unsafe cf.cf_meta -> true
				| _ -> false
			in
			(* get all captured variables it uses *)
			let captured_ht, tparams = get_captured fexpr in
			let captured = Hashtbl.fold (fun _ e acc -> e :: acc) captured_ht [] in
			let captured = List.sort (fun e1 e2 -> match e1, e2 with
				| { eexpr = TLocal v1 }, { eexpr = TLocal v2 } ->
					compare v1.v_name v2.v_name
				| _ -> assert false) captured
			in

			(*let cltypes = List.map (fun cl -> (snd cl.cl_path, TInst(map_param cl, []) )) tparams in*)
			let cltypes = List.map (fun cl -> (snd cl.cl_path, TInst(cl, []) )) tparams in

			(* create a new class that extends abstract function class, with a ctor implementation that will setup all captured variables *)
			let cfield = match gen.gcurrent_classfield with
				| None -> "Anon"
				| Some cf -> cf.cf_name
			in
			let cur_line = Lexer.get_error_line fexpr.epos in
			let path = (fst gen.gcurrent_path, Printf.sprintf "%s_%s_%d__Fun" (snd gen.gcurrent_path) cfield cur_line) in
			let cls = mk_class (get gen.gcurrent_class).cl_module path tfunc.tf_expr.epos in
			if in_unsafe then cls.cl_meta <- (Meta.Unsafe,[],Ast.null_pos) :: cls.cl_meta;

			if Common.defined gen.gcon Define.EraseGenerics then begin
				cls.cl_meta <- (Meta.HaxeGeneric,[],Ast.null_pos) :: cls.cl_meta
			end;
			cls.cl_module <- (get gen.gcurrent_class).cl_module;
			cls.cl_params <- cltypes;

			let mk_this v pos =
				{
					(mk_field_access gen { eexpr = TConst TThis; etype = TInst(cls, List.map snd cls.cl_params); epos = pos } v.v_name pos)
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

			let invokecf, invoke_field, super_args = match delegate_type with
				| None -> (* no delegate *)
					let ifield, sa = ft.closure_to_classfield { tfunc with tf_expr = func_expr } fexpr.etype fexpr.epos in
					ifield,ifield,sa
				| Some _ ->
					let pos = cls.cl_pos in
					let cf = mk_class_field "Delegate" (TFun(fun_args tfunc.tf_args, tfunc.tf_type)) true pos (Method MethNormal) [] in
					cf.cf_expr <- Some { fexpr with eexpr = TFunction { tfunc with tf_expr = func_expr }; };
					cf.cf_meta <- (Meta.Final,[],pos) :: cf.cf_meta;
					cls.cl_ordered_fields <- cf :: cls.cl_ordered_fields;
					cls.cl_fields <- PMap.add cf.cf_name cf cls.cl_fields;
					(* invoke function body: call Delegate function *)
					let ibody = {
						eexpr = TCall({
							eexpr = TField({
								eexpr = TConst TThis;
								etype = TInst(cls, List.map snd cls.cl_params);
								epos = pos;
							}, FInstance(cls, List.map snd cls.cl_params, cf));
							etype = cf.cf_type;
							epos = pos;
						}, List.map (fun (v,_) -> mk_local v pos) tfunc.tf_args);
						etype = tfunc.tf_type;
						epos = pos
					} in
					let ibody = if not (ExtType.is_void tfunc.tf_type) then
						{ ibody with eexpr = TReturn( Some ibody ) }
					else
						ibody
					in
					let ifield, sa = ft.closure_to_classfield { tfunc with tf_expr = ibody } fexpr.etype fexpr.epos in
					cf,ifield,sa
			in

			(* create the constructor *)
			(* todo properly abstract how type var is set *)

			cls.cl_super <- Some(ft.func_class, []);
			let pos = cls.cl_pos in
			let super_call =
			{
				eexpr = TCall({ eexpr = TConst(TSuper); etype = TInst(ft.func_class,[]); epos = pos }, super_args);
				etype = gen.gcon.basic.tvoid;
				epos = pos;
			} in

			let ctor_type = (TFun(ctor_sig, gen.gcon.basic.tvoid)) in
			let ctor = mk_class_field "new" ctor_type true cls.cl_pos (Method(MethNormal)) [] in
			ctor.cf_expr <- Some(
			{
				eexpr = TFunction(
				{
					tf_args = ctor_args;
					tf_type = gen.gcon.basic.tvoid;
					tf_expr = { eexpr = TBlock(super_call :: ctor_exprs); etype = gen.gcon.basic.tvoid; epos = cls.cl_pos }
				});
				etype = ctor_type;
				epos = cls.cl_pos;
			});
			cls.cl_constructor <- Some(ctor);

			(* add invoke function to the class *)
			cls.cl_ordered_fields <- invoke_field :: cls.cl_ordered_fields;
			cls.cl_fields <- PMap.add invoke_field.cf_name invoke_field cls.cl_fields;
			cls.cl_overrides <- invoke_field :: cls.cl_overrides;

			gen.gadd_to_module (TClassDecl cls) priority;

			(* if there are no captured variables, we can create a cache so subsequent calls don't need to create a new function *)
			let expr, clscapt =
				match captured, tparams with
				| [], [] ->
					let cache_var = gen.gmk_internal_name "hx" "current" in
					let cache_cf = mk_class_field cache_var (TInst(cls,[])) false func_expr.epos (Var({ v_read = AccNormal; v_write = AccNormal })) [] in
					cls.cl_ordered_statics <- cache_cf :: cls.cl_ordered_statics;
					cls.cl_statics <- PMap.add cache_var cache_cf cls.cl_statics;

					(* if (FuncClass.hx_current != null) FuncClass.hx_current; else (FuncClass.hx_current = new FuncClass()); *)

					(* let mk_static_field_access cl field fieldt pos = *)
					let hx_current = mk_static_field_access cls cache_var (TInst(cls,[])) func_expr.epos in

					let pos = func_expr.epos in
					{ fexpr with
						eexpr = TIf(
							{
								eexpr = TBinop(OpNotEq, hx_current, null (TInst(cls,[])) pos);
								etype = gen.gcon.basic.tbool;
								epos = pos;
							},
							hx_current,
							Some(
							{
								eexpr = TBinop(OpAssign, hx_current, { fexpr with eexpr = TNew(cls, [], captured) });
								etype = (TInst(cls,[]));
								epos = pos;
							}))
					}, (cls,captured)
				| _ ->
					(* change the expression so it will be a new "added class" ( captured variables arguments ) *)
					{ fexpr with eexpr = TNew(cls, List.map (fun cl -> TInst(cl,[])) tparams, List.rev captured) }, (cls,captured)
			in
			match delegate_type with
			| None ->
				expr,clscapt
			| Some _ ->
				{
					eexpr = TField(expr, FClosure(Some (cls,[]),invokecf)); (* TODO: FClosure change *)
					etype = invokecf.cf_type;
					epos = cls.cl_pos
				}, clscapt
		in

		let tvar_to_cdecl = Hashtbl.create 0 in

		let run = traverse
			gen
			~tparam_anon_decl:(fun v e fn ->
				let _, info = handle_anon_func e fn null_map_info None in
				Hashtbl.add tvar_to_cdecl v.v_id info
			)
			~tparam_anon_acc:(fun v e -> try
				let cls, captured = Hashtbl.find tvar_to_cdecl v.v_id in
				let types = match v.v_extra with
					| Some(t,_) -> t
					| _ -> assert false
				in
				let monos = List.map (fun _ -> mk_mono()) types in
				let vt = match follow v.v_type with
					| TInst(_, [v]) -> v
					| v -> v
				in
				let et = match follow e.etype with
					| TInst(_, [v]) -> v
					| v -> v
				in
				let original = apply_params types monos vt in
				unify et original;

				let monos = List.map (fun t -> apply_params types (List.map (fun _ -> t_dynamic) types) t) monos in

				let same_cl t1 t2 = match follow t1, follow t2 with
					| TInst(c,_), TInst(c2,_) -> c == c2
					| _ -> false
				in
				let passoc = List.map2 (fun (_,t) m -> t,m) types monos in
				let cltparams = List.map (fun (_,t) ->
					try
						snd (List.find (fun (t2,_) -> same_cl t t2) passoc)
					with | Not_found -> t) cls.cl_params
				in
				{ e with eexpr = TNew(cls, cltparams, captured) }
			with
				| Not_found ->
				gen.gcon.warning "This expression may be invalid" e.epos;
				e
				| Unify_error el ->
					List.iter (fun el -> gen.gcon.warning (Error.unify_error_msg (print_context()) el) e.epos) el;
				gen.gcon.warning "This expression may be invalid" e.epos;
				e
			)
			(* (handle_anon_func:texpr->tfunc->texpr) (dynamic_func_call:texpr->texpr->texpr list->texpr) *)
			(fun e f info delegate_type -> fst (handle_anon_func e f info delegate_type))
			ft.dynamic_fun_call
			(* (dynamic_func_call:texpr->texpr->texpr list->texpr) *)
		in
		let map e = Some(run e) in
		gen.gexpr_filters#add ~name:name ~priority:(PCustom priority) map


	(*
		this submodule will provide the default implementation for the C# and Java targets.

		it will have two return types: double and dynamic, and
	*)
	module DoubleAndDynamicClosureImpl =
	struct
		let get_ctx gen parent_func_class max_arity (* e.g. new haxe.lang.ClassClosure *) =
			let basic = gen.gcon.basic in

			let func_args_i i =
				let rec loop i (acc) =
					if i = 0 then (acc) else begin
						let vfloat = alloc_var (gen.gmk_internal_name "fn" ("float" ^ string_of_int i)) basic.tfloat in
						let vdyn = alloc_var (gen.gmk_internal_name "fn" ("dyn" ^ string_of_int i)) t_dynamic in

						loop (i - 1) ((vfloat, None) :: (vdyn, None) :: acc)
					end
				in
				loop i []
			in

			let args_real_to_func args =
				let arity = List.length args in
				if arity >= max_arity then
					[ alloc_var (gen.gmk_internal_name "fn" "dynargs") (basic.tarray t_dynamic), None ]
				else func_args_i arity
			in

			let func_sig_i i =
				let rec loop i acc =
					if i = 0 then acc else begin
						let vfloat = gen.gmk_internal_name "fn" ("float" ^ string_of_int i) in
						let vdyn = gen.gmk_internal_name "fn" ("dyn" ^ string_of_int i) in

						loop (i - 1) ( (vfloat,false,basic.tfloat) :: (vdyn,false,t_dynamic) :: acc )
					end
				in
				loop i []
			in

			let args_real_to_func_sig args =
				let arity = List.length args in
				if arity >= max_arity then
					[gen.gmk_internal_name "fn" "dynargs", false, basic.tarray t_dynamic]
				else begin
					func_sig_i arity
				end
			in

			let rettype_real_to_func t = match run_follow gen t with
				| TType({ t_path = [],"Null" }, _) ->
					0,t_dynamic
				| _ when like_float t && not (like_i64 t) ->
					(1, basic.tfloat)
				| _ ->
					(0, t_dynamic)
			in

			let args_real_to_func_call el (pos:Ast.pos) =
				if List.length el >= max_arity then
					[{ eexpr = TArrayDecl el; etype = basic.tarray t_dynamic; epos = pos }]
				else begin
					List.fold_left (fun acc e ->
						if like_float (gen.greal_type e.etype) && not (like_i64 (gen.greal_type e.etype)) then
							( e :: undefined e.epos :: acc )
						else
							( null basic.tfloat e.epos :: e :: acc )
					) ([]) (List.rev el)
				end
			in

			let const_type c def =
				match c with
				| TString _ -> basic.tstring
				| TInt _ -> basic.tint
				| TFloat _ -> basic.tfloat
				| TBool _ -> basic.tbool
				| _ -> def
			in

			let get_args_func args changed_args pos =
				let arity = List.length args in
				let mk_const const elocal t =
					match const with
					| None ->
						mk_cast t elocal
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
						(count + 1, (mk (TVar(v, Some(mk_const const (mk_varray count) v.v_type))) basic.tvoid pos) :: acc)
					) (0,[]) args)
				end else begin
					let _, dyn_args, float_args = List.fold_left (fun (count,fargs, dargs) arg ->
						if count land 1 = 0 then
							(count + 1, fargs, arg :: dargs)
						else
							(count + 1, arg :: fargs, dargs)
					) (1,[],[]) (List.rev changed_args) in

					let rec loop acc args fargs dargs =
						match args, fargs, dargs with
							| [], [], [] -> acc
							| (v,const) :: args, (vf,_) :: fargs, (vd,_) :: dargs ->
								let acc = { eexpr = TVar(v, Some(
									{
										eexpr = TIf(
											{ eexpr = TBinop(Ast.OpEq, mk_local vd pos, undefined pos); etype = basic.tbool; epos = pos },
											mk_cast v.v_type (mk_local vf pos),
											Some ( mk_const const (mk_local vd pos) v.v_type )
										);
										etype = v.v_type;
										epos = pos
									} )); etype = basic.tvoid; epos = pos } :: acc in
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

						(TFun(args_real_to_func_sig _sig, ret_t), arity, type_n, ret_t, ExtType.is_void ret, is_dynamic_func)
					| _ -> (print_endline (s_type (print_context()) (follow old_sig) )); assert false
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

				let func_expr = Type.concat real_get_args tf_expr in

				(* set invoke function *)
				(* todo properly abstract how naming for invoke is made *)
				let invoke_name = if is_dynamic_func then "invokeDynamic" else ("invoke" ^ (string_of_int arity) ^ (if type_number = 0 then "_o" else "_f")) in
				let invoke_name = gen.gmk_internal_name "hx" invoke_name in
				let invoke_field = mk_class_field invoke_name changed_sig false func_expr.epos (Method(MethNormal)) [] in
				let invoke_fun = {
					eexpr = TFunction {
						tf_args = changed_args;
						tf_type = changed_sig_ret;
						tf_expr = func_expr;
					};
					etype = changed_sig;
					epos = func_expr.epos;
				} in
				invoke_field.cf_expr <- Some invoke_fun;

				invoke_field, [
					ExprBuilder.make_int gen.gcon arity pos;
					ExprBuilder.make_int gen.gcon type_number pos;
				]
			in

			let dynamic_fun_call call_expr =
				let tc, params = match call_expr.eexpr with
					| TCall(tc, params) -> tc, params
					| _ -> assert false
				in
				let ct = gen.greal_type call_expr.etype in
				let postfix, ret_t =
					if like_float ct && not (like_i64 ct) then
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

			let iname i is_float =
				let postfix = if is_float then "_f" else "_o" in
				gen.gmk_internal_name "hx" ("invoke" ^ string_of_int i) ^ postfix
			in

			let map_base_classfields cl map_fn =
				let pos = cl.cl_pos in
				let this_t = TInst(cl,List.map snd cl.cl_params) in
				let this = { eexpr = TConst(TThis); etype = this_t; epos = pos } in
				let mk_this field t = { (mk_field_access gen this field pos) with etype = t } in

				let mk_invoke_i i is_float =
					let cf = mk_class_field (iname i is_float) (TFun(func_sig_i i, if is_float then basic.tfloat else t_dynamic)) false pos (Method MethNormal) [] in
					cf
				in

				let type_name = gen.gmk_internal_name "fn" "type" in
				let dynamic_arg = alloc_var (gen.gmk_internal_name "fn" "dynargs") (basic.tarray t_dynamic) in

				let mk_invoke_complete_i i is_float =

					(* let arity = i in *)
					let args = func_args_i i in

					(* api fn *)

					(* only cast if needed *)
					let mk_cast tto efrom = gen.ghandle_cast (gen.greal_type tto) (gen.greal_type efrom.etype) efrom in
					let api i t const =
						let vf, _ = List.nth args (i * 2) in
						let vo, _ = List.nth args (i * 2 + 1) in

						let needs_cast, is_float = match t, like_float t && not (like_i64 t) with
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

					let fn_expr = map_fn i ret (List.map fst args) api in

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

				let switch =
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

				let args = [dynamic_arg, None] in
				let dyn_t = TFun(fun_args args, t_dynamic) in
				let dyn_cf = mk_class_field (gen.gmk_internal_name "hx" "invokeDynamic") dyn_t false pos (Method MethNormal) [] in

				dyn_cf.cf_expr <- Some {
					eexpr = TFunction {
						tf_args = args;
						tf_type = t_dynamic;
						tf_expr = mk_block switch
					};
					etype = dyn_t;
					epos = pos;
				};

				let additional_cfs = begin
					let new_t = TFun(["arity", false, basic.tint; "type", false, basic.tint],basic.tvoid) in
					let new_cf = mk_class_field "new" (new_t) true pos (Method MethNormal) [] in
					let v_arity, v_type = alloc_var "arity" basic.tint, alloc_var "type" basic.tint in
					let mk_assign v field = mk (TBinop (OpAssign, mk_this field v.v_type, mk_local v pos)) v.v_type pos in

					let arity_name = gen.gmk_internal_name "hx" "arity" in
					new_cf.cf_expr <- Some {
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
					};

					[
						new_cf;
						mk_class_field type_name basic.tint true pos (Var { v_read = AccNormal; v_write = AccNormal }) [];
						mk_class_field arity_name basic.tint true pos (Var { v_read = AccNormal; v_write = AccNormal }) [];
					]
				end in

				dyn_cf :: (additional_cfs @ cfs)
			in

			begin
				(*
					setup fields for the abstract implementation of the Function class

					new(arity, type)
					{
						this.arity = arity;
						this.type = type;
					}

					hx::invokeX_f|o (where X is from 0 to max_arity) (args)
					{
						if (this.type == 0|1) return invokeX_o|f(args); else throw "Invalid number of arguments."
					}

					hx::invokeDynamic, which will work in the same way
				*)
				let cl = parent_func_class in
				let pos = cl.cl_pos in

				let rec mk_dyn_call arity api =
					let zero = ExprBuilder.make_float gen.gcon "0.0" pos in
					let rec loop i acc =
						if i = 0 then
							acc
						else begin
							let arr = api (i - 1) t_dynamic None in
							loop (i - 1) (zero :: arr :: acc)
						end
					in
					loop arity []
				in

				let this = mk (TConst TThis) (TInst (cl, List.map snd cl.cl_params)) pos in
				let mk_this field t = { (mk_field_access gen this field pos) with etype = t } in

				let mk_invoke_switch i api =
					let t = TFun (func_sig_i i, t_dynamic) in
					(* case i: return this.invokeX_o(0, 0, 0, 0, 0, ... arg[0], args[1]....); *)
					[ExprBuilder.make_int gen.gcon i pos], mk_return (mk (TCall(mk_this (iname i false) t, mk_dyn_call i api)) t_dynamic pos)
				in
				let rec loop_cases api arity acc =
					if arity < 0 then
						acc
					else
						loop_cases api (arity - 1) (mk_invoke_switch arity api :: acc)
				in

				let type_name = gen.gmk_internal_name "fn" "type" in
				let mk_expr i is_float vars =
					let call_expr =
						let call_t = TFun(List.map (fun v -> (v.v_name, false, v.v_type)) vars, if is_float then t_dynamic else basic.tfloat) in
						{
							eexpr = TCall(mk_this (iname i (not is_float)) call_t, List.map (fun v -> mk_local v pos) vars);
							etype = if is_float then t_dynamic else basic.tfloat;
							epos = pos
						}
					in
					{
						eexpr = TIf(
							mk (TBinop (Ast.OpNotEq, mk_this type_name basic.tint, (ExprBuilder.make_int gen.gcon (if is_float then 0 else 1) pos))) basic.tbool pos,
							mk (TThrow (ExprBuilder.make_string gen.gcon "Wrong number of arguments" pos)) t_dynamic pos,
							Some (mk_return call_expr)
						);
						etype = t_dynamic;
						epos = pos;
					}
				in

				let arities_processed = Hashtbl.create 10 in
				let max_arity = ref 0 in

				let map_fn cur_arity fun_ret_type vars (api:int->t->tconstant option->texpr) =
					let is_float = like_float fun_ret_type && not (like_i64 fun_ret_type) in
					match cur_arity with
					| -1 ->
						let dynargs = api (-1) t_dynamic None in

						(* (dynargs == null) ? 0 : dynargs.length *)
						let switch_cond = {
							eexpr = TIf(
								mk (TBinop (OpEq, dynargs, null dynargs.etype pos)) basic.tbool pos,
								mk (TConst (TInt Int32.zero)) basic.tint pos,
								Some (mk_field_access gen dynargs "length" pos));
							etype = basic.tint;
							epos = pos;
						} in

						{
							eexpr = TSwitch(
								switch_cond,
								loop_cases api !max_arity [],
								Some(mk (TThrow (ExprBuilder.make_string gen.gcon "Too many arguments" pos)) basic.tvoid pos));
							etype = basic.tvoid;
							epos = pos;
						}
					| _ ->
						if not (Hashtbl.mem arities_processed cur_arity) then begin
							Hashtbl.add arities_processed cur_arity true;
							if cur_arity > !max_arity then max_arity := cur_arity
						end;

						mk_expr cur_arity is_float vars
				in

				let cfs = map_base_classfields cl map_fn in
				List.iter (fun cf ->
					if cf.cf_name = "new" then
						parent_func_class.cl_constructor <- Some cf
					else
						parent_func_class.cl_fields <- PMap.add cf.cf_name cf parent_func_class.cl_fields
				) cfs;
				parent_func_class.cl_ordered_fields <- (List.filter (fun cf -> cf.cf_name <> "new") cfs) @ parent_func_class.cl_ordered_fields
			end;

			{
				func_class = parent_func_class;
				closure_to_classfield = closure_to_classfield;
				dynamic_fun_call = dynamic_fun_call;
				map_base_classfields = map_base_classfields;
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
	let infer_params gen pos (original_args:((string * bool * t) list * t)) (applied_args:((string * bool * t) list * t)) (params:(string * t) list) calls_parameters_explicitly : tparams =
		match params with
		| [] -> []
		| _ ->
			let args_list args = (if not calls_parameters_explicitly then t_dynamic else snd args) :: (List.map (fun (n,o,t) -> t) (fst args)) in

			let monos = List.map (fun _ -> mk_mono()) params in
			let original = args_list (get_fun (apply_params params monos (TFun(fst original_args,snd original_args)))) in
			let applied = args_list applied_args in

			(try
				List.iter2 (fun a o ->
					unify a o
					(* type_eq EqStrict a o *)
				) applied original
				(* unify applied original *)
			with | Unify_error el ->
					(* List.iter (fun el -> gen.gcon.warning (Typecore.unify_error_msg (print_context()) el) pos) el; *)
					gen.gcon.warning ("This expression may be invalid") pos
		 | Invalid_argument("List.map2") ->
					gen.gcon.warning ("This expression may be invalid") pos
			);

			List.map (fun t ->
				match follow t with
					| TMono _ ->	t_empty
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

		let rec has_type_params t =
			match follow t with
				| TInst( { cl_kind = KTypeParameter _ }, _) -> true
				| TAbstract(_, params)
				| TEnum(_, params)
				| TInst(_, params) -> List.exists (fun t -> has_type_params t) params
				| TFun(args,ret) ->
					List.exists (fun (n,o,t) -> has_type_params t) args || has_type_params ret
				| _ -> false

		let rec follow_all_md md =
			let t = match md with
				| TClassDecl { cl_kind = KAbstractImpl a } ->
					TAbstract(a, List.map snd a.a_params)
				| TClassDecl c ->
					TInst(c, List.map snd c.cl_params)
				| TEnumDecl e ->
					TEnum(e, List.map snd e.e_params)
				| TTypeDecl t ->
					TType(t, List.map snd t.t_params)
				| TAbstractDecl a ->
					TAbstract(a, List.map snd a.a_params)
			in
			Abstract.follow_with_abstracts t

		let rec is_hxgeneric md =
			match md with
			| TClassDecl { cl_kind = KAbstractImpl a } ->
				is_hxgeneric (TAbstractDecl a)
			| TClassDecl(cl) ->
				not (Meta.has Meta.NativeGeneric cl.cl_meta)
			| TEnumDecl(e) ->
				not (Meta.has Meta.NativeGeneric e.e_meta)
			| TAbstractDecl(a) when Meta.has Meta.NativeGeneric a.a_meta ->
				not (Meta.has Meta.NativeGeneric a.a_meta)
			| md -> match follow_all_md md with
				| TInst(cl,_) -> is_hxgeneric (TClassDecl cl)
				| TEnum(e,_) -> is_hxgeneric (TEnumDecl e)
				| TAbstract(a,_) -> not (Meta.has Meta.NativeGeneric a.a_meta)
				| _ -> true

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
					| TClassDecl(cl)	->
						(* first see if any meta is present (already processed) *)
						if Meta.has Meta.NativeGeneric cl.cl_meta then
							Some false
						else if Meta.has Meta.HaxeGeneric cl.cl_meta then
							Some true
						else if cl.cl_params = [] && is_hxgen md then
							(cl.cl_meta <- (Meta.HaxeGeneric,[],cl.cl_pos) :: cl.cl_meta;
							Some true)
						else if cl.cl_params = [] then
							(cl.cl_meta <- (Meta.NativeGeneric, [], cl.cl_pos) :: cl.cl_meta;
							Some false)
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
									match cl.cl_params with
										| [] ->
											(* if it's not, then it will follow hxgen *)
											if is_hxgen (TClassDecl cl) then
												cl.cl_meta <- (Meta.HaxeGeneric, [], cl.cl_pos) :: cl.cl_meta
											else
												cl.cl_meta <- (Meta.NativeGeneric, [], cl.cl_pos) :: cl.cl_meta;
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
							match e.e_params with
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
			let ret = match md with
				| TClassDecl { cl_kind = KAbstractImpl a } -> (match follow_all_md md with
					| (TInst _ | TEnum _ as t) -> (
						let md = match t with
							| TInst(cl,_) -> TClassDecl cl
							| TEnum(e,_) -> TEnumDecl e
							| _ -> assert false
						in
						let ret = set_hxgeneric gen [] true md in
						if ret = None then get (set_hxgeneric gen [] false md) else get ret)
					| TAbstract(a,_) -> true
					| _ -> true)
				| _ -> match set_hxgeneric gen [] true md with
					| None ->
						get (set_hxgeneric gen [] false md)
					| Some v ->
						v
			in
			if not ret then begin
				match md with
				| TClassDecl c ->
					let set_hxgeneric (_,param) = match follow param with
						| TInst(c,_) ->
							c.cl_meta <- (Meta.NativeGeneric, [], c.cl_pos) :: c.cl_meta
						| _ -> ()
					in
					List.iter set_hxgeneric c.cl_params;
					let rec handle_field cf =
						List.iter set_hxgeneric cf.cf_params;
						List.iter handle_field cf.cf_overloads
					in
					(match c.cl_kind with
						| KAbstractImpl a ->
							List.iter set_hxgeneric a.a_params;
						| _ -> ());
					List.iter handle_field c.cl_ordered_fields;
					List.iter handle_field c.cl_ordered_statics
				| _ -> ()
			end;
			ret

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

			let set_only_hxgeneric gen =
				let rec run md =
					match md with
						| TTypeDecl _ | TAbstractDecl _ -> md
						| _ -> ignore (set_hxgeneric gen md); md
				in
				run

			let name = "real_type_params_modf"

			let priority = solve_deps name []

			let rec get_fields gen cl params_cl params_cf acc =
				let fields = List.fold_left (fun acc cf ->
					match follow (gen.greal_type (gen.gfollow#run_f (cf.cf_type))) with
						| TInst(cli, ((_ :: _) as p)) when (not (is_hxgeneric (TClassDecl cli))) && params_has_tparams p ->
							(cf, apply_params cl.cl_params params_cl cf.cf_type, apply_params cl.cl_params params_cf cf.cf_type) :: acc
						| TEnum(e, ((_ :: _) as p)) when not (is_hxgeneric (TEnumDecl e)) && params_has_tparams p ->
							(cf, apply_params cl.cl_params params_cl cf.cf_type, apply_params cl.cl_params params_cf cf.cf_type) :: acc
						| _ -> acc
				) [] cl.cl_ordered_fields in
				match cl.cl_super with
					| Some(cs, tls) ->
						get_fields gen cs (List.map (apply_params cl.cl_params params_cl) tls) (List.map (apply_params cl.cl_params params_cf) tls) (fields @ acc)
					| None -> (fields @ acc)

			let get_cast_name cl = String.concat "_" ((fst cl.cl_path) @ [snd cl.cl_path; "cast"]) (* explicitly define it *)

			(* overrides all needed cast functions from super classes / interfaces to call the new cast function *)
			let create_stub_casts gen cl cast_cfield =
				(* go through superclasses and interfaces *)
				let p = cl.cl_pos in
				let this = { eexpr = TConst TThis; etype = (TInst(cl, List.map snd cl.cl_params)); epos = p } in

				let rec loop cls tls level reverse_params =
					if (level <> 0 || cls.cl_interface) && tls <> [] && is_hxgeneric (TClassDecl cls) then begin
						let cparams = List.map (fun (s,t) -> (s, TInst (map_param (get_cl_t t), []))) cls.cl_params in
						let name = get_cast_name cls in
						if not (PMap.mem name cl.cl_fields) then begin
							let reverse_params = List.map (apply_params cls.cl_params (List.map snd cparams)) reverse_params in
							let cfield = mk_class_field name (TFun([], t_dynamic)) false cl.cl_pos (Method MethNormal) cparams in
							let field = { eexpr = TField(this, FInstance(cl,List.map snd cl.cl_params, cast_cfield)); etype = apply_params cast_cfield.cf_params reverse_params cast_cfield.cf_type; epos = p } in
							let call =
							{
								eexpr = TCall(field, []);
								etype = t_dynamic;
								epos = p;
							} in
							let call = gen.gparam_func_call call field reverse_params [] in
							let delay () =
								cfield.cf_expr <-
								Some {
									eexpr = TFunction(
									{
										tf_args = [];
										tf_type = t_dynamic;
										tf_expr =
										{
											eexpr = TReturn( Some call );
											etype = t_dynamic;
											epos = p;
										}
									});
									etype = cfield.cf_type;
									epos = p;
								}
							in
							gen.gafter_filters_ended <- delay :: gen.gafter_filters_ended; (* do not let filters alter this expression content *)
							cl.cl_ordered_fields <- cfield :: cl.cl_ordered_fields;
							cl.cl_fields <- PMap.add cfield.cf_name cfield cl.cl_fields;
							if level <> 0 then cl.cl_overrides <- cfield :: cl.cl_overrides
						end
					end;
					let get_reverse super supertl =
						let kv = List.map2 (fun (_,tparam) applied -> (follow applied, follow tparam)) super.cl_params supertl in
						List.map (fun t ->
							try
								List.assq (follow t) kv
							with | Not_found -> t
						) reverse_params
					in
					(match cls.cl_super with
					| None -> ()
					| Some(super, supertl) ->
						loop super supertl (level + 1) (get_reverse super supertl));
					List.iter (fun (iface, ifacetl) ->
						loop iface ifacetl level (get_reverse iface ifacetl)
					) cls.cl_implements
				in
				loop cl (List.map snd cl.cl_params) 0 (List.map snd cl.cl_params)

			(*
				Creates a cast classfield, with the desired name

				Will also look for previous cast() definitions and override them, to reflect the current type and fields

				FIXME: this function still doesn't support generics that extend generics, and are cast as one of its subclasses. This needs to be taken care, by
				looking at previous superclasses and whenever a generic class is found, its cast argument must be overriden. the toughest part is to know how to type
				the current type correctly.
			*)
			let create_cast_cfield gen cl name =
				let basic = gen.gcon.basic in
				let cparams = List.map (fun (s,t) -> (s, TInst (map_param (get_cl_t t), []))) cl.cl_params in
				let cfield = mk_class_field name (TFun([], t_dynamic)) false cl.cl_pos (Method MethNormal) cparams in
				let params = List.map snd cparams in

				let fields = get_fields gen cl (List.map snd cl.cl_params) params [] in

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
				let this = { eexpr = TConst(TThis); etype = (TInst(cl, List.map snd cl.cl_params)); epos = pos } in
				let field_var = alloc_var "field" gen.gcon.basic.tstring in
				let local_field = { eexpr = TLocal(field_var); etype = field_var.v_type; epos = pos } in
				let i_var = alloc_var "i" gen.gcon.basic.tint in
				let local_i = { eexpr = TLocal(i_var); etype = gen.gcon.basic.tint; epos = pos } in
				let incr_i = { eexpr = TUnop(Ast.Increment, Ast.Postfix, local_i); etype = basic.tint; epos = pos } in
				let fields_var = alloc_var "fields" (gen.gcon.basic.tarray gen.gcon.basic.tstring) in
				let local_fields = { eexpr = TLocal(fields_var); etype = (gen.gcon.basic.tarray gen.gcon.basic.tstring); epos = pos } in

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
						let this_field = { eexpr = TField(this, FInstance(cl, List.map snd cl.cl_params, cf)); etype = t_cl; epos = pos } in
						let expr =
						{
							eexpr = TBinop(OpAssign, { eexpr = TField(local_new_me, FInstance(cl, List.map snd cl.cl_params, cf) ); etype = t_cf; epos = pos },
								try (Hashtbl.find gen.gtparam_cast (get_path t_cf)) this_field t_cf with | Not_found -> (* if not found tparam cast, it shouldn't be a valid hxgeneric *) assert false
							);
							etype = t_cf;
							epos = pos;
						} in

						[ExprBuilder.make_string gen.gcon cf.cf_name pos], expr
					) fields
				in

				let mk_typehandle =
					let thandle = alloc_var "__typeof__" t_dynamic in
					(fun cl -> { eexpr = TCall(mk_local thandle pos, [ ExprBuilder.make_static_this cl pos ]); etype = t_dynamic; epos = pos })
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
										mk_typehandle_cond (List.map snd cl.cl_params) params,
										mk_return this,
										None);
									etype = basic.tvoid;
									epos = pos;
								};
								(* var new_me = /*special create empty with tparams construct*/ *)
								{
									eexpr = TVar(new_me_var, Some(gen.gtools.r_create_empty cl params pos));
									etype = gen.gcon.basic.tvoid;
									epos = pos
								};
								(* var fields = Reflect.fields(this); *)
								{
									eexpr = TVar(fields_var, Some(gen.gtools.r_fields true this));
									etype = gen.gcon.basic.tvoid;
									epos = pos
								};
								(* var i = 0; *)
								{
									eexpr = TVar(i_var, Some(ExprBuilder.make_int gen.gcon 0 pos));
									etype = gen.gcon.basic.tvoid;
									epos = pos
								};
								{
									eexpr = TWhile( (* while (i < fields.length) *)
										{
											eexpr = TBinop(Ast.OpLt,
												local_i,
												mk_field_access gen local_fields "length" pos);
											etype = gen.gcon.basic.tbool;
											epos = pos
										},
										{
											eexpr = TBlock [
												(* var field = fields[i++]; *)
												{
													eexpr = TVar(field_var, Some { eexpr = TArray (local_fields, incr_i); etype = gen.gcon.basic.tstring; epos = pos });
													etype = gen.gcon.basic.tvoid;
													epos = pos
												};
												(
													(* default: Reflect.setField(new_me, field, Reflect.field(this, field)) *)
													let edef = gen.gtools.r_set_field gen.gcon.basic.tvoid local_new_me local_field (gen.gtools.r_field false gen.gcon.basic.tvoid this local_field) in
													if fields <> [] then
														(* switch(field) { ... } *)
														{
															eexpr = TSwitch(local_field, fields_to_cases fields, Some(edef));
															etype = gen.gcon.basic.tvoid;
															epos = pos;
														}
													else
														edef;
												)
											];
											etype = gen.gcon.basic.tvoid;
											epos = pos
										},
										Ast.NormalWhile
									);
									etype = gen.gcon.basic.tvoid;
									epos = pos;
								};
 								(* return new_me *)
								mk_return local_new_me
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
				let field = { eexpr = TField(mk_local me p, FInstance(iface, List.map snd iface.cl_params, cf)); etype = apply_params cf.cf_params params cf.cf_type; epos = p } in
				let call =
				{
					eexpr = TCall(field, []);
					etype = t_dynamic;
					epos = p;
				} in
				let call = gen.gparam_func_call call field params [] in

				(* since object.someCall<ExplicitParameterDefinition>() isn't allowed on Haxe, we need to directly apply the params and delay this call *)
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

				let implement_stub_cast cthis iface tl =
					let name = get_cast_name iface in
					if not (PMap.mem name cthis.cl_fields) then begin
						let cparams = List.map (fun (s,t) -> ("To_" ^ s, TInst(map_param (get_cl_t t), []))) iface.cl_params in
						let field = mk_class_field name (TFun([],t_dynamic)) false iface.cl_pos (Method MethNormal) cparams in
						let this = { eexpr = TConst TThis; etype = TInst(cthis, List.map snd cthis.cl_params); epos = cthis.cl_pos } in
						field.cf_expr <- Some {
							etype = TFun([],t_dynamic);
							epos = this.epos;
							eexpr = TFunction {
								tf_type = t_dynamic;
								tf_args = [];
								tf_expr = mk_block { this with
									eexpr = TReturn (Some this)
								}
							}
						};
						cthis.cl_ordered_fields <- field :: cthis.cl_ordered_fields;
						cthis.cl_fields <- PMap.add name field cthis.cl_fields
					end
				in

				let rec run md =
					match md with
						| TClassDecl ({ cl_params = [] } as cl) ->
							(* see if we're implementing any generic interface *)
							let rec check (iface,tl) =
								if tl <> [] && set_hxgeneric gen (TClassDecl iface) then
									(* implement cast stub *)
									implement_stub_cast cl iface tl;
								List.iter (fun (s,stl) -> check (s, List.map (apply_params iface.cl_params tl) stl)) iface.cl_implements;
							in
							List.iter (check) cl.cl_implements;
							md
						| TClassDecl ({ cl_params = hd :: tl } as cl) when set_hxgeneric gen md ->
							let iface = mk_class cl.cl_module cl.cl_path cl.cl_pos in
							iface.cl_array_access <- Option.map (apply_params (cl.cl_params) (List.map (fun _ -> t_dynamic) cl.cl_params)) cl.cl_array_access;
							iface.cl_extern <- cl.cl_extern;
							iface.cl_module <- cl.cl_module;
							iface.cl_private <- cl.cl_private;
							iface.cl_meta <-
								(Meta.HxGen, [], cl.cl_pos)
								::
								(Meta.Custom "generic_iface", [(EConst(Int(string_of_int(List.length cl.cl_params))), cl.cl_pos)], cl.cl_pos)
								::
								iface.cl_meta;
							Hashtbl.add ifaces cl.cl_path iface;

							iface.cl_implements <- (base_generic, []) :: iface.cl_implements;
							iface.cl_interface <- true;
							cl.cl_implements <- (iface, []) :: cl.cl_implements;

							let name = get_cast_name cl in
							let cast_cf = create_cast_cfield gen cl name in
							if not cl.cl_interface then create_stub_casts gen cl cast_cf;

							let rec loop c = match c.cl_super with
								| None -> ()
								| Some(sup,_) -> try
									let siface = Hashtbl.find ifaces sup.cl_path in
									iface.cl_implements <- (siface,[]) :: iface.cl_implements;
									()
								with | Not_found -> loop sup
							in
							loop cl;

							(if not cl.cl_interface then cl.cl_ordered_fields <- cast_cf :: cl.cl_ordered_fields);
							let iface_cf = mk_class_field name cast_cf.cf_type false cast_cf.cf_pos (Method MethNormal) cast_cf.cf_params in
							let cast_static_cf, delay = create_static_cast_cf gen iface iface_cf in

							cl.cl_ordered_statics <- cast_static_cf :: cl.cl_ordered_statics;
							cl.cl_statics <- PMap.add cast_static_cf.cf_name cast_static_cf cl.cl_statics;
							gen.gafter_filters_ended <- delay :: gen.gafter_filters_ended; (* do not let filters alter this expression content *)

							iface_cf.cf_type <- cast_cf.cf_type;
							iface.cl_fields <- PMap.add name iface_cf iface.cl_fields;
							let fields = List.filter (fun cf -> match cf.cf_kind with
								| Var _ | Method MethDynamic -> false
								| _ ->
									let is_override = List.memq cf cl.cl_overrides in
									let cf_type = if is_override && not (Meta.has Meta.Overload cf.cf_meta) then
										match find_first_declared_field gen cl cf.cf_name with
											| Some(_,_,declared_t,_,_,_,_) -> declared_t
											| _ -> assert false
									else
										cf.cf_type
									in

									not (has_type_params cf_type)
								) cl.cl_ordered_fields
							in
							let fields = List.map (fun f -> mk_class_field f.cf_name f.cf_type f.cf_public f.cf_pos f.cf_kind f.cf_params) fields in
							let fields = iface_cf :: fields in
							iface.cl_ordered_fields <- fields;
							List.iter (fun f -> iface.cl_fields <- PMap.add f.cf_name f iface.cl_fields) fields;

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
							let t = gen.greal_type e.etype in
							let unifies =
								let ctype = gen.greal_type cast_expr.etype in
								match follow ctype with
								| TInst(cl,_) -> (try
									unify ctype t;
									true
								with | Unify_error el ->
									false)
								| _ -> false
							in
							let unifies = unifies && not (PMap.mem "cs_safe_casts" gen.gcon.defines) in
							(match follow t with
								| TInst(cl, p1 :: pl) when is_hxgeneric (TClassDecl cl) && not unifies && not (Meta.has Meta.Enum cl.cl_meta) ->
									let iface = Hashtbl.find ifaces cl.cl_path in
									mk_cast e.etype (change_expr (Type.map_expr run cast_expr) cl iface (p1 :: pl))
								| _ -> Type.map_expr run e
							)
						| _ -> Type.map_expr run e
			in
			run

		let configure gen (dyn_tparam_cast:texpr->t->texpr) ifaces base_generic =
			gen.ghas_tparam_cast_handler <- true;
			let traverse = default_implementation gen dyn_tparam_cast ifaces in
			let map e = Some(traverse e) in
			gen.gsyntax_filters#add ~name:name ~priority:(PCustom priority) map;
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

			let iter_types (nt,t) =
				let cls = get_cls t in
				let orig = cls.cl_path in
				check_type (snd orig) (fun name -> cls.cl_path <- (fst orig, name))
			in

			let save_params save params =
				List.fold_left (fun save (_,t) ->
					let cls = get_cls t in
					(cls.cl_path,t) :: save) save params
			in

			List.iter (function
				| TClassDecl cl ->
					i := 0;

					let save = [] in

					found_types := PMap.empty;
					let save = save_params save cl.cl_params in
					List.iter iter_types cl.cl_params;
					let cur_found_types = !found_types in
					let save = ref save in
					List.iter (fun cf ->
						found_types := cur_found_types;
						save := save_params !save cf.cf_params;
						List.iter iter_types cf.cf_params
					) (cl.cl_ordered_fields @ cl.cl_ordered_statics);

					if !save <> [] then begin
						let save = !save in
						let res = cl.cl_restore in
						cl.cl_restore <- (fun () ->
							res();
							List.iter (fun (path,t) ->
								let cls = get_cls t in
								cls.cl_path <- path) save
						);
					end

				| TEnumDecl ( ({ e_params = hd :: tl }) ) ->
					i := 0;
					found_types := PMap.empty;
					List.iter iter_types (hd :: tl)

				| TAbstractDecl { a_params = hd :: tl } ->
					i := 0;
					found_types := PMap.empty;
					List.iter iter_types (hd :: tl)

				| _ -> ()

			) gen.gtypes_list

	end;;

end;;

(**************************************************************************************************************************)
(*																									 SYNTAX FILTERS																												*)
(**************************************************************************************************************************)

(* ******************************************* *)
(* Expression Unwrap *)
(* ******************************************* *)
(*
	This is the most important module for source-code based targets. It will follow a convention of what's an expression and what's a statement,
	and will unwrap statements where expressions are expected, and vice-versa.

	It should be one of the first syntax filters to be applied. As a consequence, it's applied after all filters that add code to the AST, and by being
	the first of the syntax filters, it will also have the AST retain most of the meaning of normal Haxe code. So it's easier to detect cases which are
	side-effects free, for example

	Any target can make use of this, but there is one requirement: The target must accept null to be set to any kind of variable. For example,
	var i:Int = null; must be accepted. The best way to deal with this is to (like it's done in C#) make null equal to "default(Type)"

	dependencies:
		While it's best for Expression Unwrap to delay its execution as much as possible, since theoretically any
		filter can return an expression that needs to be unwrapped, it is also desirable for ExpresionUnwrap to have
		the AST as close as possible as Haxe's, so it can make some correct predictions (for example, so it can
		more accurately know what can be side-effects-free and what can't).
		This way, it will run slightly after the Normal priority, so if you don't say that a syntax filter must run
		before Expression Unwrap, it will run after it.

	TODO : While statement must become do / while, with the actual block inside an if for the condition, and else for 'break'
*)
module ExpressionUnwrap =
struct
	let name = "expression_unwrap"

	(* priority: first syntax filter *)
	let priority = -10.0

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
				if underlying statement is TBinop(OpAssign/OpAssignOp), or TVar, with the right side being a Statement or a short circuit op, we can call apply_assign.

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
				will traverse the AST while there is a TBinop(OpBoolAnd/OpBoolOr) as a right-side expr, and declare new temp vars in the	for each found.
				will collect the return value, a mapped expr with all exprs as TLocal of the temp vars created


			problematic_expression_unwrap:
				check expr_kind:
					if it is NoSideEffects and not short-circuit, leave it there
					if it is ExprWithStatement and not short-circuit, call Type.map_expr problematic_expression_unwrap
					if it is Statement or Expression or short-circuit expr, call add_assign for this expression

			add_assign:
				see if the type is void. If it is, just add_statement the expression argument, and return a null value
				else create a new variable, set TVar with Some() with the expression argument, add TVar with add_statement, and return the TLocal of this expression.

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
	let rec expr_stat_map fn (expr:texpr) =
		match (no_paren expr).eexpr with
			| TBinop ( (Ast.OpAssign as op), left_e, right_e )
			| TBinop ( (Ast.OpAssignOp _ as op), left_e, right_e ) ->
				{ expr with eexpr = TBinop(op, fn left_e, fn right_e) }
			| TParenthesis _ -> assert false
			| TCall(left_e, params) ->
				{ expr with eexpr = TCall(fn left_e, List.map fn params) }
			| TNew(cl, tparams, params) ->
				{ expr with eexpr = TNew(cl, tparams, List.map fn params) }
			| TVar(v,eopt) ->
				{ expr with eexpr = TVar(v, Option.map fn eopt) }
			| TFor (v,cond,block) ->
				{ expr with eexpr = TFor(v, fn cond, block) }
			| TIf(cond,eif,eelse) ->
				{ expr with eexpr = TIf(fn cond, eif, eelse) }
			| TWhile(cond, block, flag) ->
				{ expr with eexpr = TWhile(fn cond, block, flag) }
			| TSwitch(cond, el_block_l, default) ->
				{ expr with eexpr = TSwitch( fn cond, List.map (fun (el,block) -> (List.map fn el, block)) el_block_l, default ) }
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
			| TMeta(m,e) ->
				{ expr with eexpr = TMeta(m,expr_stat_map fn e) }
			| _ -> assert false (* we only expect valid statements here. other expressions aren't valid statements *)

	let is_expr = function | Expression _ -> true | _ -> false

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

	(* statements: *)
	(* Error CS0201: Only assignment, call, increment,					 *)
	(* decrement, and new object expressions can be used as a		 *)
	(* statement (CS0201). *)
	let rec shallow_expr_type expr : shallow_expr_type =
		match expr.eexpr with
			| TCall _ when not (ExtType.is_void expr.etype) -> Both expr
			| TNew _
			| TUnop (Ast.Increment, _, _)
			| TUnop (Ast.Decrement, _, _)
			| TBinop (Ast.OpAssign, _, _)
			| TBinop (Ast.OpAssignOp _, _, _) -> Both expr
			| TIf (cond, eif, Some(eelse)) -> (match aggregate_expr_type expr_kind true [cond;eif;eelse] with
				| KExprWithStatement -> Statement
				| _ -> Both expr)
			| TConst _
			| TLocal _
			| TArray _
			| TBinop _
			| TField _
			| TEnumParameter _
			| TTypeExpr _
			| TObjectDecl _
			| TArrayDecl _
			| TFunction _
			| TCast _
			| TUnop _ -> Expression (expr)
			| TParenthesis p | TMeta(_,p) -> shallow_expr_type p
			| TBlock ([e]) -> shallow_expr_type e
			| TCall _
			| TVar _
			| TBlock _
			| TFor _
			| TWhile _
			| TSwitch _
			| TTry _
			| TReturn _
			| TBreak
			| TContinue
			| TIf _
			| TThrow _ -> Statement

	and expr_kind expr =
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
					| TBinop (Ast.OpBoolOr, e1, e2) ->	(* TODO: should OpBool never be side-effects free? *)
						aggregate true [e1;e2]
					| TBinop (Ast.OpAssign, e1, e2)
					| TBinop (Ast.OpAssignOp _, e1, e2) ->
						aggregate false [e1;e2]
					| TBinop (_, e1, e2) ->
						aggregate true [e1;e2]
					| TIf (cond, eif, Some(eelse)) -> (match aggregate true [cond;eif;eelse] with
						| KExprWithStatement -> KStatement
						| k -> k)
					| TArray (e1,e2) ->
						aggregate true [e1;e2]
					| TParenthesis e
					| TMeta(_,e)
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
				| [] -> acc
				| last :: [] ->
					(assign_fun last) :: acc
				| hd :: tl ->
					assign (hd :: acc) tl
		in
		List.rev (assign [] elist)

	let mk_get_block assign_fun e =
		match e.eexpr with
			| TBlock [] -> e
			| TBlock (el) ->
				{ e with eexpr = TBlock(apply_assign_block assign_fun el) }
			| _ ->
				{ e with eexpr = TBlock([ assign_fun e ]) }

	let add_assign gen add_statement expr =
		match expr.eexpr, follow expr.etype with
			| _, TAbstract ({ a_path = ([],"Void") },[])
			| TThrow _, _ ->
				add_statement expr;
				null expr.etype expr.epos
			| _ ->
				let var = mk_temp gen "stmt" expr.etype in
				let tvars = { expr with eexpr = TVar(var,Some(expr)) } in
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
			| TParenthesis p | TMeta(_,p) ->
				apply_assign assign_fun p
			| TVar _ ->
				right
			| _ ->
				match follow right.etype with
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
					let tvars = { right with eexpr = TVar(var, Some( { right with eexpr = TConst(TBool false); etype = gen.gcon.basic.tbool } )); etype = gen.gcon.basic.tvoid } in
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
					let tvars = { right with eexpr = TVar(var, Some( { right with eexpr = TConst(TBool false); etype = gen.gcon.basic.tbool } )); etype = gen.gcon.basic.tvoid } in
					let local = { right with eexpr = TLocal(var) } in
					add_statement tvars;

					({ expr with eexpr = TBinop(op, left, local) }, [ do_not left, { right with eexpr = TBinop(Ast.OpAssign, local, right) } ])
				| _ when acc = [] -> assert false
				| _ ->
					let var = mk_temp gen "boolv" expr.etype in
					let tvars = { expr with eexpr = TVar(var, Some( { expr with etype = gen.gcon.basic.tbool } )); etype = gen.gcon.basic.tvoid } in
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
					let tvars = { left with eexpr = TVar([var, if is_first then Some(left) else Some( { left with eexpr = TConst(TBool false) } )]); etype = gen.gcon.basic.tvoid } in
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
					let tvars = { expr with eexpr = TVar([var, Some ( { expr with eexpr = TConst(TBool false) } ) ]); etype = gen.gcon.basic.tvoid } in
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
			Type.concat e1 { e1 with
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
			Some (apply_assign (fun e -> { e with eexpr = TBinop(op, left, if ExtType.is_void left.etype then e else gen.ghandle_cast left.etype e.etype e) }) right )
		in

		let handle_return e =
			Some( apply_assign (fun e ->
				match e.eexpr with
					| TThrow _ -> e
					| _ when ExtType.is_void e.etype ->
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
			| TVar(v,Some({ eexpr = TBinop(Ast.OpBoolAnd,_,_) } as right))
			| TVar(v,Some({ eexpr = TBinop(Ast.OpBoolOr,_,_) } as right)) ->
				let right = short_circuit_op_unwrap gen add_statement right in
				Some { expr with eexpr = TVar(v, Some(right)) }
			| TVar(v,Some(right)) when shallow_expr_type right = Statement ->
				add_statement ({ expr with eexpr = TVar(v, Some(null right.etype right.epos)) });
				handle_assign Ast.OpAssign { expr with eexpr = TLocal(v); etype = v.v_type } right
			(* TIf handling *)
			| TBinop((Ast.OpAssign as op),left, ({ eexpr = TIf _ } as right))
			| TBinop((Ast.OpAssignOp _ as op),left,({ eexpr = TIf _ } as right)) when is_problematic_if right ->
				handle_assign op left right
			| TVar(v,Some({ eexpr = TIf _ } as right)) when is_problematic_if right ->
				add_statement ({ expr with eexpr = TVar(v, Some(null right.etype right.epos)) });
				handle_assign Ast.OpAssign { expr with eexpr = TLocal(v); etype = v.v_type } right
			| TWhile(cond, e1, flag) when is_problematic_if cond ->
				twhile_with_condition_statement gen add_statement expr cond e1 flag;
				Some (null expr.etype expr.epos)
			| _ -> None

	let configure gen (on_expr_as_statement:texpr->texpr option) =
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
										| Some { eexpr = TConst(TNull) } (* no op *)
										| Some { eexpr = TBlock [] } -> ()
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
		let map e = Some(traverse e) in
		gen.gsyntax_filters#add ~name:name ~priority:(PCustom priority) map
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
		Must run before ExpressionUnwrap

*)

module CastDetect =
struct

	let name = "cast_detect_2"

	let priority = solve_deps name [DBefore TypeParams.priority; DBefore ExpressionUnwrap.priority]

	(* ******************************************* *)
	(* ReturnCast *)
	(* ******************************************* *)

	(*

		Cast detection for return types can't be done at CastDetect time, since we need an
		unwrapped expression to make sure we catch all return cast detections. So this module
		is specifically to deal with that, and is configured automatically by CastDetect

		dependencies:


	*)

	module ReturnCast =
	struct

		let name = "return_cast"

		let priority = solve_deps name [DAfter priority; DAfter ExpressionUnwrap.priority]

		let default_implementation gen =
			let rec extract_expr e = match e.eexpr with
				| TParenthesis e
				| TMeta (_,e)
				| TCast(e,_) -> extract_expr e
				| _ -> e
			in
			let current_ret_type = ref None in
			let handle e tto tfrom = gen.ghandle_cast (gen.greal_type tto) (gen.greal_type tfrom) e in
			let in_value = ref false in

			let rec run e =
				let was_in_value = !in_value in
				in_value := true;
				match e.eexpr with
				| TReturn (eopt) ->
					(* a return must be inside a function *)
					let ret_type = match !current_ret_type with | Some(s) -> s | None -> gen.gcon.error "Invalid return outside function declaration." e.epos; assert false in
					(match eopt with
					| None when not (ExtType.is_void ret_type) ->
						{ e with eexpr = TReturn( Some(null ret_type e.epos)) }
					| None -> e
					| Some eret ->
						{ e with eexpr = TReturn( Some(handle (run eret) ret_type eret.etype ) ) })
				| TFunction(tfunc) ->
					let last_ret = !current_ret_type in
					current_ret_type := Some(tfunc.tf_type);
					let ret = Type.map_expr run e in
					current_ret_type := last_ret;
					ret
				| TBlock el ->
					{ e with eexpr = TBlock ( List.map (fun e -> in_value := false; run e) el ) }
				| TBinop ( (Ast.OpAssign as op),e1,e2)
				| TBinop ( (Ast.OpAssignOp _ as op),e1,e2) when was_in_value ->
					let e1 = extract_expr (run e1) in
					let r = { e with eexpr = TBinop(op, e1, handle (run e2) e1.etype e2.etype); etype = e1.etype } in
					handle r e.etype e1.etype
				| TBinop ( (Ast.OpAssign as op),({ eexpr = TField(tf, f) } as e1), e2 )
				| TBinop ( (Ast.OpAssignOp _ as op),({ eexpr = TField(tf, f) } as e1), e2 ) ->
					(match field_access_esp gen (gen.greal_type tf.etype) (f) with
						| FClassField(cl,params,_,_,is_static,actual_t,_) ->
							let actual_t = if is_static then actual_t else apply_params cl.cl_params params actual_t in
							let e1 = extract_expr (run e1) in
							{ e with eexpr = TBinop(op, e1, handle (run e2) actual_t e2.etype); etype = e1.etype }
						| _ ->
							let e1 = extract_expr (run e1) in
							{ e with eexpr = TBinop(op, e1, handle (run e2) e1.etype e2.etype); etype = e1.etype }
					)
				| TBinop ( (Ast.OpAssign as op),e1,e2)
				| TBinop ( (Ast.OpAssignOp _ as op),e1,e2) ->
					let e1 = extract_expr (run e1) in
					{ e with eexpr = TBinop(op, e1, handle (run e2) e1.etype e2.etype); etype = e1.etype }
				| _ -> Type.map_expr run e
			in
			run

		let configure gen =
			let map e = Some(default_implementation gen e) in
			gen.gsyntax_filters#add ~name:name ~priority:(PCustom priority) map

	end;;

	let get_args t = match follow t with
		| TFun(args,ret) -> args,ret
		| _ -> trace (debug_type t); assert false

	(*
		Since this function is applied under native-context only, the type paraters will already be changed
	*)
	let map_cls gen also_implements fn super =
		let rec loop c tl =
			if c == super then
				fn c tl
			else
				(match c.cl_super with
					| None -> false
					| Some (cs,tls) ->
						let tls = gen.greal_type_param (TClassDecl cs) tls in
						loop cs (List.map (apply_params c.cl_params tl) tls)
				)
				||
				(if also_implements then
					List.exists (fun (cs,tls) -> loop cs (List.map (apply_params c.cl_params tl) tls)) c.cl_implements
				else
					false)
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

	(* will return true if both arguments are compatible. If it's not the case, a runtime error is very likely *)
	let is_cl_related gen cl tl super superl =
		let is_cl_related cl tl super superl = map_cls gen (match cl.cl_kind,super.cl_kind with KTypeParameter _, _ | _,KTypeParameter _ -> false | _ -> true) (fun _ _ -> true) super cl tl in
		is_cl_related cl tl super superl || is_cl_related super superl cl tl

	let is_exactly_basic gen t1 t2 =
		match gen.gfollow#run_f t1, gen.gfollow#run_f t2 with
			| TAbstract(a1, []), TAbstract(a2, []) ->
				a1 == a2 && Common.defined gen.gcon Define.FastCast
			| TInst(c1, []), TInst(c2, []) ->
				c1 == c2 && Common.defined gen.gcon Define.FastCast
			| TEnum(e1, []), TEnum(e2, []) ->
				e1 == e2 && Common.defined gen.gcon Define.FastCast
			| _ ->
				false

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

	let unifies tfrom tto = try
		unify tfrom tto;
		true
	with | _ ->
		false

	let do_unsafe_cast gen from_t to_t e	=
		let t_path t =
			match t with
				| TInst(cl, _) -> cl.cl_path
				| TEnum(e, _) -> e.e_path
				| TType(t, _) -> t.t_path
				| TAbstract(a, _) -> a.a_path
				| TDynamic _ -> ([], "Dynamic")
				| _ -> raise Not_found
		in
		match gen.gfollow#run_f from_t, gen.gfollow#run_f to_t with
		| TInst({ cl_kind = KTypeParameter tl },_), t2 when List.exists (fun t -> unifies t t2) tl ->
			mk_cast to_t (mk_cast t_dynamic e)
		| _ ->
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

		let mk_cast fast t e =
			match e.eexpr with
				(* TThrow is always typed as Dynamic, we just need to type it accordingly *)
				| TThrow _ -> { e with etype = t }
				| _ -> if fast then mk_castfast t e else mk_cast t e
		in

		let e = { e with etype = real_from_t } in
		if try fast_eq real_to_t real_from_t with Invalid_argument("List.for_all2") -> false then e else
		match real_to_t, real_from_t with
			(* string is the only type that can be implicitly converted from any other *)
			| TInst( { cl_path = ([], "String") }, []), TInst( { cl_path = ([], "String") }, [] ) ->
				mk_cast true to_t e
			| TInst( { cl_path = ([], "String") }, []), _ ->
				mk_cast false to_t e
			| TInst(cl_to, params_to), TInst(cl_from, params_from) ->
				let ret = ref None in
				(*
					this is a little confusing:
					we are here mapping classes until we have the same to and from classes, applying the type parameters in each step, so we can
					compare the type parameters;

					If a class is found - meaning that the cl_from can be converted without a cast into cl_to,
					we still need to check their type parameters.
				*)
				ignore (map_cls gen (match cl_from.cl_kind,cl_to.cl_kind with KTypeParameter _, _ | _,KTypeParameter _ -> false | _ -> true) (fun _ tl ->
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
							ret := Some (mk_cast true to_t e);
							true
						end else
							(*
								if not, we're going to check if we only need a simple cast,
								or if we need to first cast into the dynamic version of it
							*)
							try
								List.iter2 (type_eq gen EqRightDynamic) tl params_to;
								ret := Some (mk_cast true to_t e);
								true
							with | Unify_error _ ->
								ret := Some (mk_cast true to_t (mk_cast true (TInst(cl_to, List.map (fun _ -> t_dynamic) params_to)) e));
								true
				) cl_to cl_from params_from);
				if is_some !ret then
					get !ret
				else if is_cl_related gen cl_from params_from cl_to params_to then
					mk_cast true to_t e
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
				mk_cast false to_t e
			| TMono _, _
			| TDynamic _, _ -> e
			| _, TMono _
			| _, TDynamic _ -> mk_cast false to_t e
			| TAnon (a_to), TAnon (a_from) ->
				if a_to == a_from then
					e
				else if type_iseq gen to_t from_t then (* FIXME apply unify correctly *)
					e
				else
					mk_cast true to_t e
			| _, TAnon(anon) -> (try
				let p2 = match !(anon.a_status) with
				| Statics c -> TInst(c,List.map (fun _ -> t_dynamic) c.cl_params)
				| EnumStatics e -> TEnum(e, List.map (fun _ -> t_dynamic) e.e_params)
				| AbstractStatics a -> TAbstract(a, List.map (fun _ -> t_dynamic) a.a_params)
				| _ -> raise Not_found
				in
				let tclass = match get_type gen ([],"Class") with
				| TAbstractDecl(a) -> a
				| _ -> assert false in
				handle_cast gen e real_to_t (gen.greal_type (TAbstract(tclass, [p2])))
			with | Not_found ->
				mk_cast false to_t e)
			| TAbstract (a_to, _), TAbstract(a_from, _) when a_to == a_from ->
				e
			| TAbstract _, TInst({ cl_kind = KTypeParameter _ }, _)
			| TInst({ cl_kind = KTypeParameter _ }, _), TAbstract _ ->
				do_unsafe_cast()
			| TAbstract _, _
			| _, TAbstract _ ->
				(try
					unify from_t to_t;
					mk_cast true to_t e
				with | Unify_error _ ->
					try
						unify to_t from_t;
						mk_cast true to_t e
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
				if en.e_path = cl.cl_path && Meta.has Meta.Class en.e_meta then begin
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
					mk_cast false to_t e
				else
					e
			| TType _, _ when gen.gspecial_needs_cast real_to_t real_from_t ->
				mk_cast false to_t e
			| _, TType _ when gen.gspecial_needs_cast real_to_t real_from_t ->
				mk_cast false to_t e
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
					mk_cast false to_t e
			| TAnon anon, _ ->
				if PMap.is_empty anon.a_fields then
					e
				else
					mk_cast true to_t e
			| TFun(args, ret), TFun(args2, ret2) ->
				let get_args = List.map (fun (_,_,t) -> t) in
				(try List.iter2 (type_eq gen (EqBothDynamic)) (ret :: get_args args) (ret2 :: get_args args2); e with | Unify_error _ | Invalid_argument("List.iter2") -> mk_cast true to_t e)
			| _, _ ->
				do_unsafe_cast ()

	(* end of cast handler *)
	(* ******************* *)

	let is_static_overload c name =
		match c.cl_super with
		| None -> false
		| Some (sup,_) ->
			let rec loop c =
				(PMap.mem name c.cl_statics) || (match c.cl_super with
					| None -> false
					| Some (sup,_) -> loop sup)
			in
			loop sup

	(* this is a workaround for issue #1743, as FInstance() is returning the incorrect classfield *)
	let rec clean_t t = match follow t with
		| TAbstract(a,tl) when not (Meta.has Meta.CoreType a.a_meta) ->
			clean_t (Abstract.get_underlying_type a tl)
		| t -> t

	let select_overload gen applied_f overloads types params =
		let rec check_arg arglist elist =
			match arglist, elist with
				| [], [] -> true (* it is valid *)
				| (_,_,TAbstract({ a_path = (["haxe";"extern"],"Rest") }, [t])) :: [], elist ->
					List.for_all (fun (_,_,et) -> Type.type_iseq (clean_t et) (clean_t t)) elist
				| (_,_,t) :: arglist, (_,_,et) :: elist when Type.type_iseq (clean_t et) (clean_t t) ->
					check_arg arglist elist
				| _ -> false
		in
		match follow applied_f with
		| TFun _ ->
			replace_mono applied_f;
			let args, _ = get_fun applied_f in
			let elist = List.rev args in
			let rec check_overload overloads =
				match overloads with
				| (t, cf) :: overloads ->
						let cft = apply_params types params t in
						let cft = monomorphs cf.cf_params cft in
						let args, _ = get_fun cft in
						if check_arg (List.rev args) elist then
							cf,t,false
						else if overloads = [] then
							cf,t,true (* no compatible overload was found *)
						else
							check_overload overloads
				| [] -> assert false
			in
			check_overload overloads
		| _ -> match overloads with  (* issue #1742 *)
		| (t,cf) :: [] -> cf,t,true
		| (t,cf) :: _ -> cf,t,false
		| _ -> assert false

	let choose_ctor gen cl tparams etl maybe_empty_t p =
		let ctor, sup, stl = OverloadingConstructor.cur_ctor cl tparams in
		(* get returned stl, with Dynamic as t_empty *)
		let rec get_changed_stl c tl =
			if c == sup then
				tl
			else match c.cl_super with
			| None -> stl
			| Some(sup,stl) -> get_changed_stl sup (List.map (apply_params c.cl_params tl) stl)
		in
		let ret_tparams = List.map (fun t -> match follow t with
		| TDynamic _ | TMono _ -> t_empty
		| _ -> t) tparams in
		let ret_stl = get_changed_stl cl ret_tparams in
		let ctors = ctor :: ctor.cf_overloads in
		List.iter replace_mono etl;
		(* first filter out or select outright maybe_empty *)
		let ctors, is_overload = match etl, maybe_empty_t with
		| [t], Some empty_t ->
			let count = ref 0 in
			let is_empty_call = Type.type_iseq t empty_t in
			let ret = List.filter (fun cf -> match follow cf.cf_type with
			(* | TFun([_,_,t],_) -> incr count; true *)
			| TFun([_,_,t],_) ->
				replace_mono t; incr count; is_empty_call = (Type.type_iseq t empty_t)
			| _ -> false) ctors in
			ret, !count > 1
		| _ ->
			let len = List.length etl in
			let ret = List.filter (fun cf -> List.length (fst (get_fun cf.cf_type)) = len) ctors in
			ret, (match ret with | _ :: [] -> false | _ -> true)
		in
		let rec check_arg arglist elist =
			match arglist, elist with
			| [], [] -> true
			| (_,_,t) :: arglist, et :: elist -> (try
				let t = run_follow gen t in
				unify et t;
				check_arg arglist elist
			with | Unify_error el ->
				(* List.iter (fun el -> gen.gcon.warning (Typecore.unify_error_msg (print_context()) el) p) el; *)
				false)
			| _ ->
				false
		in
		let rec check_cf cf =
			let t = apply_params sup.cl_params stl cf.cf_type in
			replace_mono t;
			let args, _ = get_fun t in
			check_arg args etl
		in
		match is_overload, ctors with
			| false, [c] ->
				false, c, sup, ret_stl
			| _ ->
				is_overload, List.find check_cf ctors, sup, ret_stl

	let change_rest tfun elist =
		let rec loop acc arglist elist = match arglist, elist with
			| (_,_,TAbstract({ a_path = (["haxe";"extern"],"Rest") },[t])) :: [], elist ->
				List.rev (List.map (fun _ -> "rest",false,t) elist @ acc)
			| (n,o,t) :: arglist, _ :: elist ->
				loop ((n,o,t) :: acc) arglist elist
			| _, _ ->
				List.rev acc
		in
		let args,ret = get_fun tfun in
		TFun(loop [] args elist, ret)

	let fastcast_if_needed gen expr real_to_t real_from_t =
		if Common.defined gen.gcon Define.FastCast then begin
			if type_iseq gen real_to_t real_from_t then
				{ expr with etype = real_to_t }
			else
				mk_castfast real_to_t { expr with etype=real_from_t }
		end else
			handle_cast gen expr real_to_t real_from_t

	(*
		Type parameter handling
		It will detect if/what type parameters were used, and call the cast handler
		It will handle both TCall(TField) and TCall by receiving a texpr option field: e
		Also it will transform the type parameters with greal_type_param and make

		handle_impossible_tparam - should cases where the type parameter is impossible to be determined from the called parameters be Dynamic?
		e.g. static function test<T>():T {}
	*)

	(* match e.eexpr with | TCall( ({ eexpr = TField(ef, f) }) as e1, elist ) -> *)
	let handle_type_parameter gen e e1 ef ~clean_ef ~overloads_cast_to_base f elist calls_parameters_explicitly =
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
		(* this part was rewritten at roughly r6477 in order to correctly support overloads *)
		(match field_access_esp gen real_type (f) with
		| FClassField (cl, params, _, cf, is_static, actual_t, declared_t) when e <> None && (cf.cf_kind = Method MethNormal || cf.cf_kind = Method MethInline) ->
				(* C# target changes params with a real_type function *)
				let params = match follow clean_ef.etype with
					| TInst(_,params) -> params
					| _ -> params
				in
				let local_mk_cast t expr =
					(* handle_cast gen expr t expr.etype *)
					if is_exactly_basic gen t expr.etype then
						expr
					else
						mk_castfast t expr
				in

				let ecall = get e in
				let ef = ref ef in
				let is_overload = cf.cf_overloads <> [] || Meta.has Meta.Overload cf.cf_meta || (is_static && is_static_overload cl (field_name f)) in
				let cf, actual_t, error = match is_overload with
					| false ->
							(* since actual_t from FClassField already applies greal_type, we're using the get_overloads helper to get this info *)
							let t = if cf.cf_params = [] then (* this if statement must be eliminated - it's a workaround for #3516 + infer params. *)
								actual_t
							else
								declared_t
							in
							cf,t,false
					| true ->
					let (cf, actual_t, error), is_static = match f with
						| FInstance(c,_,cf) | FClosure(Some (c,_),cf) ->
							(* get from overloads *)
							(* FIXME: this is a workaround for issue #1743 . Uncomment this code after it was solved *)
							(* let t, cf = List.find (fun (t,cf2) -> cf == cf2) (Overloads.get_overloads cl (field_name f)) in *)
							(* cf, t, false *)
							select_overload gen e1.etype (Overloads.get_overloads cl (field_name f)) cl.cl_params params, false
						| FStatic(c,f) ->
							(* workaround for issue #1743 *)
							(* f,f.cf_type, false *)
							select_overload gen e1.etype ((f.cf_type,f) :: List.map (fun f -> f.cf_type,f) f.cf_overloads) [] [], true
						| _ ->
							gen.gcon.warning "Overloaded classfield typed as anonymous" ecall.epos;
							(cf, actual_t, true), true
					in

					if not (is_static || error) then match find_first_declared_field gen cl ~exact_field:{ cf with cf_type = actual_t } cf.cf_name with
					| Some(cf_orig,actual_t,_,_,declared_cl,tl,tlch) ->
						let rec is_super e = match e.eexpr with
							| TConst TSuper -> true
							| TParenthesis p | TMeta(_,p) -> is_super p
							| _ -> false
						in
						if declared_cl != cl && overloads_cast_to_base && not (is_super !ef) then begin
							let pos = (!ef).epos in
							ef := {
								eexpr = TCall(
									{ eexpr = TLocal(alloc_var "__as__" t_dynamic); etype = t_dynamic; epos = pos },
									[!ef]);
								etype = TInst(declared_cl,List.map (apply_params cl.cl_params params) tl);
								epos = pos
							}
						end;
						{ cf_orig with cf_name = cf.cf_name },actual_t,false
					| None ->
						gen.gcon.warning "Cannot find matching overload" ecall.epos;
						cf, actual_t, true
					else
						cf,actual_t,error
				in

				(* take off Rest param *)
				let actual_t = change_rest actual_t elist in
				(* set the real (selected) class field *)
				let f = match f with
					| FInstance(c,tl,_) -> FInstance(c,tl,cf)
					| FClosure(c,_) -> FClosure(c,cf)
					| FStatic(c,_) -> FStatic(c,cf)
					| f -> f
				in
				let error = error || (match follow actual_t with | TFun _ -> false | _ -> true) in
				if error then (* if error, ignore arguments *)
					if ExtType.is_void ecall.etype then
						{ ecall with eexpr = TCall({ e1 with eexpr = TField(!ef, f) }, elist ) }
					else
						local_mk_cast ecall.etype { ecall with eexpr = TCall({ e1 with eexpr = TField(!ef, f) }, elist ) }
				else begin
					(* infer arguments *)
					(* let called_t = TFun(List.map (fun e -> "arg",false,e.etype) elist, ecall.etype) in *)
					let called_t = match follow e1.etype with | TFun _ -> e1.etype | _ -> TFun(List.map (fun e -> "arg",false,e.etype) elist, ecall.etype)	in (* workaround for issue #1742 *)
					let called_t = change_rest called_t elist in
					let fparams = TypeParams.infer_params gen ecall.epos (get_fun (apply_params cl.cl_params params actual_t)) (get_fun called_t) cf.cf_params calls_parameters_explicitly in
					(* get what the backend actually sees *)
					(* actual field's function *)
					let actual_t = get_real_fun gen actual_t in
					let real_params = gen.greal_type_param (TClassDecl cl) params in
					let function_t = apply_params cl.cl_params real_params actual_t in
					let real_fparams = if calls_parameters_explicitly then
						gen.greal_type_param (TClassDecl cl) fparams
					else
						gen.greal_type_param (TClassDecl cl) (TypeParams.infer_params gen ecall.epos (get_fun function_t) (get_fun (get_real_fun gen called_t)) cf.cf_params calls_parameters_explicitly) in
					let function_t = get_real_fun gen (apply_params cf.cf_params real_fparams function_t) in
					let args_ft, ret_ft = get_fun function_t in
					(* applied function *)
					let applied = elist in
					(* check types list *)
					let new_ecall, elist = try
						let elist = List.map2 (fun applied (_,_,funct) ->
							match is_overload || real_fparams <> [], applied.eexpr with
							| true, TConst TNull ->
								mk_castfast (gen.greal_type funct) applied
							| true, _ -> (* when not (type_iseq gen (gen.greal_type applied.etype) funct) -> *)
								let ret = handle_cast gen applied (funct) (gen.greal_type applied.etype) in
								(match ret.eexpr with
								| TCast _ -> ret
								| _ -> local_mk_cast (funct) ret)
							| _ ->
								handle_cast gen applied (funct) (gen.greal_type applied.etype)
						) applied args_ft in
						{ ecall with
							eexpr = TCall(
								{ e1 with eexpr = TField(!ef, f) },
								elist);
						}, elist
					with | Invalid_argument("List.map2") ->
						gen.gcon.warning ("This expression may be invalid" ) ecall.epos;
						{ ecall with eexpr = TCall({ e1 with eexpr = TField(!ef, f) }, elist) }, elist
					in
					let new_ecall = if fparams <> [] then gen.gparam_func_call new_ecall { e1 with eexpr = TField(!ef, f) } fparams elist else new_ecall in
					let ret = handle_cast gen new_ecall (gen.greal_type ecall.etype) (gen.greal_type ret_ft) in
					(match gen.gcon.platform, cf.cf_params, ret.eexpr with
						| _, _, TCast _ -> ret
						| Java, _ :: _, _ ->
							(* this is a workaround for a javac openjdk issue with unused type parameters and return type inference *)
							(* see more at issue #3123 *)
							mk_cast (gen.greal_type ret_ft) new_ecall
						| _ -> ret)
				end
		| FClassField (cl,params,_,{ cf_kind = (Method MethDynamic | Var _) },_,actual_t,_) ->
			(* if it's a var, we will just try to apply the class parameters that have been changed with greal_type_param *)
			let t = apply_params cl.cl_params (gen.greal_type_param (TClassDecl cl) params) (gen.greal_type actual_t) in
			return_var (handle_cast gen { e1 with eexpr = TField(ef, f) } (gen.greal_type e1.etype) (gen.greal_type t))
		| FClassField (cl,params,_,cf,_,actual_t,_) ->
			return_var (handle_cast gen { e1 with eexpr = TField({ ef with etype = t_dynamic }, f) } e1.etype t_dynamic) (* force dynamic and cast back to needed type *)
		| FEnumField (en, efield, true) ->
			let ecall = match e with | None -> trace (field_name f); trace efield.ef_name; gen.gcon.error "This field should be called immediately" ef.epos; assert false | Some ecall -> ecall in
			(match en.e_params with
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
					let t = apply_params en.e_params (gen.greal_type_param (TEnumDecl en) cf_params) actual_t in
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

	(** overloads_cast_to_base argument will cast overloaded function types to the class that declared it. **)
	(**			This is necessary for C#, and if true, will require the target to implement __as__, as a `quicker` form of casting **)
	let configure gen ?(overloads_cast_to_base = false) maybe_empty_t calls_parameters_explicitly =
		let handle e t1 t2 = handle_cast gen e (gen.greal_type t1) (gen.greal_type t2) in

		let in_value = ref false in

		let rec clean_cast e = match e.eexpr with
		 | TCast(e,_) -> clean_cast e
		 | TParenthesis(e) | TMeta(_,e) -> clean_cast e
		 | _ -> e
		in

		let get_abstract_impl t = match t with
			| TAbstract(a,pl) when not (Meta.has Meta.CoreType a.a_meta) ->
				Abstract.get_underlying_type a pl
			| t -> t
		in

		let rec is_abstract_to_struct t = match t with
			| TAbstract(a,pl) when not (Meta.has Meta.CoreType a.a_meta) ->
				is_abstract_to_struct (Abstract.get_underlying_type a pl)
			| TInst(c,_) when Meta.has Meta.Struct c.cl_meta ->
				true
			| _ -> false
		in

		let binop_type op main_expr e1 e2 =
			let name = Common.platform_name gen.gcon.platform in
			let basic = gen.gcon.basic in
			(* If either operand is of type decimal, the other operand is converted to type decimal, or a compile-time error occurs if the other operand is of type float or double.
			 * Otherwise, if either operand is of type double, the other operand is converted to type double.
			 * Otherwise, if either operand is of type float, the other operand is converted to type float.
			 * Otherwise, if either operand is of type ulong, the other operand is converted to type ulong, or a compile-time error occurs if the other operand is of type sbyte, short, int, or long.
			 * Otherwise, if either operand is of type long, the other operand is converted to type long.
			 * Otherwise, if either operand is of type uint and the other operand is of type sbyte, short, or int, both operands are converted to type long.
			 * Otherwise, if either operand is of type uint, the other operand is converted to type uint.
			 * Otherwise, both operands are converted to type int.
			 *  *)
			let t1, t2 = follow (run_follow gen e1.etype), follow (run_follow gen e2.etype) in
			match t1, t2 with
				| TAbstract(a1,[]), TAbstract(a2,[]) when a1 == a2 ->
					{ main_expr with eexpr = TBinop(op, e1, e2); etype = e1.etype }
				| TInst(i1,[]), TInst(i2,[]) when i1 == i2 ->
					{ main_expr with eexpr = TBinop(op, e1, e2); etype = e1.etype }
				| TInst({ cl_path = ([],"String") },[]), _ when op = OpAdd ->
					{ main_expr with eexpr = TBinop(op, e1, mk_cast basic.tstring e2); etype = basic.tstring }
				| _, TInst({ cl_path = ([],"String") },[]) when op = OpAdd ->
					{ main_expr with eexpr = TBinop(op, mk_cast basic.tstring e1, e2); etype = basic.tstring }
				| TAbstract({ a_path = ([], "Float") }, []), _ ->
					{ main_expr with eexpr = TBinop(op, e1, e2); etype = e1.etype }
				| _, TAbstract({ a_path = ([], "Float") }, []) ->
					{ main_expr with eexpr = TBinop(op, e1, e2); etype = e2.etype }
				| TAbstract({ a_path = ([], "Single") }, []), _ ->
					{ main_expr with eexpr = TBinop(op, e1, e2); etype = e1.etype }
				| _, TAbstract({ a_path = ([], "Single") }, []) ->
					{ main_expr with eexpr = TBinop(op, e1, e2); etype = e2.etype }
				| TAbstract({ a_path = ([pf], "UInt64") }, []), _ when pf = name ->
					{ main_expr with eexpr = TBinop(op, e1, e2); etype = e1.etype }
				| _, TAbstract({ a_path = ([pf], "UInt64") }, []) when pf = name ->
					{ main_expr with eexpr = TBinop(op, e1, e2); etype = e2.etype }
				| TAbstract({ a_path = ([pf], "Int64") }, []), _ when pf = name ->
					{ main_expr with eexpr = TBinop(op, e1, e2); etype = e1.etype }
				| _, TAbstract({ a_path = ([pf], "Int64") }, []) when pf = name ->
					{ main_expr with eexpr = TBinop(op, e1, e2); etype = e2.etype }
				| TAbstract({ a_path = ([], "UInt") }, []), tother when like_int tother ->
					let ti64 = mt_to_t_dyn ( get_type gen ([name], "Int64") ) in
					let ret = { main_expr with eexpr = TBinop(op, e1, e2); etype = ti64 } in
					if op <> OpDiv then
						mk_cast t1 ret
					else
						ret
				| tother, TAbstract({ a_path = ([], "UInt") }, []) when like_int tother ->
					let ti64 = mt_to_t_dyn ( get_type gen ([name], "Int64") ) in
					let ret = { main_expr with eexpr = TBinop(op, e1, e2); etype = ti64 } in
					if op <> OpDiv then
						mk_cast t2 ret
					else
						ret
				| TAbstract({ a_path = ([], "UInt") }, []), _ ->
					{ main_expr with eexpr = TBinop(op, e1, e2); etype = e1.etype }
				| _, TAbstract({ a_path = ([], "UInt") }, []) ->
					{ main_expr with eexpr = TBinop(op, e1, e2); etype = e2.etype }
				| TAbstract(a1,[]), TAbstract(a2,[]) ->
					{ main_expr with eexpr = TBinop(op, e1, e2); etype = basic.tint }
				| _ ->
					{ main_expr with eexpr = TBinop(op, e1, e2) }
		in
		let binop_type = if Common.defined gen.gcon Define.FastCast then
			binop_type
		else
			fun op main_expr e1 e2 -> { main_expr with eexpr = TBinop(op, e1, e2) }
		in

		let rec run ?(just_type = false) e =
			let handle = if not just_type then handle else fun e t1 t2 -> { e with etype = gen.greal_type t2 } in
			let was_in_value = !in_value in
			in_value := true;
			match e.eexpr with
				| TConst ( TInt _ | TFloat _ | TBool _ as const ) ->
					(* take off any Null<> that it may have *)
					let t = follow (run_follow gen e.etype) in
					(* do not allow constants typed as Single - need to cast them *)
					let real_t = match const with
						| TInt _ -> gen.gcon.basic.tint
						| TFloat _ -> gen.gcon.basic.tfloat
						| TBool _ -> gen.gcon.basic.tbool
						| _ -> assert false
					in
					handle e t real_t
				| TCast( { eexpr = TConst TNull }, _ ) ->
					{ e with eexpr = TConst TNull }
				| TCast( { eexpr = TCall( { eexpr = TLocal { v_name = "__delegate__" } } as local, [del] ) } as e2, _) ->
					{ e with eexpr = TCast({ e2 with eexpr = TCall(local, [Type.map_expr run del]) }, None) }

				| TBinop ( (Ast.OpAssign | Ast.OpAssignOp _ as op), e1, e2 ) ->
					let e1 = run ~just_type:true e1 in
					let e2 = handle (run e2) e1.etype e2.etype in
					{ e with eexpr = TBinop(op, clean_cast e1, e2) }
				| TBinop ( (Ast.OpShl | Ast.OpShr | Ast.OpUShr as op), e1, e2 ) ->
					let e1 = run e1 in
					let e2 = handle (run e2) (gen.gcon.basic.tint) e2.etype in
					let rett = binop_type op e e1 e2 in
					{ e with eexpr = TBinop(op, e1, e2); etype = rett.etype }
				| TBinop( (OpAdd | OpMult | OpDiv | OpSub | OpAnd | OpOr | OpXor | OpMod) as op, e1, e2 ) ->
					binop_type op e (run e1) (run e2)
				| TBinop( (OpEq | OpNotEq | OpGt | OpGte | OpLt | OpLte | OpBoolAnd | OpBoolOr) as op, e1, e2 ) ->
					handle { e with eexpr = TBinop(op, run e1, run e2) } e.etype gen.gcon.basic.tbool
				| TField(ef, f) ->
					handle_type_parameter gen None e (run ef) ~clean_ef:ef ~overloads_cast_to_base:overloads_cast_to_base f [] calls_parameters_explicitly
				| TArrayDecl el ->
					let et = e.etype in
					let base_type = match follow et with
						| TInst({ cl_path = ([], "Array") } as cl, bt) -> gen.greal_type_param (TClassDecl cl) bt
						| _ ->
							gen.gcon.warning (debug_type et) e.epos;
							(match gen.gcurrent_class with
								| Some cl -> print_endline (s_type_path cl.cl_path)
								| _ -> ());
							assert false
					in
					let base_type = List.hd base_type in
					{ e with eexpr = TArrayDecl( List.map (fun e -> handle (run e) base_type e.etype) el ); etype = et }
				| TCall ({ eexpr = TLocal { v_name = "__array__" } } as arr_local, el) ->
					let et = e.etype in
					let base_type = match follow et with
						| TInst(cl, bt) -> gen.greal_type_param (TClassDecl cl) bt
						| _ -> assert false
					in
					let base_type = List.hd base_type in
					{ e with eexpr = TCall(arr_local, List.map (fun e -> handle (run e) base_type e.etype) el ); etype = et }
				| TCall( ({ eexpr = TLocal v } as local), params ) when String.get v.v_name 0 = '_' && String.get v.v_name 1 = '_' && Hashtbl.mem gen.gspecial_vars v.v_name ->
					{ e with eexpr = TCall(local, List.map (fun e -> (match e.eexpr with TBlock _ -> in_value := false | _ -> ()); run e) params) }
				| TCall( ({ eexpr = TField(ef, f) }) as e1, elist ) ->
					handle_type_parameter gen (Some e) (e1) (run ef) ~clean_ef:ef ~overloads_cast_to_base:overloads_cast_to_base f (List.map run elist) calls_parameters_explicitly

				(* the TNew and TSuper code was modified at r6497 *)
				| TCall( { eexpr = TConst TSuper } as ef, eparams ) ->
					let cl, tparams = match follow ef.etype with
					| TInst(cl,p) ->
						cl,p
					| _ -> assert false in
					(try
						let is_overload, cf, sup, stl = choose_ctor gen cl tparams (List.map (fun e -> e.etype) eparams) maybe_empty_t e.epos in
						let handle e t1 t2 =
							if is_overload then
								let ret = handle e t1 t2 in
								match ret.eexpr with
								| TCast _ -> ret
								| _ -> mk_cast (gen.greal_type t1) e
							else
								handle e t1 t2
						in
						let stl = gen.greal_type_param (TClassDecl sup) stl in
						let args, _ = get_fun (apply_params sup.cl_params stl cf.cf_type) in
						let eparams = List.map2 (fun e (_,_,t) ->
							handle (run e) t e.etype
						) eparams args in
						{ e with eexpr = TCall(ef, eparams) }
					with | Not_found ->
						gen.gcon.warning "No overload found for this constructor call" e.epos;
						{ e with eexpr = TCall(ef, List.map run eparams) })
				| TCall (ef, eparams) ->
					(match ef.etype with
						| TFun(p, ret) ->
							handle ({ e with eexpr = TCall(run ef, List.map2 (fun param (_,_,t) -> handle (run param) t param.etype) eparams p) }) e.etype ret
						| _ -> Type.map_expr run e
					)
				(* the TNew and TSuper code was modified at r6497 *)
				| TNew ({ cl_kind = KTypeParameter _ }, _, _) ->
					Type.map_expr run e
				| TNew (cl, tparams, eparams) -> (try
					let is_overload, cf, sup, stl = choose_ctor gen cl tparams (List.map (fun e -> e.etype) eparams) maybe_empty_t e.epos in
					let handle e t1 t2 =
						if is_overload then
							let ret = handle e t1 t2 in
							match ret.eexpr with
							| TCast _ -> ret
							| _ -> mk_cast (gen.greal_type t1) e
						else
							handle e t1 t2
					in
					let stl = gen.greal_type_param (TClassDecl sup) stl in
					let args, _ = get_fun (apply_params sup.cl_params stl cf.cf_type) in
					let eparams = List.map2 (fun e (_,_,t) ->
						handle (run e) t e.etype
					) eparams args in
					{ e with eexpr = TNew(cl, tparams, eparams) }
				with | Not_found ->
					gen.gcon.warning "No overload found for this constructor call" e.epos;
					{ e with eexpr = TNew(cl, tparams, List.map run eparams) })
				| TArray(arr, idx) ->
					let arr_etype = match follow arr.etype with
						| (TInst _ as t) -> t
						| TAbstract (a, pl) when not (Meta.has Meta.CoreType a.a_meta) ->
							follow (Abstract.get_underlying_type a pl)
						| t -> t
					in
					let idx = run idx in
					let idx = match gen.greal_type idx.etype with
						| TAbstract({ a_path = [],"Int" },_) -> idx
						| _ -> match handle idx gen.gcon.basic.tint (gen.greal_type idx.etype) with
							| ({ eexpr = TCast _ } as idx) -> idx
							| idx -> mk_cast gen.gcon.basic.tint idx
					in
					let e = { e with eexpr = TArray(run arr, idx) } in
					(* get underlying class (if it's a class *)
					(match arr_etype with
						| TInst(cl, params) ->
							(* see if it implements ArrayAccess *)
							(match cl.cl_array_access with
								| None -> e
								| Some t ->
									(* if it does, apply current parameters (and change them) *)
									(* let real_t = apply_params_internal (List.map (gen.greal_type_param (TClassDecl cl))) cl params t in *)
									let param = apply_params cl.cl_params (gen.greal_type_param (TClassDecl cl) params) t in
									let real_t = apply_params cl.cl_params params param in
									(* see if it needs a cast *)

									fastcast_if_needed gen e (gen.greal_type e.etype) (gen.greal_type real_t)
									(* handle (e) (gen.greal_type e.etype) (gen.greal_type real_t) *)
							)
						| _ -> Type.map_expr run e)
				| TVar (v, eopt) ->
					{ e with eexpr = TVar (v, match eopt with
							| None -> eopt
							| Some e -> Some( handle (run e) v.v_type e.etype ))
					}
				(* FIXME deal with in_value when using other statements that may not have a TBlock wrapped on them *)
				| TIf (econd, ethen, Some(eelse)) when was_in_value ->
					{ e with eexpr = TIf (handle (run econd) gen.gcon.basic.tbool econd.etype, handle (run ethen) e.etype ethen.etype, Some( handle (run eelse) e.etype eelse.etype ) ) }
				| TIf (econd, ethen, eelse) ->
					{ e with eexpr = TIf (handle (run econd) gen.gcon.basic.tbool econd.etype, (in_value := false; run (mk_block ethen)), Option.map (fun e -> in_value := false; run (mk_block e)) eelse) }
				| TWhile (econd, e1, flag) ->
					{ e with eexpr = TWhile (handle (run econd) gen.gcon.basic.tbool econd.etype, (in_value := false; run (mk_block e1)), flag) }
				| TSwitch (cond, el_e_l, edef) ->
					{ e with eexpr = TSwitch(run cond, List.map (fun (el,e) -> (List.map run el, (in_value := false; run (mk_block e)))) el_e_l, Option.map (fun e -> in_value := false; run (mk_block e)) edef) }
				| TFor (v,cond,e1) ->
					{ e with eexpr = TFor(v, run cond, (in_value := false; run (mk_block e1))) }
				| TTry (e, ve_l) ->
					{ e with eexpr = TTry((in_value := false; run (mk_block e)), List.map (fun (v,e) -> in_value := false; (v, run (mk_block e))) ve_l) }
				| TBlock el ->
					let i = ref 0 in
					let len = List.length el in
					{ e with eexpr = TBlock ( List.map (fun e ->
						incr i;
						if !i <> len || not was_in_value then
							in_value := false;
						run e
					) el ) }
				| TCast (expr, md) when ExtType.is_void (follow e.etype) ->
					run expr
				| TCast (expr, md) ->
					let rec get_null e =
						match e.eexpr with
						| TConst TNull -> Some e
						| TParenthesis e | TMeta(_,e) -> get_null e
						| _ -> None
					in

					(match get_null expr with
					| Some enull ->
							if gen.gcon.platform = Cs then
								{ enull with etype = gen.greal_type e.etype }
							else
								mk_cast (gen.greal_type e.etype) enull
					| _ when is_abstract_to_struct expr.etype && type_iseq gen e.etype (get_abstract_impl expr.etype) ->
						run { expr with etype = expr.etype }
					| _ when is_exactly_basic gen expr.etype e.etype ->
						run { expr with etype = expr.etype }
					| _ ->
						match gen.greal_type e.etype, gen.greal_type expr.etype with
							| (TInst(c,tl) as tinst1), TAbstract({ a_path = ["cs"],"Pointer" }, [tinst2]) when type_iseq gen tinst1 (gen.greal_type tinst2) ->
								run expr
							| _ ->
								let last_unsafe = gen.gon_unsafe_cast in
								gen.gon_unsafe_cast <- (fun t t2 pos -> ());
								let ret = handle (run expr) e.etype expr.etype in
								gen.gon_unsafe_cast <- last_unsafe;
								match ret.eexpr with
									| TCast _ -> { ret with etype = gen.greal_type e.etype }
									| _ -> { e with eexpr = TCast(ret,md); etype = gen.greal_type e.etype }
					)
				(*| TCast _ ->
					(* if there is already a cast, we should skip this cast check *)
					Type.map_expr run e*)
				| TFunction f ->
					in_value := false;
					Type.map_expr run e

				| _ -> Type.map_expr run e
		in
		gen.ghandle_cast <- (fun tto tfrom expr -> handle_cast gen expr (gen.greal_type tto) (gen.greal_type tfrom));
		let map e = match gen.gcurrent_classfield with
			| Some(cf) when Meta.has (Meta.Custom ":skipCastDetect") cf.cf_meta ->
				None
			| _ ->
				Some(run e)
		in
		gen.gsyntax_filters#add ~name:name ~priority:(PCustom priority) map;
		ReturnCast.configure gen

end;;

(* ******************************************* *)
(* Reflection-enabling Class fields *)
(* ******************************************* *)

(*
	This is the most hardcore codegen part of the code. There's much to improve so this code can be more readable, but at least it's running correctly right now! This will be improved. (TODO)

	This module will create class fields that enable reflection for targets that have a slow or inexistent reflection abilities. Because of the similarity
	of strategies between what should have been different modules, they are all unified in this reflection-enabling class fields.

	They include:
		* Get(throwErrors, isCheck) / Set fields . Remember to allow implements Dynamic also.
		* Invoke fields() -> You need to configure how many invoke_field fields there will be. + invokeDynamic
		* Has field -> parameter in get field that returns __undefined__ if it doesn't exist.

		* GetType -> return the current Class<> / Enum<>
		* Fields() -> returns all the fields / static fields. Remember to allow implements Dynamic also

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

	type rcf_hash_conflict_ctx = {
		t : t;
		add_names : texpr->texpr->texpr;
		get_conflict : texpr->texpr->texpr->texpr;
		set : texpr->texpr->texpr->texpr->texpr;
		delete : texpr->texpr->texpr->texpr;
	}

	type rcf_ctx =
	{
		rcf_gen : generator_ctx;
		rcf_ft : ClosuresToClass.closures_ctx;
		rcf_optimize : bool;

		rcf_object_iface : tclass;

		rcf_max_func_arity : int;

		(*
			the hash lookup function. can be an inlined expr or simply a function call.
			its only needed features is that it should return the index of the key if found, and the
			complement of the index of where it should be inserted if not found (Ints).

			hash->hash_array->length->returning expression
		*)
		rcf_hash_function : texpr->texpr->texpr->texpr;

		rcf_lookup_function : texpr->texpr;

		(* hash_array->length->pos->value *)
		rcf_insert_function : texpr->texpr->texpr->texpr->texpr;

		(* hash_array->length->pos->value *)
		rcf_remove_function : texpr->texpr->texpr->texpr;

		rcf_hash_fields : (int, string) Hashtbl.t;

		rcf_hash_paths : (path * int, string) Hashtbl.t;

		rcf_hash_conflict_ctx : rcf_hash_conflict_ctx option;

		(*
			main expr -> field expr -> field string -> possible hash int (if optimize) -> possible set expr -> should_throw_exceptions -> changed expression

			Changes a get / set field to the runtime resolution function
		*)
		rcf_on_getset_field : texpr->texpr->string->int32 option->texpr option->bool->texpr;

		rcf_on_call_field : texpr->texpr->string->int32 option->texpr list->texpr;
	}

	let new_ctx gen ft object_iface optimize dynamic_getset_field dynamic_call_field hash_function lookup_function insert_function remove_function hash_conflict_ctx =
		{
			rcf_gen = gen;
			rcf_ft = ft;

			rcf_optimize = optimize;

			rcf_object_iface = object_iface;

			rcf_max_func_arity = 10;

			rcf_hash_function = hash_function;
			rcf_lookup_function = lookup_function;

			rcf_insert_function = insert_function;
			rcf_remove_function = remove_function;

			rcf_hash_fields = Hashtbl.create 100;
			rcf_hash_paths = Hashtbl.create 100;

			rcf_on_getset_field = dynamic_getset_field;
			rcf_on_call_field = dynamic_call_field;
			rcf_hash_conflict_ctx = hash_conflict_ctx;
		}

	(*
		methods as a bool option is a little laziness of my part.
			None means that methods are included with normal fields;
			Some(true) means collect only methods
			Some(false) means collect only fields (and MethDynamic fields)
	*)
	let collect_fields cl (methods : bool option) =
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
			let acc = collect_cfs cl.cl_ordered_fields acc in
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
		if Sys.word_size = 64 then Int32.to_int (Int32.shift_right (Int32.shift_left (Int32.of_int !h) 1) 1) else !h

	let hash_field ctx f pos =
		let h = hash f in
		(try
			let f2 = Hashtbl.find ctx.rcf_hash_paths (ctx.rcf_gen.gcurrent_path, h) in
			if f <> f2 then ctx.rcf_gen.gcon.error ("Field conflict between " ^ f ^ " and " ^ f2) pos
		with Not_found ->
			Hashtbl.add ctx.rcf_hash_paths (ctx.rcf_gen.gcurrent_path, h) f;
			Hashtbl.replace ctx.rcf_hash_fields h f);
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
				mk (TConst (TInt i)) ctx.rcf_gen.gcon.basic.tint pos
			| false ->
				ExprBuilder.make_string ctx.rcf_gen.gcon field_name pos

	let call_super ctx fn_args ret_t cf cl this_t pos =
		{
			eexpr = TCall({
				eexpr = TField({ eexpr = TConst(TSuper); etype = this_t; epos = pos }, FInstance(cl,List.map snd cl.cl_params,cf));
				etype = TFun(fun_args fn_args, ret_t);
				epos = pos;
			}, List.map (fun (v,_) -> mk_local v pos) fn_args);
			etype = ret_t;
			epos = pos;
		}


	let mk_throw ctx str pos = { eexpr = TThrow (ExprBuilder.make_string ctx.rcf_gen.gcon str pos); etype = ctx.rcf_gen.gcon.basic.tvoid; epos = pos }

	let enumerate_dynamic_fields ctx cl when_found base_arr =
		let gen = ctx.rcf_gen in
		let basic = gen.gcon.basic in
		let pos = cl.cl_pos in

		let vtmp = alloc_var "i" basic.tint in

		let mk_for arr len =
			let t = if ctx.rcf_optimize then basic.tint else basic.tstring in
			let convert_str e = if ctx.rcf_optimize then ctx.rcf_lookup_function e else e in
			let tmpinc = { eexpr = TUnop(Ast.Increment, Ast.Postfix, mk_local vtmp pos); etype = basic.tint; epos = pos } in
			[
				{ eexpr = TBinop(OpAssign, mk_local vtmp pos, ExprBuilder.make_int ctx.rcf_gen.gcon 0 pos); etype = basic.tint; epos = pos };
				{
					eexpr = TWhile (
						{ eexpr = TBinop(Ast.OpLt, mk_local vtmp pos, len); etype = basic.tbool; epos = pos },
						mk_block (when_found (convert_str { eexpr = TArray (arr, tmpinc); etype = t; epos = pos })),
						Ast.NormalWhile
					);
					etype = basic.tvoid;
					epos = pos
				}
			]
 		in

		let this_t = TInst(cl, List.map snd cl.cl_params) in
		let this = { eexpr = TConst(TThis); etype = this_t; epos = pos } in
		let mk_this field t = { (mk_field_access gen this field pos) with etype = t } in

		{ eexpr = TVar (vtmp,None); etype = basic.tvoid; epos = pos }
		::
		if ctx.rcf_optimize then
			mk_for (mk_this (gen.gmk_internal_name "hx" "hashes") (gen.gclasses.nativearray basic.tint)) (mk_this (gen.gmk_internal_name "hx" "length") basic.tint)
			@
			mk_for (mk_this (gen.gmk_internal_name "hx" "hashes_f") (gen.gclasses.nativearray basic.tint)) (mk_this (gen.gmk_internal_name "hx" "length_f") basic.tint)
			@
			(
				let conflict_ctx = Option.get ctx.rcf_hash_conflict_ctx in
				let ehead = mk_this (gen.gmk_internal_name "hx" "conflicts") conflict_ctx.t in
				[conflict_ctx.add_names ehead base_arr]
			)
		else
			mk_for (mk_this (gen.gmk_internal_name "hx" "hashes") (gen.gclasses.nativearray basic.tstring)) (mk_this (gen.gmk_internal_name "hx" "length") basic.tint)
			@
			mk_for (mk_this (gen.gmk_internal_name "hx" "hashes_f") (gen.gclasses.nativearray basic.tstring)) (mk_this (gen.gmk_internal_name "hx" "length_f") basic.tint)

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
	let abstract_dyn_lookup_implementation ctx this field_local hash_local may_value is_float pos =
		let gen = ctx.rcf_gen in
		let basic = gen.gcon.basic in
		let mk_this field t = { (mk_field_access gen this field pos) with etype = t } in
		let a_t = if ctx.rcf_optimize then basic.tint else basic.tstring in
		let hx_hashes = mk_this (gen.gmk_internal_name "hx" "hashes") (gen.gclasses.nativearray a_t) in
		let hx_hashes_f = mk_this (gen.gmk_internal_name "hx" "hashes_f") (gen.gclasses.nativearray a_t) in
		let hx_dynamics = mk_this (gen.gmk_internal_name "hx" "dynamics") (gen.gclasses.nativearray t_empty) in
		let hx_dynamics_f = mk_this (gen.gmk_internal_name "hx" "dynamics_f") (gen.gclasses.nativearray basic.tfloat) in
		let hx_length = mk_this (gen.gmk_internal_name "hx" "length") (basic.tint) in
		let hx_length_f = mk_this (gen.gmk_internal_name "hx" "length_f") (basic.tint) in
		let res = alloc_var "res" basic.tint in
		let fst_hash, snd_hash, fst_dynamics, snd_dynamics, fst_length, snd_length =
			if is_float then
				hx_hashes_f, hx_hashes, hx_dynamics_f, hx_dynamics, hx_length_f, hx_length
			else
				hx_hashes, hx_hashes_f, hx_dynamics, hx_dynamics_f, hx_length, hx_length_f
		in
		let res_local = mk_local res pos in
		let gte = {
			eexpr = TBinop(Ast.OpGte, res_local, { eexpr = TConst(TInt(Int32.zero)); etype = basic.tint; epos = pos });
			etype = basic.tbool;
			epos = pos;
		} in
		let mk_tarray arr idx =
			{
				eexpr = TArray(arr, idx);
				etype = gen.gclasses.nativearray_type arr.etype;
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
					{ eexpr = TVar(res, Some(ctx.rcf_hash_function hash_local fst_hash fst_length)); etype = basic.tvoid; epos = pos };
					{ eexpr = TIf(gte, mk_return (mk_tarray fst_dynamics res_local), Some({
						eexpr = TBlock(
						[
							{ eexpr = TBinop(Ast.OpAssign, res_local, ctx.rcf_hash_function hash_local snd_hash snd_length); etype = basic.tint; epos = pos };
							{ eexpr = TIf(gte, mk_return (mk_tarray snd_dynamics res_local), None); etype = ret_t; epos = pos }
						]);
						etype = ret_t;
						epos = pos;
					})); etype = ret_t; epos = pos }
				] in

				if ctx.rcf_optimize then
					let conflict_ctx = Option.get ctx.rcf_hash_conflict_ctx in
					let ehead = mk_this (gen.gmk_internal_name "hx" "conflicts") conflict_ctx.t in
					let vconflict = alloc_var "conflict" conflict_ctx.t in
					let local_conflict = mk_local vconflict pos in
					[mk (TIf (
						mk (TBinop (OpLt, hash_local, ExprBuilder.make_int gen.gcon 0 pos)) basic.tbool pos,
						mk (TBlock [
							mk (TVar (vconflict, Some (conflict_ctx.get_conflict ehead hash_local field_local))) basic.tvoid pos;
							mk (TIf (
								mk (TBinop (OpNotEq, local_conflict, mk (TConst TNull) local_conflict.etype pos)) basic.tbool pos,
								mk_return (Codegen.field local_conflict "value" t_dynamic pos),
								None
							)) basic.tvoid pos;
						]) basic.tvoid pos,
						Some (mk (TBlock block) basic.tvoid pos)
					)) basic.tvoid pos]
				else
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
				let neg_res = { eexpr = TUnop(Ast.NegBits, Ast.Prefix, res_local); etype = basic.tint; epos = pos } in

				let res2 = alloc_var "res2" basic.tint in
				let res2_local = mk_local res2 pos in
				let gte2 = {
					eexpr = TBinop(Ast.OpGte, res2_local, { eexpr = TConst(TInt(Int32.zero)); etype = basic.tint; epos = pos });
					etype = basic.tbool;
					epos = pos;
				} in

				let block =
				[
					{ eexpr = TVar(res, Some(ctx.rcf_hash_function hash_local fst_hash fst_length)); etype = basic.tvoid; epos = pos };
					{
						eexpr = TIf(gte,
							mk_return { eexpr = TBinop(Ast.OpAssign, mk_tarray fst_dynamics res_local, value_local); etype = value_local.etype; epos = pos },
							Some({ eexpr = TBlock([
								{ eexpr = TVar( res2, Some(ctx.rcf_hash_function hash_local snd_hash snd_length)); etype = basic.tvoid; epos = pos };
								{
									eexpr = TIf(gte2, { eexpr = TBlock([
										ctx.rcf_remove_function snd_hash snd_length res2_local;
										ctx.rcf_remove_function snd_dynamics snd_length res2_local;
										mk (TUnop(Decrement,Postfix,snd_length)) basic.tint pos
									]); etype = t_dynamic; epos = pos }, None);
									etype = t_dynamic;
									epos = pos;
								}
							]); etype = t_dynamic; epos = pos }));
						etype = t_dynamic;
						epos = pos;
					};
					ctx.rcf_insert_function fst_hash fst_length neg_res hash_local;
					ctx.rcf_insert_function fst_dynamics fst_length neg_res value_local;
					mk (TUnop(Increment,Postfix,fst_length)) basic.tint pos;
				] in

				let block =
					if ctx.rcf_optimize then
						let conflict_ctx = Option.get ctx.rcf_hash_conflict_ctx in
						let ehead = mk_this (gen.gmk_internal_name "hx" "conflicts") conflict_ctx.t in
						[mk (TIf (
							mk (TBinop (OpLt, hash_local, ExprBuilder.make_int gen.gcon 0 pos)) basic.tbool pos,
							conflict_ctx.set ehead hash_local field_local value_local,
							Some (mk (TBlock block) basic.tvoid pos)
						)) basic.tvoid pos]
					else
						block
				in
				block @ [mk_return value_local]

	let get_delete_field ctx cl is_dynamic =
		let pos = cl.cl_pos in
		let this_t = TInst(cl, List.map snd cl.cl_params) in
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
			let hx_hashes = mk_this (gen.gmk_internal_name "hx" "hashes") (gen.gclasses.nativearray a_t) in
			let hx_hashes_f = mk_this (gen.gmk_internal_name "hx" "hashes_f") (gen.gclasses.nativearray a_t) in
			let hx_dynamics = mk_this (gen.gmk_internal_name "hx" "dynamics") (gen.gclasses.nativearray t_empty) in
			let hx_dynamics_f = mk_this (gen.gmk_internal_name "hx" "dynamics_f") (gen.gclasses.nativearray basic.tfloat) in
			let hx_length = mk_this (gen.gmk_internal_name "hx" "length") (basic.tint) in
			let hx_length_f = mk_this (gen.gmk_internal_name "hx" "length_f") (basic.tint) in
			let res = alloc_var "res" basic.tint in
			let res_local = mk_local res pos in
			let gte = {
				eexpr = TBinop(Ast.OpGte, res_local, { eexpr = TConst(TInt(Int32.zero)); etype = basic.tint; epos = pos });
				etype = basic.tbool;
				epos = pos;
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
			let common = [
				{ eexpr = TVar(res,Some(ctx.rcf_hash_function local_switch_var hx_hashes hx_length)); etype = basic.tvoid; epos = pos };
				{
					eexpr = TIf(gte, { eexpr = TBlock([
						ctx.rcf_remove_function hx_hashes hx_length res_local;
						ctx.rcf_remove_function hx_dynamics hx_length res_local;
						mk (TUnop(Decrement,Postfix,hx_length)) basic.tint pos;
						mk_return { eexpr = TConst(TBool true); etype = basic.tbool; epos = pos }
					]); etype = t_dynamic; epos = pos }, Some({ eexpr = TBlock([
						{ eexpr = TBinop(Ast.OpAssign, res_local, ctx.rcf_hash_function local_switch_var hx_hashes_f hx_length_f); etype = basic.tint; epos = pos };
						{ eexpr = TIf(gte, { eexpr = TBlock([
							ctx.rcf_remove_function hx_hashes_f hx_length_f res_local;
							ctx.rcf_remove_function hx_dynamics_f hx_length_f res_local;
							mk (TUnop(Decrement,Postfix,hx_length_f)) basic.tint pos;
							mk_return { eexpr = TConst(TBool true); etype = basic.tbool; epos = pos }
						]); etype = t_dynamic; epos = pos }, None); etype = t_dynamic; epos = pos }
					]); etype = t_dynamic; epos = pos }));
					etype = t_dynamic;
					epos = pos;
				};
				mk_return { eexpr = TConst(TBool false); etype = basic.tbool; epos = pos }
			] in

			if ctx.rcf_optimize then
				let v_name = match tf_args with (v,_) :: _ -> v | _ -> assert false in
				let local_name = mk_local v_name pos in
				let conflict_ctx = Option.get ctx.rcf_hash_conflict_ctx in
				let ehead = mk_this (gen.gmk_internal_name "hx" "conflicts") conflict_ctx.t in
				(mk (TIf (
					mk (TBinop (OpLt, local_switch_var, ExprBuilder.make_int gen.gcon 0 pos)) basic.tbool pos,
					mk (TReturn (Some (conflict_ctx.delete ehead local_switch_var local_name))) basic.tvoid pos,
					None
				)) basic.tvoid pos) :: common
			else
				common
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
				| TParenthesis e | TMeta(_,e) -> is_side_effects_free e
				| TArrayDecl el -> List.for_all is_side_effects_free el
				| TCast(e,_) -> is_side_effects_free e
				| _ -> false
		in

		let pos = cl.cl_pos in
		let gen = ctx.rcf_gen in
		let basic = gen.gcon.basic in
		let hasht = if ctx.rcf_optimize then basic.tint else basic.tstring in

		let hashes_field = gen.gmk_internal_name "hx" "hashes", gen.gclasses.nativearray hasht in
		let hashes_f_field = gen.gmk_internal_name "hx" "hashes_f", gen.gclasses.nativearray hasht in
		let dynamics_field = gen.gmk_internal_name "hx" "dynamics", gen.gclasses.nativearray t_empty in
		let dynamics_f_field = gen.gmk_internal_name "hx" "dynamics_f", gen.gclasses.nativearray basic.tfloat in
		let fields =
		[
			hashes_field;
			dynamics_field;
			hashes_f_field;
			dynamics_f_field;
		] in

		let hashes_var = alloc_var (fst hashes_field) (snd hashes_field) in
		let hashes_f_var = alloc_var (fst hashes_f_field) (snd hashes_f_field) in
		let tf_args = [
			hashes_var, None;
			alloc_var (fst dynamics_field) (snd dynamics_field), None;
			hashes_f_var, None;
			alloc_var (fst dynamics_f_field) (snd dynamics_f_field), None;
		] in

		let this = { eexpr = TConst TThis; etype = TInst(cl, List.map snd cl.cl_params); epos = pos } in
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
					eexpr = TBlock(
						List.map (fun (v,_) ->
							{ eexpr = TBinop(Ast.OpAssign, mk_this v.v_name v.v_type, mk_local v pos); etype = v.v_type; epos = pos }
						) tf_args
						@
						[
							mk (TBinop(OpAssign, mk_this (gen.gmk_internal_name "hx" "length") basic.tint, gen.gclasses.nativearray_len (mk_local hashes_var pos) pos)) basic.tint pos;
							mk (TBinop(OpAssign, mk_this (gen.gmk_internal_name "hx" "length_f") basic.tint, gen.gclasses.nativearray_len (mk_local hashes_f_var pos) pos)) basic.tint pos;
						]
					);
					etype = basic.tvoid;
					epos = pos
				}
			});
			etype = fun_t;
			epos = pos
		});

		add_constructor cl ctor;
		(* default ctor also *)
		let ctor = mk_class_field "new" (TFun([],basic.tvoid)) false pos (Method MethNormal) [] in
		ctor.cf_expr <- Some {
			eexpr = TFunction {
				tf_type = basic.tvoid;
				tf_args = [];
				tf_expr = {
					eexpr = TBlock(List.map (fun (f,t) ->
						{ eexpr = TBinop(Ast.OpAssign, mk_this f t,{ eexpr = TCall(mk_local v_nativearray pos, []); etype = t; epos = pos; }); etype = t; epos = pos }
					) fields);
					etype = basic.tvoid;
					epos = pos;
				}
			};
			etype = ctor.cf_type;
			epos = pos;
		};
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
							if like_float real_t && not (like_i64 real_t) then
								loop tl acc ((name, gen.ghandle_cast basic.tfloat real_t expr) :: acc_f)
							else
								loop tl ((name, gen.ghandle_cast t_dynamic real_t expr) :: acc) acc_f
		in

		let may_hash_field s =
			if ctx.rcf_optimize then begin
				mk (TConst (TInt (hash_field_i32 ctx pos s))) basic.tint pos
			end else begin
				ExprBuilder.make_string gen.gcon s pos
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
						exprs_before := { eexpr = TVar(var,Some expr); etype = basic.tvoid; epos = expr.epos } :: !exprs_before;
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

			let ret = {
				e with eexpr = TNew(cl,[],
					[
						mk_nativearray_decl gen hasht (List.map fst odecl) pos;
						mk_nativearray_decl gen t_empty (List.map snd odecl) pos;
						mk_nativearray_decl gen hasht (List.map fst odecl_f) pos;
						mk_nativearray_decl gen basic.tfloat (List.map snd odecl_f) pos;
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
					mk_class_field (gen.gmk_internal_name "hx" "hashes") (gen.gclasses.nativearray hasht) false pos (Var { v_read = AccNormal; v_write = AccNormal }) [];
					mk_class_field (gen.gmk_internal_name "hx" "dynamics") (gen.gclasses.nativearray t_empty) false pos (Var { v_read = AccNormal; v_write = AccNormal }) [];
					mk_class_field (gen.gmk_internal_name "hx" "hashes_f") (gen.gclasses.nativearray hasht) false pos (Var { v_read = AccNormal; v_write = AccNormal }) [];
					mk_class_field (gen.gmk_internal_name "hx" "dynamics_f") (gen.gclasses.nativearray basic.tfloat) false pos (Var { v_read = AccNormal; v_write = AccNormal }) [];
				] in

				(if cl.cl_path <> (["haxe"; "lang"], "DynamicObject") then
					List.iter (fun cf -> cf.cf_expr <- Some { eexpr = TCall(mk_local v_nativearray pos, []); etype = cf.cf_type; epos = cf.cf_pos }) new_fields
				);

				let new_fields =
					if ctx.rcf_optimize then
						let f = mk_class_field (gen.gmk_internal_name "hx" "conflicts") (Option.get ctx.rcf_hash_conflict_ctx).t false pos (Var { v_read = AccNormal; v_write = AccNormal }) [] in
						f :: new_fields
					else
						new_fields
				in

				let delete = get_delete_field ctx cl true in

				let new_fields = new_fields @ [
					mk_class_field (gen.gmk_internal_name "hx" "length") (basic.tint) false pos (Var { v_read = AccNormal; v_write = AccNormal }) [];
					mk_class_field (gen.gmk_internal_name "hx" "length_f") (basic.tint) false pos (Var { v_read = AccNormal; v_write = AccNormal }) [];
					delete;
				] in

				List.iter (fun cf ->
					cl.cl_fields <- PMap.add cf.cf_name cf cl.cl_fields
				) new_fields;

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
						If class has @:dynamicObject meta, also create another new() class with its parameters as constructor arguments
				*)

				cl.cl_ordered_fields <- cl.cl_ordered_fields @ new_fields;
				if is_override then cl.cl_overrides <- delete :: cl.cl_overrides
			end
		end else if not is_override then begin
			let delete = get_delete_field ctx cl false in
			cl.cl_ordered_fields <- cl.cl_ordered_fields @ [delete];
			cl.cl_fields <- PMap.add delete.cf_name delete cl.cl_fields
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

		let this = { eexpr = TConst(TThis); etype = TInst(cl, List.map snd cl.cl_params); epos = pos } in

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
						let is_check_local = mk_local (get is_check_opt) pos in
						[
							{
								eexpr = TIf(is_check_local, mk_return (undefined pos), Some( mk_may_check_throw "Field not found." ));
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
					let v_name = match fields_args with (v,_) :: _ -> v | _ -> assert false in
					abstract_dyn_lookup_implementation ctx this (mk_local v_name pos) (mk_local switch_var pos) (Option.map (fun v -> mk_local v pos) value_opt) is_float pos
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

			let handle_prop = alloc_var "handleProperties" basic.tbool in
			let handle_prop_local = mk_local handle_prop pos in

			let this = { eexpr = TConst TThis; etype = TInst(cl, List.map snd cl.cl_params); epos = pos } in
			let mk_this_call_raw name fun_t params =
				{ eexpr = TCall( { (mk_field_access gen this name pos) with etype = fun_t; }, params ); etype = snd (get_args fun_t); epos = pos }
			in

			let fun_type = ref (TFun([], basic.tvoid)) in
			let fun_name = ctx.rcf_gen.gmk_internal_name "hx" ( (if is_set then "setField" else "getField") ^ (if is_float then "_f" else "") ) in
			let cfield = mk_class_field fun_name !fun_type false pos (Method MethNormal) [] in

			let maybe_cast e = e in

			let t = TInst(cl, List.map snd cl.cl_params) in

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
									{ eexpr = TField({ eexpr = TConst TSuper; etype = t; epos = pos }, FInstance(cl, List.map snd cl.cl_params, cfield)); etype = !fun_type; epos = pos },
									(List.map (fun (v,_) -> mk_local v pos) args) );
								etype = if is_float then basic.tfloat else t_dynamic;
								epos = pos;
							};
			in

			(* if it is set function, there are some different set fields to do *)
			let do_default, do_field, tf_args = if is_set then begin
				let value_var = alloc_var "value" (if is_float then basic.tfloat else t_dynamic) in
				let value_local = { eexpr = TLocal(value_var); etype = value_var.v_type; epos = pos } in
				let tf_args = tf_args @ [value_var,None; handle_prop, None; ] in
				let lookup_name = gen.gmk_internal_name "hx" ("lookupSetField" ^ if is_float then "_f" else "") in

				let do_default =
						fun () ->
							mk_return (mk_this_call_raw lookup_name (TFun(fun_args (field_args @ [value_var,None]),value_var.v_type)) ( List.map (fun (v,_) -> mk_local v pos) field_args @ [ value_local ] ))
				in

				let do_field cf cf_type =
					let get_field ethis = { eexpr = TField (ethis, FInstance(cl, List.map snd cl.cl_params, cf)); etype = cf_type; epos = pos } in
					let this = { eexpr = TConst(TThis); etype = t; epos = pos } in
					let value_local = if is_float then match follow cf_type with
						| TInst({ cl_kind = KTypeParameter _ }, _) ->
							mk_cast t_dynamic value_local
						| _ ->
							value_local
						else
							value_local
					in

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
						| Var { v_write = AccCall } ->
							let bl =
							[
								mk_this_call_raw ("set_" ^ cf.cf_name) (TFun(["value",false,cf.cf_type], cf.cf_type)) [ value_local ];
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

				(mk_do_default tf_args do_default, do_field, tf_args)
			end else begin
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
						| Var { v_read = AccCall } when Type.is_extern_field cf ->
							mk_return (mk_this_call_raw ("get_" ^ cf.cf_name) (TFun(["value",false,cf.cf_type], cf.cf_type)) [	])
						| Var { v_read = AccCall } ->
							{
								eexpr = TIf(
									handle_prop_local,
									mk_return (mk_this_call_raw ("get_" ^ cf.cf_name) (TFun(["value",false,cf.cf_type], cf.cf_type)) [	]),
									Some { eexpr = TField (ethis, FInstance(cl, List.map snd cl.cl_params, cf)); etype = cf_type; epos = pos }
								);
								etype = cf_type;
								epos = pos;
							}
						| Var _
						| Method MethDynamic -> { eexpr = TField (ethis, FInstance(cl,List.map snd cl.cl_params,cf)); etype = cf_type; epos = pos }
						| _ ->
								{ eexpr = TField (this, FClosure(Some (cl,[]), cf)); etype = cf_type; epos = pos } (* TODO: FClosure change *)
				in

				let do_field cf cf_type =
					let this = { eexpr = TConst(TThis); etype = t; epos = pos } in
					match is_float, follow cf_type with
						| true, TInst( { cl_kind = KTypeParameter _ }, _ ) ->
							mk_return (mk_cast basic.tfloat (mk_cast t_dynamic (get_field cf cf_type this cl cf.cf_name)))
						| _ ->
							mk_return (maybe_cast (get_field cf cf_type this cl cf.cf_name ))
				in
				(mk_do_default tf_args do_default, do_field, tf_args)
			end in

			let get_fields() =
				let ret = collect_fields cl ( if is_float || is_set then Some (false) else None ) in
				let ret = if is_set then List.filter (fun (_,cf) ->
					match cf.cf_kind with
					(* | Var { v_write = AccNever } -> false *)
					| _ -> not (Meta.has Meta.ReadOnly cf.cf_meta)) ret
				else
					List.filter (fun (_,cf) ->
					match cf.cf_kind with
					(* | Var { v_read = AccNever } -> false *)
					| _ -> true) ret in
				if is_float then
					List.filter (fun (_,cf) -> (* TODO: maybe really apply_params in cf.cf_type. The benefits would be limited, though *)
						match follow (ctx.rcf_gen.greal_type (ctx.rcf_gen.gfollow#run_f cf.cf_type)) with
							| TDynamic _ | TMono _
							| TInst ({ cl_kind = KTypeParameter _ }, _) -> true
							| t when like_float t && not (like_i64 t) -> true
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

			let content =
				let fields = get_fields() in
				let fields = List.filter (fun (_, cf) -> match is_set, cf.cf_kind with
					| true, Var { v_write = AccCall } -> true
					| false, Var { v_read = AccCall } -> true
					| _ -> not (Type.is_extern_field cf)) fields
				in
				(if fields <> [] then has_fields := true);
				let cases = List.map (fun (names, cf) ->
					(if names = [] then assert false);
					(List.map (switch_case ctx pos) names, do_field cf cf.cf_type)
				) fields in
				let default = Some(do_default()) in

				mk_block { eexpr = TSwitch(local_switch_var, cases, default); etype = basic.tvoid; epos = pos }
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

				(if is_override then cl.cl_overrides <- cfield	:: cl.cl_overrides)
			end else ()
		in
		mk_cfield true true;
		mk_cfield true false;
		mk_cfield false false;
		mk_cfield false true

	let implement_getFields ctx cl =
		let gen = ctx.rcf_gen in
		let basic = gen.gcon.basic in
		let pos = cl.cl_pos in

		(*
			function __hx_getFields(baseArr:Array<String>)
			{
				//add all variable fields
				//then:
				super.__hx_getFields(baseArr);
			}
		*)
		let name = gen.gmk_internal_name "hx" "getFields" in
		let v_base_arr = alloc_var "baseArr" (basic.tarray basic.tstring) in
		let base_arr = mk_local v_base_arr pos in

		let tf_args = [(v_base_arr,None)] in
		let t = TFun(fun_args tf_args, basic.tvoid) in
		let cf = mk_class_field name t false pos (Method MethNormal) [] in

		let e_pushfield = mk_field_access gen base_arr "push" pos in
		let mk_push value = mk (TCall (e_pushfield, [value])) basic.tint pos in

		let has_value = ref false in
		let map_fields =
			List.map (fun (_,cf) ->
				match cf.cf_kind with
					| Var _
					| Method MethDynamic when not (List.memq cf cl.cl_overrides) ->
						has_value := true;
						mk_push (ExprBuilder.make_string gen.gcon cf.cf_name pos)
					| _ -> null basic.tvoid pos
			)
		in

		(*
			if it is first_dynamic, then we need to enumerate the dynamic fields
		*)
		let exprs =
			if is_some cl.cl_dynamic && is_first_dynamic cl then begin
				has_value := true;
				enumerate_dynamic_fields ctx cl mk_push base_arr
			end else
				[]
		in

		let exprs =
			if is_override cl then
				let tparams = List.map snd cl.cl_params in
				let esuper = mk (TConst TSuper) (TInst(cl, tparams)) pos in
				let efield = mk (TField (esuper, FInstance (cl, tparams, cf))) t pos in
				exprs @ [mk (TCall (efield, [base_arr])) basic.tvoid pos]
			else
				exprs
		in

		let exprs = map_fields (collect_fields cl (Some false)) @ exprs in

		cf.cf_expr <- Some {
			eexpr = TFunction({
				tf_args = tf_args;
				tf_type = basic.tvoid;
				tf_expr = mk (TBlock exprs) basic.tvoid pos
			});
			etype = t;
			epos = pos
		};

		if !has_value || not (is_override cl) then begin
			cl.cl_ordered_fields <- cl.cl_ordered_fields @ [cf];
			cl.cl_fields <- PMap.add cf.cf_name cf cl.cl_fields;
			(if is_override cl then cl.cl_overrides <- cf :: cl.cl_overrides)
		end


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

		let dynamic_arg = alloc_var "dynargs" (basic.tarray t_dynamic) in
		let all_args = field_args @ [ dynamic_arg, None ] in
		let fun_t = TFun(fun_args all_args, t_dynamic) in

		let this_t = TInst(cl, List.map snd cl.cl_params) in
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
				| TAbstract ({ a_path = ([], "Void") },[]) -> ret
				| _ -> ret
			in
			mk_this_call_raw cf.cf_name (TFun(args, ret)) params
		in

		let extends_hxobject = extends_hxobject cl in
		ignore extends_hxobject;

		(* creates a invokeField of the class fields listed here *)
		(*
			function invokeField(field, dynargs)
			{
				switch(field)
				{
					case "a": this.a(dynargs[0], dynargs[1], dynargs[2]...);
					default: super.invokeField //or this.getField(field).invokeDynamic(dynargs)
				}
			}
		*)

		let dyn_fun = mk_class_field (ctx.rcf_gen.gmk_internal_name "hx" "invokeField") fun_t false cl.cl_pos (Method MethNormal) [] in

		let mk_switch_dyn cfs old =
			let get_case (names,cf) =
				has_method := true;
				let i = ref 0 in
				let dyn_arg_local = mk_local dynamic_arg pos in
				let cases = List.map (switch_case ctx pos) names in
				(cases,
					{ eexpr = TReturn(Some (mk_this_call cf (List.map (fun (name,_,t) ->
							let ret = { eexpr = TArray(dyn_arg_local, ExprBuilder.make_int ctx.rcf_gen.gcon !i pos); etype = t_dynamic; epos = pos } in
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

			let default = if !is_override then
				{ eexpr = TReturn(Some (call_super ctx all_args t_dynamic dyn_fun cl this_t pos) ); etype = basic.tvoid; epos = pos }
			else (
				let field = begin
					let fun_name = ctx.rcf_gen.gmk_internal_name "hx" "getField" in
					let tf_args, _ = field_type_args ctx pos in
					let tf_args, args = fun_args tf_args, field_args_exprs in

					let tf_args, args = tf_args @ ["throwErrors",false, basic.tbool],       args @ [ExprBuilder.make_bool gen.gcon true pos] in
					let tf_args, args = tf_args @ ["isCheck", false, basic.tbool],          args @ [ExprBuilder.make_bool gen.gcon false pos] in
					let tf_args, args = tf_args @ ["handleProperties", false, basic.tbool], args @ [ExprBuilder.make_bool gen.gcon false pos] in

					mk (TCall ({ (mk_field_access gen this fun_name pos) with etype = TFun(tf_args, t_dynamic) }, args)) t_dynamic pos
				end in
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
			let nonstatics = collect_fields cl (Some true) in

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

			mk_switch_dyn nonstatics !old_nonstatics
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

		let this_t = TInst(cl, List.map snd cl.cl_params) in
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

			mk_return expr
		in

		let all_cfs = List.filter (fun cf -> cf.cf_name <> "new" && cf.cf_name <> (invokedyn) && match cf.cf_kind with Method _ -> true | _ -> false) (ctx.rcf_ft.map_base_classfields cl map_fn) in

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

		let this_t = TInst(cl, List.map snd cl.cl_params) in
		let this = { eexpr = TConst(TThis); etype = this_t ; epos = pos } in
		let mk_this field t = { (mk_field_access gen this field pos) with etype = t } in

		let tf_args = field_args @ [obj_arg, None] in
		let cfs, ctor_body = List.fold_left (fun (acc_cf,acc_expr) (v,_) ->
			let cf = mk_class_field v.v_name v.v_type false pos (Var { v_read = AccNormal; v_write = AccNormal } ) [] in
			let expr = { eexpr = TBinop(Ast.OpAssign, mk_this v.v_name v.v_type, mk_local v pos); etype = v.v_type; epos = pos } in
			(cf :: acc_cf, expr :: acc_expr)
		) ([], [])	tf_args in

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
					(List.map (fun (v,_) -> mk_this v.v_name v.v_type) field_args) @ [ call_arg ]
				);
				etype = t_dynamic;
				epos = pos
			} in

			let expr = if like_float ret && not (like_int ret) then mk_cast ret expr else expr in

			mk_return expr
		in

		let all_cfs = List.filter (fun cf -> cf.cf_name <> "new" && match cf.cf_kind with Method _ -> true | _ -> false) (ctx.rcf_ft.map_base_classfields cl map_fn) in

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
					eexpr = TCall({ eexpr = TConst(TSuper); etype = TInst(cl,[]); epos = pos }, [ExprBuilder.make_int ctx.rcf_gen.gcon (-1) pos; ExprBuilder.make_int ctx.rcf_gen.gcon (-1) pos]);
					etype = basic.tvoid;
					epos = pos
				} :: ctor_body); etype = basic.tvoid; epos = pos }
			});
			etype = ctor_t;
			epos = pos
		};

		cl.cl_constructor <- Some ctor_cf;

		let closure_fun eclosure e field is_static =
			let f = ExprBuilder.make_string gen.gcon field eclosure.epos in
			let args = if ctx.rcf_optimize then [ f; { eexpr = TConst(TInt (hash_field_i32 ctx eclosure.epos field)); etype = basic.tint; epos = eclosure.epos } ] else [ f ] in
			let args = args @ [ mk_cast (TInst(ctx.rcf_object_iface, [])) e ] in

			{ eclosure with eexpr = TNew(cl,[],args) }
		in
		closure_fun

	let get_closure_func ctx closure_cl =
		let gen = ctx.rcf_gen in
		let basic = gen.gcon.basic in
		let closure_func eclosure e field is_static =
			mk_cast eclosure.etype { eclosure with
				eexpr = TNew(closure_cl, [], [
					e;
					ExprBuilder.make_string gen.gcon field eclosure.epos
				] @ (
					if ctx.rcf_optimize then [ { eexpr = TConst(TInt (hash_field_i32 ctx eclosure.epos field)); etype = basic.tint; epos = eclosure.epos } ] else []
				));
				etype = TInst(closure_cl,[])
			}
		in
		closure_func

	(*
			main expr -> field expr -> field string -> possible set expr -> should_throw_exceptions -> changed expression

			Changes a get / set
		*
		mutable rcf_on_getset_field : texpr->texpr->string->texpr option->bool->texpr;*)

	let configure_dynamic_field_access ctx =
		let gen = ctx.rcf_gen in
		let is_dynamic expr fexpr field =
			match (field_access_esp gen (gen.greal_type fexpr.etype) field) with
			| FEnumField _
			| FClassField _ -> false
			| _ -> true
		in

		let maybe_hash = if ctx.rcf_optimize then fun str pos -> Some (hash_field_i32 ctx pos str) else fun str pos -> None in
		DynamicFieldAccess.configure gen is_dynamic
			(fun expr fexpr field set is_unsafe ->
				let hash = maybe_hash field fexpr.epos in
				ctx.rcf_on_getset_field expr fexpr field hash set is_unsafe
			)
			(fun ecall fexpr field call_list ->
				let hash = maybe_hash field fexpr.epos in
				ctx.rcf_on_call_field ecall fexpr field hash call_list
			);
		()


	(* ******************************************* *)
	(* UniversalBaseClass *)
	(* ******************************************* *)
	(*
		Sets the universal base class for hxgen types (HxObject / IHxObject)

		dependencies:
			As a rule, it should be one of the last module filters to run so any @:hxgen class created in the process
			-Should- only run after TypeParams.RealTypeParams.Modf
	*)
	module UniversalBaseClass =
	struct
		let name = "rcf_universal_base_class"
		let priority = min_dep +. 10.

		let configure gen baseclass baseinterface basedynamic =
			let rec run md =
				if is_hxgen md then
					match md with
					| TClassDecl ({ cl_interface = true } as cl) when cl.cl_path <> baseclass.cl_path && cl.cl_path <> baseinterface.cl_path && cl.cl_path <> basedynamic.cl_path ->
						cl.cl_implements <- (baseinterface, []) :: cl.cl_implements
					| TClassDecl ({ cl_kind = KAbstractImpl _ }) ->
						(* don't add any base classes to abstract implementations *)
						()
					| TClassDecl ({ cl_super = None } as cl) when cl.cl_path <> baseclass.cl_path && cl.cl_path <> baseinterface.cl_path && cl.cl_path <> basedynamic.cl_path ->
						if is_some cl.cl_dynamic then
							cl.cl_super <- Some (basedynamic,[])
						else
							cl.cl_super <- Some (baseclass,[])
					| TClassDecl ({ cl_super = Some(super,_) } as cl) when cl.cl_path <> baseclass.cl_path && cl.cl_path <> baseinterface.cl_path && not (is_hxgen (TClassDecl super)) ->
						cl.cl_implements <- (baseinterface, []) :: cl.cl_implements
					| _ ->
						()
			in
			let map md = Some(run md; md) in
			gen.gmodule_filters#add ~name:name ~priority:(PCustom priority) map
	end;;


	(*
		Priority: must run AFTER UniversalBaseClass
	*)
	let priority = solve_deps name [DAfter UniversalBaseClass.priority]

	let configure ?slow_invoke ctx baseinterface =
		let gen = ctx.rcf_gen in
		let run = (fun md -> match md with
			| TClassDecl cl when is_hxgen md && ( not cl.cl_interface || cl.cl_path = baseinterface.cl_path ) && (match cl.cl_kind with KAbstractImpl _ -> false | _ -> true) ->
				(implement_dynamics ctx cl);
				(if not (PMap.mem (gen.gmk_internal_name "hx" "lookupField") cl.cl_fields) then implement_final_lookup ctx cl);
				(if not (PMap.mem (gen.gmk_internal_name "hx" "getField") cl.cl_fields) then implement_get_set ctx cl);
				(if not (PMap.mem (gen.gmk_internal_name "hx" "invokeField") cl.cl_fields) then implement_invokeField ctx ~slow_invoke:slow_invoke cl);
				(if not (PMap.mem (gen.gmk_internal_name "hx" "getFields") cl.cl_fields) then implement_getFields ctx cl);
				None
			| _ -> None)
		in

		gen.gmodule_filters#add ~name:name ~priority:(PCustom priority) run

end;;

(* ******************************************* *)
(* Object Declaration Mapper *)
(* ******************************************* *)
module ObjectDeclMap =
struct
	let name = "object_decl_map"
	let priority = solve_deps name []

	let configure gen map_fn =
		let rec run e =
			match e.eexpr with
			| TObjectDecl odecl ->
				let e = Type.map_expr run e in
				(match e.eexpr with TObjectDecl odecl -> map_fn e odecl | _ -> assert false)
			| _ ->
				Type.map_expr run e
		in
		let map e = Some(run e) in
		gen.gsyntax_filters#add ~name:name ~priority:(PCustom priority) map
end;;


(* ******************************************* *)
(* EnumToClass *)
(* ******************************************* *)
(*
	For languages that don't support parameterized enums and/or metadata in enums, we need to transform
	enums into normal classes. This is done at the first module pass by creating new classes with the same
	path inside the modules, and removing the actual enum module by setting it as en extern.

	* The target must create its own strategy to deal with reflection. As it is right now, we will have a base class
	which the class will extend, create @:$IsEnum metadata for the class, and create @:alias() metadatas for the fields,
	with their tag order (as a string) as their alias. If you are using ReflectionCFs, then you don't have to worry
	about that, as it's already generating all information needed by the haxe runtime.
	so they can be
*)
module EnumToClass =
struct
	let name = "enum_to_class"
	let priority = solve_deps name []

	type t = {
		ec_tbl : (path, tclass) Hashtbl.t;
	}

	let new_t () = {
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
			It needs that the target platform implements __array__() as a shortcut to declare haxe.ds.Vector
	*)
	module EnumToClassModf =
	struct
		let name = "enum_to_class_mod"
		let priority = solve_deps name [DBefore ReflectionCFs.priority; DBefore TypeParams.RealTypeParams.RealTypeParamsModf.priority]

		let pmap_exists fn pmap = try PMap.iter (fun a b -> if fn a b then raise Exit) pmap; false with | Exit -> true

		let has_any_meta en =
			let has_meta meta = List.exists (fun (m,_,_) -> match m with Meta.Custom _ -> true | _ -> false) meta in
			has_meta en.e_meta || pmap_exists (fun _ ef -> has_meta ef.ef_meta) en.e_constrs

		let convert gen t base_class base_param_class en =
			let handle_type_params = false in (* TODO: look into this *)
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

			let super, has_params = if Meta.has Meta.FlatEnum en.e_meta then base_class, false else base_param_class, true in

			cl.cl_super <- Some(super,[]);
			cl.cl_extern <- en.e_extern;
			en.e_meta <- (Meta.Class, [], pos) :: en.e_meta;
			cl.cl_module <- en.e_module;
			cl.cl_meta <- ( Meta.Enum, [], pos ) :: cl.cl_meta;

			(match gen.gcon.platform with
				| Cs when Common.defined gen.gcon Define.CoreApiSerialize ->
					cl.cl_meta <- ( Meta.Meta, [ (EField( (EConst (Ident "System"), null_pos ), "Serializable" ), null_pos) ], null_pos ) :: cl.cl_meta
				| _ -> ());
			let c_types =
				if handle_type_params then
					List.map (fun (s,t) -> (s, TInst (map_param (get_cl_t t), []))) en.e_params
				else
					[]
			in

			cl.cl_params <- c_types;

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
								List.map (fun (s,t) -> (s, TInst (map_param (get_cl_t t), []))) en.e_params
							else
								[]
						in

						let ef_type =
							let fn, types = if handle_type_params then snd, dup_types else (fun _ -> t_dynamic), en.e_params in
							let t = apply_params en.e_params (List.map fn types) ef.ef_type in
							apply_params ef.ef_params (List.map fn ef.ef_params) t
						in

						let params, ret = get_fun ef_type in
						let cf_params = if handle_type_params then dup_types @ ef.ef_params else [] in

						let cf = mk_class_field name ef_type true pos (Method MethNormal) cf_params in
						cf.cf_meta <- [];

						let tf_args = List.map (fun (name,opt,t) ->  (alloc_var name t, if opt then Some TNull else None) ) params in
						let arr_decl = mk_nativearray_decl gen t_dynamic (List.map (fun (v,_) -> mk_local v pos) tf_args) pos in
						let expr = {
							eexpr = TFunction({
								tf_args = tf_args;
								tf_type = ret;
								tf_expr = mk_block ( mk_return { eexpr = TNew(cl,List.map snd dup_types, [ExprBuilder.make_int gen.gcon old_i pos; arr_decl] ); etype = TInst(cl, List.map snd dup_types); epos = pos } );
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
						let cf = mk_class_field name actual_t true pos (Var { v_read = AccNormal; v_write = AccNever }) [] in
						let args = if has_params then
							[ExprBuilder.make_int gen.gcon old_i pos; null (gen.gclasses.nativearray t_dynamic) pos]
						else
							[ExprBuilder.make_int gen.gcon old_i pos]
						in
						cf.cf_meta <- [Meta.ReadOnly,[],pos];
						cf.cf_expr <- Some {
							eexpr = TNew(cl, List.map (fun _ -> t_empty) cl.cl_params, args);
							etype = TInst(cl, List.map (fun _ -> t_empty) cl.cl_params);
							epos = pos;
						};
						cf
				in
				cl.cl_statics <- PMap.add cf.cf_name cf cl.cl_statics;
				cf
			) en.e_names in
			let constructs_cf = mk_class_field "__hx_constructs" (gen.gclasses.nativearray basic.tstring) true pos (Var { v_read = AccNormal; v_write = AccNever }) [] in
			constructs_cf.cf_meta <- [Meta.ReadOnly,[],pos];
			constructs_cf.cf_expr <- Some (mk_nativearray_decl gen basic.tstring (List.map (fun s -> { eexpr = TConst(TString s); etype = basic.tstring; epos = pos }) en.e_names) pos);

			cl.cl_ordered_statics <- constructs_cf :: cfs @ cl.cl_ordered_statics ;
			cl.cl_statics <- PMap.add "__hx_constructs" constructs_cf cl.cl_statics;

			let getTag_cf_type = tfun [] basic.tstring in
			let getTag_cf = mk_class_field "getTag" getTag_cf_type true pos (Method MethNormal) [] in
			getTag_cf.cf_meta <- [(Meta.Final, [], pos)];
			getTag_cf.cf_expr <- Some {
				eexpr = TFunction {
					tf_args = [];
					tf_type = basic.tstring;
					tf_expr = {
						eexpr = TReturn (Some (
							let e_constructs = mk_static_field_access_infer cl "__hx_constructs" pos [] in
							let e_this = mk (TConst TThis) (TInst (cl,[])) pos in
							let e_index = mk_field_access gen e_this "index" pos in
							{
								eexpr = TArray(e_constructs,e_index);
								etype = basic.tstring;
								epos = pos;
							}
						));
						epos = pos;
						etype = basic.tvoid;
					}
				};
				etype = getTag_cf_type;
				epos = pos;
			};

			cl.cl_ordered_fields <- getTag_cf :: cl.cl_ordered_fields ;
			cl.cl_fields <- PMap.add "getTag" getTag_cf cl.cl_fields;
			cl.cl_overrides <- getTag_cf :: cl.cl_overrides;
			cl.cl_meta <- (Meta.NativeGen,[],cl.cl_pos) :: cl.cl_meta;
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
		let configure gen t convert_all convert_if_has_meta enum_base_class param_enum_class =
			let convert e = convert gen t enum_base_class param_enum_class e in
			let run md =
				match md with
				| TEnumDecl e when is_hxgen md ->
					if convert_all then
						convert e
					else if convert_if_has_meta && has_any_meta e then
						convert e
					else if not (Meta.has Meta.FlatEnum e.e_meta) then
						convert e
					else begin
						(* take off the :hxgen meta from it, if there's any *)
						e.e_meta <- List.filter (fun (n,_,_) -> not (n = Meta.HxGen)) e.e_meta;
						md
					end
				| _ ->
					md
			in
			let map md = Some(run md) in
			gen.gmodule_filters#add ~name:name ~priority:(PCustom priority) map
	end;;

	(* ******************************************* *)
	(* EnumToClassExprf *)
	(* ******************************************* *)
	(*
		Enum to class Expression Filter

		dependencies:
			Should run before TArrayTransform, since it generates array access expressions
	*)
	module EnumToClassExprf =
	struct
		let name = "enum_to_class_exprf"
		let priority = solve_deps name [DBefore TArrayTransform.priority]

		let configure gen t opt_get_native_enum_tag =
			let rec run e =
				let get_converted_enum_type et =
					let en, eparams = match follow (gen.gfollow#run_f et) with
						| TEnum(en,p) -> en, p
						| _ -> raise Not_found
					in
					let cl = Hashtbl.find t.ec_tbl en.e_path in
					TInst(cl, eparams)
				in

				match e.eexpr with
				| TCall (({eexpr = TField(_, FStatic({cl_path=[],"Type"},{cf_name="enumIndex"}))} as left), [f]) ->
					let f = run f in
					(try
						mk_field_access gen {f with etype = get_converted_enum_type f.etype} "index" e.epos
					with Not_found ->
						{ e with eexpr = TCall(left, [f]) })
				| TEnumParameter(f, _,i) ->
					let f = run f in
					(* check if en was converted to class *)
					(* if it was, switch on tag field and change cond type *)
					let f = try
						{ f with etype = get_converted_enum_type f.etype }
					with Not_found ->
						f
					in
					let cond_array = { (mk_field_access gen f "params" f.epos) with etype = gen.gclasses.nativearray t_dynamic } in
					Codegen.index gen.gcon cond_array i e.etype e.epos
				| _ ->
					Type.map_expr run e
			in
			let map e = Some(run e) in
			gen.gexpr_filters#add ~name:name ~priority:(PCustom priority) map

	end;;

	let configure gen opt_get_native_enum_tag convert_all convert_if_has_meta enum_base_class param_enum_class =
		let t = new_t () in
		EnumToClassModf.configure gen t convert_all convert_if_has_meta enum_base_class param_enum_class;
		EnumToClassExprf.configure gen t opt_get_native_enum_tag
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
					(match follow ret with | TAbstract({ a_path = ([], "Bool") }, []) -> () | _ -> raise Not_found) ->
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

		TODO: I think TFor is always rewritten to TWhile before getting into the generator nowadays,
		so this filter could probably be removed. Gotta ask Simon about it.
	*)
	module IteratorsInterfaceExprf =
	struct
		let name = "iterators_interface_exprf"
		let priority = solve_deps name [DBefore DynamicFieldAccess.priority]

		let mk_access gen v name pos =
			let field_t =
				try match follow v.v_type with
					| TInst(cl, params) ->
						let field = PMap.find name cl.cl_fields in
						apply_params cl.cl_params params field.cf_type
					| TAnon(anon) ->
						let field = PMap.find name anon.a_fields in
						field.cf_type
					| _ -> t_dynamic
				with | Not_found -> t_dynamic
			in
			{ (mk_field_access gen (mk_local v pos) name pos) with etype = field_t }

		let configure gen =
			let basic = gen.gcon.basic in
			let rec run e =
				match e.eexpr with
				| TFor(var, in_expr, block) ->
					let in_expr = run in_expr in
					let temp = mk_temp gen "iterator" in_expr.etype in
					let block = [
						{ eexpr = TVar(temp, Some(in_expr)); etype = basic.tvoid; epos = in_expr.epos };
						{
							eexpr = TWhile(
								{ eexpr = TCall(mk_access gen temp "hasNext" in_expr.epos, []); etype = basic.tbool; epos = in_expr.epos },
								Type.concat ({
									eexpr = TVar(var, Some({ eexpr = TCall(mk_access gen temp "next" in_expr.epos, []); etype = var.v_type; epos = in_expr.epos }));
									etype = basic.tvoid;
									epos = in_expr.epos
								}) ( run block ),
								Ast.NormalWhile);
							etype = basic.tvoid;
							epos = e.epos
						}
					] in
					{ eexpr = TBlock(block); etype = e.etype; epos = e.epos }
				| _ ->
					Type.map_expr run e
			in
			let map e = Some(run e) in
			gen.gexpr_filters#add ~name:name ~priority:(PCustom priority) map
	end;;

	let configure gen =
		IteratorsInterfaceExprf.configure gen

end;;


(* ******************************************* *)
(* SwitchToIf *)
(* ******************************************* *)
(*
	Just a syntax filter which changes switch expressions to if() else if() else if() ...
*)
module SwitchToIf =
struct
	let name = "switch_to_if"
	let priority = solve_deps name []

	let rec simplify_expr e = match e.eexpr with
		| TParenthesis e
		| TMeta(_,e) -> simplify_expr e
		| _ -> e

	let configure gen (should_convert:texpr->bool) =
		let basic = gen.gcon.basic in
		let rec run e =
			match e.eexpr with
				| TSwitch(cond,cases,default) when should_convert e ->
					let cond_etype, should_cache = match gen.gfollow#run_f cond.etype with
						| TType({ t_path = ([], "Null") }, [t]) ->
							let rec take_off_nullable t = match gen.gfollow#run_f t with
								| TType({ t_path = ([], "Null") }, [t]) -> take_off_nullable t
								| _ -> t
							in

							take_off_nullable t, true
						| _ -> cond.etype, false
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

								mk_local var cond.epos, [ { eexpr = TVar(var,Some(cond)); etype = basic.tvoid; epos = cond.epos } ]
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
								| None ->
									raise Exit
								| Some d -> run d
						in

						try
							{ e with eexpr = TBlock(fst_block @ [loop cases]) }
						with | Exit ->
							{ e with eexpr = TBlock [] }
					end
				| TSwitch(cond,cases,default) -> (try
					match (simplify_expr cond).eexpr with
						| TCall( { eexpr = TField(_,FStatic({ cl_path = [],"Type" }, { cf_name = "enumIndex" })) }, [enum] ) ->
							let real_enum = match enum.etype with
								| TEnum(e,_) -> e
								| _ -> raise Not_found
							in
							if Meta.has Meta.Class real_enum.e_meta then raise Not_found;
							let enum_expr = mk_mt_access (TEnumDecl(real_enum)) e.epos in
							let fields = Hashtbl.create (List.length real_enum.e_names) in
							PMap.iter (fun _ ef -> Hashtbl.add fields ef.ef_index ef) real_enum.e_constrs;
							let cases = List.map (fun (el,e) ->
								List.map (fun e -> match e.eexpr with
								| TConst(TInt i) ->
									let ef = Hashtbl.find fields (Int32.to_int i) in
									{ e with eexpr = TField(enum_expr, FEnum(real_enum,ef)); etype = TEnum(real_enum,List.map (fun _ -> t_dynamic) real_enum.e_params) }
								| _ -> raise Not_found) el, run e
							) cases in
							{ e with eexpr = TSwitch(enum,cases,Option.map run default) }
						| _ -> raise Not_found
					with Not_found -> Type.map_expr run e)
				| _ -> Type.map_expr run e
		in
		let map e = Some(run e) in
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
*)
module ClassInstance =
struct
	let priority = solve_deps "class_instance" []

	let configure gen (change_expr:texpr->module_type->texpr) =
		let rec run e =
			match e.eexpr with
					| TCall( ({ eexpr = TLocal({ v_name = ("__is__" | "__as__" | "__typeof__") } as v) } as local), calls ) when Hashtbl.mem gen.gspecial_vars v.v_name ->
						{ e with eexpr = TCall(local, List.map (fun e ->
							match e.eexpr with
							| TTypeExpr _ -> e
							| _ -> run e) calls) }
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
		let map e = Some(run e) in
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
		Needs to be run after all cast detection modules
*)
module HardNullableSynf =
struct
	let name = "hard_nullable"
	let priority = solve_deps name [DAfter CastDetect.ReturnCast.priority]

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
			is_null_t gen (apply_params t.t_params tl t.t_type)
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

	let configure gen unwrap_null wrap_val null_to_dynamic has_value opeq_handler =
		gen.gfollow#add ~name:(name ^ "_follow") (follow_addon gen);

		let is_null_t = is_null_t gen in
		let is_string t = match gen.greal_type t with
			| TInst({ cl_path=([],"String") },_) -> true
			| _ -> false
		in
		let handle_unwrap to_t e =
			let e_null_t = get (is_null_t e.etype) in
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

		let cur_block = ref [] in
		let add_tmp v e p =
			cur_block := { eexpr = TVar(v,e); etype = gen.gcon.basic.tvoid; epos = p } :: !cur_block
		in
		let get_local e = match e.eexpr with
			| TLocal _ ->
				e, e
			| _ ->
				let v = mk_temp gen "nulltmp" e.etype in
				add_tmp v (Some (null e.etype e.epos)) e.epos;
				let local = { e with eexpr = TLocal(v) } in
				mk_paren { e with eexpr = TBinop(Ast.OpAssign, local, e) }, local
		in
		let rec run e =
			match e.eexpr with
				| TBlock(bl) ->
					let lst = !cur_block in
					cur_block := [];
					List.iter (fun e ->
						let e = run e in
						cur_block := (e :: !cur_block)
					) bl;
					let ret = !cur_block in
					cur_block := lst;
					{ e with eexpr = TBlock(List.rev ret) }
				| TCast(v, _) ->
					let null_et = is_null_t e.etype in
					let null_vt = is_null_t v.etype in
					(match null_vt, null_et with
						| Some(vt), None when is_string e.etype ->
							let v = run v in
							{ e with eexpr = TCast(null_to_dynamic v,None) }
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
						| Some(vt), Some(et) when not (type_iseq (run_follow gen vt) (run_follow gen et)) ->
							(* check if has value and convert *)
							let vlocal_fst, vlocal = get_local (run v) in
							{
								eexpr = TIf(
									has_value vlocal_fst,
									handle_wrap (mk_cast et (unwrap_null vlocal)) et,
									Some( handle_wrap (null et e.epos) et ));
								etype = e.etype;
								epos = e.epos
							}
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
											Type.map_expr run e
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
														{ eexpr = TVar(v, Some evars); etype = gen.gcon.basic.tvoid; epos = e.epos };
														{ e with eexpr = TBinop( Ast.OpAssign, e1, handle_wrap { e with eexpr = TBinop (op, handle_unwrap t1 e1, handle_unwrap t2 (run e2) ) } t1 ) }
													]) }
											)
										| _ -> assert false
									)

								| _ ->
									Type.map_expr run e (* casts are already dealt with normal CastDetection module *)
							)
						| Ast.OpEq | Ast.OpNotEq ->
							(match e1.eexpr, e2.eexpr with
								| TConst(TNull), _ when is_some e2_t ->
									let e = has_value (run e2) in
									if op = Ast.OpEq then
										{ e with eexpr = TUnop(Ast.Not, Ast.Prefix, e) }
									else
										e
								| _, TConst(TNull) when is_some e1_t ->
									let e = has_value (run e1) in
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
						| Ast.OpAdd when is_string e1.etype || is_string e2.etype ->
							let e1 = if is_some e1_t then
								null_to_dynamic (run e1)
							else
								run e1
							in
							let e2 = if is_some e2_t then
								null_to_dynamic (run e2)
							else
								run e2
							in
							let e_t = is_null_t e.etype in
							if is_some e_t then
								wrap_val { eexpr = TBinop(op,e1,e2); etype = get e_t; epos = e.epos } (get e_t) true
							else
								{ e with eexpr = TBinop(op,e1,e2) }
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
		let run e = match e.eexpr with
			| TFunction tf ->
				run { e with eexpr = TFunction { tf with tf_expr = mk_block tf.tf_expr } }
			| TBlock _ ->
				run e
			| _ -> match run (mk_block e) with
				| { eexpr = TBlock([e]) } -> e
				| e -> e
		in
		let map e = Some(run e) in
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

	let configure gen native_array_cl =
		let rec run e =
			match e.eexpr with
			| TArrayDecl el ->
				let cl, params = match follow e.etype with
					| TInst(({ cl_path = ([], "Array") } as cl), ( _ :: _  as params)) -> cl, params
					| TInst(({ cl_path = ([], "Array") } as cl), []) -> cl, [t_dynamic]
					| _ -> assert false
				in
				let changed_params = gen.greal_type_param (TClassDecl cl) params in
				{ e with eexpr = TNew(cl, changed_params, [ { e with eexpr = TArrayDecl(List.map run el); etype = TInst(native_array_cl, changed_params) } ]	); }
			| _ -> Type.map_expr run e
		in
		let map e = Some(run e) in
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

	let configure gen (change_loop:texpr->int->add_to_block_api->texpr) (change_break:texpr->int->add_to_block_api->texpr) =
		let in_switch = ref false in
		let cur_block = ref [] in
		let to_add = ref [] in
		let did_found = ref (-1) in

		let api expr before =
			if before then cur_block := expr :: !cur_block else to_add := expr :: !to_add
		in
		let num = ref 0 in
		let cur_num = ref 0 in

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
				let last_num = !cur_num in
				in_switch := false;
				incr num;
				cur_num := !num;
				did_found := -1;
					let new_e = Type.map_expr run e in (* assuming that no loop will be found in the condition *)
					let new_e = if !did_found <> -1 then change_loop new_e !did_found api else new_e in
				did_found := last_found;
				in_switch := last_switch;
				cur_num := last_num;

				new_e
			| TSwitch _ ->
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
				if !in_switch then (did_found := !cur_num; change_break e !cur_num api) else e
			| _ -> Type.map_expr run e
		in
		let map e = Some(run e) in
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
			| TParenthesis(e) | TMeta(_,e) -> get_constant_expr e
			| _ -> None

	let traverse gen java_mode =
		let basic = gen.gcon.basic in
		let should_warn = false in

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

		let handle_case = fun (expr,kind) ->
			match kind with
			| Normal when has_fallback expr -> expr
			| Normal -> Type.concat expr (mk_sbreak expr.epos)
			| BreaksLoop | BreaksFunction -> expr
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
					let changed = if not (ExtType.is_void tf.tf_type) && kind <> BreaksFunction then
						Type.concat changed { eexpr = TReturn( Some (null tf.tf_type expr.epos) ); etype = basic.tvoid; epos = expr.epos }
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
						match get_constant_expr cond, flag, !has_break with
							| Some (TBool true), _, false ->
								has_break := last_has_break;
								{ expr with eexpr = TWhile(cond, block, flag) }, BreaksFunction
							| Some (TBool false), NormalWhile, _ ->
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

	let configure gen java_mode =
		let run = traverse gen java_mode in
		let map e = Some(run e) in
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
		It must run before OverloadingConstructor, since OverloadingConstructor will change optional structures behavior
*)
module DefaultArguments =
struct
	let name = "default_arguments"
	let priority = solve_deps name [ DBefore OverloadingConstructor.priority ]

	let gen_check basic t nullable_var const pos =
		let is_null t = match t with TType({t_path = ([],"Null")}, _) -> true | _ -> false in
		let needs_cast t1 t2 = match is_null t1, is_null t2 with
			| true, false | false, true -> true
			| _ -> false
		in

		let const_t = match const with
			| TString _ -> basic.tstring | TInt _ -> basic.tint | TFloat _ -> basic.tfloat
			| TNull -> t | TBool _ -> basic.tbool | _ -> assert false
		in
		let const = { eexpr = TConst(const); etype = const_t; epos = pos } in
		let const = if needs_cast t const_t then mk_cast t const else const in

		let arg = mk_local nullable_var pos in
		let arg = if needs_cast t nullable_var.v_type then mk_cast t arg else arg in

		{
			eexpr = TIf(
				{ eexpr = TBinop(Ast.OpEq, mk_local nullable_var pos, null nullable_var.v_type pos); etype = basic.tbool; epos = pos },
				const,
				Some(arg)
			);
			etype = t;
			epos = pos;
		}

	let add_opt gen block pos (var,opt) =
		match opt with
		| None | Some TNull ->
			(var,opt)
		| Some (TString str) ->
			block := Codegen.set_default gen.gcon var (TString str) pos :: !block;
			(var, opt)
		| Some const ->
			let basic = gen.gcon.basic in
			let nullable_var = mk_temp gen var.v_name (basic.tnull var.v_type) in
			let orig_name = var.v_name in
			var.v_name <- nullable_var.v_name;
			nullable_var.v_name <- orig_name;
			(* var v = (temp_var == null) ? const : cast temp_var; *)
			let evar = mk (TVar(var, Some(gen_check basic var.v_type nullable_var const pos))) basic.tvoid pos in
			block := evar :: !block;
			(nullable_var, opt)

	let rec change_func gen cf =
		List.iter (change_func gen) cf.cf_overloads;
		let is_ctor = cf.cf_name = "new" in
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
						let arg_assoc = List.map2 (fun (v,o) (v2,_) -> v,(v2,o) ) tf.tf_args tf_args in
						let rec extract_super e = match e.eexpr with
							| TBlock(({ eexpr = TCall({ eexpr = TConst TSuper }, _) } as e2) :: tl) ->
								e2, tl
							| TBlock(hd :: tl) ->
								let e2, tl2 = extract_super hd in
								e2, tl2 @ tl
							| _ -> raise Not_found
						in
						let block = try
							if not is_ctor then raise Not_found;
							(* issue #2570 *)
							(* check if the class really needs the super as the first statement -
							just to make sure we don't inadvertently break any existing code *)
							let rec check cl =
								if not (is_hxgen (TClassDecl cl)) then
									()
								else match cl.cl_super with
									| None ->
										raise Not_found
									| Some (cl,_) ->
										check cl
							in
							(match gen.gcurrent_class with
								| Some cl -> check cl
								| _ -> ());
							let super, tl = extract_super tf.tf_expr in
							(match super.eexpr with
								| TCall({ eexpr = TConst TSuper } as e1, args) ->
									(* any super argument will be replaced by an inlined version of the check *)
									let found = ref false in
									let rec replace_args e = match e.eexpr with
										| TLocal(v) -> (try
											let v2,o = List.assq v arg_assoc in
											let o = match o with
												| None -> raise Not_found
												| Some o -> o
											in
											found := true;
											gen_check gen.gcon.basic v.v_type v2 o e.epos
										with | Not_found -> e)
										| _ -> Type.map_expr replace_args e
									in
									let args = List.map (replace_args) args in
									{ tf.tf_expr with eexpr = TBlock((if !found then { super with eexpr = TCall(e1,args) } else super) :: !block @ tl) }
								| _ -> assert false)
							with | Not_found ->
								Type.concat { tf.tf_expr with eexpr = TBlock(!block); etype = basic.tvoid } tf.tf_expr
						in

						args := fun_args tf_args;
						cf.cf_expr <- Some( {texpr with eexpr = TFunction( { tf with
							tf_args = tf_args;
							tf_expr = block
						} ); etype = TFun(!args, ret) } );
						cf.cf_type <- TFun(!args, ret)

					| _ -> ()
				);
				(if !found then cf.cf_type <- TFun(!args, ret))
			| _, _ -> assert false

	let configure gen =
		let run md =
			(match md with
			| TClassDecl cl ->
				List.iter (change_func gen) cl.cl_ordered_fields;
				List.iter (change_func gen) cl.cl_ordered_statics;
				Option.may (change_func gen) cl.cl_constructor;
			| _ -> ());
			Some(md);
		in
		gen.gmodule_filters#add ~name:name ~priority:(PCustom priority) run

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
*)
module InterfaceVarsDeleteModf =
struct
	let name = "interface_vars"
	let priority = solve_deps name []

	let configure gen =
		let run md =
			match md with
			| TClassDecl ({ cl_interface = true } as cl) ->
				let to_add = ref [] in
				let fields = List.filter (fun cf ->
					match cf.cf_kind with
					| Var _ when gen.gcon.platform = Cs && Meta.has Meta.Event cf.cf_meta ->
						true
					| Var vkind when not (Type.is_extern_field cf && Meta.has Meta.Property cf.cf_meta) ->
						(match vkind.v_read with
							| AccCall ->
								let newcf = mk_class_field ("get_" ^ cf.cf_name) (TFun([],cf.cf_type)) true cf.cf_pos (Method MethNormal) [] in
								to_add := newcf :: !to_add;
							| _ -> ()
						);
						(match vkind.v_write with
							| AccCall ->
								let newcf = mk_class_field ("set_" ^ cf.cf_name) (TFun(["val",false,cf.cf_type],cf.cf_type)) true cf.cf_pos (Method MethNormal) [] in
								to_add := newcf :: !to_add;
							| _ -> ()
						);
						cl.cl_fields <- PMap.remove cf.cf_name cl.cl_fields;
						false
					| Method MethDynamic ->
						(* TODO OPTIMIZATION - add a `_dispatch` method to the interface which will call the dynamic function itself *)
						cl.cl_fields <- PMap.remove cf.cf_name cl.cl_fields;
						false
					| _ ->
						true
				) cl.cl_ordered_fields in

				cl.cl_ordered_fields <- fields;

				List.iter (fun cf ->
					match field_access gen (TInst(cl,List.map snd cl.cl_params)) cf.cf_name with
					| FNotFound | FDynamicField _ ->
						cl.cl_ordered_fields <- cf :: cl.cl_ordered_fields;
						cl.cl_fields <- PMap.add cf.cf_name cf cl.cl_fields
					| _ ->
						()
				) !to_add
			| _ -> ()
		in
		let map md = Some(run md; md) in
		gen.gmodule_filters#add ~name:name ~priority:(PCustom priority) map
end;;


(* ******************************************* *)
(* InterfaceProps *)
(* ******************************************* *)
(*
	This module filter will go through all declared properties, and see if they are conforming to a native interface.
	If they are, it will add Meta.Property to it.
*)
module InterfaceProps =
struct
	let name = "interface_props"
	let priority = solve_deps name []

	let configure gen =
		let run md =
			match md with
			| TClassDecl ({ cl_interface = false; cl_extern = false } as cl) ->
				let vars = List.fold_left (fun acc (iface,_) ->
					if Meta.has Meta.CsNative iface.cl_meta then
						List.filter (fun cf -> match cf.cf_kind with
							| Var { v_read = AccCall } | Var { v_write = AccCall } -> true
							| _ -> false
						) iface.cl_ordered_fields @ acc
					else
						acc
				) [] cl.cl_implements in
				let vars = List.map (fun cf -> cf.cf_name) vars in
				if vars <> [] then
					List.iter (fun cf -> match cf.cf_kind with
						| Var { v_read = AccCall } | Var { v_write = AccCall } when List.mem cf.cf_name vars ->
							cf.cf_meta <- (Meta.Property, [], Ast.null_pos) :: cf.cf_meta
						| _ -> ()
					) cl.cl_ordered_fields
			| _ ->
				()
		in
		let map md = Some(run md; md) in
		gen.gmodule_filters#add ~name:name ~priority:(PCustom priority) map
end;;


(* ******************************************* *)
(* Int Division Synf *)
(* ******************************************* *)
(*
	On targets that support int division, this module will force a float division to be performed.
	It will also look for casts to int or use of Std.int() to optimize this kind of operation.

	dependencies:
		since it depends on nothing, but many modules might generate division expressions,
		it will be one of the last modules to run
*)
module IntDivisionSynf =
struct
	let name = "int_division_synf"
	let priority = solve_deps name [ DAfter ExpressionUnwrap.priority; DAfter ObjectDeclMap.priority; DAfter ArrayDeclSynf.priority ]

	let configure gen =
		let basic = gen.gcon.basic in
		let rec is_int t = match follow t with
			| TInst({ cl_path = (["haxe";"lang"],"Null") }, [t]) -> is_int t
			| t ->
				like_int t && not (like_i64 t)
		in
		let is_exactly_int t = match follow t with
			| TAbstract ({ a_path=[],"Int" }, []) -> true
			| _ -> false
		in
		let rec run e =
			match e.eexpr with
			| TBinop((Ast.OpDiv as op), e1, e2) when is_int e1.etype && is_int e2.etype ->
				{ e with eexpr = TBinop(op, mk_cast basic.tfloat (run e1), run e2) }
			| TCall(
					{ eexpr = TField(_, FStatic({ cl_path = ([], "Std") }, { cf_name = "int" })) },
					[ ({ eexpr = TBinop((Ast.OpDiv as op), e1, e2) } as ebinop ) ]
				) when is_int e1.etype && is_int e2.etype ->
				let e = { ebinop with eexpr = TBinop(op, run e1, run e2); etype = basic.tint } in
				if not (is_exactly_int e1.etype && is_exactly_int e2.etype) then
					mk_cast basic.tint e
				else
					e
			| TCast( ({ eexpr = TBinop((Ast.OpDiv as op), e1, e2) } as ebinop ), _ )
			| TCast( ({ eexpr = TBinop(( (Ast.OpAssignOp Ast.OpDiv) as op), e1, e2) } as ebinop ), _ ) when is_int e1.etype && is_int e2.etype && is_int e.etype ->
				let ret = { ebinop with eexpr = TBinop(op, run e1, run e2); etype = e.etype } in
				if not (is_exactly_int e1.etype && is_exactly_int e2.etype) then
					mk_cast e.etype ret
				else
					e
			| _ ->
				Type.map_expr run e
		in

		let map e = Some(run e) in
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

	let configure gen =
		let rec take_off_cast run e =
			match e.eexpr with
			| TCast (c, _) -> take_off_cast run c
			| _ -> run e
		in
		let rec traverse e =
			match e.eexpr with
			| TBlock bl ->
				let bl = List.map (take_off_cast traverse) bl in
				{ e with eexpr = TBlock bl }
			| TTry (block, catches) ->
				{ e with eexpr = TTry(traverse (mk_block block), List.map (fun (v,block) -> (v, traverse (mk_block block))) catches) }
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
		let map e = Some(traverse e) in
		gen.gsyntax_filters#add ~name:name ~priority:(PCustom priority) map
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

	let configure gen =
		let run md =
			(match md with
			| TClassDecl ({ cl_kind = KAbstractImpl a } as c) ->
				List.iter (
					function
					| ({ cf_name = "_new" } as cf) ->
						cf.cf_params <- cf.cf_params @ a.a_params
					| cf when Meta.has Meta.Impl cf.cf_meta ->
						(match cf.cf_expr with
						| Some({ eexpr = TFunction({ tf_args = (v, _) :: _ }) }) when Meta.has Meta.This v.v_meta ->
							cf.cf_params <- cf.cf_params @ a.a_params
						| _ -> ())
					| _ -> ()
				) c.cl_ordered_statics
			| _ -> ());
			Some md
		in
		gen.gmodule_filters#add ~name:name ~priority:(PCustom priority) run
end;;

(* ******************************************* *)
(* FixOverrides *)
(* ******************************************* *)

(*

	Covariant return types, contravariant function arguments and applied type parameters may change
	in a way that expected implementations / overrides aren't recognized as such.
	This filter will fix that.

	dependencies:
		FixOverrides expects that the target platform is able to deal with overloaded functions
		It must run after DefaultArguments, otherwise code added by the default arguments may be invalid

*)

module FixOverrides =
struct

	let name = "fix_overrides"

	let priority = solve_deps name [DAfter DefaultArguments.priority]

	(*
		if the platform allows explicit interface implementation (C#),
		specify a explicit_fn_name function (tclass->string->string)
		Otherwise, it expects the platform to be able to handle covariant return types
	*)
	let run ~explicit_fn_name ~get_vmtype gen =
		let implement_explicitly = is_some explicit_fn_name in
		let run md = match md with
			| TClassDecl ( { cl_interface = true; cl_extern = false } as c ) ->
				(* overrides can be removed from interfaces *)
				c.cl_ordered_fields <- List.filter (fun f ->
					try
						if Meta.has Meta.Overload f.cf_meta then raise Not_found;
						let f2 = Codegen.find_field gen.gcon c f in
						if f2 == f then raise Not_found;
						c.cl_fields <- PMap.remove f.cf_name c.cl_fields;
						false;
					with Not_found ->
						true
				) c.cl_ordered_fields;
				md
			| TClassDecl({ cl_extern = false } as c) ->
				let this = { eexpr = TConst TThis; etype = TInst(c,List.map snd c.cl_params); epos = c.cl_pos } in
				(* look through all interfaces, and try to find a type that applies exactly *)
				let rec loop_iface (iface:tclass) itl =
					List.iter (fun (s,stl) -> loop_iface s (List.map (apply_params iface.cl_params itl) stl)) iface.cl_implements;
					let real_itl = gen.greal_type_param (TClassDecl iface) itl in
					let rec loop_f f =
						List.iter loop_f f.cf_overloads;
						let ftype = apply_params iface.cl_params itl f.cf_type in
						let real_ftype = get_real_fun gen (apply_params iface.cl_params real_itl f.cf_type) in
						replace_mono real_ftype;
						let overloads = Overloads.get_overloads c f.cf_name in
						try
							let t2, f2 =
								match overloads with
								| (_, cf) :: _ when Meta.has Meta.Overload cf.cf_meta -> (* overloaded function *)
									(* try to find exact function *)
									List.find (fun (t,f2) ->
										Overloads.same_overload_args ~get_vmtype ftype t f f2
									) overloads
								| _ :: _ ->
									(match field_access gen (TInst(c, List.map snd c.cl_params)) f.cf_name with
									| FClassField(_,_,_,f2,false,t,_) -> t,f2 (* if it's not an overload, all functions should have the same signature *)
									| _ -> raise Not_found)
								| [] -> raise Not_found
							in
							replace_mono t2;
							(* if we find a function with the exact type of real_ftype, it means this interface has already been taken care of *)
							if not (type_iseq (get_real_fun gen (apply_params f2.cf_params (List.map snd f.cf_params) t2)) real_ftype) then begin
								(match f.cf_kind with | Method (MethNormal | MethInline) -> () | _ -> raise Not_found);
								let t2 = get_real_fun gen t2 in
								if List.length f.cf_params <> List.length f2.cf_params then raise Not_found;
								replace_mono t2;
								match follow (apply_params f2.cf_params (List.map snd f.cf_params) t2), follow real_ftype with
								| TFun(a1,r1), TFun(a2,r2) when not implement_explicitly && not (type_iseq r1 r2) && Overloads.same_overload_args ~get_vmtype real_ftype t2 f f2 ->
									(* different return types are the trickiest cases to deal with *)
									(* check for covariant return type *)
									let is_covariant = match follow r1, follow r2 with
										| _, TDynamic _ -> true
										| r1, r2 -> try
											unify r1 r2;
											true
										with | Unify_error _ -> false
									in
									(* we only have to worry about non-covariant issues *)
									if not is_covariant then begin
										(* override return type and cast implemented function *)
										let args, newr = match follow t2, follow (apply_params f.cf_params (List.map snd f2.cf_params) real_ftype) with
											| TFun(a,_), TFun(_,r) -> a,r
											| _ -> assert false
										in
										f2.cf_type <- TFun(args,newr);
										(match f2.cf_expr with
										| Some ({ eexpr = TFunction tf } as e) ->
												f2.cf_expr <- Some { e with eexpr = TFunction { tf with tf_type = newr } }
										| _ -> ())
									end
								| TFun(a1,r1), TFun(a2,r2) ->
									(* just implement a function that will call the main one *)
									let name, is_explicit = match explicit_fn_name with
										| Some fn when not (type_iseq r1 r2) && Overloads.same_overload_args ~get_vmtype real_ftype t2 f f2 ->
												fn iface itl f.cf_name, true
										| _ -> f.cf_name, false
									in
									let p = f2.cf_pos in
									let newf = mk_class_field name real_ftype true f.cf_pos (Method MethNormal) f.cf_params in
									let vars = List.map (fun (n,_,t) -> alloc_var n t) a2 in

									let args = List.map2 (fun v (_,_,t) -> mk_cast t (mk_local v f2.cf_pos)) vars a1 in
									let field = { eexpr = TField(this, FInstance(c,List.map snd c.cl_params,f2)); etype = TFun(a1,r1); epos = p } in
									let call = { eexpr = TCall(field, args); etype = r1; epos = p } in
									(* let call = gen.gparam_func_call call field (List.map snd f.cf_params) args in *)
									let is_void = ExtType.is_void r2 in

									newf.cf_expr <- Some {
										eexpr = TFunction({
											tf_args = List.map (fun v -> v,None) vars;
											tf_type = r2;
											tf_expr = (if is_void then call else {
												eexpr = TReturn (Some (mk_cast r2 call));
												etype = r2;
												epos = p
											})
										});
										etype = real_ftype;
										epos = p;
									};
									(try
										let fm = PMap.find name c.cl_fields in
										fm.cf_overloads <- newf :: fm.cf_overloads
									with | Not_found ->
										c.cl_fields <- PMap.add name newf c.cl_fields;
										c.cl_ordered_fields <- newf :: c.cl_ordered_fields)
								| _ -> assert false
							end
						with | Not_found -> ()
					in
					List.iter (fun f -> match f.cf_kind with | Var _ -> () | _ -> loop_f f) iface.cl_ordered_fields
				in
				List.iter (fun (iface,itl) -> loop_iface iface itl) c.cl_implements;
				(* now go through all overrides, *)
				let rec check_f f =
					(* find the first declared field *)
					let is_overload = Meta.has Meta.Overload f.cf_meta in
					let decl = if is_overload then
						find_first_declared_field gen c ~get_vmtype ~exact_field:f f.cf_name
					else
						find_first_declared_field gen c ~get_vmtype f.cf_name
					in
					match decl with
					| Some(f2,actual_t,_,t,declared_cl,_,_)
						when not (Overloads.same_overload_args ~get_vmtype actual_t (get_real_fun gen f.cf_type) f2 f) ->
							(match f.cf_expr with
							| Some({ eexpr = TFunction(tf) } as e) ->
								let actual_args, _ = get_fun (get_real_fun gen actual_t) in
								let new_args, vardecl = List.fold_left2 (fun (args,vdecl) (v,_) (_,_,t) ->
									if not (type_iseq (gen.greal_type v.v_type) (gen.greal_type t)) then begin
										let new_var = mk_temp gen v.v_name t in
										(new_var,None) :: args, (v, Some(mk_cast v.v_type (mk_local new_var f.cf_pos))) :: vdecl
									end else
										(v,None) :: args, vdecl
								) ([],[]) tf.tf_args actual_args in
								let block = { eexpr = TBlock(List.map (fun (v,ve) ->
									{
										eexpr = TVar(v,ve);
										etype = gen.gcon.basic.tvoid;
										epos = tf.tf_expr.epos
									}) vardecl);
									etype = gen.gcon.basic.tvoid;
									epos = tf.tf_expr.epos
								} in
								if Meta.has Meta.Overload f.cf_meta then begin
									(* if it is overload, create another field with the requested type *)
									let f3 = mk_class_field f.cf_name t f.cf_public f.cf_pos f.cf_kind f.cf_params in
									let p = f.cf_pos in
									let old_args, old_ret = get_fun f.cf_type in
									let args, ret = get_fun t in
									let tf_args = List.map (fun (n,o,t) -> alloc_var n t, None) args in
									let f3_mk_return = if ExtType.is_void ret then (fun e -> e) else (fun e -> mk_return (mk_cast ret e)) in
									f3.cf_expr <- Some {
										eexpr = TFunction({
											tf_args = List.rev new_args;
											tf_type = ret;
											tf_expr = Type.concat block (mk_block (f3_mk_return {
												eexpr = TCall(
													{
														eexpr = TField(
															{ eexpr = TConst TThis; etype = TInst(c, List.map snd c.cl_params); epos = p },
															FInstance(c,List.map snd c.cl_params,f));
														etype = f.cf_type;
														epos = p
													},
													List.map2 (fun (v,_) (_,_,t) -> mk_cast t (mk_local v p)) tf_args old_args);
												etype = old_ret;
												epos = p
											}))
										});
										etype = t;
										epos = p;
									};
									(* make sure we skip cast detect - otherwise this new function will make the overload detection go crazy *)
									f3.cf_meta <- (Meta.Custom(":skipCastDetect"), [], f3.cf_pos) :: f3.cf_meta;
									gen.gafter_expr_filters_ended <- ((fun () ->
										f.cf_overloads <- f3 :: f.cf_overloads;
									) :: gen.gafter_expr_filters_ended);
									f3
								end else begin
									(* if it's not overload, just cast the vars *)
									if vardecl <> [] then
									f.cf_expr <- Some({ e with
										eexpr = TFunction({ tf with
											tf_args = List.rev new_args;
											tf_expr = Type.concat block tf.tf_expr
										});
									});
									f
								end
							| _ -> f)
					| _ -> f
				in
				if not c.cl_extern then
					c.cl_overrides <- List.map (fun f -> check_f f) c.cl_overrides;
				md
			| _ -> md
		in
		run

	let configure ?explicit_fn_name ~get_vmtype gen =
		let delay () =
			Hashtbl.clear gen.greal_field_types
		in
		gen.gafter_mod_filters_ended <- delay :: gen.gafter_mod_filters_ended;
		let run = run ~explicit_fn_name ~get_vmtype gen in
		let map md = Some(run md) in
		gen.gmodule_filters#add ~name:name ~priority:(PCustom priority) map
end;;


(* ******************************************* *)
(* Normalize *)
(* ******************************************* *)
(*
	- Filters out enum constructor type parameters from the AST; See Issue #1796
	- Filters out monomorphs
	- Filters out all non-whitelisted AST metadata

	dependencies:
		No dependencies; but it still should be one of the first filters to run,
		as it will help normalize the AST
*)
module Normalize =
struct
	let name = "normalize_type"
	let priority = max_dep

	let rec filter_param t =
		match t with
		| TInst({ cl_kind = KTypeParameter _ } as c,_) when Meta.has Meta.EnumConstructorParam c.cl_meta ->
			t_dynamic
		| TMono r ->
			(match !r with
			| None -> t_dynamic
			| Some t -> filter_param t)
		| TInst(_,[]) | TEnum(_,[]) | TType(_,[]) | TAbstract(_,[]) ->
			t
		| TType(t,tl) ->
			TType(t,List.map filter_param tl)
		| TInst(c,tl) ->
			TInst(c,List.map filter_param tl)
		| TEnum(e,tl) ->
			TEnum(e,List.map filter_param tl)
		| TAbstract({ a_path = (["haxe";"extern"],"Rest") } as a,tl) ->
			TAbstract(a, List.map filter_param tl)
		| TAbstract(a,tl) when not (Meta.has Meta.CoreType a.a_meta) ->
			filter_param (Abstract.get_underlying_type a tl)
		| TAbstract(a,tl) ->
			TAbstract(a, List.map filter_param tl)
		| TAnon a ->
			TAnon {
				a_fields = PMap.map (fun f -> { f with cf_type = filter_param f.cf_type }) a.a_fields;
				a_status = a.a_status;
			}
		| TFun(args,ret) ->
			TFun(List.map (fun (n,o,t) -> (n,o,filter_param t)) args, filter_param ret)
		| TDynamic _ ->
			t
		| TLazy f ->
			filter_param (!f())

	let configure gen ~metas =
		let rec run e =
			match e.eexpr with
			| TMeta (entry, e) when not (Hashtbl.mem metas entry) ->
				run e
			| _ ->
				map_expr_type (fun e -> run e) filter_param (fun v -> v.v_type <- filter_param v.v_type; v) e
		in
		let map e = Some (run e) in
		gen.gexpr_filters#add ~name:name ~priority:(PCustom priority) map;

		let run md =
			match md with
			| TClassDecl cl ->
				let rec map cf =
					cf.cf_type <- filter_param cf.cf_type;
					List.iter map cf.cf_overloads
				in
				List.iter map cl.cl_ordered_fields;
				List.iter map cl.cl_ordered_statics;
				Option.may map cl.cl_constructor
			| _ ->
				()
		in
		let map md = Some (run md; md) in
		gen.gmodule_filters#add ~name:name ~priority:(PCustom priority) map

end;;


(* ******************************************* *)
(* InterfaceMetas *)
(* ******************************************* *)
(*
	Deal with metadata on interfaces by taking it off from interface, and adding a new class with `_HxMeta` suffix

	dependencies:
		Must run before InitFunction
*)
module InterfaceMetas =
struct
	let name = "interface_metas"
	let priority = solve_deps name [ DBefore InitFunction.priority ]

	let configure gen =
		let run md =
			match md with
			| TClassDecl ({ cl_interface = true; cl_ordered_statics = (_ :: _) } as cl) ->
				cl.cl_ordered_statics <- [];
				let path = fst cl.cl_path,snd cl.cl_path ^ "_HxMeta" in
				(match Codegen.build_metadata gen.gcon (TClassDecl cl) with
				| Some expr ->
					let ncls = mk_class cl.cl_module path cl.cl_pos in
					let cf = mk_class_field "__meta__" expr.etype false expr.epos (Var { v_read = AccNormal; v_write = AccNormal }) [] in
					cf.cf_expr <- Some expr;
					ncls.cl_statics <- PMap.add "__meta__" cf ncls.cl_statics;
					ncls.cl_ordered_statics <- cf :: ncls.cl_ordered_statics;
					gen.gadd_to_module (TClassDecl(ncls)) priority;
				| _ -> ())
			| _ -> ()
		in
		let map md = run md; Some(md) in
		gen.gmodule_filters#add ~name:name ~priority:(PCustom priority) map
end;;
