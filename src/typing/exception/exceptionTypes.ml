open Common
open Typecore
open Type

type exception_context = {
	typer : typer;
	basic : basic_types;
	config : exceptions_config;
	wildcard_catch_type : Type.t;
	base_throw_type : Type.t;
	throws_anything : bool;
	catches_anything : bool;
	haxe_exception_class : tclass;
	haxe_exception_type : Type.t;
	haxe_native_stack_trace : tclass;
	value_exception_type : Type.t;
	value_exception_class : tclass;
}