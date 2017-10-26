package js;

import haxe.extern.Rest;

/**
	Generate JavaScript syntax not directly supported by Haxe.
	Use only at low-level when specific target-specific code-generation is required.
**/
extern class Syntax {
	/**
		Inject `code` directly into generated source.

		`code` must be a string constant.

		Additional `args` are supported to provide code interpolation, for example:
		```
		Syntax.code("console.log({0}, {1})", "hi", 42);
		```
		will generate
		```
		console.log("hi", 42);
		```
	**/
	static function code(code:String, args:Rest<Dynamic>):Dynamic;

	/**
		Generate `new cl(...args)` expression.
	**/
	@:overload(function(cl:String, args:Rest<Dynamic>):Dynamic {})
	static function new_<T>(cl:Class<T>, args:Rest<Dynamic>):T;

	/**
		Generate `v instanceof cl` expression.
	**/
	@:pure static function instanceof(v:Dynamic, cl:Class<Dynamic>):Bool;

	/**
		Generate `typeof o` expression.
	**/
	@:pure static function typeof(o:Dynamic):String;

	/**
		Genearte `a === b` expression.
	**/
	@:pure static function strictEq(a:Dynamic, b:Dynamic):Bool;

	/**
		Genearte `a !== b` expression.
	**/
	@:pure static function strictNeq(a:Dynamic, b:Dynamic):Bool;

	/**
		Generate `delete o[f]` expression.
	**/
	@:overload(function(o:Dynamic, f:Int):Bool {})
	static function delete(o:Dynamic, f:String):Bool;
}
