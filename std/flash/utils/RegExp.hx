package flash.utils;

@:realPath("flash.utils.RegExp") @:native("RegExp") extern class RegExp implements Dynamic {
	var dotall(default,never) : Bool;
	var extended(default,never) : Bool;
	var global(default,never) : Bool;
	var ignoreCase(default,never) : Bool;
	var lastIndex : Int;
	var multiline(default,never) : Bool;
	var source(default,never) : String;
	function new(?pattern : Dynamic, ?options : Dynamic) : Void;
	@:ns("http://adobe.com/AS3/2006/builtin") function exec(?s : String) : Dynamic;
	@:ns("http://adobe.com/AS3/2006/builtin") function test(?s : String) : Bool;
}
