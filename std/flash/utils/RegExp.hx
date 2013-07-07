package flash.utils;

@:native("RegExp") extern class RegExp implements Dynamic {
	var dotall(default,null) : Bool;
	var extended(default,null) : Bool;
	var global(default,null) : Bool;
	var ignoreCase(default,null) : Bool;
	var lastIndex : Int;
	var multiline(default,null) : Bool;
	var source(default,null) : String;
	function new(?pattern : Dynamic, ?options : Dynamic) : Void;
	@:ns("http://adobe.com/AS3/2006/builtin") function exec(?s : String) : Dynamic;
	@:ns("http://adobe.com/AS3/2006/builtin") function test(?s : String) : Bool;
}
