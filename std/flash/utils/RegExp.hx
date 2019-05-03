package flash.utils;

@:realPath("flash.utils.RegExp") @:native("RegExp") extern class RegExp implements Dynamic {
	var dotall(get,never) : Bool;
	var extended(get,never) : Bool;
	var global(get,never) : Bool;
	var ignoreCase(get,never) : Bool;
	var lastIndex(get,set) : Int;
	var multiline(get,never) : Bool;
	var source(get,never) : String;
	function new(?pattern : Dynamic, ?options : Dynamic) : Void;
	@:ns("http://adobe.com/AS3/2006/builtin") function exec(?s : String) : Dynamic;
	private function get_dotall() : Bool;
	private function get_extended() : Bool;
	private function get_global() : Bool;
	private function get_ignoreCase() : Bool;
	private function get_lastIndex() : Int;
	private function get_multiline() : Bool;
	private function get_source() : String;
	private function set_lastIndex(value : Int) : Int;
	@:ns("http://adobe.com/AS3/2006/builtin") function test(?s : String) : Bool;
}
