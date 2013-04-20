@:nativeGen extern class String {
	var length(default, null) : Int;
	function new(string:String) : Void;
	function toUpperCase() : String;
	function toLowerCase() : String;
	function charAt( index : Int) : String;
	function charCodeAt( index : Int) : Null<Int>;
	function indexOf( str : String, ?startIndex : Int ) : Int;
	function lastIndexOf( str : String, ?startIndex : Int ) : Int;
	function split( delimiter : String ) : Array<String>;
	function substr( pos : Int, ?len : Int ) : String;
	function substring( startIndex : Int, ?endIndex : Int ) : String;
	function toString() : String;
	public static function fromCharCode( code : Int ) : String;
}