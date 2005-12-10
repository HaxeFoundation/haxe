extern class String {

	var length : Int;

	function new(string:String) : Void;

	function toUpperCase() : String;
	function toLowerCase() : String;

	function charAt( index : Int) : String;
	function charCodeAt( index : Int) : Int;

	function indexOf( value : String, startIndex : Int ) : Int;
	function lastIndexOf( value : String, startIndex : Int ) : Int;
	function split( delimiter : String ) : Array<String>;
	function substr( pos : Int, len : Int ) : String;

	function toString() : String;

}
