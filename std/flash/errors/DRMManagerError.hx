package flash.errors;

@:require(flash10_1) extern class DRMManagerError extends Error {
	var subErrorID(default,null) : Int;
	function new(message : String, id : Int, subErrorID : Int) : Void;
	function toString() : String;
}
