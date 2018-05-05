package flash.errors;

@:require(flash10_1) extern class DRMManagerError extends Error {
	var subErrorID(default,never) : Int;
	function new(message : String, id : Int, subErrorID : Int) : Void;
	function toString() : String;
}
