package flash.errors;

extern class IllegalOperationError extends Error {
	function new(?message : String, id : Int = 0) : Void;
}
