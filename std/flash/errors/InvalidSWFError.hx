package flash.errors;

extern class InvalidSWFError extends Error {
	function new(?message : String, id : Int = 0) : Void;
}
