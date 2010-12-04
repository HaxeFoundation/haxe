package flash.errors;

extern class StackOverflowError extends Error {
	function new(?message : String, id : Int = 0) : Void;
}
